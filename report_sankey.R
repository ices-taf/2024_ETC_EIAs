# Library
library(icesTAF)
library(networkD3)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)
library(webshot2)

conf_agg <- function(x) {
  c("Low", "Medium", "High")[round(ceiling(x))]
}


# Load df saved after 00_preprocess.R script
unique(data$Sector)
unique(data$Pressure)
head(data)

# Reorganize df to source>target
data$id <- as.character(row.names(data))

# Filter low values for sankey
data <- data |> filter(ImpactRisk > 0.001)

links <-
  data |>
  # transmute(source = Sector, target = Pressure, value = ImpactRisk, linkgroup = Pressure) |>
  transmute(source = Sector, target = Pressure, value = ImpactRisk, linkgroup = Pressure, Confidence = Confidence) |>
  rbind(
    data |> transmute(source = Pressure, target = Ecological.Characteristic, value = ImpactRisk, linkgroup = Pressure, Confidence = Confidence)
    # data |> transmute(source = Pressure, target = Ecological.Characteristic, value = ImpactRisk, linkgroup = Pressure)
  ) |>
  group_by(source, target, linkgroup) |>
  summarise(
    value = sum(value) * 10,
    conf_num = ceiling(median(Confidence, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # summarise(value = sum(value) * 10) |>
  mutate(Confidence = c("Low", "Medium", "High")[conf_num]) |>
  select(-conf_num) |>
  arrange(value)

summary(links$value)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- links |>
  group_by(source) |>
  summarise(value = sum(value))
names(nodes) <- c("name", "value")
nodes1 <- links |>
  group_by(target) |>
  summarise(value = sum(value))
names(nodes1) <- c("name", "value")
nodes <- rbind(nodes, nodes1)
nodes <- nodes |>
  group_by(name) |>
  summarise(value = sum(value)) |>
  arrange(-value)
nodes$group <- "nodes"
nodes <- nodes |> select(-value)


# With networkD3, connection must be provided using id, not using real name like in the data dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1
nodes <- data.frame(nodes)
nodes$name <- as.character(nodes$name)
links <- data.frame(links)
links <- links |> arrange(value, IDsource, IDtarget)



# prepare color scale: I give one specific color for each node.
# n <- length(unique(links$linkgroup))
# pal <- brewer.pal(n, "Set2")[1:n]
# pal2 <- unlist(mapply(brewer.pal, n - nrow(pal), "Dark2"))
# pal <- rbind(pal, pal2)



pal <- c( "#f46d43", "#fdae61",  "#abdda4", "#66c2a5","#9e0142","#ff002b", "#3288bd", "#5e4fa2", "#2801d3", "#03a874")
# pal10 <- grDevices::colorRampPalette(pal, space = "rgb")(length(unique(links$linkgroup))
pal <- paste(shQuote(pal), collapse = ", ")
dom <- unique(links$linkgroup)
dom <- paste(shQuote(dom), collapse = ", ")

nodes$node_group <- "AllNodes"
my_color <-
  paste0("d3.scaleOrdinal().domain(['AllNodes',", dom, "]).range(['#6B7280',", pal, "])")



# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "IDsource", 
  Target = "IDtarget",
  Value = "value", 
  NodeID = "name",
  fontSize = 24, 
  units = "Impact Risk",
  nodeWidth = 28,
  colourScale = my_color,
  LinkGroup = "linkgroup", 
  NodeGroup = "node_group"
  # iterations = 0
)
p









# #####################################  confidence as link group #####################################
nodes$node_group <- "AllNodes"

# 2) Extend your colourScale domain to include the node group
col_scale <- 'd3.scaleOrdinal()
  .domain(["AllNodes","Low","Medium","High"])
  .range(["#6B7280","#f06a2e","#ffd60a","#03a874"])'

#### ---- Create sankey with confidence as link group ----
p2 <- sankeyNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "IDsource", 
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "Impact Risk",
  LinkGroup = "Confidence",
  NodeGroup = "node_group",
  fontSize = 24, 
  nodeWidth = 28,
  colourScale = col_scale
  # iterations = 0
)
p2
# ---- Inject legend + tooltips (rendered inside SVG) ----
p2 <- htmlwidgets::onRender(
  p2,
  '
  function(el, x) {
    const labels = ["Low","Medium","High"];
    const colors = ["#f06a2e","#ffd60a","#03a874"];

    // Layout: SVG + legend side by side
    const root = d3.select(el)
      .style("display","flex")
      .style("gap","12px")
      .style("align-items","flex-start");

    // Ensure the SVG is the first child (some browsers may reorder)
    const svg = root.select("svg");
    const svgWrap = root.insert("div", ":first-child")
      .style("flex","1 1 auto")
      .style("overflow","visible");
    svgWrap.node().appendChild(svg.node());

    // Legend as a separate HTML box
    const legend = root.append("div")
      .attr("class","legend-box")
      .style("flex","0 0 160px")
      .style("padding","8px 10px")
      .style("border","1px solid #ddd")
      .style("border-radius","6px")
      .style("background","#fff")
      .style("font","12px sans-serif");

    legend.append("div")
      .style("font-weight","600")
      .style("margin-bottom","6px")
      .text("Confidence");

    const row = legend.selectAll("div.item")
      .data(labels).enter().append("div")
      .attr("class","item")
      .style("display","flex")
      .style("align-items","center")
      .style("gap","8px")
      .style("margin","4px 0");

    row.append("span")
      .style("display","inline-block")
      .style("width","14px")
      .style("height","14px")
      .style("border","1px solid #999")
      .style("background",(d,i)=>colors[i]);

    row.append("span").text(d=>d);

    // Tooltips for links (d3 v5 signature: (event, d))
    d3.select(el).selectAll(".link")
      .on("mouseover", function(event, d) {
        const src = x.nodes[d.source.index].name;
        const tgt = x.nodes[d.target.index].name;
        const val = d.value;
        const conf = d.group;
        this.setAttribute("title", `${src} → ${tgt}\\nValue: ${val}\\nConfidence: ${conf}`);
      });
  }
  '
)


# save the widget
saveWidget(p, file = "sankey.html", selfcontained = TRUE)
saveWidget(p2, file = "sankey_confidence.html", selfcontained = TRUE)




# save the widget
webshot("sankey.html", "sankey.png", vwidth = 1200, vheight = 1200)
# webshot("sankey_confidence.html", "sankey_confidence.png", vwidth = 1200, vheight = 1200)
## set chrome path
# Sys.setenv(CHROMOTE_CHROME = "C:/Users/luca.lamoni/AppData/Local/Google/Chrome/Application/chrome.exe")
webshot2::webshot(
  "sankey_confidence.html",
  file    = "sankey_confidence.png",
  vwidth  = 1400,    # give room if legend is to the right
  vheight = 1200,
  delay   = 1,       # <- important
  cliprect = "viewport"
)


