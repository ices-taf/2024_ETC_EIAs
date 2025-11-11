# Library
library(icesTAF)
library(networkD3)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)

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
data <- data |> filter(ImpactRisk > 0.005)

links <-
  data |>
  transmute(source = Sector, target = Pressure, value = ImpactRisk, linkgroup = Pressure) |>
  # transmute(source = Sector, target = Pressure, value = ImpactRisk, linkgroup = Pressure, Confidence = Confidence) |>
  rbind(
    # data |> transmute(source = Pressure, target = Ecological.Characteristic, value = ImpactRisk, linkgroup = Pressure, Confidence = Confidence)
    data |> transmute(source = Pressure, target = Ecological.Characteristic, value = ImpactRisk, linkgroup = Pressure)
  ) |>
  group_by(source, target, linkgroup) |>
  # summarise(value = sum(value) * 10, Confidence = conf_agg(Confidence)) |>
  summarise(value = sum(value) * 10) |>
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
  Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
  Value = "value", NodeID = "name",
  fontSize = 17, units = "Impact Risk",
  colourScale = my_color,
  LinkGroup = "linkgroup", NodeGroup = "node_group", iterations = 0
)
p

# col_scale <- 'd3.scaleOrdinal()
#   .domain(["Low","Medium","High"])
#   .range(["rgba(0,90,181,0.25)","rgba(0,90,181,0.6)","rgba(0,90,181,1.0)"])'







# #####################################  confidence as link group #####################################
# nodes$node_group <- "AllNodes"

# # 2) Extend your colourScale domain to include the node group
# col_scale <- 'd3.scaleOrdinal()
#   .domain(["AllNodes","Low","Medium","High"])
#   .range(["#6B7280","#f06a2e","#ffd60a","#8edafa"])'

# #### ---- Create sankey with confidence as link group ----
# p2 <- sankeyNetwork(
#   Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
#   Value = "value", NodeID = "name",
#   units = "Impact Risk",
#   LinkGroup = "Confidence",
#   NodeGroup = "node_group",
#   fontSize = 12, nodeWidth = 28,
#   colourScale = col_scale
# )
# p2
# # ---- Inject legend + tooltips (rendered inside SVG) ----
# p2 <- htmlwidgets::onRender(
#   p2,
#   '
#   function(el, x) {
#     const svg = d3.select(el).select("svg");
#     const W = +svg.attr("width") || el.getBoundingClientRect().width;
#     const labels = ["Low","Medium","High"];
#     const colors = ["rgba(0,90,181,0.25)","rgba(0,90,181,0.6)","rgba(0,90,181,1.0)"];

#     // Legend group (top-right)
#     const pad = 10, rowH = 20, sw = 14;
#     const boxW = 150, boxH = pad*2 + 16 + labels.length*rowH;
#     const g = svg.append("g")
#       .attr("class","legend")
#       .attr("transform", `translate(${W - boxW - 10}, 10)`);

#     g.append("rect")
#       .attr("width", boxW).attr("height", boxH)
#       .attr("rx", 6).attr("ry", 6)
#       .style("fill", "white").style("stroke", "#ccc");

#     g.append("text").text("Confidence")
#       .attr("x", pad).attr("y", pad + 12)
#       .style("font-size","12px").style("font-weight","600");

#     const items = g.append("g").attr("transform", `translate(${pad}, ${pad+18})`)
#       .selectAll("g").data(labels).enter().append("g")
#       .attr("transform", (d,i) => `translate(0, ${i*rowH})`);

#     items.append("rect")
#       .attr("width", sw).attr("height", sw).attr("y", -11)
#       .style("fill", (d,i) => colors[i]).style("stroke", "none");

#     items.append("text")
#       .attr("x", sw + 8).attr("y", 0)
#       .style("font-size","12px").style("dominant-baseline","central")
#       .text(d => d);

#     // Simple title tooltips for links
#     d3.select(el).selectAll(".link")
#       .on("mouseover", function(d) {
#         const src = x.nodes[d.source.index].name;
#         const tgt = x.nodes[d.target.index].name;
#         const val = d.value;
#         const conf = d.group;
#         this.setAttribute("title", `${src} → ${tgt}\\nValue: ${val}\\nConfidence: ${conf}`);
#       });
#   }
#   '
# )
# html_file <- "sankey_confidence_with_legendNEW.html"

# save the widget
saveWidget(p, file = "sankey.html", selfcontained = TRUE)
# saveWidget(p2, file = html_file, selfcontained = TRUE)




# save the widget
webshot("sankey.html", "sankey.png", vwidth = 1200, vheight = 1200)
# webshot(html_file, "sankey_confidence_with_legendNEW.png", vwidth = 1200, vheight = 1200)


