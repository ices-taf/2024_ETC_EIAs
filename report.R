## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(glue)

mkdir("report")

# Load df -----------
all_data <- readRDS("data/all_data.rds")


sankeys <- list()
sankey_confidence <- list()

# tab <- "Fisheries-NEA"
for (tab in names(all_data)) {
  mkdir(glue("report/{tab}"))
  data <- all_data[[tab]]

  setwd(glue("report/{tab}"))
  sourceTAF("../../report_sankey.R")
  setwd("../..")

  sankeys[[tab]] <- p
  sankey_confidence[[tab]] <- p2

  # write out to csv files
  write.taf(data[, -c(7:9, 11:12)], file = glue("report/{tab}.csv"), quote = TRUE)
}

save(sankeys, file = "report/sankeys.rds")

# combine into a single html report
rmarkdown::render("report.Rmd", output_file = "report.html", output_dir = "report", output_options = list(self_contained = TRUE))

# make tables
rmarkdown::render("report_tables_stage2.Rmd", output_dir = "report")
