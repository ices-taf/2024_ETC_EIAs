library(icesTAF)
library(glue)
mkdir("data")

source("utilities_pressures.R")

# read in data

# files <- c(
#   "Fisheries-NEA",
#   "Fisheries-Med",
#   "Aquaculture-NEA",
#   "Aquaculture-Med"
# )

files <- c(
  "2025_aquaculture-allRegions-NEA",
  "2025_aquaculture-allRegions-MED",
  "2025_aquaculture-allRegions-BLT",
  "2025_aquaculture-lowTrophic-GNS",
  "2025_aquaculture-lowTrophic-ADR",
  "2025_fisheries-BLT-S",
  "2025_fisheries-BLT-N",
  "2025_fisheries-MED",
  "2025_fisheries-ADR",
  "2025_fisheries-SoS",
  "2025_fisheries-ONA",
  "2025_fisheries-GNS"
)

all_data <- sapply(files, function(x) calc_scores(glue("boot/data/google_sheets/{x}.csv")), simplify = FALSE)

# do we include a confidence measure.... ?

saveRDS(all_data, file = "data/all_data.rds")
