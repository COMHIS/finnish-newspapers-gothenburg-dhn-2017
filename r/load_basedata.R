
datadir <- "~/projects/comhis/data-public/finnish-newspapers/circulation090217/"

circulation_data <- read.csv(paste0(datadir, "circulation-utf8.csv"))
newspaper_base_data <- read.csv(paste0(datadir, "newspapers-utf8.csv"))
circulation_areas_data <- read.csv(paste0(datadir, "circulation_areas-utf8.csv"))
locations_data <- read.csv(paste0(datadir, "publication_locations-utf8.csv"))
