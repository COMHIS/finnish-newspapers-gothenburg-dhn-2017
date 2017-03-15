
source("./load_basedata.R")
source("./circulation-functions.R")
source("./newspaper_metadata_functions.R")
source("./locations_functions.R")


enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN")
municipalities_coordinates <- read.csv("../../data-public/data-refined/finnish_municipalities.csv")
enriched_locations_data <- enrich_locations_data(locations_data, enriched_newspaper_metadata)


subset1800_1920 <- subset(enriched_locations_data, end_year >= 1800 & start_year <= 1920 & MAA == "FI")
subset1800_1920_newspapers <- subset(subset1800_1920, subset1800_1920$ISSN %in% newspapers_only_subset$ISSN)
publications_per_year_per_location <- get_publications_per_year_per_location(subset1800_1920_newspapers, 1800:1920)
publications_per_year_per_location_with_coords <- enrich_locations_data_with_coordinates(publications_per_year_per_location,
                                                                                         "location", municipalities_coordinates)


# newspapers1860 <- subset(publications_per_year_per_location_with_coords, year == 1860)
# newspapers1870 <- subset(publications_per_year_per_location_with_coords, year == 1870)
# newspapers1880 <- subset(publications_per_year_per_location_with_coords, year == 1880)


library(ggplot2)
library(ggmap)


get_publication_mapplot_for_year <- function(publications_per_year_per_location_with_coords, subset_year, basemap) {
  year_subset_data <- subset(publications_per_year_per_location_with_coords, year == subset_year)
  newspaper_mapplot <- ggmap(basemap) + 
    geom_point(data = year_subset_data,
               aes(x = lon, y = lat),
               color = "red",
               size = (year_subset_data$newspapers) * 3,
               alpha = 0.6) +
    ggtitle(subset_year) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  return(newspaper_mapplot)
}


finland_map <- get_map(location = c(lon = 26, lat = 63), zoom = 6)

# newspaper_mapplot <- ggmap(finland_map) + 
#   geom_point(data = publications_per_year_per_location_with_coords,
#              aes(x = lon, y = lat),
#              color = "red",
#              size = publications_per_year_per_location_with_coords$newspapers,
#              alpha = 0.05)
# 
# newspaper_mapplot

# mapplot1840 <- get_publication_mapplot_for_year(publications_per_year_per_location_with_coords, 1840, finland_map)
# mapplot1840
# mapplot1900 <- get_publication_mapplot_for_year(publications_per_year_per_location_with_coords, 1900, finland_map)
# mapplot1900


library(animation)

saveGIF({
  for (year in 1800:1920) {
    year_plot <- get_publication_mapplot_for_year(publications_per_year_per_location_with_coords, year, finland_map)
    print(year_plot)
  }
}, movie.name = "output/animations/newspapers_per_year_1800-1920.gif", interval = 0.5, ani.width = 800, ani.height = 776)

