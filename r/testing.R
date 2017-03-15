
source("./load_basedata.R")
source("./functions-data/circulation-functions.R")
source("./functions-data/newspaper_metadata_functions.R")

enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
# circs_per_year <- get_circulation_per_issn_for_year_range(1700:2000, cleaned_circulation_data)
# first year with circulation data = 1810
only_newspapers <- subset(newspaper_base_data, AINYLEISMAARE == "SAN")

only_newspapers2 <- only_newspapers
only_newspapers2$ISSN <- as.character(only_newspapers2$ISSN )
only_newspapers2 <- subset(only_newspapers2, select = c(ISSN, PAANIMEKE))
only_newspapers2$title <- as.character(only_newspapers2$PAANIMEKE)
only_newspapers2 <- subset(only_newspapers2, select = c(ISSN, title))

cleaned_circulation_data2 <- cleaned_circulation_data
cleaned_circulation_data2$ISSN <- as.character(cleaned_circulation_data2$ISSN)
cleaned_circulation_data2$title <- NA

circ_for_export <- merge(cleaned_circulation_data2, only_newspapers2, by = "ISSN", all.x = TRUE)
circ_for_export2 <- subset(circ_for_export, start_year <= 1860)
circ_for_export2$title <- circ_for_export2$title.y
circ_for_export2 <- subset(circ_for_export2, select = -c(title.x, title.y, ALPVM, LOPVM, LEVIKKI_S))
write.csv(circ_for_export2, "circulation_data_up_to1860.csv")

get_issn_coverage_in_circ_data_in_year_range <- function(enriched_newspaper_metadata, cleaned_circulation_data, year_range) {
  return_data <- data.frame(year = integer(), circulation_issn_numbers = integer(), metadata_issn_numbers = integer(), coverage = numeric())
  
  for (year in year_range) {
    # print(paste0("Processing year ", year))
    year_circ_issn_numbers <- length(get_unique_circulation_issn_for_year(cleaned_circulation_data, year))
    year_meta_issn_numbers <- length(get_metadata_issn_for_year(enriched_newspaper_metadata, year))
    coverage <- year_circ_issn_numbers / year_meta_issn_numbers
    new_row <- data.frame(year = as.integer(year),
                          circulation_issn_numbers = as.integer(year_circ_issn_numbers),
                          metadata_issn_numbers = as.integer(year_meta_issn_numbers),
                          coverage = as.numeric(coverage))
    return_data <- rbind(return_data, new_row)
  }
  
  return(return_data)
}

enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
enriched_newspaper_metadata_newspapers_only <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN")
circulation_coverage <- get_issn_coverage_in_circ_data_in_year_range(enriched_newspaper_metadata_newspapers_only, cleaned_circulation_data, 1810:1950)

library(ggplot2)
library(scales)

circulation_coverage_plot <- ggplot(circulation_coverage, aes(x = year, y = coverage)) +
  geom_bar(stat = "identity", width = 1) +
  scale_x_continuous(name = "Year", breaks = seq(1810, 1950, 10)) +
  scale_y_continuous(name = "SAN ISSN numbers with circulation data", breaks = seq(0, 1, 0.05), labels = percent)
circulation_coverage_plot

