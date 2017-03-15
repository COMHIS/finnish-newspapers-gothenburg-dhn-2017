
source("./load_basedata.R")
source("./functions-data/circulation-functions.R")
source("./functions-data/newspaper_metadata_functions.R")


enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN")



loc_np_overlap_papers <- enriched_newspaper_metadata[(enriched_newspaper_metadata$ISSN %in% locations_data$ISSN), ]
loc_notin_np_metadata <- locations_data[(locations_data$ISSN %in% enriched_newspaper_metadata$ISSN),]
new_np_overlap <- (newspapers_only_subset$ISSN %in% locations_data$ISSN)
sum(new_np_overlap == TRUE)

finnish_newspapers_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" &
                                      (substr(KIELI, 1, 3) == "fin"))
swedish_newspapers_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" &
                                      (substr(KIELI, 1, 3) == "swe"))
finnish_periodicals_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "AIK" &
                                      (substr(KIELI, 1, 3) == "fin"))
swedish_periodicals_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "AIK" &
                                       (substr(KIELI, 1, 3) == "swe"))

year_range <- 1820:1900

finnish_newspapers_per_year <-
  get_metadata_issn_amount_for_year_range(finnish_newspapers_subset, year_range, identifier = "finSAN")
swedish_newspapers_per_year <-
  get_metadata_issn_amount_for_year_range(swedish_newspapers_subset, year_range, identifier = "sweSAN")
finnish_periodicals_per_year <-
  get_metadata_issn_amount_for_year_range(finnish_periodicals_subset, year_range, identifier = "finAIK")
swedish_periodicals_per_year <-
  get_metadata_issn_amount_for_year_range(swedish_periodicals_subset, year_range, identifier = "sweAIK")

fin_swe_totals <- rbind(finnish_newspapers_per_year,
                        swedish_newspapers_per_year,
                        finnish_periodicals_per_year,
                        swedish_periodicals_per_year)

library(ggplot2)

totals_by_language_per_year_plot <- ggplot(fin_swe_totals, aes(x = year, y = issn_numbers, colour = identifier)) +
  geom_line(size = 1) +
  scale_x_continuous(name="year", breaks = seq(1820, 1900, 10)) +
  scale_y_continuous(name = "titles", breaks = seq(0, 10000, 50))


totals_by_language_per_year_plot
