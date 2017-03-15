source("./functions-data/circulation-functions.R")
source("./functions-data/newspaper_metadata_functions.R")
source("./functions-data/locations_functions.R")
source("./load_basedata.R")

enriched_circulation_data <- enrich_circulation_data(circulation_data)
enriched_circulation_data[is.na(enriched_circulation_data$end_year), "end_year"] <- 2020
enriched_circulation_data[is.na(enriched_circulation_data$start_year), "start_year"] <- 1500

enriched_newspaper_basedata <-  enrich_newspaper_metadata(newspaper_base_data)

# rm(circulation_data)
# rm(newspaper_base_data)

relevant_base_metadata <- subset(enriched_newspaper_basedata, start_year <= 1860)
relevant_base_metadata <- subset(relevant_base_metadata, JULKAISUMAA == "FI" | JULKAISUMAA == "fi")
relevant_base_metadata <- subset(relevant_base_metadata, AINYLEISMAARE == "SAN" | AINYLEISMAARE == "AIK")
relevant_base_metadata <- relevant_base_metadata[, c("ISSN", "PAANIMEKE", "AINYLEISMAARE",
                                                    "JULKAISUMAA", "KIELI", "KOKO", "KORKEUS_CM",
                                                    "LEVEYS_CM", "KUSTANTAJA", "TEKSTITYYPPI",
                                                    "start_date", "end_date", "start_year", "end_year")]

enriched_locations_data <- enrich_locations_data(locations_data, enriched_newspaper_basedata)
relevant_locations_data <- subset(enriched_locations_data, start_year <= 1860)
# nrow(relevant_locations_data)
# length(unique(as.character(relevant_locations_data$ISSN)))
# = sama, eli lokaatio tarvitaan vaan kerran / lehti
relevant_locations_data <- relevant_locations_data[, c("ISSN", "KAUPUNKI_NORM")]


relevant_base_metadata <- merge(relevant_base_metadata, relevant_locations_data, by = "ISSN", all.x = TRUE)

first_circ_year <- 1800
last_circ_year  <- 1860
i <- first_circ_year
while (i <= last_circ_year) {
  relevant_base_metadata[, as.character(paste0("levikki_", i))] <- NA
  i <- i + 1
}
enriched_circulation_data <- subset(enriched_circulation_data, start_year <= 1860)


add_circulation_data_per_year <- function(relevant_base_metadata, enriched_circulation_data) {
  row_amount <- nrow(relevant_base_metadata)
  i <- 1
  
  while (i <= row_amount) {
    issn <- as.character(relevant_base_metadata[i, "ISSN"])
    year_range <- 1800:1860
    for (year in year_range) {
      if (year >= relevant_base_metadata[i, "start_year"] &
          year <= relevant_base_metadata[i, "end_year"]) {
        if (issn %in% (as.character(enriched_circulation_data[, "ISSN"]))) {
          circulation <- get_issn_circulation_for_year(issn, year, enriched_circulation_data)
        } else {
          circulation <- 0
        }
      } else {
        circulation <- NA
      }
      relevant_base_metadata[i, paste0("levikki_", year)] <- circulation
    }
    i <- i + 1
  }

  return(relevant_base_metadata)
}

relevant_base_metadata$PAANIMEKE <- as.character(relevant_base_metadata$PAANIMEKE)
relevant_base_metadata$AINYLEISMAARE <- as.character(relevant_base_metadata$AINYLEISMAARE)
relevant_base_metadata <- add_circulation_data_per_year(relevant_base_metadata, enriched_circulation_data)
relevant_base_metadata_ordered <- relevant_base_metadata[order(relevant_base_metadata$AINYLEISMAARE, relevant_base_metadata$PAANIMEKE), ]
write.csv(relevant_base_metadata_ordered, "levikki1800-1860.csv", row.names = FALSE, na = "")
