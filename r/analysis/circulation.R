
source("./functions-data/circulation-functions.R")
source("./load_basedata.R")


enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
circs_per_year <- get_circulation_per_issn_for_year_range(1800:1920, cleaned_circulation_data)

only_newspapers <- subset(newspaper_base_data, AINYLEISMAARE == "SAN")
swe_issn <- get_metadata_subset_issn_numbers(only_newspapers, "KIELI", c("swe", "finswe"))
fin_issn <- get_metadata_subset_issn_numbers(only_newspapers, "KIELI", c("fin"))
swe_circ_subset <- get_circulation_data_subset_by_issn(cleaned_circulation_data, swe_issn)
fin_circ_subset <- get_circulation_data_subset_by_issn(cleaned_circulation_data, fin_issn)
swe_circ_totals <- get_total_circulation_per_year(swe_circ_subset, 1900:2000)
fin_circ_totals <- get_total_circulation_per_year(fin_circ_subset, 1900:2000)
fin_circ_totals$lang <- rep("fin", nrow(fin_circ_totals))
swe_circ_totals$lang <- rep("swe", nrow(fin_circ_totals))
fin_swe_totals <- rbind(fin_circ_totals, swe_circ_totals)


library(ggplot2)
options(scipen = 1000000)

plot1 <- ggplot(circs_per_year, aes(x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = issn), width = 1) +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) 

plot2 <- ggplot(fin_swe_totals, aes(x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = lang), width = 1) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) 


plot1
plot2

