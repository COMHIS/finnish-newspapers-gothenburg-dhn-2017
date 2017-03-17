
library(ggplot2)
library(reshape2)
source("./load_basedata.R")
source("./functions-data/circulation-functions.R")
source("./functions-data/newspaper_metadata_functions.R")
source("./functions-data/locations_functions.R")
source("./functions-data/functions-common.R")

enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN")
enriched_locations_data <- enrich_locations_data(locations_data, enriched_newspaper_metadata)

# new1918 <- get_changedata_by_location_for_year(newspapers_only_subset, enriched_locations_data, 1918, change_type = "start")
# end1918 <- get_changedata_by_location_for_year(newspapers_only_subset, enriched_locations_data, 1918, change_type = "end")

old_and_new <- get_start_and_end_years_for_year_range(newspapers_only_subset, 1810:1920)
old_and_new_plotdata <- old_and_new[, c("year", "new_papers", "closed_papers")]
old_and_new_plotdata$closed_papers <- old_and_new_plotdata$closed_papers * (-1)
old_and_new_plotdata$difference <- old_and_new_plotdata$new_papers + old_and_new_plotdata$closed_papers
old_and_new_plotdata <- melt(old_and_new_plotdata, id = "year")

background_events <- read.csv("../data/finnish-newspapers/unified/censorship_events.csv")
background_events$event <- factor(background_events$event,
                                  levels = as.character(background_events$event))

start_vs_end_plot <- ggplot() +
  geom_rect(data = background_events,
            mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = event),
            alpha = 0.4, size = 0) +
  geom_line(data = old_and_new_plotdata,
            aes(x = year, y = value, colour = variable),
            size = 1) +
  scale_x_continuous(name = "year", breaks = seq(1800, 2000, 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


plots <- list(start_vs_end = start_vs_end_plot)

save_plots_png(plots, prefix = "censorship", size_preset = "large")
