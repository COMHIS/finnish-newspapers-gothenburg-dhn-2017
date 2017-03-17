
source("./load_basedata.R")
source("./functions-data/newspaper_metadata_functions.R")
source("./functions-data/locations_functions.R")
source("./functions-data/provinciality_functions.R")
source("./functions-data/functions-common.R")

library(reshape2)
library(ggplot2)


get_newspaper_provincility_plot <- function(newspapers_provinciality_plot_data) {
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_provinciality_plot_data,
             aes(x = year, y = value, fill = variable),
             width = 1,
             alpha = 0.9) +
    scale_fill_brewer(palette = "Set3")
  return(newspapers_provinciality_plot)
}


get_newspaper_provincility_plot_proportional <- function(newspapers_provinciality_plot_data) {
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_provinciality_plot_data,
             aes(x = year, y = value, fill = variable),
             position = "fill", 
             width = 1,
             alpha = 0.9) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::percent)
  return(newspapers_provinciality_plot)
}


get_number_of_locations_with_min_x_papers_plot <- function(newspapers_provinciality_plot_data, x_papers) {
  plot_data <- get_number_of_locations_with_more_than_x_papers_per_y_years(newspapers_provinciality_plot_data, x_papers)
  plot <- ggplot() +
    geom_col(data = plot_data,
             aes(x = time_segment, y = locations),
             width = 1,
             alpha = 0.9) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(breaks = seq(0,
                                    max(plot_data$locations),
                                    2)) +
    scale_x_continuous(breaks = seq(min(plot_data$time_segment),
                                    max(plot_data$time_segment),
                                    10)) +
    theme(axis.title.x=element_blank())
  return(plot)
}


get_newspaper_sum_plot_data <- function(newspapers_provinciality_plot_data) {
  return_data <- data.frame(year = integer(), papers = integer())
  for (query_year in unique(newspapers_provinciality_plot_data$year)) {
    year_subset <- subset(newspapers_provinciality_plot_data, year == query_year)
    year_sum <- sum(year_subset$value, na.rm = TRUE)
    new_row <- data.frame(year = query_year, papers = year_sum)
    return_data <- rbind(return_data, new_row)
  }
  return(return_data)
}


get_newspaper_sum_plot <- function(newspapers_sum_plot_data) {
  newspapers_provinciality_plot <- ggplot() +
    geom_line(data = newspapers_sum_plot_data,
             aes(x = year, y = papers),
             size = 1.5, colour = "blue") +
    scale_y_continuous(breaks = seq(0,
                                    max(newspapers_sum_plot_data$papers),
                                    10)) +
    scale_x_continuous(breaks = seq(min(newspapers_sum_plot_data$year),
                                    max(newspapers_sum_plot_data$year),
                                    10)) +
    theme(axis.title.x=element_blank())
  return(newspapers_provinciality_plot)
}


get_newspaper_finswe_plot <- function(newspapers_language_share_sum) {
  newspapers_finswe_plot <- ggplot() +
    geom_col(data = newspapers_language_share_sum,
             aes(x = year, y = papers, fill = lang),
             width = 1,
             alpha = 0.7) +
    scale_fill_manual(values = c("blue", "black", "#006633", "yellow")) +
    scale_x_continuous(breaks = seq(min(newspapers_language_share_sum$year),
                                    max(newspapers_language_share_sum$year),
                                    20))
  return(newspapers_finswe_plot)
}


get_newspaper_finswe_plot_proportional <- function(newspapers_language_share_sum) {
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_language_share_sum,
             aes(x = year, y = papers, fill = lang),
             position = "fill", 
             width = 1,
             alpha = 0.7) +
    scale_fill_manual(values = c("blue", "black", "#006633", "yellow")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "share") +
    scale_x_continuous(breaks = seq(min(newspapers_language_share_sum$year),
                                    max(newspapers_language_share_sum$year),
                                    20))
  return(newspapers_provinciality_plot)
}



enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
finnish_newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" & JULKAISUMAA == "FI")
enriched_newspapers_locations_data <- enrich_locations_data(locations_data, finnish_newspapers_only_subset)

year_range <- 1800:1920

finnish_newspapers_only_subset_fin <- subset(finnish_newspapers_only_subset, KIELI == "fin" | KIELI == "finswe")
finnish_newspapers_only_subset_swe <- subset(finnish_newspapers_only_subset, KIELI == "swe" | KIELI == "swefin")
finnish_newspapers_only_subset_rus <- subset(finnish_newspapers_only_subset, KIELI == "rus")
finnish_newspapers_only_subset_ger <- subset(finnish_newspapers_only_subset, KIELI == "ger")

newspapers_provinciality_plot_data <- get_provinciality_plot_data(finnish_newspapers_only_subset, enriched_newspapers_locations_data, 1800:1920)
newspapers_provinciality_plot_data$variable <- as.character(newspapers_provinciality_plot_data$variable)
newspapers_provinciality_plot_data[newspapers_provinciality_plot_data$variable == "Viipuri", ]$variable <- "Vyborg"

newspapers_fin_sum <-
  get_newspaper_sum_plot_data(get_provinciality_plot_data(finnish_newspapers_only_subset_fin, enriched_newspapers_locations_data, 1800:2000))
newspapers_fin_sum$lang <- "fin"
newspapers_swe_sum <-
  get_newspaper_sum_plot_data(get_provinciality_plot_data(finnish_newspapers_only_subset_swe, enriched_newspapers_locations_data, 1800:2000))
newspapers_swe_sum$lang <- "swe"
newspapers_ger_sum <-
  get_newspaper_sum_plot_data(get_provinciality_plot_data(finnish_newspapers_only_subset_ger, enriched_newspapers_locations_data, 1800:2000))
newspapers_ger_sum$lang <- "ger"
newspapers_rus_sum <-
  get_newspaper_sum_plot_data(get_provinciality_plot_data(finnish_newspapers_only_subset_rus, enriched_newspapers_locations_data, 1800:2000))
newspapers_rus_sum$lang <- "rus"


newspapers_finswegerrus_sum <- rbind(newspapers_fin_sum, newspapers_swe_sum, newspapers_ger_sum, newspapers_rus_sum)


newspapers_language_share_plot <- get_newspaper_finswe_plot(newspapers_finswegerrus_sum)
newspapers_language_share_plot_proportional <- get_newspaper_finswe_plot_proportional(newspapers_finswegerrus_sum)

number_of_locations_with_min_2_papers_plot <-
  get_number_of_locations_with_min_x_papers_plot(newspapers_provinciality_plot_data, 2)
number_of_locations_with_min_3_papers_plot <-
  get_number_of_locations_with_min_x_papers_plot(newspapers_provinciality_plot_data, 3)
number_of_locations_with_min_4_papers_plot <-
  get_number_of_locations_with_min_x_papers_plot(newspapers_provinciality_plot_data, 4)
newspapers_sum_plot <-
  get_newspaper_sum_plot(get_newspaper_sum_plot_data(newspapers_provinciality_plot_data))


top5_newspapers_provinciality_plot_data <- get_topx_newspapers_data(newspapers_provinciality_plot_data,
                                                                    top_x = 5)
top5_newspapers_provinciality_plot <- get_newspaper_provincility_plot(top5_newspapers_provinciality_plot_data)
top5_newspapers_provinciality_plot_proportional <-
  get_newspaper_provincility_plot_proportional(top5_newspapers_provinciality_plot_data)

top10_newspapers_provinciality_plot_data <- get_topx_newspapers_data(newspapers_provinciality_plot_data,
                                                                     top_x = 10)
top10_newspapers_provinciality_plot <- get_newspaper_provincility_plot(top10_newspapers_provinciality_plot_data)
top10_newspapers_provinciality_plot_proportional <-
  get_newspaper_provincility_plot_proportional(top10_newspapers_provinciality_plot_data)

locations_234_plot <- ggplot() +
  geom_line(data = get_number_of_locations_with_more_than_x_papers_per_y_years(newspapers_provinciality_plot_data, 2),
            aes(x = time_segment, y = locations, colour = "2"),
            size = 1.5) +
  geom_line(data = get_number_of_locations_with_more_than_x_papers_per_y_years(newspapers_provinciality_plot_data, 3),
            aes(x = time_segment, y = locations, colour = "3"),
            size = 1.5) +
  geom_line(data = get_number_of_locations_with_more_than_x_papers_per_y_years(newspapers_provinciality_plot_data, 4),
            aes(x = time_segment, y = locations, colour = "4"),
            size = 1.5) +
  scale_fill_brewer(palette = "Set3") +
  labs(colour = "Min papers") +
  theme(axis.title.x = element_blank())



# top10_newspapers_provinciality_plot

plots <- list(locations_with_min_2_papers = number_of_locations_with_min_2_papers_plot,
              locations_with_min_3_papers = number_of_locations_with_min_3_papers_plot,
              locations_with_min_4_papers = number_of_locations_with_min_4_papers_plot,
              top5_newspapers_provinciality = top5_newspapers_provinciality_plot,
              top5_newspapers_provinciality_proportional = top5_newspapers_provinciality_plot_proportional,
              top10_newspapers_provinciality = top10_newspapers_provinciality_plot,
              top10_newspapers_provinciality_proportional = top10_newspapers_provinciality_plot_proportional,
              newspapers_sum = newspapers_sum_plot,
              locations_234 = locations_234_plot,
              newspapers_language_share = newspapers_language_share_plot,
              newspapers_language_share_proportional = newspapers_language_share_plot_proportional)

save_plots_png(plots, prefix = "public_discourse")
