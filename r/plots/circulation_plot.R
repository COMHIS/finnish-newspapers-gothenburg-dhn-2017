
library(ggplot2)
library(scales)
options(scipen = 1000000)

get_circulation_plotdata <- function(circulation_data) {
  
  circulation_plot_data <- data.frame(issn = character(),
                                      title = character(),
                                      lang = character(),
                                      location = character(),
                                      year = integer(),
                                      circulation = integer(),
                                      estimate = character())
  i <- 1
  while (i <= nrow(circulation_data)) {
    for (year in 1800:1860) {
      circulation1 = as.character(circulation_data[i, paste0("levikki_", year)]) 
      
      if (!is.na(circulation1)) {
        if (circulation1 != "") {
          issn = circulation_data[i, "ISSN"]
          title = circulation_data[i, "PAANIMEKE"]
          lang = circulation_data[i, "KIELI"]
          location = circulation_data[i, "KAUPUNKI_NORM"]
          if (location == "") {
            location <- "unknown"
          }
          circulation = as.integer(gsub("[^0-9]", "", circulation1))
          estimate = gsub("[0-9]", "", circulation1)
          
          new_row <- data.frame(issn = issn, title = title, lang = lang, year = year,
                                location = location,
                                circulation = circulation, estimate = estimate)
          circulation_plot_data <- rbind(circulation_plot_data, new_row)
        }
      }
    }
    i <- i + 1
  }
  return(circulation_plot_data)
}  


get_decade <- function(year) {
  decade <- floor(year / 10) * 10
  return(decade)
}


get_location_totals_by_decade <- function(circulation_plot_data) {
  unique_locations <- unique(circulation_plot_data$location)
  circulation_plot_data$decade <- get_decade(circulation_plot_data$year)
  unique_decades <- unique(circulation_plot_data$decade)
  
  return_data <- data.frame(decade = integer(), location = character(), circulation = double())

  for (query_decade in unique_decades) {
    print(query_decade)
    decade_subset <- subset(circulation_plot_data, decade == query_decade)

    for (query_location in unique_locations) {
      location_subset <- subset(decade_subset, location == query_location)
      circulation_in_location = sum(location_subset$circulation) / 10
      new_row <- data.frame(decade = query_decade,
                            location = query_location,
                            circulation = circulation_in_location)
      return_data <- rbind(return_data, new_row)
    }
  }
  return(return_data)
}


get_page_area <- function(page_size) {
  a3 <- 0.297 * 0.420
  if (page_size == "A3") {
    return(a3)
  } else if (page_size == "A4") {
    return(a3 / 2)
  } else if (page_size == "A5") {
    return(a3 / 4)
  } else if (page_size == "A2") {
    return(a3 * 2)
  } else if (page_size == "A3-A2" | page_size == "A2-A3") {
    return((a3 + (a3 * 2)) / 2)
  } else if (page_size == "A4-A3") {
    return(((a3 / 2) + (a3)) / 2)
  } else {
    return(NA)
  }
}


get_paper_consumption <- function(circulation, page_area, page_size, issues) {
  consumption <- circulation * page_area * page_size * issues
  return(consumption)
}


circulation_data <- read.csv("../../data-public/data-refined/circulation_1800-1860.csv")
circulation_plot_data <- get_circulation_plotdata(circulation_data)
circulation_plot_data$location <- as.character(circulation_plot_data$location)
circulation_plot_data[circulation_plot_data$location == "Viipuri", ]$location <- "Vyborg"


# paper consumption = issues * circulation * page size * page numbers

source("./load_basedata.R")
circulation_plot_data$page_size <- NA
circulation_plot_data$page_area <- NA
circulation_plot_data$pages <- NA
circulation_plot_data$issues <- NA
issues_data <- read.csv("../../data-public/data-refined/issues_by_papers_by_years.csv")
pages_data <- read.csv("../../data-public/data-refined/pages_by_papers_by_years.csv")
names(issues_data) <- c("title", "year", "issues")
names(pages_data) <- c("issn", "year", "pages")

get_issues_for_title_year <- function(issues_data, query_title, query_year) {
  issues_subset <- subset(issues_data, title == query_title & year == query_year)
  if (length(issues_subset) > 1) {
    issues <- issues_subset[1, "issues"]
  }
  return(issues)
}


get_pages_for_issn_year <- function(pages_data, query_issn, query_year) {
  pages_subset <- subset(pages_data, issn == query_issn & year == query_year)
  if (length(pages_subset) > 1) {
    pages <- pages_subset[1, "pages"]
  }
  return(pages)
}


for (row in 1:nrow(circulation_plot_data)) {
  issn <- as.character(circulation_plot_data[row, "issn"])
  page_size <- as.character(newspaper_base_data[newspaper_base_data$ISSN == issn, "KOKO"])
  if (page_size == "") {
    page_size <- "A4"
  }
  circulation_plot_data[row, "page_size"] <- page_size
  circulation_plot_data[row, "page_area"] <- get_page_area(page_size)
  circulation_plot_data[row, "issues"] <- get_issues_for_title_year(issues_data,
                                                                    as.character(circulation_plot_data[row, "title"]),
                                                                    as.character(circulation_plot_data[row, "year"]))
  circulation_plot_data[row, "pages"] <- get_pages_for_issn_year(pages_data,
                                                                 as.character(circulation_plot_data[row, "issn"]),
                                                                 as.character(circulation_plot_data[row, "year"]))
}


circulation_plot_data$paper_consumption <- NA

for (row in 1:nrow(circulation_plot_data)) {
  circulation_plot_data$paper_consumption <- get_paper_consumption(
    circulation_plot_data$circulation,
    circulation_plot_data$page_area,
    circulation_plot_data$pages,
    circulation_plot_data$issues)
}

circulation_plot_data_top5_papercon <- subset(circulation_plot_data, location == "Turku" |
                                           location == "Helsinki" |
                                           location == "Vyborg" |
                                           location == "Oulu" |
                                           location == "Kuopio")
circulation_plot_data_top5_papercon <-
  subset(circulation_plot_data_top5_papercon, !is.na(paper_consumption))

# write.csv(circulation_plot_data_top5_papercon, "circulation_plot_data_top5_papercon.csv", row.names = F)

plot_cities_paper_con <- ggplot(circulation_plot_data_top5_papercon, aes(x = year, y = paper_consumption)) + 
  geom_bar(stat = "identity", aes(fill = location), width = 1) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  labs(y = "Paper consumption estimate, m2",
       x = "Year",
       fill = "Location")

location_totals_by_decade <- get_location_totals_by_decade(circulation_plot_data)
location_totals_by_decade <- subset(location_totals_by_decade, decade != 1860)

loc_tot <- data.frame(location = character(), total_circ = double())
for (location in unique(location_totals_by_decade$location)) {
  circu <- sum(location_totals_by_decade[location_totals_by_decade$location == location, "circulation"])
  # print(paste0(location, " -- ", circu))
  loc_total = data.frame(location = location, total_circ = circu)
  loc_tot <- rbind(loc_tot, loc_total)
}
loc_tot_ordered <- loc_tot[with(loc_tot, order(-total_circ)), ]

location_totals_by_decade_top5 <- subset(location_totals_by_decade, location %in% (loc_tot_ordered$location[1:5]))

plot_top5_locs <- ggplot(location_totals_by_decade_top5, aes(x = decade, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = location)) +
  theme( #legend.position = "none",
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(location_totals_by_decade_top5$decade),
                                  max(location_totals_by_decade_top5$decade),
                                  10))

plot_languages <- ggplot(circulation_plot_data, aes(x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = lang), width = 1) +
  theme( #legend.position = "none",
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) 

plot_cities <- ggplot(circulation_plot_data, aes(x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = location), width = 1) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

plot_estimates_proportinal <- ggplot(circulation_plot_data, aes(
  x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = estimate),
           position = "fill", width = 1) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent)

plot_estimates <- ggplot(circulation_plot_data, aes(
  x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = estimate),
           width = 1) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")


plots <- list(paper_consumption = plot_cities_paper_con,
              top5_locs_decade = plot_top5_locs,
              cities = plot_cities,
              languages = plot_languages,
              estimates = plot_estimates,
              estimates_proportinal = plot_estimates_proportinal)

plot_filename_prefix <- paste0("circulation_", Sys.Date(), "_")

for (item in 1:length(plots)) {
  filename <- paste0("./output/plots/",
                     plot_filename_prefix,
                     names(plots)[item],
                     ".png")
  ggsave(filename, plots[[item]], width = 6, height = 4, dpi = 300)  
}

