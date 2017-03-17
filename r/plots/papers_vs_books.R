source("./load_basedata.R")
source("./functions-data/newspaper_metadata_functions.R")
source("./functions-data/locations_functions.R")
source("./functions-data/provinciality_functions.R")
source("./functions-data/functions-common.R")

library(reshape2)
library(ggplot2)


get_books_per_year_range <- function(fennicadata, start_year, end_year) {
  results_data <- data.frame(year = integer(), books = integer())
  
  for (year in start_year:end_year) {
    year_subset <- subset(fennicadata, publication_year == year)
    year_subset_count <- nrow(year_subset)
    new_row <- data.frame(year = year, books = year_subset_count)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_papers_per_year_range <- function(newspapers_provinciality_plot_data, start_year, end_year) {
  results_data <- data.frame(year = integer(), papers = integer())
  
  for (query_year in start_year:end_year) {
    year_subset <- subset(newspapers_provinciality_plot_data, year == query_year)
    year_subset_count <- sum(year_subset$value, na.rm = TRUE)
    new_row <- data.frame(year = query_year, papers = year_subset_count)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_units_per_decade <- function(units_per_year) {
  colnames(units_per_year) <- c("year", "units")
  first_decade <- ceiling(min(units_per_year$year) / 10) * 10
  last_decade <- floor(max(units_per_year$year - 1) / 10) * 10
  
  decades <- seq(first_decade, last_decade, 10)
  
  results_data <- data.frame(decade = integer(), units_avg = double())
  
  for (decade in decades) {
    decade_subset <- subset(units_per_year, year >= decade & year < decade + 10)
    decade_average <- sum(decade_subset$units) / 10
    new_row <- data.frame(decade = decade, units_avg = decade_average)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_books_per_paper_decade <- function(books_data, papers_provinciality_plotdata, start_year, end_year) {
  books <- get_books_per_year_range(books_data, start_year, end_year)
  papers <- get_papers_per_year_range(papers_provinciality_plotdata, start_year, end_year)
  
  books_decade <- get_units_per_decade(books)
  papers_decade <- get_units_per_decade(papers)
  
  books_per_paper_decade <- books_decade
  books_per_paper_decade$papers <- papers_decade$units_avg
  books_per_paper_decade$ratio <- books_per_paper_decade$units_avg / books_per_paper_decade$papers

  # replace Inf in ratio with NA
  books_per_paper_decade[books_per_paper_decade$ratio == Inf | is.na(books_per_paper_decade$ratio),
                         "ratio"] <- NA 
  
  return(books_per_paper_decade)
}


get_books_per_paper_decade_plotdata <- function(newspapers_provinciality_plot_data,
                                                fennica_data,
                                                start_year = 1820,
                                                end_year = 1910,
                                                location = NA) {
  
  if (!is.na(location)) {
    newspapers_location <- subset(newspapers_provinciality_plot_data_all_fin, variable == location)
    if (location == "Viipuri") {
      fennicadata_location <- subset(fennica_data, publication_place == "Vyborg" | publication_place == "Viipuri")
    } else {
      fennicadata_location <- subset(fennica_data, publication_place == location)
    }
  } else {
    newspapers_location <- newspapers_provinciality_plot_data
    fennicadata_location <- fennica_data
  }
  
  books_per_paper_decade <-
    get_books_per_paper_decade(books = fennicadata_location,
                               papers = newspapers_location,
                               start_year = 1820,
                               end_year = 1910)
  
  books_per_paper_decade$location <- location
  
  names(books_per_paper_decade) <- c("time", "books", "papers", "ratio", "location")
  return(books_per_paper_decade)
}


enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" & JULKAISUMAA == "FI" & KIELI == "fin")
enriched_newspapers_locations_data <- enrich_locations_data(locations_data, newspapers_subset)
fennicadata <- readRDS("./data/final_fennica_data.Rds") # this data is not public

start_year <- 1820
end_year <- 1910

newspapers_provinciality_plot_data_all_fin <-
  get_provinciality_plot_data(newspapers_subset,
                              enriched_newspapers_locations_data,
                              start_year:end_year)
newspapers_provinciality_plot_data_all_fin$variable <- as.character(newspapers_provinciality_plot_data_all_fin$variable)
newspapers_provinciality_plot_data_all_fin[newspapers_provinciality_plot_data_all_fin$variable == "Viipuri", ]$variable <- "Vyborg"

fennicadata_all_fin <- subset(fennicadata, language.Finnish == TRUE)


# fix countries:
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Vyborg" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Sortavala" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Jääski" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin_finland <- subset(fennicadata_all_fin, country == "Finland")


books_per_paper_fin_1820_1910_decade <-
  get_books_per_paper_decade(books = fennicadata_all_fin_finland,
                             papers = newspapers_provinciality_plot_data_all_fin,
                             start_year = 1820,
                             end_year = 1910)


books_per_paper_decade_plotdata_viipuri <-
  get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                      fennicadata_all_fin,
                                      start_year = 1820,
                                      end_year = 1910,
                                      location = "Vyborg")

books_per_paper_decade_plotdata_helsinki <-
  get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                      fennicadata_all_fin,
                                      start_year = 1820,
                                      end_year = 1910,
                                      location = "Helsinki")

books_per_paper_decade_plotdata_turku <-
  get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                      fennicadata_all_fin,
                                      start_year = 1820,
                                      end_year = 1910,
                                      location = "Turku")

books_per_paper_decade_plotdata_oulu <-
  get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                      fennicadata_all_fin,
                                      start_year = 1820,
                                      end_year = 1910,
                                      location = "Oulu")

books_per_paper_decade_plotdata_vaasa <-
  get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                      fennicadata_all_fin,
                                      start_year = 1820,
                                      end_year = 1910,
                                      location = "Vaasa")

books_per_paper_decade_plotdata_top5 <- rbind(books_per_paper_decade_plotdata_viipuri,
                                             books_per_paper_decade_plotdata_helsinki,
                                             books_per_paper_decade_plotdata_turku,
                                             books_per_paper_decade_plotdata_oulu,
                                             books_per_paper_decade_plotdata_vaasa)


# papers_issuedata <- read.csv("../data/finnish-newspapers/misc/circulation_plot_data_top5_papercon.csv")
# papers_issuedata_fin <- subset(papers_issuedata, lang == "fin")
# papers_issuedata_fin <- papers_issuedata_fin[, c("year", "issues", "location", "title")]

issue_basedata <- read.csv("../data/finnish-newspapers/misc/issues_by_papers_by_years.csv")
names(issue_basedata) <- c("title", "year", "issues")


get_issuedata <- function(enriched_newspaper_metadata,
                          enriched_newspapers_locations_data,
                          issue_basedata,
                          start_year,
                          end_year) {
  
  results_data <- data.frame(year = integer(), issn = character(), title = character(), issues = integer(), location = character())

  for (query_year in start_year:end_year) {
    year_newspapers_subset <- subset(enriched_newspapers_locations_data, start_year <= query_year & end_year >= query_year)
    
    for (query_issn in as.character(year_newspapers_subset$ISSN)) {
      query_title <- as.character(subset(enriched_newspaper_metadata, ISSN == query_issn)[1, "PAANIMEKE"])
      location <- as.character(subset(year_newspapers_subset, ISSN == query_issn)$KAUPUNKI_NORM)
      issue_basedata_subset <- subset(issue_basedata, title == query_title & year == query_year)

      if (nrow(issue_basedata_subset) >= 1) {
        issues <- issue_basedata_subset$issues[1]
      } else {
        issues <- NA
      }
      new_row <- data.frame(year = query_year, issn = query_issn, title = query_title, issues = issues, location = location)
      results_data <- rbind(results_data, new_row)
    }
  }
  return(results_data)
}


papers_issuedata_fin <- get_issuedata(newspapers_subset,
                                      enriched_newspapers_locations_data,
                                      issue_basedata,
                                      1800,
                                      1909)
papers_issuedata_fin$location <- as.character(papers_issuedata_fin$location)
papers_issuedata_fin <- subset(papers_issuedata_fin, !is.na(location))
papers_issuedata_fin[papers_issuedata_fin$location == "Viipuri", ]$location <- "Vyborg"
papers_issuedata_fin <- subset(papers_issuedata_fin, !is.na(issues))

papers_issuedata_fin_top5 <- subset(papers_issuedata_fin, location %in% c("Helsinki", "Turku", "Vyborg", "Oulu", "Kuopio"))


get_books_per_paper_issue_decade <- function(fennicadata_all_fin, papers_issuedata_fin) {
  plot_data <- data.frame(decade = integer(), papers = double(), location = character(), books = double())
  locations <- as.character(unique(papers_issuedata_fin$location))

    for (query_location in locations) {
    papers_location_subset <- subset(papers_issuedata_fin, location == query_location)
    papers_location_subset <- papers_location_subset[, c("year", "issues")]
    names(papers_location_subset) <- c("year", "units")
    print(query_location)

    papers_location_issues_per_decade <- get_units_per_decade(papers_location_subset)
    papers_location_issues_per_decade$location <- query_location
    
    books_location_subset <- subset(fennicadata_all_fin, publication_place == query_location)
    books_per_year <- get_books_per_year_range(books_location_subset, 1800, 1909)
    books_per_decade <- get_units_per_decade(books_per_year)
    books_per_decade$location <- query_location

    paper_issues_per_book <- papers_location_issues_per_decade
    names(paper_issues_per_book) <- c("decade", "issues", "location")
    paper_issues_per_book$books <- NA
    for (decade in paper_issues_per_book$decade) {
      books <- books_per_decade[books_per_decade$decade == decade, "units_avg"]
      paper_issues_per_book[paper_issues_per_book$decade == decade, "books"] <- books
    }
    paper_issues_per_book$ratio <- paper_issues_per_book$issues / paper_issues_per_book$books
    plot_data <- rbind(plot_data, paper_issues_per_book)
  }
  
  return(plot_data)
}

books_per_paper_issue_decade_plotdata_top5 <- get_books_per_paper_issue_decade(fennicadata_all_fin, papers_issuedata_fin_top5)


get_decade_sums <- function(books_per_paper_issue_decade_plotdata_top5)  {
  results_data <- data.frame(decade = integer(), ratio = double())
  for (query_decade in unique(books_per_paper_issue_decade_plotdata_top5$decade)) {
    decade_subset <- subset(books_per_paper_issue_decade_plotdata_top5, decade == query_decade)
    decade_ratio <- sum(decade_subset$issues) / sum(decade_subset$books)
    new_row <- data.frame(decade = query_decade, ratio = decade_ratio)
    results_data <- rbind(results_data, new_row)
  }
  return(results_data)
}

books_per_paper_issue_decade_plotdata_all <- get_decade_sums(books_per_paper_issue_decade_plotdata_top5)
  
  
get_issues_per_book_plot <- function(books_per_paper_issue_decade_plotdata, books_per_paper_issue_decade_plotdata_all) {
  issues_per_book_plot <- ggplot() +
    geom_col(data = books_per_paper_issue_decade_plotdata_all,
             aes(x = decade, y = ratio), alpha = 0.3) +
    geom_line(data = books_per_paper_issue_decade_plotdata,
             aes(x = decade, y = ratio, colour = location),
             size = 1.5) +
    scale_x_continuous(breaks = seq(min(books_per_paper_issue_decade_plotdata$decade),
                                    max(books_per_paper_issue_decade_plotdata$decade),
                                    10)) +
    labs(y = "Newspaper Issues / Book")

  return(issues_per_book_plot)
}


get_books_per_paper_plot <- function(books_per_paper_plotdata) {
  names(books_per_paper_plotdata) <- c("time", "books", "papers", "ratio")
  books_per_papers_plot <- ggplot() +
    geom_col(data = books_per_paper_plotdata,
             aes(x = time, y = ratio)) +
    scale_x_continuous(breaks = seq(min(books_per_paper_plotdata$time),
                                    max(books_per_paper_plotdata$time),
                                    10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

  return(books_per_papers_plot)
}

issues_per_book_plot_top5 <- get_issues_per_book_plot(books_per_paper_issue_decade_plotdata_top5, books_per_paper_issue_decade_plotdata_all)

books_per_paper_decade_fin_plot <- get_books_per_paper_plot(books_per_paper_fin_1820_1910_decade)


books_per_papers_locations_top5_plot <- ggplot() +
  geom_line(data = books_per_paper_decade_plotdata_top5,
           aes(x = time, y = ratio, colour = location), size = 2) +
  scale_x_continuous(breaks = seq(1820,
                                  1900,
                                  10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

books_per_papers_locations_top5_combined_plot <- ggplot() +
  geom_col(data = books_per_paper_fin_1820_1910_decade,
           aes(x = decade, y = ratio), alpha = 0.3) +
  geom_line(data = books_per_paper_decade_plotdata_top5,
            aes(x = time, y = ratio, colour = location), size = 2) +
  scale_x_continuous(breaks = seq(1820,
                                  1900,
                                  10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


plots <- list(books_per_papers_decade_fin = books_per_paper_decade_fin_plot,
              books_per_papers_locations_top5_fin = books_per_papers_locations_top5_plot,
              books_per_papers_locations_top5_fin_combined = books_per_papers_locations_top5_combined_plot,
              issues_per_book = issues_per_book_plot_top5
              )

save_plots_png(plots, prefix = "books_vs_papers")
