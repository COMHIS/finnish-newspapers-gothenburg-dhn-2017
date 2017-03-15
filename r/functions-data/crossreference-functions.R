
library(jsonlite)


fix_duplicate_issn <- function(issn) {
  cleaned_issn <- unlist(strsplit(issn, " "))[1]
  return(cleaned_issn)
}


create_crossreference_data <- function(input_data_json) {
  cross_references_list <- fromJSON(input_data_json)
  cross_references_list <- cross_references_list[2:length(cross_references_list)]
  return_data <- data.frame(from = character(), to = character(), volume = integer())
  to_index <- 1
  while (to_index <= length(cross_references_list)) {
    # print(names(cross_references_list[from_index]))
    to <- names(cross_references_list[to_index])
    origin_list <- cross_references_list[[to_index]]
    origin_list_names <- names(origin_list)
    origin_list_names <- unlist(lapply(origin_list_names, fix_duplicate_issn))
    origin_list_values <- as.integer(origin_list)
    to_list <- rep(to, length(origin_list_values))
    new_row <- data.frame(from = origin_list_names,
                          to = to_list,
                          volume = origin_list_values)
    to_index <- to_index + 1
    return_data <- rbind(return_data, new_row)
  }
  return_data$from <- as.character(return_data$from)
  return_data$to <- as.character(return_data$to)
  return(return_data)
}


filter_references_to_self <- function(crossreference_data) {
  filtered_crossreference_data <- crossreference_data
  not_autoref_indices <-
    as.character(filtered_crossreference_data$from) != as.character(filtered_crossreference_data$to)
  filtered_crossreference_data <- filtered_crossreference_data[not_autoref_indices, ]
  return(filtered_crossreference_data)
}


filter_crossrefenrence_data_with_issn <- function(crossreference_data, issn_numbers) {
  issn_numbers <- as.character(issn_numbers)
  filtered_data <- crossreference_data
  from_filter <- filtered_data$from %in% issn_numbers
  to_filter <- filtered_data$to %in% issn_numbers
  combined_filter <- from_filter & to_filter
  filtered_data <- filtered_data[combined_filter, ]
  return(filtered_data)
}


enrich_crossreference_data_with_pol_affilitation <- function(crossreference_data,
                                                             political_affiliation_data) {
  enriched_crossreference_data <- crossreference_data
  enriched_crossreference_data$from_pol <- NA
  enriched_crossreference_data$to_pol <- NA
  
  row_id <- 1
  while (row_id <= nrow(enriched_crossreference_data)) {
    from_issn <- as.character(enriched_crossreference_data[row_id, "from"])
    from_pol <- as.character(
      political_affiliation_data[political_affiliation_data$id == from_issn,
                                 "pol"])
    enriched_crossreference_data[row_id, "from_pol"] <- from_pol
    to_issn <- as.character(enriched_crossreference_data[row_id, "to"])
    to_pol <- as.character(
      political_affiliation_data[political_affiliation_data$id == to_issn,
                                 "pol"])
    enriched_crossreference_data[row_id, "to_pol"] <- to_pol
    row_id <- row_id + 1
  }
  return(enriched_crossreference_data)
}

