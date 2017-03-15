
source("functions-data/crossreference-functions.R")
source("./load_basedata.R")
source("functions-data/newspaper_metadata_functions.R")


cross_references_1905_jsonfile <- "input-data/references_v1.json"
political_affiliation_data <- read.csv("input-data/1905newspapers_parties_cleaned2.csv")

crossreference_data <- create_crossreference_data(cross_references_1905_jsonfile)
crossreference_data <- filter_crossrefenrence_data_with_issn(crossreference_data,
                                                             political_affiliation_data$id)
enriched_crossreference_data <-
  enrich_crossreference_data_with_pol_affilitation(crossreference_data,
                                                   political_affiliation_data)
enriched_crossreference_data <- filter_references_to_self(enriched_crossreference_data)
enriched_crossreference_data$from_names <-
  get_names_by_issn(enriched_crossreference_data$from, newspaper_base_data)
enriched_crossreference_data$to_names <-
  get_names_by_issn(enriched_crossreference_data$to, newspaper_base_data)


# eri (esim.)työväenlehtien viittaukset muihin

# tyypeittäin viittaukset muihin tyyppeihin. abs + suhteellinen


library(ggplot2)
library(gridExtra)

# unique_affiliations <- unique(enriched_crossreference_data$from_pol)

# plots <- vector()
# for (affiliation in unique_affiliations) {
#   affiliation <- as.character(affiliation)
#   plot_subset <- subset(enriched_crossreference_data, from_pol == affiliation)
#   plot <- ggplot(data = plot_subset, aes(x = reorder(from_names, volume), y = volume)) +
#     geom_bar(stat = "identity",aes(fill = to_pol)) +
#     theme(axis.text.x = element_text(angle = -45, , hjust = 0))
#   plots <- c(plots, plot)
# }
# grid.arrange(plots)

t_subset <- subset(enriched_crossreference_data, from_pol == "t")
ns_subset <- subset(enriched_crossreference_data, from_pol == "ns")
vs_subset <- subset(enriched_crossreference_data, from_pol == "vs")
p_subset <- subset(enriched_crossreference_data, from_pol == "p")
kr_subset <- subset(enriched_crossreference_data, from_pol == "kr")


t_plot <- ggplot(data = t_subset, aes(x = reorder(from_names, volume), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("T")

t2_plot <- ggplot(data = t_subset, aes(x = (from_names), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("T")


ns_plot <- ggplot(data = ns_subset, aes(x = reorder(from_names, volume), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("NS")

vs_plot <- ggplot(data = vs_subset, aes(x = reorder(from_names, volume), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("VS")

p_plot <- ggplot(data = p_subset, aes(x = reorder(from_names, volume), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("P")

kr_plot <- ggplot(data = kr_subset, aes(x = reorder(from_names, volume), y = volume)) +
  geom_bar(stat = "identity",aes(fill = to_pol)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank()) +
  ggtitle("KR")

grid.arrange(t_plot, ns_plot, vs_plot, p_plot, kr_plot, ncol = 2)
