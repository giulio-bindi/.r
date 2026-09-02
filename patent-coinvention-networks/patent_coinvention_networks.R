# ==============================================================================
# Co-invention Networks and the Evolution of Global Patenting
# BSc Thesis — PATSTAT-based Analysis (Pre- vs Post-2000)
# ==============================================================================
#
# Goal: characterize how patenting activity has changed structurally since
# 2000 — in volume, geography, institutional composition, collaboration
# networks, and technology sectors.
#
# Data: EPO PATSTAT (2022 Spring, sample edition)
#   - tls201_appln.csv      : patent applications
#   - tls206_person.csv     : applicants / inventors
#   - tls207_pers_appln.csv : person-application links
#   - tls209_appln_ipc.csv  : IPC technology classification
#
# NOTE ON DATA: PATSTAT files are large and licensed by the EPO, so they are
# not redistributed in this repo. Download the sample edition to reproduce.
#
# NOTE ON MANUAL CURATION: applicant names in PATSTAT are inconsistent
# (duplicates, typos, alternate spellings — see standardize_names() below).
# The top ~400 applicants by patent count were extracted programmatically,
# then manually verified and classified by country, continent and entity
# type (company / public body / university) into "top_applicants_curated.csv" (included
# in data/ — this is derived/curated data, not raw PATSTAT, so it's fine to
# version). Parts A and B run directly on this file alone.
# ==============================================================================

library(tidyverse)
library(igraph)
library(data.table)
library(lubridate)
library(ineq)
library(tidytext)   # needed for reorder_within() / scale_y_reordered() in Part B

# ---- 0. Setup -----------------------------------------------------------------

data_dir   <- "data"
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

save_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(file.path(output_dir, filename), plot,
         width = width, height = height, dpi = 300, bg = "white")
}

# ---- 1. Load PATSTAT data ------------------------------------------------------

appln <- fread(file.path(data_dir, "tls201_appln.csv"), encoding = "UTF-8") %>%
  mutate(
    appln_id          = as.character(appln_id),
    appln_filing_date = as.Date(appln_filing_date),
    year              = year(appln_filing_date),
    period            = if_else(year < 2000, "Pre-2000", "Post-2000")
  ) %>%
  filter(!is.na(appln_filing_date), year >= 1970, year < 2020)

person <- fread(file.path(data_dir, "tls206_person.csv"), encoding = "UTF-8") %>%
  mutate(person_id = as.character(person_id))

pers_appln <- fread(file.path(data_dir, "tls207_pers_appln.csv"), encoding = "UTF-8") %>%
  mutate(appln_id = as.character(appln_id), person_id = as.character(person_id))

# ---- 2. Name standardization ---------------------------------------------------
# Maps known applicant-name variants to one canonical form. Applied below
# before generating the candidate list for manual review.

standardize_names <- function(name) {
  name <- toupper(name)
  case_when(
    str_detect(name, "GENERAL ELECTRIC|GEN ELECTRIC")                     ~ "GENERAL ELECTRIC COMPANY",
    str_detect(name, "WOBBEN|WOODEN PROPERTIES|VOBBEN PROPERTIZ")         ~ "WOODEN PROPERTIES GMBH",
    str_detect(name, "SIEMENS AG|SIEMENS AKTIENGESELLSCHAFT|西门子公司")   ~ "SIEMENS AG",
    str_detect(name, "SIEMENS GAMESA|西门子歌美飒")                        ~ "SIEMENS GAMESA RENEWABLE ENERGY",
    str_detect(name, "VESTAS WIND SYS|VESTAS WIND SYSTEM A/S|VESTAS WIND SYSTEMS") ~ "VESTAS WIND SYSTEMS A/S",
    str_detect(name, "MITSUBISHI HEAVY")                                  ~ "MITSUBISHI HEAVY INDUSTRIES",
    str_detect(name, "HITACHI")                                           ~ "HITACHI LTD",
    str_detect(name, "GOLDWIND")                                          ~ "GOLDWIND SCIENCE & TECHNOLOGY CO., LTD.",
    str_detect(name, "GAMESA INNOVATION")                                 ~ "GAMESA INNOVATION & TECHNOLOGY",
    str_detect(name, "REPOWER|SENVION")                                   ~ "SENVION",
    str_detect(name, "ＮＴＮ|NTN CORP")                                    ~ "NTN CORPORATION",
    str_detect(name, "TOSHIBA")                                           ~ "TOSHIBA CORP",
    str_detect(name, "SHANGHAI ELECTRIC WIND")                            ~ "SHANGHAI ELECTRIC WIND POWER GROUP CO., LTD.",
    str_detect(name, "ZF FRIEDRICHSHAFEN")                                ~ "ZF FRIEDRICHSHAFEN AG",
    str_detect(name, "ROBERT BOSCH")                                      ~ "ROBERT BOSCH GMBH",
    str_detect(name, "LM WP|LM WIND|LM GLASFIBER")                        ~ "LM WIND POWER A/S",
    str_detect(name, "NORTH CHINA ELECTRIC POWER")                        ~ "NORTH CHINA ELECTRIC POWER UNIVERSITY",
    str_detect(name, "HOHAI UNIVERSITY")                                  ~ "HOHAI UNIVERSITY",
    str_detect(name, "HARBIN ENGINEERING")                                ~ "HARBIN ENGINEERING UNIVERSITY",
    str_detect(name, "SOUTHEAST UNIVERSITY")                              ~ "SOUTHEAST UNIVERSITY",
    str_detect(name, "ZHEJIANG UNIVERSITY")                               ~ "ZHEJIANG UNIVERSITY",
    str_detect(name, "TIANJIN UNIVERSITY")                                ~ "TIANJIN UNIVERSITY",
    str_detect(name, "TONGJI UNIVERSITY")                                 ~ "TONGJI UNIVERSITY",
    str_detect(name, "CHONGQING UNIVERSITY")                              ~ "CHONGQING UNIVERSITY",
    str_detect(name, "XI'AN JIAOTONG")                                    ~ "XI'AN JIAOTONG UNIVERSITY",
    str_detect(name, "BOEING COMPANY")                                    ~ "THE BOEING COMPANY",
    TRUE ~ name
  )
}

# Candidate list of top applicants post-2000, for manual review and
# classification (country / continent / entity type) into top_manual.xlsx
top_post2000_candidates <- pers_appln %>%
  filter(applt_seq_nr > 0) %>%
  inner_join(person, by = "person_id") %>%
  inner_join(appln %>% select(appln_id, year), by = "appln_id") %>%
  filter(!is.na(person_name), year >= 2000) %>%
  mutate(person_name = standardize_names(person_name)) %>%
  group_by(person_name) %>%
  summarise(n_patents = n(), .groups = "drop") %>%
  arrange(desc(n_patents)) %>%
  slice_head(n = 400)
# write_csv(top_post2000_candidates, file.path(output_dir, "top_400_candidates.csv"))
# -> manually verified and classified into data/top_applicants_curated.csv, loaded below.

top_inventors <- readr::read_csv2(file.path(data_dir, "top_applicants_curated.csv"),
                                   locale = readr::locale(encoding = "UTF-8"),
                                   show_col_types = FALSE)

# Sum of patents WITHIN the curated top-inventors list, by period — this is
# the denominator for the country/continent share charts below. It is not
# the full PATSTAT corpus total (that data isn't available here), so these
# shares read as "share of the top applicants' patents", not "share of all
# patents ever filed".
top_period_totals <- top_inventors %>%
  group_by(periodo) %>%
  summarise(period_total = sum(n_brevetti), .groups = "drop")

# ==============================================================================
# PART A — VOLUME AND GEOGRAPHY OF PATENTING, PRE VS POST 2000
# ==============================================================================

patents_per_year <- appln %>% count(year)

p_trend <- ggplot(patents_per_year, aes(year, n)) +
  geom_line(color = "#1F3A93", linewidth = 1.2) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red") +
  annotate("text", x = 2001, y = max(patents_per_year$n), label = "Year 2000", color = "red", hjust = 0) +
  labs(title = "Patent Applications Over Time", x = "Year", y = "Patents") +
  theme_minimal(base_size = 13)
save_plot(p_trend, "01_patents_over_time.png")

period_growth <- patents_per_year %>%
  mutate(period = if_else(year < 2000, "Pre-2000", "Post-2000")) %>%
  group_by(period) %>%
  summarise(
    start = first(n), end = last(n), years = max(year) - min(year),
    avg_annual_growth = (end - start) / years, .groups = "drop"
  )

# Geography: uses the manually classified top_inventors dataset
country_period <- top_inventors %>%
  group_by(paese_verificato, periodo) %>%
  summarise(total_patents = sum(n_brevetti), .groups = "drop")

top10_countries <- country_period %>%
  group_by(paese_verificato) %>%
  summarise(total = sum(total_patents), .groups = "drop") %>%
  slice_max(total, n = 10) %>%
  pull(paese_verificato)

country_period_share <- country_period %>%
  filter(paese_verificato %in% top10_countries) %>%
  left_join(top_period_totals, by = "periodo") %>%
  mutate(share_pct = total_patents / period_total * 100)

p_country_share <- ggplot(country_period_share,
                           aes(reorder(paese_verificato, -share_pct), share_pct, fill = periodo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "Country Share of Patents — Top 10, Pre vs Post 2000",
       x = "Country", y = "Share of period total (%)", fill = "Period") +
  theme_minimal(base_size = 13)
save_plot(p_country_share, "02_country_share.png")

continent_period_share <- top_inventors %>%
  group_by(continente, periodo) %>%
  summarise(total_patents = sum(n_brevetti), .groups = "drop") %>%
  left_join(top_period_totals, by = "periodo") %>%
  mutate(share_pct = total_patents / period_total * 100)

p_continent_share <- ggplot(continent_period_share, aes(continente, share_pct, fill = periodo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "Continent Share of Patents, Pre vs Post 2000",
       x = "Continent", y = "Share of period total (%)", fill = "Period") +
  theme_minimal(base_size = 13)
save_plot(p_continent_share, "03_continent_share.png")

country_growth <- country_period %>%
  pivot_wider(names_from = periodo, values_from = total_patents, values_fill = 0) %>%
  mutate(
    abs_growth = `Post-2000` - `Pre-2000`,
    pct_growth = if_else(`Pre-2000` == 0, NA_real_, (`Post-2000` - `Pre-2000`) / `Pre-2000` * 100)
  ) %>%
  arrange(desc(pct_growth))

kruskal_by_continent <- kruskal.test(n_brevetti ~ as.factor(continente), data = top_inventors)

p_productivity_continent <- ggplot(top_inventors, aes(continente, n_brevetti, fill = continente)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Patent Productivity by Continent", x = "Continent", y = "Patents (log scale)") +
  theme_minimal(base_size = 13)
save_plot(p_productivity_continent, "04_productivity_by_continent.png")

# ---- Concentration indices (HHI) -----------------------------------------------
# Herfindahl-Hirschman Index: sum of squared shares. Higher = more concentrated
# in fewer countries/institutions; lower = more evenly spread.

compute_hhi <- function(df, period_col, category_col, value_col) {
  df %>%
    group_by({{ period_col }}, {{ category_col }}) %>%
    summarise(n = sum({{ value_col }}), .groups = "drop") %>%
    group_by({{ period_col }}) %>%
    mutate(share = n / sum(n)) %>%
    summarise(HHI = sum(share^2), .groups = "drop")
}

geo_hhi <- compute_hhi(top_inventors, periodo, paese_verificato, n_brevetti)

p_geo_hhi <- ggplot(geo_hhi, aes(periodo, HHI, fill = periodo)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "Geographic Concentration of Patents (HHI)", x = NULL, y = "HHI") +
  theme_minimal(base_size = 13)
save_plot(p_geo_hhi, "05_geo_hhi.png")

# ==============================================================================
# PART B — INSTITUTIONAL COMPOSITION AND CONCENTRATION
# ==============================================================================

entity_composition <- top_inventors %>%
  filter(tipo_ente != "Altro") %>%
  count(periodo, tipo_ente, name = "n") %>%
  group_by(periodo) %>%
  mutate(pct = n / sum(n) * 100)

p_entity_composition <- ggplot(entity_composition, aes(tipo_ente, pct, fill = periodo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "Entity Type Composition, Pre vs Post 2000", x = "Entity type", y = "Share (%)") +
  theme_minimal(base_size = 13)
save_plot(p_entity_composition, "06_entity_composition.png")

kruskal_by_entity <- kruskal.test(n_brevetti ~ as.factor(tipo_ente), data = top_inventors)

p_productivity_entity <- ggplot(top_inventors, aes(tipo_ente, n_brevetti, fill = tipo_ente)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Patent Productivity by Entity Type", x = "Entity type", y = "Patents (log scale)") +
  theme_minimal(base_size = 13)
save_plot(p_productivity_entity, "07_productivity_by_entity.png")

entity_hhi <- compute_hhi(top_inventors, periodo, tipo_ente, n_brevetti)

p_entity_hhi <- ggplot(entity_hhi, aes(periodo, HHI, fill = periodo)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "Institutional Concentration of Patents (HHI)", x = NULL, y = "HHI") +
  theme_minimal(base_size = 13)
save_plot(p_entity_hhi, "08_entity_hhi.png")

# ---- Inequality among top inventors (Gini / Lorenz curve) ----------------------

gini_by_period <- top_inventors %>%
  group_by(periodo) %>%
  summarise(gini_index = ineq(n_brevetti, type = "Gini"), .groups = "drop")

png(file.path(output_dir, "09_lorenz_curves.png"), width = 1000, height = 500)
par(mfrow = c(1, 2))
plot(Lc(filter(top_inventors, periodo == "Pre-2000")$n_brevetti),  main = "Lorenz Curve — Pre-2000",  col = "blue")
plot(Lc(filter(top_inventors, periodo == "Post-2000")$n_brevetti), main = "Lorenz Curve — Post-2000", col = "red")
dev.off()

top10_per_period <- top_inventors %>%
  group_by(periodo) %>%
  slice_max(n_brevetti, n = 10) %>%
  ungroup() %>%
  mutate(
    label = paste0(person_name, " (", paese_verificato, ")"),
    label = reorder_within(label, n_brevetti, periodo)
  )

p_top10 <- ggplot(top10_per_period, aes(n_brevetti, label, fill = periodo)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  facet_wrap(~ periodo, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Top 10 Most Prolific Inventors, Pre vs Post 2000", x = "Patents", y = NULL) +
  theme_minimal(base_size = 13)
save_plot(p_top10, "10_top10_inventors.png")

# ==============================================================================
# PART C — GLOBALIZATION: INTERNATIONAL VS NATIONAL FILINGS
# ==============================================================================

family_size <- appln %>%
  group_by(inpadoc_family_id) %>%
  summarise(family_size = n(), .groups = "drop")

appln <- appln %>%
  left_join(family_size, by = "inpadoc_family_id") %>%
  mutate(
    log_family_size = log1p(family_size),
    filing_type     = if_else(family_size > 1, "International", "National")
  )

p_family_size <- ggplot(appln, aes(log_family_size, fill = period)) +
  geom_density(alpha = 0.5) +
  labs(title = "Patent Family Size Distribution, Pre vs Post 2000", x = "log(family size)", y = "Density") +
  theme_minimal(base_size = 13)
save_plot(p_family_size, "11_family_size_density.png")

globalization_by_period <- appln %>%
  count(period, filing_type) %>%
  group_by(period) %>%
  mutate(pct = n / sum(n) * 100)

p_globalization <- ggplot(globalization_by_period, aes(period, pct, fill = filing_type)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("International" = "#4682B4", "National" = "#CD5C5C")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "International vs National Filings, Pre vs Post 2000", x = NULL, y = "Share", fill = "Filing type") +
  theme_minimal(base_size = 13)
save_plot(p_globalization, "12_globalization_share.png")

globalization_by_year <- appln %>%
  filter(year <= 2015) %>%   # later years underestimate family size (still accumulating)
  count(year, filing_type) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n) * 100)

p_globalization_trend <- ggplot(globalization_by_year, aes(year, pct, color = filing_type)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Share of International Filings Over Time", x = "Filing year", y = "Share (%)", color = "Filing type") +
  theme_minimal(base_size = 13)
save_plot(p_globalization_trend, "13_globalization_trend.png")

# ==============================================================================
# PART D — CO-INVENTION NETWORK ANALYSIS
# ==============================================================================
# Builds a co-invention network (edge = two people on the same patent) for a
# random sample of up to 5,000 collaborative applications per period, then
# compares network structure and centrality pre vs post 2000.

build_coinvention_network <- function(appln_data, pers_appln_data, person_data,
                                       start_date, end_date, label, max_applns = 5000) {

  appln_period <- appln_data %>%
    filter(appln_filing_date >= as.Date(start_date), appln_filing_date < as.Date(end_date)) %>%
    select(appln_id, appln_filing_date, docdb_family_size, nb_citing_docdb_fam)

  data <- pers_appln_data %>%
    filter(appln_id %in% appln_period$appln_id) %>%
    left_join(appln_period, by = "appln_id") %>%
    left_join(person_data %>% select(person_id, person_name, person_ctry_code), by = "person_id") %>%
    filter(!is.na(person_name))

  collab_applns <- data %>% count(appln_id) %>% filter(n >= 2) %>% pull(appln_id)
  data <- data %>% filter(appln_id %in% collab_applns)

  set.seed(123)
  sampled_applns <- sample(unique(data$appln_id), size = min(max_applns, length(unique(data$appln_id))))
  data_sampled <- data %>% filter(appln_id %in% sampled_applns)

  edges <- data_sampled %>%
    group_by(appln_id) %>%
    summarise(pairs = list(combn(person_id, 2, simplify = FALSE)), .groups = "drop") %>%
    unnest(pairs) %>%
    mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) %>%
    count(from, to, name = "weight")

  nodes <- data_sampled %>%
    group_by(person_id) %>%
    summarise(
      person_name       = first(person_name),
      person_ctry_code  = first(person_ctry_code),
      avg_citations     = mean(nb_citing_docdb_fam, na.rm = TRUE),
      avg_family_size   = mean(docdb_family_size, na.rm = TRUE),
      n_patents         = n(),
      .groups = "drop"
    )

  g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

  centrality <- tibble(
    person_id   = V(g)$name,
    degree      = degree(g),
    betweenness = betweenness(g, normalized = TRUE)
  ) %>%
    left_join(nodes, by = "person_id") %>%
    mutate(period = label)

  list(centrality = centrality, graph = g)
}

net_pre2000  <- build_coinvention_network(appln, pers_appln, person, "1970-01-01", "2000-01-01", "pre-2000")
net_post2000 <- build_coinvention_network(appln, pers_appln, person, "2000-01-02", "2020-01-01", "post-2000")

centrality_all <- bind_rows(net_pre2000$centrality, net_post2000$centrality)
write_csv(centrality_all, file.path(output_dir, "centrality_comparison.csv"))

top15_pre  <- net_pre2000$centrality  %>% slice_max(betweenness, n = 15)
top15_post <- net_post2000$centrality %>% slice_max(betweenness, n = 15)

p_degree_dist <- ggplot(centrality_all, aes(period, degree, fill = period)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("pre-2000" = "#4682B4", "post-2000" = "#CD5C5C")) +
  scale_y_log10() +
  labs(title = "Network Degree Distribution, Pre vs Post 2000", x = NULL, y = "Degree (log scale)") +
  theme_minimal(base_size = 13)
save_plot(p_degree_dist, "14_degree_distribution.png")

p_citation_density <- ggplot(centrality_all, aes(avg_citations, fill = period)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("pre-2000" = "#4682B4", "post-2000" = "#CD5C5C")) +
  scale_x_log10() +
  labs(title = "Citation Density by Period", x = "Average citations (log scale)", y = "Density")
save_plot(p_citation_density, "15_citation_density.png")

graph_summary <- function(g, label) {
  tibble(
    period      = label,
    nodes       = vcount(g),
    edges       = ecount(g),
    density     = edge_density(g),
    components  = components(g)$no,
    modularity  = modularity(cluster_louvain(g))
  )
}

network_summary <- bind_rows(
  graph_summary(net_pre2000$graph,  "pre-2000"),
  graph_summary(net_post2000$graph, "post-2000")
)

# ==============================================================================
# PART E — TECHNOLOGY SECTORS (IPC CLASSIFICATION)
# ==============================================================================

ipc <- fread(file.path(data_dir, "tls209_appln_ipc.csv"), encoding = "UTF-8") %>%
  mutate(appln_id = as.character(appln_id), ipc_class_symbol = as.character(ipc_class_symbol)) %>%
  filter(!is.na(ipc_class_symbol)) %>%
  mutate(ipc_section = substr(ipc_class_symbol, 1, 1), ipc_class = substr(ipc_class_symbol, 1, 3))

ipc_with_period <- ipc %>%
  inner_join(appln %>% select(appln_id, period), by = "appln_id")

ipc_section_share <- ipc_with_period %>%
  count(period, ipc_section) %>%
  group_by(period) %>%
  mutate(pct = n / sum(n) * 100)

p_ipc_section <- ggplot(ipc_section_share, aes(ipc_section, pct, fill = period)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Pre-2000" = "#4682B4", "Post-2000" = "#CD5C5C")) +
  labs(title = "IPC Section Share, Pre vs Post 2000", x = "IPC section", y = "Share (%)") +
  theme_minimal(base_size = 13)
save_plot(p_ipc_section, "16_ipc_section_share.png")

ipc_section_shift <- ipc_section_share %>%
  select(ipc_section, period, pct) %>%
  pivot_wider(names_from = period, values_from = pct) %>%
  mutate(shift_pp = `Post-2000` - `Pre-2000`) %>%
  arrange(desc(abs(shift_pp)))

ipc_class_shift <- ipc_with_period %>%
  count(period, ipc_class) %>%
  group_by(period) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  select(ipc_class, period, pct) %>%
  pivot_wider(names_from = period, values_from = pct) %>%
  mutate(shift_pp = `Post-2000` - `Pre-2000`) %>%
  arrange(desc(abs(shift_pp)))

ipc_top10_shift <- ipc_class_shift %>% slice_max(abs(shift_pp), n = 10)

p_ipc_top10_shift <- ggplot(ipc_top10_shift, aes(reorder(ipc_class, shift_pp), shift_pp, fill = shift_pp > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Decrease", "Increase"), guide = "none") +
  labs(title = "Top 10 IPC Classes by Shift, Pre vs Post 2000", x = "IPC class", y = "Shift (pp)") +
  theme_minimal(base_size = 13)
save_plot(p_ipc_top10_shift, "17_ipc_top10_shift.png")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("Total patents (top applicants list):", sum(top_period_totals$period_total), "\n")
print(top_period_totals)
print(period_growth)
print(kruskal_by_continent)
print(kruskal_by_entity)
print(gini_by_period)
print(network_summary)
cat("Analysis complete. Figures saved to:", output_dir, "\n")
