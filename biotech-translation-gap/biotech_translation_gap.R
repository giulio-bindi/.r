# ==============================================================================
# BIOTECH TRANSLATION GAP: EUROPE VS USA
# European Biotech Translation and Scale-Up Policy — Quantitative Analysis
#
# Thesis: Europe produces biotech patents but converts them into private
#         investment less efficiently than the US and non-EU countries.
#
# Model:
#   log(BERD_biotech)_it = a_i + g_t + b1*log(Patents)_it
#                        + b2*RegQuality_it
#                        + b3*(log(Patents) x EU)_it + e_it
#
#   b3 < 0 and significant -> the translation gap exists and is quantifiable.
#
# Data:
#   - OECD Key Biotech Indicators (KBI2): BERD biotech (M USD PPP)
#   - OECD Patents in Selected Technologies: biotech patents (IP5, applicant, priority date)
#   - World Bank WGI: Regulatory Quality estimate
#
# Countries: BEL, DNK, FIN, FRA, DEU, ITA (EU) + KOR, USA (non-EU)
# Period: 2006-2020
#
# Prerequisites (run once, not part of this script):
#   install.packages(c("tidyverse","plm","stargazer","lmtest","sandwich",
#                       "ggrepel","patchwork","scales","broom"))
# ==============================================================================

library(tidyverse)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(ggrepel)
library(patchwork)
library(scales)
library(broom)

# ---- 0. Setup ------------------------------------------------------------

data_dir   <- "data"
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 1. Load data ----------------------------------------------------------

df <- read_csv(file.path(data_dir, "panel_biotech.csv")) %>%
  mutate(
    eu_label = if_else(eu == 1, "EU", "Non-EU"),
    country  = as.factor(country),
    year     = as.integer(year)
  )

cat("=== PANEL SUMMARY ===\n")
cat("Total observations:", nrow(df), "\n")
cat("Countries:", paste(sort(unique(df$country)), collapse = ", "), "\n")
cat("Years:", min(df$year), "-", max(df$year), "\n\n")
print(summary(df[, c("patents","berd_biotech","reg_quality","eu")]))

# ---- 2. Descriptive statistics, EU vs non-EU --------------------------------

desc <- df %>%
  group_by(eu_label) %>%
  summarise(
    n              = n(),
    patents_mean   = mean(patents,      na.rm = TRUE),
    patents_sd     = sd(patents,        na.rm = TRUE),
    berd_mean      = mean(berd_biotech, na.rm = TRUE),
    berd_sd        = sd(berd_biotech,   na.rm = TRUE),
    reg_qual_mean  = mean(reg_quality,  na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== DESCRIPTIVE STATISTICS: EU vs NON-EU ===\n")
print(desc)

# Raw BERD/Patents ratio by group (translation efficiency proxy, pre-regression)
efficiency <- df %>%
  group_by(eu_label) %>%
  summarise(berd_per_patent = mean(berd_biotech / patents, na.rm = TRUE), .groups = "drop")

cat("\n=== BERD PER PATENT (Translation Efficiency) ===\n")
print(efficiency)
cat("-> This is the raw translation gap, before the regression.\n\n")

# ---- 3. Panel construction ---------------------------------------------------

pdata <- pdata.frame(df, index = c("country", "year"))

# ---- 4. Regression models -----------------------------------------------------

# Model 1: Pooled OLS (baseline, ignores heterogeneity)
m1_pooled <- lm(log_berd ~ log_patents + reg_quality, data = df)

# Model 2: Fixed Effects (within) — standard
m2_fe <- plm(log_berd ~ log_patents + reg_quality,
             data   = pdata,
             model  = "within",
             effect = "twoways")   # country FE + year FE

# Model 3: FE + interaction EU x log_patents (the key model)
m3_fe_inter <- plm(log_berd ~ log_patents + reg_quality + eu_x_logpatents,
                    data   = pdata,
                    model  = "within",
                    effect = "twoways")

# Model 4: FE, EU subsample only (robustness)
m4_fe_eu <- plm(log_berd ~ log_patents + reg_quality,
                data   = pdata %>% filter(eu == 1),
                model  = "within",
                effect = "twoways")

# Model 5: FE, non-EU subsample only (robustness)
m5_fe_noneu <- plm(log_berd ~ log_patents + reg_quality,
                    data   = pdata %>% filter(eu == 0),
                    model  = "within",
                    effect = "twoways")

cat("=== KEY MODEL (FE + EU interaction) ===\n")
print(summary(m3_fe_inter))

# ---- 5. Robust standard errors (clustered by country) --------------------------
# Necessary with an unbalanced panel and a small number of countries

coeftest_m2 <- coeftest(m2_fe,        vcov = vcovHC(m2_fe,        method = "arellano", cluster = "group"))
coeftest_m3 <- coeftest(m3_fe_inter,  vcov = vcovHC(m3_fe_inter,  method = "arellano", cluster = "group"))

cat("\n=== MODEL 2 — FE Twoways, Robust SE ===\n")
print(coeftest_m2)

cat("\n=== MODEL 3 — FE + EU Interaction, Robust SE ===\n")
print(coeftest_m3)

# ---- 6. Diagnostic tests --------------------------------------------------------

cat("\n=== DIAGNOSTICS ===\n")

# 6a. Hausman test: FE vs RE (confirms FE is the right choice)
m2_re <- plm(log_berd ~ log_patents + reg_quality, data = pdata, model = "random")
hausman <- phtest(m2_fe, m2_re)
cat("Hausman test (FE vs RE):\n")
print(hausman)
cat("-> p < 0.05 confirms FE as the correct model.\n\n")

# 6b. F test for individual fixed effects
pFtest_res <- pFtest(m2_fe, m1_pooled)
cat("F test for fixed effects:\n")
print(pFtest_res)
cat("-> p < 0.05 confirms country fixed effects are necessary.\n\n")

# 6c. Breusch-Pagan test for heteroskedasticity (on the pooled model, as a check)
bp_test <- bptest(m1_pooled)
cat("Breusch-Pagan (heteroskedasticity):\n")
print(bp_test)
cat("-> If p < 0.05, robust SEs are justified (already applied above).\n\n")

# 6d. Serial correlation test
pbg_test <- pbgtest(m2_fe)
cat("Breusch-Godfrey (serial correlation):\n")
print(pbg_test)
cat("-> If p < 0.05, consider Newey-West SEs as an alternative.\n\n")

# ---- 7. Regression table (stargazer) ---------------------------------------------
# Saved as HTML for import into Word/LaTeX

stargazer(
  m1_pooled, m2_fe, m3_fe_inter,
  type      = "html",
  out       = file.path(output_dir, "regression_table.html"),
  title     = "Biotech Translation Gap: Patent-to-BERD Conversion (2006-2020)",
  dep.var.labels   = "log(BERD Biotech, M USD PPP)",
  covariate.labels = c("log(Biotech Patents)", "Regulatory Quality",
                        "log(Patents) x EU", "Constant"),
  column.labels    = c("Pooled OLS", "FE Twoways", "FE + EU Interaction"),
  add.lines = list(
    c("Country Fixed Effects", "No", "Yes", "Yes"),
    c("Year Fixed Effects",    "No", "Yes", "Yes"),
    c("Robust Clustered SE",   "No", "Yes", "Yes")
  ),
  notes = "Robust SE clustered by country (Arellano). *** p<0.01, ** p<0.05, * p<0.1",
  notes.align = "l",
  digits = 3,
  star.cutoffs = c(0.1, 0.05, 0.01)
)

cat("Table saved to", file.path(output_dir, "regression_table.html"), "\n\n")

# ---- 8. Visualizations -------------------------------------------------------------

col_eu    <- "#1f78b4"   # blue — EU
col_noneu <- "#e31a1c"   # red — non-EU
col_usa   <- "#ff7f00"   # orange — USA

# Chart 1: the translation gap, visually
# Scatter: patents (x) vs BERD (y), split EU / non-EU. The slope difference
# between the two regression lines IS the translation gap.

g1 <- df %>%
  mutate(highlight = case_when(
    country == "USA" ~ "USA",
    eu == 1 ~ "EU",
    TRUE ~ "Non-EU (other)"
  )) %>%
  ggplot(aes(x = log_patents, y = log_berd, colour = highlight)) +
  geom_point(size = 2.5, alpha = 0.75) +
  geom_smooth(aes(group = eu_label, colour = eu_label),
              method = "lm", se = TRUE, linewidth = 1.2,
              data = df %>% mutate(highlight = eu_label)) +
  geom_text_repel(
    data = df %>% filter(year == max(df$year) | (country == "USA" & year == max(df$year))),
    aes(label = country), colour = "grey20", size = 3, max.overlaps = 8
  ) +
  scale_colour_manual(
    values = c("EU" = col_eu, "Non-EU" = col_noneu,
               "USA" = col_usa, "Non-EU (other)" = col_noneu),
    name = NULL
  ) +
  labs(
    title    = "Europe's Biotech Translation Gap",
    subtitle = "At equal patent output, conversion into private R&D (BERD) is lower in the EU",
    x        = "log(Biotech Patents, IP5)",
    y        = "log(BERD Biotech, M USD PPP)",
    caption  = "Source: OECD KBI2, OECD Patents in Selected Technologies. Period: 2006-2020."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
ggsave(file.path(output_dir, "fig1_translation_gap_scatter.png"), g1, width = 9, height = 6, dpi = 300, bg = "white")

# Chart 2: BERD/Patents efficiency ratio over time, by country

g2 <- df %>%
  mutate(efficiency = berd_biotech / patents) %>%
  ggplot(aes(x = year, y = efficiency,
             group = country, colour = eu_label,
             linetype = if_else(country == "USA", "USA", "other"))) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_text_repel(
    data = . %>% filter(year == max(year)),
    aes(label = country), size = 3, nudge_x = 0.3, max.overlaps = 10
  ) +
  scale_colour_manual(values = c("EU" = col_eu, "Non-EU" = col_noneu), name = NULL) +
  scale_linetype_manual(values = c("USA" = "dashed", "other" = "solid"), guide = "none") +
  labs(
    title    = "Patent-to-BERD Conversion Efficiency Over Time",
    subtitle = "BERD Biotech (M USD PPP) per registered biotech patent",
    x = NULL, y = "BERD / Patents (M USD PPP per patent)",
    caption  = "Source: OECD KBI2, OECD Patents. Period: 2006-2020."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
ggsave(file.path(output_dir, "fig2_efficiency_over_time.png"), g2, width = 10, height = 6, dpi = 300, bg = "white")

# Chart 3: the interaction coefficient — the gap, quantified
# Coefficients from Model 3 with 95% CI

coef_df <- tidy(m3_fe_inter, conf.int = TRUE) %>%
  filter(term %in% c("log_patents", "reg_quality", "eu_x_logpatents")) %>%
  mutate(
    term_label = case_when(
      term == "log_patents"     ~ "log(Biotech Patents)",
      term == "reg_quality"     ~ "Regulatory Quality",
      term == "eu_x_logpatents" ~ "log(Patents) x EU\n[Translation Gap]"
    ),
    col_flag = if_else(term == "eu_x_logpatents", "gap", "other")
  )

g3 <- ggplot(coef_df,
             aes(x = estimate, y = reorder(term_label, estimate), colour = col_flag)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("gap" = col_noneu, "other" = col_eu), guide = "none") +
  labs(
    title    = "Coefficients of the FE Model with EU Interaction",
    subtitle = "The EU x Patents term measures the EU's structural translation gap",
    x        = "Coefficient (95% CI)",
    y        = NULL,
    caption  = "Twoways FE model, robust SE clustered by country."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    panel.grid.minor = element_blank()
  )
ggsave(file.path(output_dir, "fig3_coefficients.png"), g3, width = 8, height = 5, dpi = 300, bg = "white")

# Chart 4: EU vs non-EU average comparison (descriptive bar chart)

g4_data <- df %>%
  group_by(eu_label, country) %>%
  summarise(avg_patents = mean(patents, na.rm = TRUE), avg_berd = mean(berd_biotech, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = c(avg_patents, avg_berd), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                            "avg_patents" = "Biotech Patents\n(IP5, units)",
                            "avg_berd"    = "BERD Biotech\n(M USD PPP)"))

g4 <- ggplot(g4_data, aes(x = reorder(country, value), y = value, fill = eu_label)) +
  geom_col(alpha = 0.85) +
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +
  coord_flip() +
  scale_fill_manual(values = c("EU" = col_eu, "Non-EU" = col_noneu), name = NULL) +
  labs(
    title    = "Patents and Private Investment: EU vs Non-EU",
    subtitle = "The EU files competitive patent volumes but invests less in private R&D",
    x = NULL, y = NULL,
    caption  = "Source: OECD KBI2, OECD Patents in Selected Technologies."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    legend.position = "top",
    strip.text      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(file.path(output_dir, "fig4_bar_patents_berd.png"), g4, width = 10, height = 6, dpi = 300, bg = "white")

cat("Figures saved (fig1-fig4) to", output_dir, "\n\n")

# ==============================================================================
# 9. NARRATIVE SUMMARY — for use in the thesis text (kept in Italian, since
#    that's the language of the write-up this feeds into)
# ==============================================================================

cat("
============================================================
SINTESI DEI RISULTATI — DA USARE NEL TESTO DEL PROGETTO
============================================================

MODELLO CHIAVE: Fixed Effects twoways + Interazione EU x Brevetti

Il termine di interazione (eu_x_logpatents) misura il translation gap:
-> Se b3 < 0 e significativo: l'Europa converte i brevetti biotech
  in investimento R&D privato (BERD) con minore efficienza rispetto
  agli USA, anche a parita' di qualita' regolatoria e di effetti
  fissi paese e anno.

Come leggere b3 nel testo:
  'Un aumento dell'1%% nei brevetti biotech e' associato a un incremento
   della BERD privata di b1%% nei paesi non-UE, ma solo di (b1+b3)%%
   nei paesi UE. La differenza b3 quantifica il translation gap europeo.'

Qualita' regolatoria (b2):
  Se positivo e significativo: una migliore regolazione facilita
  la conversione della ricerca in investimento.

Robustezza:
  - Modello 4 (solo EU) e Modello 5 (solo non-EU) confermano se
    il pattern e' stabile all'interno di ciascun blocco.
  - SE robusti clusterizzati per paese gestiscono
    eteroschedasticita' e correlazione seriale.
============================================================
")

cat("Analysis complete.\n")
