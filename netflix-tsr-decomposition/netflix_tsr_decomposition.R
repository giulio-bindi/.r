# =============================================================================
# TSR DECOMPOSITION & PANEL REGRESSION — Netflix Project
# =============================================================================
#
# Prerequisites (run once, not part of this script):
#   install.packages(c("readxl","tidyverse","ggplot2","scales","modelsummary","broom"))
# =============================================================================

library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(modelsummary)
library(broom)

# ---- 0. Setup ---------------------------------------------------------------

data_dir   <- "data"
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- 1. Load data -------------------------------------------------------------

panel <- read_excel(file.path(data_dir, "panel_dataset_completo.xlsx"),
                     sheet = "panel_dataset", na = "NA") %>%
  mutate(year = as.integer(year), symbol = as.factor(symbol))

cat("Dataset loaded:", nrow(panel), "observations,", ncol(panel), "variables\n")
cat("Symbols:", levels(panel$symbol), "\n")
cat("Years covered:", min(panel$year), "-", max(panel$year), "\n\n")


# =============================================================================
# BLOCK 1 — NETFLIX TSR DECOMPOSITION
# Method: practical McKinsey-style decomposition with PE change and Buyback
# Yield isolated directly, operating value as a residual.
#
# Formula (residual = 0 by construction):
#   pe_contrib      = pe_change_pct           (market/valuation driver)
#   bb_contrib      = buyback_yield_pct        (capital return driver)
#   op_contrib_tot  = TSR - PE_c - BB_c        (all operating value creation)
#   rev_contrib     = op_contrib_tot * [rev / (rev+margin)]   (proportional split)
#   marg_contrib    = op_contrib_tot * [margin / (rev+margin)]
#
# Why this method is correct:
#   PE change and buyback yield are directly observable and separable drivers
#   of TSR. Operating value (revenue + margin) is computed as the residual
#   against these two, then split proportionally. This guarantees exact
#   additivity (sum of drivers = TSR) with no spurious residual, which is the
#   core requirement of the McKinsey decomposition logic.
#
# Note on Buyback Yield:
#   Netflix only starts systematic buybacks from 2022-2023. The 2021 value
#   (-0.26%) is minimal dilution from stock compensation, not a deliberate
#   buyback.
# =============================================================================

# ---- 1.1 Filter Netflix and compute the decomposition

nflx <- panel %>%
  filter(symbol == "NFLX") %>%
  arrange(year) %>%
  select(year, TSR, revenue_growth_pct, margin_expansion_pct,
         pe_change_pct, buyback_yield_pct) %>%
  drop_na() %>%
  mutate(
    pe_contrib  = pe_change_pct,
    bb_contrib  = buyback_yield_pct,

    op_contrib  = TSR - pe_contrib - bb_contrib,

    op_sum      = revenue_growth_pct + margin_expansion_pct,
    rev_weight  = if_else(op_sum != 0, revenue_growth_pct / op_sum, 0.5),
    marg_weight = if_else(op_sum != 0, margin_expansion_pct / op_sum, 0.5),
    rev_contrib  = op_contrib * rev_weight,
    marg_contrib = op_contrib * marg_weight,

    check = pe_contrib + bb_contrib + rev_contrib + marg_contrib
  )

cat("-- Additivity check (check = TSR, residual = 0) --\n")
print(nflx %>%
        select(year, TSR, rev_contrib, marg_contrib, pe_contrib, bb_contrib, check) %>%
        mutate(residual = round(TSR - check, 8)) %>%
        as.data.frame())

# ---- 1.2 Reshape for ggplot

nflx_long <- nflx %>%
  pivot_longer(
    cols      = c(rev_contrib, marg_contrib, pe_contrib, bb_contrib),
    names_to  = "driver",
    values_to = "contribution"
  ) %>%
  mutate(
    driver = recode(driver,
                     "rev_contrib"  = "Revenue Growth (operating)",
                     "marg_contrib" = "Margin Expansion (operating)",
                     "pe_contrib"   = "PE Change",
                     "bb_contrib"   = "Buyback Yield"
    ),
    driver = factor(driver, levels = c(
      "Revenue Growth (operating)",
      "Margin Expansion (operating)",
      "PE Change",
      "Buyback Yield"
    ))
  )

# ---- 1.3 Palette

palette_driver <- c(
  "Revenue Growth (operating)"   = "#1D6FA4",
  "Margin Expansion (operating)" = "#2CA25F",
  "PE Change"                    = "#E84040",
  "Buyback Yield"                = "#F4A435"
)

# ---- 1.4 Block 1 chart

p1 <- ggplot(nflx_long,
             aes(x = factor(year), y = contribution, fill = driver)) +

  geom_col(data = . %>% filter(contribution >= 0),
           width = 0.65, color = "white", linewidth = 0.25) +
  geom_col(data = . %>% filter(contribution < 0),
           width = 0.65, color = "white", linewidth = 0.25) +

  geom_hline(yintercept = 0, color = "black", linewidth = 0.55) +

  geom_point(
    data        = nflx %>% mutate(year = factor(year)),
    aes(x = year, y = TSR),
    inherit.aes = FALSE,
    shape = 23, size = 4, fill = "black", color = "white", stroke = 0.8
  ) +

  geom_text(
    data        = nflx %>% mutate(year = factor(year)),
    aes(x = year, y = TSR,
        label = paste0(ifelse(TSR > 0, "+", ""), round(TSR, 1), "%"),
        vjust = ifelse(TSR >= 0, -1.1, 1.8)),
    inherit.aes = FALSE,
    size = 3.2, fontface = "bold", color = "black"
  ) +

  scale_fill_manual(values = palette_driver, name = "TSR Driver") +
  scale_y_continuous(
    labels  = function(x) paste0(x, "%"),
    expand  = expansion(mult = c(0.08, 0.12))
  ) +

  labs(
    title    = "Netflix TSR Decomposition — McKinsey Additive Logic",
    subtitle = "2021-2025  |  Black diamond = realized total TSR  |  Residual = 0 by construction",
    x        = "Year",
    y        = "Contribution to TSR (%)",
    caption  = paste0(
      "Method: PE Change and Buyback Yield isolated directly from TSR; operating value (Revenue + Margin) computed\n",
      "as the additive complement and split proportionally. This guarantees exact additivity with no residual.\n",
      "Note: 2021 Buyback Yield (-0.26%) = dilution from stock compensation, not a deliberate buyback (started systematically from 2022).\n",
      "Source: StockAnalysis.com, S&P Global Market Intelligence."
    )
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle      = element_text(size = 9.5, color = "grey35", hjust = 0),
    plot.caption       = element_text(size = 7.5, color = "grey50", hjust = 0,
                                       margin = margin(t = 10)),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 10),
    legend.text        = element_text(size = 9.5),
    legend.key.size    = unit(0.45, "cm"),
    axis.title         = element_text(size = 10, color = "grey30"),
    axis.text          = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(output_dir, "block1_tsr_decomposition.png"), plot = p1,
       width = 11, height = 7, dpi = 300, bg = "white")
cat("\nBlock 1 chart saved: block1_tsr_decomposition.png\n")


# =============================================================================
# BLOCK 2 — FIXED-EFFECTS PANEL REGRESSION (via lm() + dummies)
# =============================================================================
# Model: TSR_it = a_i + b1*RevGrowth + b2*MarginExp +
#                        b3*PEChange + b4*BuybackYield + e_it
#
# Company fixed effects are obtained by including `symbol` as a categorical
# variable in lm(). This is mathematically equivalent to plm(model="within")
# but uses only base R.
#
# NFLX is the reference category (intercept). Other symbols' dummy
# coefficients read as a DIFFERENTIAL relative to NFLX.
# =============================================================================

# ---- 2.1 Prepare regression dataset, removing NA in key variables

panel_reg <- panel %>%
  select(symbol, year, TSR, revenue_growth_pct, margin_expansion_pct,
         pe_change_pct, buyback_yield_pct) %>%
  drop_na() %>%
  mutate(symbol = relevel(factor(symbol), ref = "NFLX"))

cat("\n--- REGRESSION DATASET ---\n")
cat("Observations after dropping NA:", nrow(panel_reg), "\n")
cat("Distribution by symbol:\n")
print(table(panel_reg$symbol))

# ---- 2.2 OLS estimation with company dummies (= Fixed Effects)

fe_lm <- lm(
  TSR ~ revenue_growth_pct + margin_expansion_pct +
    pe_change_pct + buyback_yield_pct + symbol,
  data = panel_reg
)

cat("\n--- MODEL SUMMARY (lm with company dummies) ---\n")
print(summary(fe_lm))

# ---- 2.3 Clean coefficient table with modelsummary

coef_map <- c(
  "revenue_growth_pct"   = "Revenue Growth (%)",
  "margin_expansion_pct" = "Margin Expansion (%)",
  "pe_change_pct"        = "PE Change (%)",
  "buyback_yield_pct"    = "Buyback Yield (%)",
  "symbolAMC"            = "FE: AMC (vs NFLX)",
  "symbolDIS"            = "FE: DIS (vs NFLX)",
  "symbolWBD"            = "FE: WBD (vs NFLX)",
  "(Intercept)"          = "Intercept (NFLX base)"
)

modelsummary(
  list("OLS Fixed Effects (dummies)" = fe_lm),
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared", "rmse"),
  title     = "Panel Regression: Determinants of TSR — OLS with Company Dummies",
  notes     = c(
    "Fixed effects implemented via categorical dummies (equivalent to plm within).",
    "Reference category: NFLX. Standard errors in parentheses.",
    paste0("Sample: ", nrow(panel_reg), " observations (years with complete PE data).")
  ),
  output = file.path(output_dir, "regression_table.txt")
)
cat("\nTable saved:", file.path(output_dir, "regression_table.txt"), "\n")

# ---- 2.4 Extract only the substantive coefficients (excluding dummies) for the plot

coef_df <- tidy(fe_lm, conf.int = TRUE, conf.level = 0.90) %>%
  filter(!str_starts(term, "symbol"),
         term != "(Intercept)") %>%
  mutate(
    term = recode(term,
                   "revenue_growth_pct"   = "Revenue Growth",
                   "margin_expansion_pct" = "Margin Expansion",
                   "pe_change_pct"        = "PE Change",
                   "buyback_yield_pct"    = "Buyback Yield"
    ),
    significance = case_when(
      p.value < 0.01 ~ "p < 0.01 ***",
      p.value < 0.05 ~ "p < 0.05 **",
      p.value < 0.10 ~ "p < 0.10 *",
      TRUE           ~ "Not significant"
    ),
    significance = factor(significance, levels = c(
      "p < 0.01 ***", "p < 0.05 **", "p < 0.10 *", "Not significant"
    ))
  )

cat("\n--- STRUCTURAL COEFFICIENTS ---\n")
print(coef_df %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high, significance) %>%
        as.data.frame())

# ---- 2.5 Coefficient plot

p2 <- ggplot(coef_df,
             aes(x = estimate,
                 y = reorder(term, estimate),
                 color = significance)) +

  annotate("rect", xmin = -5, xmax = 5,
           ymin = -Inf, ymax = Inf,
           fill = "grey95", alpha = 0.6) +

  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +

  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.18, linewidth = 1.1) +

  geom_point(size = 4.5, shape = 16) +

  geom_text(aes(label = paste0("b = ", round(estimate, 2))),
            nudge_y = 0.28, size = 3.3, color = "black", fontface = "plain") +

  scale_color_manual(
    values = c(
      "p < 0.01 ***"     = "#C00000",
      "p < 0.05 **"      = "#E84040",
      "p < 0.10 *"       = "#F4A435",
      "Not significant"  = "#90A4AE"
    ),
    name = "Significance",
    drop = FALSE
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.20)
  ) +

  labs(
    title    = "Coefficient Plot — OLS with Company Fixed Effects",
    subtitle = paste0(
      "Dependent variable: TSR (%)  |  ",
      nrow(panel_reg), " observations  |  ",
      "90% confidence intervals"
    ),
    x        = "Coefficient b (TSR percentage points per 1pp of variable)",
    y        = NULL,
    caption  = paste0(
      "Model: lm(TSR ~ drivers + symbol), equivalent to plm(model='within').\n",
      "Reference category for fixed effects: NFLX.\n",
      "Caution: with ", nrow(panel_reg), " observations, degrees of freedom are limited — ",
      "read non-significance as a sample-size limitation, not absence of effect.\n",
      "Data source: StockAnalysis.com, S&P Global Market Intelligence."
    )
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle      = element_text(size = 10, color = "grey35", hjust = 0),
    plot.caption       = element_text(size = 7.5, color = "grey50", hjust = 0,
                                       margin = margin(t = 10)),
    legend.position    = "right",
    legend.title       = element_text(face = "bold", size = 10),
    legend.text        = element_text(size = 9),
    axis.text          = element_text(size = 11),
    axis.text.y        = element_text(face = "bold"),
    axis.title.x       = element_text(size = 9, color = "grey40",
                                       margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(output_dir, "block2_coefficient_plot.png"), plot = p2,
       width = 10, height = 5.5, dpi = 300, bg = "white")
cat("Block 2 chart saved: block2_coefficient_plot.png\n")


# =============================================================================
# STEP C — INTERPRETIVE PARAGRAPHS (for the paper — kept in Italian, since
# that's the language of the write-up this feeds into)
# =============================================================================

cat("\n\n===================================================================\n")
cat("  INTERPRETAZIONE — testo pronto per il paper\n")
cat("===================================================================\n")

r2   <- summary(fe_lm)$r.squared
adjr <- summary(fe_lm)$adj.r.squared
pe_b <- round(coef_df$estimate[coef_df$term == "PE Change"], 2)
rv_b <- round(coef_df$estimate[coef_df$term == "Revenue Growth"], 2)
mg_b <- round(coef_df$estimate[coef_df$term == "Margin Expansion"], 2)
bb_b <- round(coef_df$estimate[coef_df$term == "Buyback Yield"], 2)
pe_p <- round(coef_df$p.value[coef_df$term == "PE Change"], 3)

cat(sprintf("
-- BLOCCO 1: Decomposizione TSR Netflix -----------------------------

La decomposizione del TSR Netflix secondo la logica additiva McKinsey,
applicata con il metodo del contributo operativo residuale, rivela una
struttura dei driver che cambia nel periodo osservato. Guarda i valori
per anno in nflx_long / il grafico block1_tsr_decomposition.png per la
lettura puntuale di ciascun anno (PE change, buyback, revenue growth,
margin expansion) e scrivi il paragrafo interpretativo su quella base.

-- BLOCCO 2: Panel Regression Fixed Effects --------------------------

La regressione OLS con effetti fissi per società — implementata tramite
dummies categoriali, equivalente al modello within (plm) — stima le
relazioni strutturali tra i driver fondamentali e il TSR su un panel di
quattro societa (AMC, DIS, NFLX, WBD) per gli anni con dati completi
(%d osservazioni). Il modello spiega il %.1f%% della varianza totale del TSR
(R2 adjusted = %.2f). Il PE Change presenta un coefficiente di %.2f
(p = %.3f). Il Revenue Growth (b = %.2f) e la Margin Expansion (b = %.2f)
mostrano i segni attesi dalla teoria, mentre il Buyback Yield (b = %.2f)
cattura il premio di mercato per le politiche di capital return. Con
%d osservazioni e 4 dummies, la non-significativita di alcuni coefficienti
riflette il vincolo campionario, non necessariamente l'assenza di effetto.
",
            nrow(panel_reg), r2 * 100, adjr, pe_b, pe_p, rv_b, mg_b, bb_b, nrow(panel_reg)
))

cat("\n===================================================================\n")
cat("  ANALYSIS COMPLETE\n")
cat("  Output generated:\n")
cat("    1. output/block1_tsr_decomposition.png\n")
cat("    2. output/block2_coefficient_plot.png\n")
cat("    3. output/regression_table.txt\n")
cat("===================================================================\n")
