# Netflix TSR Decomposition & Panel Regression

McKinsey-style additive decomposition of Netflix's Total Shareholder Return
(2021-2025), plus a fixed-effects panel regression across four media/entertainment
companies (AMC, DIS, NFLX, WBD) to estimate which drivers move TSR.

**Data:** `data/panel_dataset_completo.xlsx` (sheet `panel_dataset`, included) —
TSR, revenue growth, margin expansion, PE change, buyback yield per company/year.
Source: StockAnalysis.com / S&P Global Market Intelligence. See the `Note_Metodologiche`
sheet in the same file for per-field sourcing notes (e.g. why PE change is NA
for some company-years, Disney's fiscal-year convention).

**Method:**
- Block 1: additive TSR decomposition for Netflix — PE change and buyback yield
  isolated directly, operating value (revenue + margin) as the residual, split
  proportionally. Exact additivity by construction (verified: residual = 0 for
  every year in the actual data).
- Block 2: OLS with company fixed effects (dummies), NFLX as reference category.

**Key finding:** margin expansion (b=21.1, p<0.01) and revenue growth (b=-5.4,
p<0.01) are the significant structural drivers of TSR in this sample; PE change
is directional but not significant at 11 observations. Netflix's 2022 crash
(TSR -51%) was overwhelmingly a PE de-rating event (-44pp), while the
2023-2024 recovery was PE-driven then increasingly margin-driven.

## Run it

1. `install.packages(c("readxl","tidyverse","ggplot2","scales","modelsummary","broom"))`
2. `data/panel_dataset_completo.xlsx` is already included — no extra download needed
3. Run `netflix_tsr_decomposition.R` — figures and the regression table are saved to `output/`

## Files

- `netflix_tsr_decomposition.R` — full analysis script
- `data/panel_dataset_completo.xlsx` — panel dataset + methodology notes (included)
- `output/` — generated charts and the regression table

## Notes on what changed from the original script

- Moved both `install.packages()` calls out of the script (see "Run it" above)
- Removed "v2" from titles and output filenames — versioning belongs to git, not filenames
- Tested end-to-end against the real dataset — no bugs found; the additivity
  check confirms the decomposition math is exactly correct for every year
- The Block 1 narrative paragraph was hand-written static text in the original
  (not recomputed from data), so it would silently go stale on a re-run —
  replaced with a pointer to the actual numbers. A verified, up-to-date version
  of that paragraph (matching this run's real figures) is in the chat where
  this was cleaned up, ready to paste into the thesis text
- Block 2's interpretive paragraph was already template-driven (`sprintf`) and
  is untouched — it recomputes correctly from whatever data you run it on
- Everything else (titles, comments, plot labels) translated to English; the
  Step C interpretive text stays in Italian, since it's meant to be pasted
  directly into the (Italian) thesis
