# Europe's Biotech Translation Gap

LUISS "Innovazione" course project: does Europe convert biotech patents into
private R&D investment as efficiently as the US and other non-EU countries?

**Data:** merged panel of 8 countries (BEL, DNK, FIN, FRA, DEU, ITA — EU;
KOR, USA — non-EU), 2006-2020:
- OECD Key Biotech Indicators (KBI2): business R&D expenditure in biotech (BERD, M USD PPP)
- OECD Patents in Selected Technologies: biotech patents (IP5 family, applicant, priority date)
- World Bank Worldwide Governance Indicators: Regulatory Quality estimate

`data/panel_biotech.csv` is included — built by merging the three public
sources above (all open/CC-BY licensed), log-transforming patents and BERD,
and flagging EU membership. It's derived/compact data (80 rows), not a bulk
redistribution of the source datasets.

**Method:** panel fixed-effects regression (country + year FE) with a
log(patents) x EU interaction term, robust country-clustered SE, and a full
diagnostic battery (Hausman, F-test for fixed effects, Breusch-Pagan,
Breusch-Godfrey). See the script's header comment for the model equation.

**Key finding:** *(fill in after running — this reconstructed panel gives a
positive, non-significant interaction term (p=0.56), i.e. no significant
translation gap in this data. Your original run may have shown a different
result — re-run and update this line with what you actually get from your
own source files.)*

## Run it

1. `install.packages(c("tidyverse","plm","stargazer","lmtest","sandwich","ggrepel","patchwork","scales","broom"))`
2. `data/panel_biotech.csv` is already included — no extra download needed
3. Run `biotech_translation_gap.R` — figures and the regression table (HTML) are saved to `output/`

## Files

- `biotech_translation_gap.R` — full analysis script
- `data/panel_biotech.csv` — merged panel (included)
- `output/` — generated charts and the regression table

## Notes on what changed from the original script

- Moved `install.packages()` out of the script (see "Run it" above) and fixed `setwd()`
- Fixed a real bug: `geom_text_repel()` in the main scatter chart inherited a
  `colour` aesthetic from the parent plot that its own (unmutated) data didn't
  have, which crashed the plot — gave the labels a fixed colour instead
- Regression table paths now go through `output_dir` instead of the working directory
- The narrative summary block at the end is kept in Italian, since it's meant to be pasted directly into the (Italian) thesis text — everything else (titles, comments, plot labels) is in English
