# Co-invention Networks and the Evolution of Global Patenting

BSc thesis project: how has global patenting activity changed structurally
since 2000 — in volume, geography, institutional composition, collaboration
networks, and technology sectors?

**Data:** EPO PATSTAT (2022 Spring, sample edition). Not included in this
repo — files are large and licensed by the EPO; download the sample
edition to reproduce.

**Manual curation:** applicant names in PATSTAT are inconsistent across
filings (duplicates, typos, alternate spellings). The top ~400 applicants
by patent count were extracted programmatically, then manually verified
and classified by country, continent and entity type (company / public
body / university) into `data/top_applicants_curated.csv` — this file *is* included,
since it's curated/derived data rather than raw PATSTAT. Parts A and B
run on this file alone and are fully reproducible without PATSTAT access.
Note: the country/continent share figures in those parts are shares
*within this top-applicants list* (602 entries), not shares of all patents
ever filed — the full PATSTAT corpus isn't available here to compute the
latter.

**Method:**
- Temporal and geographic trend analysis, pre vs post 2000
- Concentration analysis: HHI (geographic and institutional), Gini index / Lorenz curves
- Globalization: international vs national filings, patent family size
- Co-invention network analysis: graph construction, degree/betweenness centrality, community detection (igraph)
- Technology sector analysis via IPC classification

**Key finding:** *(fill in after running — e.g. patenting activity has
become more globalized and more concentrated in a smaller set of large
players since 2000, with network density/modularity shifting from X to Y.)*

## Run it

- **Parts A-B only** (geography, concentration, inequality — no PATSTAT
  needed): just run the script with `data/top_applicants_curated.csv` in place, it's
  already included. Tested and confirmed working end-to-end.
- **Parts C-E** (globalization, network analysis, IPC sectors) additionally
  need PATSTAT tables in `data/`: `tls201_appln.csv`, `tls206_person.csv`,
  `tls207_pers_appln.csv`, `tls209_appln_ipc.csv` (not included, see above)

Figures are saved automatically to `output/`.

## Files

- `patent_coinvention_networks.R` — full analysis script
- `data/top_applicants_curated.csv` — manually curated top-applicants list (included)
- `output/` — generated charts and the centrality comparison CSV
