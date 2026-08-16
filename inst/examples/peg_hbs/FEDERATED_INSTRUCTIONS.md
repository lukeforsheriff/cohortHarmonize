# Federated harmonization — a runbook (PEG × HBS)

How the two sites cooperate **without ever sharing raw data**. Two roles:

- **Harmonization lead (you, BWH/HBS):** author the shared recipe, harmonize HBS
  locally, and combine the two harmonized outputs at the end.
- **PEG site (UCLA):** harmonize PEG locally and send back one output file.

Nothing patient-level ever crosses between sites — only the *recipe* (out) and the
*harmonized outputs* (back) move.

---

## What you SEND to the PEG site (a small "kit" — contains NO patient data)

1. A one-line install for the package: `remotes::install_github("lukeforsheriff/cohortHarmonize")`
2. Four config files (plain CSV/R text, no data inside):
   - `dataschema.csv` — the shared target variables
   - `peg_source_map.csv` — how PEG's raw columns map to the schema
   - `dq_metadata.csv` — value rules for the quality report
   - `register_peg_hbs_rules.R` — the custom recode rules
3. The script `run_peg_site.R`.

(Email/Box/shared-drive is fine — none of these files contain PHI.)

## What the PEG site DOES (send them these steps)

1. Install R + the package (the one-line install above).
2. Put the 4 config files and `run_peg_site.R` in one folder; open `run_peg_site.R`.
3. Edit the top line `PEG_DIR` to point at their PEG data folder.
4. Run the script top to bottom. It reads their PEG data **locally**, harmonizes it
   to the shared schema, shows them their own coverage + quality report, and writes
   **one file: `peg_harmonized.rds`** (harmonized variables only — no raw PEG fields).
5. After their data office signs off, they send you that one file.

## What you RECEIVE

- Exactly one file: **`peg_harmonized.rds`**. That's it. It holds PEG's harmonized
  records (the shared DataSchema variables + an id + `source = "PEG"`), not the raw
  PEG codebook fields.

## What YOU then do (in `run_hbs_site.R`)

```r
library(cohortHarmonize)
# you already harmonized HBS locally -> hbs_harmonized.rds
combined <- combine_harmonized(c("hbs_harmonized.rds", "peg_harmonized.rds"))
View(combined)                         # <- this is how you SEE the combined data,
dq_report(combined, "dq_metadata.csv") #    including PEG, without ever touching PEG's raw records
```

`combine_harmonized()` just stacks the two harmonized outputs. Because both sites
used the **same** `dataschema.csv`, the columns line up automatically.

---

## Key points to reassure the PEG team

- **They keep control of their raw data.** It never leaves their machine; the script
  only reads it locally and writes a de-identified, schema-level output.
- **The config files are not data** — they're a recipe (variable names + rules), safe
  to receive and inspect before running.
- **Stricter option:** if their data office won't allow record-level output to leave,
  they change one word — `mode = "summary"` in step 5 — and the file they send holds
  only aggregate statistics (counts/means/distributions), never individual rows. You
  then compare summaries across sites rather than pooling rows.
- **They can inspect first:** `run_peg_site.R` prints their own coverage and quality
  report before writing anything, so they see exactly what will be shared.
