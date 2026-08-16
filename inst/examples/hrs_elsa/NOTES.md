# HRS + ELSA example

A second worked example (alongside `peg_hbs/`) showing `cohortHarmonize` on two
**unrelated aging cohorts** — the Health and Retirement Study (US) and the
English Longitudinal Study of Ageing (UK) — to demonstrate the package generalizes
well beyond the Parkinson's work it was built for.

**What's here (all editable CSVs + two short R scripts):**

| file | what it is |
|---|---|
| `dataschema.csv` | 10 shared target variables (demographics, health, mood, behaviour) |
| `hrs_source_map.csv` | maps HRS's raw columns → the schema |
| `elsa_source_map.csv` | maps ELSA's raw columns → the schema |
| `dq_metadata.csv` | admissible values / limits for the quality report |
| `hrs_synthetic.csv`, `elsa_synthetic.csv` | small stand-in data so it runs offline |
| `register_hrs_elsa_rules.R` | one custom rule (`edu_years_to_cat`) |
| `run_hrs_elsa.R` | run it end-to-end |

**The teaching point:** the two synthetic files use *different variable names and
different codings* on purpose — ELSA's `sex` is 0/1 (HRS's is 1/2), ELSA's
conditions are 1=yes/2=no (HRS's are 0/1), and ELSA's self-rated health and
education categories run in the opposite direction. The source maps + built-in
`map` rule reconcile all of that onto one schema. That's the real job of
harmonization — not just renaming, but re-coding to a common target.

**To run:** open `run_hrs_elsa.R` and run it top to bottom (it uses the synthetic
files). To use the **real** data, register at <https://g2aging.org>, download the
Harmonized HRS and Harmonized ELSA Stata files, read them with `source_haven()`,
and point the maps' `source_columns` at the real wave variables (the codebook lists
them). The engine, rules, and schema don't change.
