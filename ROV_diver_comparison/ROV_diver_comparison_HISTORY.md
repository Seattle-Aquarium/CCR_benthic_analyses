# ROV–diver methods comparison — project history

**Repository:** [Seattle-Aquarium/CCR_benthic_analyses](https://github.com/Seattle-Aquarium/CCR_benthic_analyses) → `ROV_diver_comparison/`
**Local path:** `Coastal_Climate_Resilience\GitHub\CCR_benthic_analyses\ROV_diver_comparison`
**Active period:** 2026-07-20 → 2026-08-06 (10 Claude Code sessions, ~215 prompts)
**Status at archive time:** analyses complete; technical report in LaTeX, under revision.

This is the largest and longest-running of the archived projects. It is a formal
methods comparison asking whether ROV-based imagery surveys recover the same
benthic community signal that Reef Check diver surveys do.

---

## 1. What the project is

Two sampling methods were run over the same transects at two Elliott Bay sites —
**Centennial Park (CP)** and the **Elliott Bay Marina breakwater (EBM)** — across
**two seasons (summer, winter)** and **two depth strata (deep, shallow)**, giving
24 transects total.

| | ROV | Diver |
|---|---|---|
| Unit of observation | Photo (≈60 per transect) | Transect |
| Percent-cover method | 50 randomly distributed points per photo, classified in CoralNet-Toolbox | Uniform Point Contact (UPC) |
| Abundance method | VIAME object detection annotations | Direct invertebrate counts |
| Spatial detail | Per-metre along transect, lat/lon per photo | Transect-level totals |

Each 30 m transect is flown **twice** — an outward pass on one side of the
transect tape and a return pass on the other — so ~60 photos per transect, 30 per
pass.

---

## 2. Chronology of the work

### 2026-07-20 — Data structure and the percent-cover pipeline
*Session `9b54d606`, 12 prompts*

Starting point was `data/ROV/HSIL_percent_cover.csv`: 33 metadata columns followed
by ~30 percent-cover columns. Built the first R/tidyverse wrangling pipeline and
established the **three-stage transformation** that the whole project rests on:

1. **Points per photo** — multiply each percent-cover proportion by 50 to recover
   the actual number of classified points (0.50 × 50 = 25 of 50 points).
2. **Transect sums** — sum those point counts within transect, producing 24 rows ×
   30 categories, plus a `total_points` column (number of photos × 50).
3. **Transect proportions** — divide each category sum by that row's `total_points`.

Also established the repo convention that governs everything downstream:
`data/` = inputs, `code/` = scripts, `results/` = transformed data ready for
analysis, `figs/` = figures.

Introduced `combine.cols()` to merge fine ROV categories into coarser ones that
match Reef Check's scheme — e.g. four red-algae categories (branching, bushy,
filamentous, flat-leaf) into `combined_red_algae`; CCA + encrusting into
`combined_encrusting_algae`; filamentous + *Ulva* into `combined_green_algae`.

> **Important gotcha, carried forward:** combined columns are *additive duplicates*
> of their constituents. Including them makes a row's percent-cover sum exceed 1
> (and point counts exceed 50). They must be excluded from any analysis that
> assumes a closed composition — notably the NMDS community matrix.

Battery columns (`Battery_V`, `_A`, `_W`, `_mAh_used`, `_Wh_used`) were explicitly
retained through the pipeline.

### 2026-07-22 — Diver UPC wrangling and ROV↔diver category mapping
*Session `c0481cb8`, 33 prompts*

Reworked `wrangle_diver_data.R` and `wrangle_data_functions.R` to emit
`results/diver/diver_UPC_percentage.csv`. Normalised all diver column names to
lowercase-underscore (`Cover_Green Algae` → `cover_green_algae`).

Built the **category crosswalk** between ROV and diver schemes — the conceptual
core of the comparison — and produced Markdown tables for a GitHub Issue in
`CCR_benthic_analyses`. Key asymmetries established here:

- `red_algae_branching` has **no diver analog**.
- `cover_articulated_coralline` has **no ROV analog** (articulated coralline does
  not occur in Elliott Bay).
- Substrate ordered largest → smallest: reef/boulder, rock, cobble, pebble, sand, silt.

New diver combined categories, with a naming convention settled after one
false start: **every combined column starts with `combined_`**.

- `combined_substrate_boulder` ← `substrate_large_boulder_(50cm-1m-wa)` + `substrate_reef`
- `combined_substrate_pebble` ← `substrate_pebble_(0.5-5cm-wa)` + `substrate_cobble_(5-15cm-wa)`

### 2026-07-26 — Outward/return pass classification and distribution figures
*Session `ca46bda5`, 36 prompts*

Used per-photo lat/lon to classify each image as **outward** or **return** pass,
adding a `pass` column. This unlocked spatially explicit plots along the transect.

Figure development, iterated heavily:
- Proportion-through-space plots for sugar kelp (*Saccharina*) and sieve kelp
  (*Agarum*), scientific names used throughout.
- Kernel density distributions of percent cover — abandoned as primary display
  because the data are **strongly zero-inflated** and the densities were
  uninterpretable.
- Resolution: plot only non-zero observations, with the zero fraction reported
  explicitly rather than hidden in the density.
- Final form modelled on **Fig. 4 of Randell et al. 2022** (kelp forest dynamics
  controlled by substrate complexity): violin + points, with the **percentage of
  photos where the taxon occurred printed as large black text above each transect's
  violin**, replacing a separate bar chart.

### 2026-07-28 (a) — NMDS ordination
*Session `85f485e1`, 25 prompts*

Two-dimensional ordination on the **photo-level** community matrix
(`HSIL_percent-cover_photo-level.csv`, columns `pebble` → `red_algae_encrusting`),
**Bray–Curtis** dissimilarity, following the existing house pattern in the
repo's `community_analyses/` folder.

Workflow: run `metaMDS`, extract (x, y) coordinates from the best-fitting
ordination, bind them back to the community matrix + metadata, write out for
visualisation; save taxa correlation coefficients separately. Produced
`NMDS.R`, `NMDS_functions.R`, `NMDS_visualization.R`,
`NMDS_visualization_functions.R`.

The `combined_*` columns were **excluded** from the ordination for the
double-counting reason above.

Final visualisation: a **single ordination** showing all site × depth × season
groups at once with points and ellipses, so overlap is directly readable. Colour
scheme deliberately inherited from the violin plots — **EBM in blues, CP in
oranges**, with depth and season varying shade within each site's family.

### 2026-07-28 (b) — VIAME abundance export cleanup
*Session `93f6ec11`, 20 prompts*

Cleaned `HSIL_viame_export.csv` and joined metadata (`Date`, `season`, `pass`,
`site`, `transect`, `depth`, `Transect_ID`) from the photo-level percent-cover
file by photo name. Renamed cryptic columns (e.g. `SS_ochre` →
`ochre_mottled_star`) to match diver column names in `diver_invert_abundance.csv`.

> **Data-quality finding — worth remembering.** A large share of VIAME photos did
> not match a percent-cover photo. The distinction that mattered: this was a
> **join-coverage** problem, not 69% of Transect IDs being mislabelled. Understanding
> which of those two it was drove the rest of the session.

The deeper issue: volunteers annotated **every** photo to maximise training
imagery for object detection, which means the same physical organism appears in
multiple overlapping photos and is **double-counted** in raw abundance. This
motivated `build_HSIL_viame_abundance_corrected.R` and the
`_corrected` / `_corrected_summed` result files.

Also built `annotate_viame_detections.R` — copies survey images into a separate
folder and **superimposes the object-detection boxes** on them, so the annotation
set can be visually audited against the imagery.

Outputs: `HSIL_abundances_photo_scale.csv`, `HSIL_abundances_averaged.csv`.

### 2026-07-31 — Statistical models and the technical report
*Session `62b4bc29`, 80 prompts — the single largest session in the archive*

Context loaded at the start (all of these are worth re-supplying to a fresh Claude):
- `GitHub_Digest_CCR_core_program.md` — program overview
- `2024_YearEnd_Report_Urban-Kelp-Research-Project.pdf` — Port of Seattle year-end
  report; extensive CoralNet-Toolbox methodology
- `ROV_field_methods_manuscript.tex` + `_supplementary.tex` — the MEE draft
- `QAPP_2024.pdf` (full, best motivation for the comparison), `QAPP_2025.pdf`
  (CoralNet-Toolbox updates), `QAPP_2026.pdf` (Kelp Quest on Zooniverse)

Work done:
- Refactored `models.rmd` — separated **data preparation** from **modelling**, so
  the Rmd is a standalone modelling document. New scripts:
  `build_combined_abundance.R`, `build_combined_percent_cover.R`.
- Added a unique `key` column (`CP_1_summer`, `EBM_4_summer`, …) because site +
  transect alone is not unique across seasons.
- Percent-cover comparison uses diver transect percentages vs. the ROV
  **transect-averaged** values (`HSIL_percent-cover_transect-averaged.csv`).
- Ran 8 percent-cover models across all seasons; all converged cleanly. LaTeX
  results tables generated in the project's established format.
- Ran a **winter-only substrate** model set as an *addition* (not a replacement),
  because substrate visibility differs seasonally — results in
  `percent_cover_model_results_winter_substrate.csv`.
- Interpretation drafted around the finding that **red and green algae do not
  differ substantially between ROV and divers** (red marginally significant,
  plausibly an artefact of combining multiple ROV categories), and a follow-up
  audit of *where the ROV-side substrate area goes*, category by category.

### 2026-08-01 — LaTeX report repair
*Session `b64171e0`, 4 prompts*

Fixed compile errors in `ROV_diver_comparison/report/main.tex`. Wrapped long
column headers in `tab:toolbox_training` onto two lines with `\makecell`. Moved
**all figures and tables to the end of the document**, each on its own page,
before the bibliography and appendix — an organisational aid during heavy
results/discussion editing.

### 2026-08-04 — Reef Check basin-scale kelp density
*Session `55a004a1`, 7 prompts*

Slightly adjacent (touches `understory_kelp_indicator`). Analysed
`reef_check_cleaned.csv`: 2025 kelp density as a percentage of the 2021–2024
average, per site, then averaged to **basin** (10–11 basins), then to a single
Puget Sound–wide value per taxon.

> **Methodological decision, deliberately reached:** zeros are **kept** (a real
> observation of absence); NAs are dropped with the divisor reduced accordingly,
> and the number of contributing sites/basins is reported as an explicit column so
> the reader can see what each average rests on. Infinities arise where the prior
> average is zero and are handled separately. This was questioned, discussed, and
> settled — don't silently revert it.

### 2026-08-05 — Composite figures and the R environment note
*Session `1e53e74d`, 23 prompts*

- Rebuilt `kelp_sugar_sieve_standardized_overlay` as **PDF** (vector preferred over
  PNG) with enlarged axis/legend/title text; single shared y-axis title
  "standardized z-score"; legend title `method` removed; "Diver" capitalised for
  consistency with other figures.
- Programmatically composited `sugar.jpg` and `sieve.jpg` into the PDF from R, one
  photo to the right of each row.
- Built a **schematic figure**: 30 boxes representing the 30 photos of a 30 m
  outward pass, each containing 5 smaller boxes standing in for the 50 random
  percent-cover points (10 points each) — a visual explanation of the sampling design.
- Produced `CLAUDE_R_ENVIRONMENT.md` (see §4) to solve a recurring problem where
  Claude could not locate the R installation and so could not iterate on code itself.

### 2026-08-06 — CoralNet-Toolbox training table
*Session `68ad6656`, 4 prompts*

Populated `tab:toolbox_categories_training` with real counts, partitioning total
annotations **70% training / 20% validation / 10% testing**. Accuracy figures
re-sourced from the current model at
`machine_learning\Toolbox\models\classify\EB_HSIL_2025_01_19` (superseding the
outdated `ElliottBay_balanced_070201` numbers). Categories span Kelp, Brown algae,
Red algae, Green algae, Substrate, and Invertebrates/other.

---

## 3. Repository state at archive time

```
ROV_diver_comparison/
├── code/     19 R scripts (see below)
├── data/     ROV/ + diver inputs
├── figs/     NMDS, abundance, percent-cover, photos,
│             proportion_across_space, schematic, violin, z-score
├── report/   main.tex — the technical report
└── results/
    ├── ROV/       NMDS, abundance, percent_cover
    ├── diver/     diver_UPC_percentage.csv, diver_algae_density.csv,
    │              diver_invert_abundance.csv
    └── combined/  ROV_diver_abundance_combined.csv
                   ROV_diver_percent_cover_combined.csv
                   abundance_model_results.csv
                   abundance_model_results_final.csv
                   percent_cover_model_results.csv
                   percent_cover_model_results_winter_substrate.csv
```

**Script roles.** `RunMe.R` and `load_packages.R` drive execution.
Wrangling: `wrangle_HSIL_percent-cover_data.R`, `wrangle_diver_data.R`,
`wrangle_data_functions.R`. Abundance correction:
`build_HSIL_viame_abundance_corrected.R`, `annotate_viame_detections.R` (+ `_functions`).
Joins: `build_combined_abundance.R`, `build_combined_percent_cover.R`.
Models: `abundance_models.R`, `percent-cover_models.R`.
Ordination: `NMDS.R`, `NMDS_functions.R`, `NMDS_visualization.R`, `NMDS_visualization_functions.R`.
Figures: `data_visualization.R` (+ `_functions`).

---

## 4. Conventions and gotchas to carry forward

1. **`data/` → `code/` → `results/` → `figs/`.** Inputs are never written to;
   `results/` holds analysis-ready output.
2. **`combined_*` columns are additive duplicates.** Exclude from ordinations and
   any closed-composition analysis. Always prefix with `combined_`.
3. **The 50-points-per-photo conversion** is the hinge of the percent-cover
   pipeline. Percent → points → transect sum → transect proportion.
4. **Photo-level vs transect-averaged** are different analysis scales and are not
   interchangeable. Diver comparisons use transect-averaged ROV values.
5. **VIAME abundances are double-counted** in raw form because every photo was
   annotated. Use the `_corrected` files.
6. **Colour convention:** EBM = blues, CP = oranges; depth and season vary shade
   within site. Reused across violin, z-score, and NMDS figures.
7. **Figures are PDF where possible** (vector), with PNG alongside for preview.
8. **R environment — there are two installs and only one works.**

   **Use:** `C:\Users\randellz\AppData\Local\Programs\R\R-4.5.1\bin\Rscript.exe`
   (R 4.5.1, ~387 packages: tidyverse, magick, patchwork, legendry, ggtext,
   glmmTMB, lme4). Not on PATH — always call by full absolute path.

   **Ignore:** `C:\Program Files\R\R-4.1.3\bin\Rscript.exe`. This is the one that
   *is* discoverable via the registry and Program Files, which makes it a trap.
   Its library is nearly empty, and it sees the same `C:/R/win-library/4.5` path
   whose packages are binary-incompatible with 4.1.3 — so
   `requireNamespace(..., quietly=TRUE)` silently returns FALSE while
   `find.package()` succeeds. It looks exactly like "package not installed".

   **Run R from PowerShell, not Bash.** Through Git Bash, `.libPaths()` drops
   `C:/R/win-library/4.5` where most packages live, and `magick` fails as missing
   despite being installed. PowerShell 5.1 also wraps R's normal stderr startup
   messages into a `NativeCommandError`, so a successful run looks failed —
   redirect with `*> outfile.txt` and check `$LASTEXITCODE` rather than trusting
   red text.

   **Never edit a script while `Rscript` is executing it** — R streams the file, so
   an edit shifts byte offsets underneath it and it dies with a bogus parse error.

   `CLAUDE_R_ENVIRONMENT.md` (root of this archive) is the full write-up.
   **Supply it at the start of any new session involving R.**
9. **Git workflow:** work on a personal branch (`zhr_1`, `zhr_repo_cleaning`), PR
   into `main`. GitHub Desktop is the usual tool; `gh` CLI is installed and
   authenticated as a fallback.

---

## 5. Where to pick up

- The technical report (`report/main.tex`) is the live deliverable — results and
  discussion sections were mid-edit, with figures parked at the end of the
  document for organisational purposes.
- Abundance models have both a `_results` and a `_results_final` file; confirm
  which is authoritative before building on them.
- The ecological interpretation of the ROV–diver differences (particularly the
  substrate accounting) was the most recent line of thinking and is the natural
  thread to resume.

---

*Companion transcripts: `transcripts/CCR_benthic_analyses__ROV_diver_comparison/`
(10 sessions). Prompt-only digests: `digests/CCR_benthic_analyses__ROV_diver_comparison/`.*
