# Cohort Profiler

An automated data-profiling Shiny application for tabular cohort datasets, built for
deployment inside an [Aridhia](https://www.aridhia.com/) Digital Research Environment
(DRE) Project Workspace. It produces a fast statistical profile of any CSV file —
missingness, outliers, correlations, distributions, group comparisons, timelines, and
user-defined range validation — without any data leaving the secure workspace boundary.

The entire application is a single self-contained `app.R` file. There are no external
service calls; all analysis runs locally against files already on the workspace disk.

Wathc a video of how to use cohort profiler here:
https://scribehow.com/embed-preview/Analyze_Synthetic_Alzheimers_Data_in_Aridhia_Workspaces__Bx7yuLm8Rfmfpg3XnOPDuQ?as=video&size=flexible&voice=shimmer&scaleMode=contain

---

## Contents

- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Running the app](#running-the-app)
- [Input data format](#input-data-format)
- [Variable labels](#variable-labels)
- [Validation rules](#validation-rules)
- [Saving outputs](#saving-outputs)
- [Tab reference](#tab-reference)
- [How it works](#how-it-works)
- [Limitations](#limitations)
- [Project structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Disclaimer](#disclaimer)

---

## Features

- **One-click profiling.** Browse the workspace file tree, pick a CSV, and get a complete
  statistical profile in seconds.
- **Missingness heatmap.** Visualise the pattern of missing values across columns and rows.
- **Outlier detection.** Per-column outlier flagging using Tukey IQR fences (1.5 × IQR),
  with a detailed density view showing the fenced tails.
- **Correlation matrix.** Pairwise-complete Pearson correlations across all numeric columns.
- **Distributions.** Histograms for numeric columns (with a Shapiro–Wilk normality test)
  and top-value bar charts for categorical columns.
- **Group analysis.** Boxplots with jittered points split by any categorical column, plus a
  Kruskal–Wallis test for differences between groups.
- **Timelines.** Record density over time and column-completeness trends, aggregated by
  week, month, quarter, or year.
- **Cohort comparison.** Load a second dataset and overlay distributions, compare category
  proportions, and view a side-by-side statistics table for every matched column — useful
  for checking harmonisation between sites or comparing pipeline versions.
- **Range validation.** Define expected value ranges per column (`column,min,max`) and the
  app flags violations with counts, percentages, and sample offending values.
- **Variable labelling.** Upload a label CSV to replace cryptic column names with
  human-readable descriptions across every plot, axis, and menu, while keeping the original
  names in parentheses for traceability.
- **Save outputs to the workspace.** Every chart and the summary statistics can be saved
  directly into a chosen workspace folder via a built-in folder picker, so outputs are
  immediately visible to the DRE file manager and available for airlock review.
- **Read-only and secure.** No workspace files are modified, and no data crosses the
  environment boundary.

---

## Requirements

- **R** 4.0 or newer.
- The following R packages: `shiny`, `shinydashboard`, `DT`, `ggplot2`, `dplyr`, `tidyr`,
  `scales`, `readr`, `tools`.

The app installs any missing packages automatically on first launch from the
CRAN mirror `https://cloud.r-project.org` (which is on the default Aridhia DRE network
allowlist). Packages that are already present are not reinstalled.

If your workspace has a tightened network allowlist that blocks CRAN, install the packages
ahead of time from RStudio or the workspace terminal instead:

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "ggplot2",
  "dplyr", "tidyr", "scales", "readr", "tools"
), repos = "https://cloud.r-project.org")
```

---

## Input data format

The app reads standard CSV files via `readr::read_csv()`. Column types are inferred
automatically and classified as **numeric**, **character**, **logical**, or **date/time**.

Date columns are detected from native date types or by parsing character columns against
these formats:

```
YYYY-MM-DD    DD/MM/YYYY    MM/DD/YYYY
YYYY/MM/DD    DD-MM-YYYY    YYYYMMDD
```

A character column is treated as a date when at least 80% of its sampled values parse
successfully under one of those formats.

---

## Variable labels

To make plots and tables readable for collaborators unfamiliar with your coding
conventions, upload a two-column label CSV from the **Column Labels** section of the
sidebar. It must have headers `column` and `label`:

```csv
column,label
age_at_dx,Age at Diagnosis
rbans_total,RBANS Total Score
bmi_baseline,BMI (Baseline)
```

Once loaded, labels are applied everywhere — axis titles, select menus, the column summary
table, and the exported summary CSV. The original column name is retained in parentheses,
e.g. *Age at Diagnosis (age_at_dx)*, so every output stays traceable to the source data.

Headers `name,label` are also accepted, and if no recognised headers are found the first
two columns are used as name and label respectively.

---

## Validation rules

In the **Validation** tab, define one rule per line as `column,min,max`:

```
age,0,120
bmi,10,80
sbp,,300
weight,0,
```

- Leave **min** blank for an upper-bound-only rule (`sbp,,300` flags values above 300).
- Leave **max** blank for a lower-bound-only rule (`weight,0,` flags values below 0).
- Column names are **case-sensitive** and must match the data exactly.
- Only numeric columns are evaluated.
- Lines beginning with `#` are ignored.

Click **Run Validation** to get a table of violations per rule, with the violation count,
the percentage of non-missing values affected, and a sample of the offending values.

---

## Saving outputs

Every chart has a **Save PNG** button, and the column statistics can be saved as a
**Summary CSV**. Saving opens a folder picker rooted at `/home/workspace/files/` where you
can:

- navigate into subfolders,
- create a new folder,
- and edit the filename (a sensible dated default is pre-filled).

The file is written **directly into the chosen workspace folder**, so it appears
immediately in the DRE file manager and is available for export through the standard
airlock review workflow. PNGs are saved at 300 DPI for publication quality.

> Outputs are written into the secure workspace, not downloaded to your local machine.
> Moving files out of the environment still requires the normal airlock process.

---

## Tab reference

| Tab | What it shows |
|-----|----------------|
| **Overview** | Row/column counts, completeness, total outliers, duplicate rows, column-type breakdown, and alerts for high-missingness, outlier, and likely-identifier columns. |
| **Column Summary** | Per-column statistics: type, missingness, unique count, outlier count, and (for numeric columns) min, median, mean, max, SD. Adds a label column when labels are loaded. |
| **Missingness** | Heatmap of missing vs present values over a sample of up to 300 rows. |
| **Correlations** | Pearson correlation matrix for numeric columns, computed on pairwise-complete observations. |
| **Outliers** | Density curve with shaded IQR tails for a selected numeric column. |
| **Distributions** | Histogram (numeric, with Shapiro–Wilk test) or top-value bar chart (categorical). |
| **Group Analysis** | Boxplots + jitter split by a categorical column, with a Kruskal–Wallis p-value. |
| **Timeline** | Record density and column-completeness trends over time, by week/month/quarter/year. |
| **Compare** | Overlay a second dataset: distribution overlays, category-proportion comparison, and a side-by-side statistics table for matched columns. |
| **Validation** | User-defined range-rule checking with a violation report. |
| **Data Preview** | Paginated raw data with per-column search filters. |
| **Help** | Built-in user guide. |

---

## How it works

- **Single file.** All UI, server logic, helper functions, and CSS live in `app.R`.
- **Outlier method.** Outliers are values outside the Tukey fences `Q1 − 1.5·IQR` and
  `Q3 + 1.5·IQR`. Columns with fewer than four non-missing values are not assessed.
- **Statistical tests.** Distribution normality uses Shapiro–Wilk (sampled to 5,000 points
  for large columns); group differences use the Kruskal–Wallis rank test.
- **Self-installing dependencies.** On launch the app checks each required package with
  `requireNamespace()` and installs any that are missing from CRAN before loading them.
- **Workspace-aware paths.** The file browser and save dialog are both confined to
  `/home/workspace/files/` and cannot navigate above it.

---

## Limitations

- Large files (roughly 100 MB and above) can take several seconds to load and render.
- The missingness heatmap samples up to 300 rows for readability; it is a visual summary,
  not an exhaustive cell-by-cell map.
- Automatic package installation depends on the workspace network allowlist permitting
  access to `cloud.r-project.org`. If it does not, install the packages manually first.
- The app reads CSV files only.

---

## Project structure

```
.
├── app.R        # the entire application (UI, server, helpers, CSS)
└── README.md
```

---



## License

> No license has been specified yet. Add a `LICENSE` file and update this section — for
> example, the [MIT License](https://choosealicense.com/licenses/mit/) for permissive use.

---

## Disclaimer

"Aridhia" and the Digital Research Environment (DRE) are products of Aridhia Informatics.
This project is built to run inside that platform but is not affiliated with or endorsed by
Aridhia unless stated otherwise by the repository owner.
