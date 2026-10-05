# BIDS Bootcamp 2026: Data Characterization & Visualization

Materials for the BIDS bootcamp talk *Data Characterization & Visualization* (October 5, 2026).

| File | What it is |
|---|---|
| `2026-10-05-characterization-visualization.html` | The slides. Open in a browser. |
| `characterization-notebook.html` | The companion notebook, rendered. Open in a browser. |
| `characterization-notebook.qmd` | The notebook source. It reproduces every result and figure in the slides. |

## Running the notebook

The notebook uses [Eunomia](https://github.com/OHDSI/Eunomia), a small synthetic OMOP CDM database that runs on your own computer. It needs no access to real data.

1. Install R, RStudio, and [Quarto](https://quarto.org).
2. Install the packages:

   ```r
   install.packages(c("remotes", "dplyr", "tidyr", "ggplot2", "patchwork", "ragg", "scales"))
   remotes::install_github("OHDSI/Eunomia")
   remotes::install_github("OHDSI/DatabaseConnector")
   remotes::install_github("OHDSI/FeatureExtraction")
   remotes::install_github("OHDSI/CohortIncidence")
   ```

3. Open `characterization-notebook.qmd` and run the chunks in order, or render it:

   ```bash
   quarto render characterization-notebook.qmd
   ```

The notebook writes each figure to `images/characterization/`, which is where the slides read them from.
