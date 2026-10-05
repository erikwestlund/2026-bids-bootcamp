# BIDS Bootcamp 2026

Slides and companion notebooks for the BIDS bootcamp talks. Each talk has a slide deck and a Quarto notebook that reproduces every result and figure in the slides.

## Talks

| Date | Talk | Slides | Notebook |
|---|---|---|---|
| October 5, 2026 | Data Characterization & Visualization | `2026-10-05-characterization-visualization.html` | `characterization-notebook.qmd` ([rendered](characterization-notebook.html)) |

Open the slides and the rendered notebooks in a browser.

## Running the notebooks

The notebooks use [Eunomia](https://github.com/OHDSI/Eunomia), a small synthetic OMOP CDM database that runs on your own computer. They need no access to real data.

1. Install R, [Quarto](https://quarto.org), and either RStudio or Positron.
2. Open `2026-bids-bootcamp.Rproj` in RStudio, or open this folder in Positron.
3. Open a notebook. The first time, uncomment the install lines in its `packages` chunk and run them.
4. Run the chunks in order, or render the notebook:

   ```bash
   quarto render <name>-notebook.qmd
   ```

Each notebook writes its figures to `images/<talk>/`, for example `images/characterization/`. The slides read the figures from there.
