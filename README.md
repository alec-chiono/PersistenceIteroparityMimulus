# PersistenceIteroparityMimulus

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15014229.svg)](https://doi.org/10.5281/zenodo.15014229)

Code and data for analyses of seed persistence and germination in semelparous and iteroparous populations of *Mimulus guttatus*.

**Author:** Alec Chiono  
**Affiliation:** Department of Ecology and Evolutionary Biology, University of Colorado Boulder  
**Contact:** alec.chiono@colorado.edu  
**License:** The Unlicense (public domain)

---

## Associated publication

> Chiono, A., Sellinger, E., & Emery, N. C. (2026). Seeds of annual and perennial populations of *Mimulus guttatus* exhibit differential responses to cold stratification. *American Journal of Botany*, 113(2), e70199. https://doi.org/10.1002/ajb2.70199

---

## Overview

Life history theory predicts that seed persistence (viable seeds remaining dormant in the seed bank) and iteroparity (reproducing across multiple years) are alternative strategies for spreading reproductive risk over time. Therefore, these two life history delays should rarely co-evolve because they perform similar functions in variable environments. This study tested whether seed persistence and iteroparity are negatively correlated by comparing germination and persistence in field-collected seeds from three semelparous (annual) and three iteroparous (perennial) populations of *Mimulus guttatus* (yellow monkeyflower) from the San Francisco Bay Area. Seeds were exposed to a factorial combination of water potential (0, −0.5, −1 MPa) and cold stratification treatments (0 or 2 weeks at 4°C). All ungerminated seeds were then evaluated using tetrazolium assays to distinguish viable but dormant seeds from inviable ones. Results showed that the relationship between seed persistence and life history is context-dependent: semelparous seeds persisted more under high water potential and no cold stratification, but cold stratification unexpectedly increased persistence in iteroparous seeds, reversing the expected pattern. Within-population variation in cold stratification responses was substantial, indicating that this relationship is more complex than current life history theory predicts.

---

## Repository structure

```
PersistenceIteroparityMimulus/
├── data/                          # Input data files
│   ├── germ_df.csv               # Raw germination and seed viability data
│   ├── time_df.csv               # Germination timing data
│   ├── site_info.csv             # Site metadata and coordinates
│   ├── dem_urls.csv              # URLs for digital elevation model downloads
│   └── region_water_area.*       # Shapefiles for water bodies (map)
├── scripts/                       # R analysis scripts (run in order)
│   ├── 01_wrangle.R              # Data cleaning and factor coding
│   ├── 02_fit_models.R           # Fit Bayesian hierarchical models with rstanarm
│   ├── 03_check_fits.R           # Model diagnostics
│   ├── 04_predict.R              # Generate posterior predictions
│   ├── 05_visualizations.R       # Create main manuscript figures
│   ├── 06_make_figureS1.R        # Create map with site locations
│   └── 07_figure_for_social_media.R  # Create simplified figure for outreach
├── source/                        # Helper functions
│   └── stan_diagnostics.R        # Stan model diagnostic utilities
├── output/                        # Model objects and intermediate results (auto-generated)
│   ├── persistence_fit.RDS       # Fitted persistence model
│   ├── viability_fit.RDS         # Fitted viability model
│   ├── time_fit.RDS              # Fitted germination timing model
│   └── dems/                     # Downloaded digital elevation models
├── figures/                       # Publication-ready figures (auto-generated)
│   ├── figure1.pdf               # Main results: persistence by treatment
│   ├── figure2.pdf               # Within- and between-population variation
│   ├── figureS1.png              # Map of study sites with photos
│   ├── figureS2.png              # Seed viability results
│   ├── figureS3.png              # Germination timing results
│   └── socials_figure_raw.pdf    # Simplified figure for social media
├── photos/                        # Field site and plant photos
│   ├── S3.jpg                    # Semelparous site photo
│   ├── I1.jpg                    # Iteroparous site photo
│   └── plants.jpg                # Combined photo panel
├── PersistenceIteroparityMimulus.Rproj
├── LICENSE
└── README.md
```

---

## Data files

### `germ_df.csv`
Raw germination and seed viability data from the experiment. Each row represents one well (group of ~10 seeds). Columns:
- `plate_pair`, `plate`, `well` — Experimental design variables (blocking structure)
- `ecotype` — Semelparous or Iteroparous
- `site` — Population code (S1–S3 for semelparous; I1–I3 for iteroparous)
- `seed_family` — Maternal line identifier (nested within site)
- `cold_stratification` — Treatment (Cold stratified or Not cold stratified)
- `water_potential` — Treatment in MPa (0, −0.5, or −1)
- `num_germinated_seeds` — Count of seeds with visible radicle emergence
- `num_tz_category1`, `num_tz_category2` — Viable ungerminated seeds (tetrazolium assay categories 1 & 2)
- `num_tz_category3`, `num_tz_category4` — Inviable ungerminated seeds (tetrazolium assay categories 3 & 4)

### `time_df.csv`
Timing of germination for individual seeds that germinated. Used to fit a geometric (germination timing) model. Columns:
- Experimental design and treatment columns (as above)
- `Date` — Date of germination observation
- `Day` — Days elapsed since seeds were moved to germination-inducing conditions

### `site_info.csv`
Metadata for study populations. Columns: site code, ecotype, latitude, longitude, and other location details.

### `dem_urls.csv`
URLs for downloading digital elevation model (DEM) tiles from USGS for the study region.

### `region_water_area.*`
Shapefiles for water bodies in the San Francisco Bay Area, used in map generation.

---

## Scripts

Run scripts in the order below. All file paths are relative to the project root.

### 1. `scripts/01_wrangle.R`
Data cleaning and preparation. Reads raw CSV files and converts categorical variables to factors with appropriate level ordering for downstream visualization and analysis. Outputs: `germ_df` and `time_df` (loaded into environment).

### 2. `scripts/02_fit_models.R`
Fits three Bayesian hierarchical logistic regression models using `rstanarm::stan_glmer()`:
- **Persistence model:** Proportion of viable seeds that remained ungerminated (logit link)
- **Viability model:** Proportion of all seeds that were viable (logit link)
- **Timing model:** Day of germination for seeds that germinated (geometric distribution; models as binomial with 1 success and Day−1 failures)

All models include:
- Fixed effects: ecotype, water potential, cold stratification, and all interactions
- Random intercepts: site/seed_family (population structure), plate/plate_pair (experimental blocking)
- Random slopes for cold stratification by seed family (to capture within-family variation in treatment response)

Models are cached as RDS files in `output/` to avoid refitting.

### 3. `scripts/03_check_fits.R`
Bayesian model diagnostics. Checks Rhat, effective sample size, divergent transitions, tree depth, and E-BFMI using custom `stan_diagnostics()` function. Visualizes rank plots to assess chain mixing.

### 4. `scripts/04_predict.R`
Generates posterior predictions from fitted models:
- **Marginal predictions** (ignoring random effects) for overall treatment effects
- **Conditional predictions** (including random effects) to evaluate within- and among-population variation by seed family

Stores predictions in long format for visualization and analysis.

### 5. `scripts/05_visualizations.R`
Creates three main manuscript figures and supplementary figures using `ggplot2`, `ggdist`, and `patchwork`:
- **Figure 1A:** Posterior distributions of persistence proportions by treatment combination
- **Figure 1B:** Posterior distributions of differences between ecotypes (with posterior credible intervals showing % probability of each direction)
- **Figure 1C:** Effect of cold stratification on each ecotype at each water potential
- **Figure 2:** Within- and between-population variation in cold stratification responses (spaghetti plots showing individual seed families)
- **Figure S2:** Seed viability (proportion of all seeds that were viable)
- **Figure S3:** Germination timing (expected day of germination)

### 6. `scripts/06_make_figureS1.R`
Creates the map figure showing study site locations overlaid on topography and water bodies. Downloads DEM tiles, reads shapefiles, and integrates field photos of representative plants from semelparous and iteroparous sites.

---

## Dependencies

All analyses require R (version 4.3+). Install required packages:

```r
# Core data wrangling and visualization
install.packages("tidyverse")       # ggplot2, dplyr, tidyr, etc.
install.packages("ggdist")          # ggdist for uncertainty visualization
install.packages("patchwork")       # Combining plots
install.packages("ggthemes")        # theme_tufte()
install.packages("shadowtext")      # Text with halos (alternative labeling)

# Bayesian modeling
install.packages("rstanarm")        # Bayesian regression via Stan
install.packages("tidybayes")       # For posterior predictions and plotting
install.packages("rstan")           # Stan backend (auto-installed via rstanarm)

# Geospatial (for map generation)
install.packages("raster")          # Raster operations (DEM)
install.packages("sf")              # Simple features (shapefiles)
install.packages("prettymapr")      # Scale bars and map aesthetics
install.packages("mapdata")         # Map data (state outlines)
install.packages("jpeg")            # Reading JPEG photos

# Utilities
install.packages("librarian")       # Lightweight package manager (shelf())
install.packages("magrittr")        # %<>% pipe operator
remotes::install_github("rmcelreath/rethinking")  # trankplot(), other utilities
```

### Package versions
- Analyses conducted with R 4.3.2

---

## Reproducibility

To reproduce all analyses and figures:

1. **Clone or download** this repository to your local machine.

2. **Open the R project:** 
   ```r
   # In RStudio, open PersistenceIteroparityMimulus.Rproj
   # This sets working directory to the project root
   ```

3. **Install dependencies** (see above).

4. **Run scripts in order:**
   ```r
   source("scripts/01_wrangle.R")
   source("scripts/02_fit_models.R")          # Takes ~5 min (Stan compilation & sampling)
   source("scripts/03_check_fits.R")          # Check diagnostics
   source("scripts/04_predict.R")             # Generate predictions
   source("scripts/05_visualizations.R")      # Creates Figures 1, 2, S2, S3
   source("scripts/06_make_figureS1.R")       # Creates Figure S1 (downloads DEMs first)
   ```

5. **Output locations:**
   - Figures: `figures/`
   - Model objects (RDS files): `output/`
   - Downloaded DEMs: `output/dems/`

### Notes on reproducibility

- **Stan sampling:** Random number generation is set with explicit seeds in `02_fit_models.R` to ensure reproducible posterior samples.

---

## Key findings

1. **Context-dependent relationship:** Seed persistence and iteroparity are not universally negatively correlated as life history theory predicts. The relationship depends on water availability and cold cues.

2. **Differential responses to cold stratification:** Cold stratification decreased germination (increased persistence) in semelparous seeds but increased germination (decreased persistence) in iteroparous seeds—opposite effects that contradict the common assumption that cold stratification universally promotes germination in *Mimulus*.

3. **Substantial within-population variation:** Individual seed families within populations responded differently to treatments, indicating genetic or maternal environmental variation in germination responses not explained by ecotype or population-level differences.

4. **Practical implications:** Studies using *Mimulus guttatus* seeds should be cautious about applying cold stratification uniformly, as this treatment may inadvertently select for different phenotypes in annual vs. perennial ecotypes.

---

## Citation

If you use this code or data, please cite:

**Chiono, A., Sellinger, E., & Emery, N. C. (2026).** Seeds of annual and perennial populations of *Mimulus guttatus* exhibit differential responses to cold stratification. *American Journal of Botany*, 113(2), e70199. https://doi.org/10.1002/ajb2.70199

And cite the repository:

**Chiono, A. (2025).** *alec-chiono/PersistenceIteroparityMimulus: v1.0* [Software]. Zenodo. https://doi.org/10.5281/zenodo.15014229

---

## Contact & support

For questions about the code, methods, or data, contact Alec Chiono at alec.chiono@colorado.edu.

For issues or suggestions, please use the GitHub issue tracker or contact the authors directly.
