# Implementing a capital-use matrix in FIGARO: an environmental application to the ICT sector

**Author:** Joris BLAIN

## 1. Overview

This repository provides an automated workflow to model a capital-use
matrix aligned with FIGARO (Eurostat-JRC), leveraging EU KLEMS and OECD
data, and to compute production footprints based on this matrix.

This work is directly related to the research paper: *"Implementing a
capital-use matrix in FIGARO: an environmental application to the ICT sector"*. All
files are essentially enumerating a set of functions regrouped in order
to facilitate the partial or total reuse of the code and to be
reproducible and flexible, including the possibility to test the
sensitivity of the results to individual inputs or heuristics.

## 2. Repository structure

The code is organized into **6 R files**, each dedicated to a specific
role, and one folder of input data:

| File | Purpose |
|------------------------------------|------------------------------------|
| [`main.R`](main.R) | Entry point: installs and loads the packages, sources the other files and defines `compute_footprint_K`. |
| [`data.R`](data.R) | Functions for data fetching and preprocessing. |
| [`computations.R`](computations.R) | Construction of the capital-use matrix, ICT shares, and the capital-augmented system used for the valuation by age of the capital. |
| [`vintage.R`](vintage.R) | Capital use by vintage (valuation by age of the capital). |
| [`mapping.R`](mapping.R) | Functions to convert and map data between classification systems (e.g., FIGARO, EU KLEMS, OECD). |
| [`utils.R`](utils.R) | Utility functions and constants (cache, classification codes, groups of industries studied). |
| [`data/`](data) | Input data distributed with the code (see below). |
| [`LICENSE`](LICENSE.md) | License file (see [Section 4](#4-license)). |

### Data distributed with the code

Four extracts of OECD data, restricted to the variables and years used by
the code, are stored in `data/` in parquet format (2.0 MB in total). They
were downloaded from the OECD Data Explorer on 4 November 2026. They are
distributed with the code because downloads through the OECD API failed
repeatedly, which would prevent the code from running.

| File | OECD dataflow (SDMX) | Content | Years |
|------------------------------------|------------------------------------|------------------------------------|------------------------------------|
| `oecd_fixed_assets_net.parquet` | DSD_NAMAIN10, DF_TABLE9A | Net stocks of fixed assets by asset and industry, current prices | 2010-2021 |
| `oecd_cfc.parquet` | DSD_NAMAIN10, DF_TABLE6 | Consumption of fixed capital (P51C) and gross value added (B1G) by industry | 2010-2021 |
| `oecd_sdbs_ict.parquet` | DSD_SDBSBSC_ISIC4, DF_SDBS_ISIC4 | Production of the ICT branches, used for the ICT shares | 2010-2021 |
| `oecd_exchange_rates.parquet` | DSD_NAMAIN10, DF_TABLE4 | Exchange rates, national currency per US dollar | 2010-2024 |

Five csv files from four national sources of consumption of fixed capital
are also stored in `data/`: Albania (INSTAT), China (NBS, two files), India
(MOSPI) and Russia (Rosstat). The other inputs (FIGARO tables, EU KLEMS, Eurostat,
UN data, EXIOBASE) are downloaded when needed and kept in the temporary
folder of the user, so that each file is downloaded only once.

## 3. Setup and Execution


### `build_capital_use`

The function `build_capital_use` returns by default the capital-use and
the capital stock matrices, both expressed in an industry-by-industry
perspective. The user is required to fill the desired year, with the
2010 to 2021 time span supported with respect to data sources
availabilities.

### `compute_footprint_K`

The function `compute_footprint_K` directly bridges the capital
endogenization procedure and the production footprint computations in
order to return the subsequent distributed footprint. More precisely,
the user is required to fill the desired year and sectors of interest,
while the default option returns the ICT sector footprint. With
`capital = FALSE`, the function returns the classic footprint, without
the capital-use matrix.

With `vintage = TRUE`, the function also values the capital term by the
age of the capital goods, using the emission intensities of the year in
which each vintage was acquired instead of those of the year of analysis.
The result then contains an additional element `VINTAGE`, with the
footprint without the capital term (`CURRENT`), the capital term
valued in the year of analysis (`CAPITAL`) and by vintage
(`CAPITAL_BY_VINTAGE`, split by year of acquisition in `BY_VINTAGE`),
and the total footprint (`TOTAL`). It requires `capital = TRUE`, and the
FIGARO tables and Eurostat emission accounts of every year from 2010 to
the year of analysis are downloaded, which takes longer.

## 4. License

This project is licensed under the **GNU General Public License v3.0**
([GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)).
