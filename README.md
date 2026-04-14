# Implementing a capital-use matrix in FIGARO: an environmental application to the ICT sector

**Author:** Joris BLAIN\

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

The repository is organized into **5 clearly named files**, each
dedicated to a specific role:

| File | Purpose |
|------------------------------------|------------------------------------|
| [`main.R`](main.R) | Main script to execute core functions of the workflow. |
| [`data.R`](data.R) | Functions for data fetching and preprocessing. |
| [`mapping.R`](mapping.R) | Functions to convert and map data between classification systems (e.g., FIGARO, EU KLEMS, OECD). |
| [`utils.R`](utils.R) | Utility functions (small, reusable, and cross-cutting). |
| [`LICENSE`](LICENSE) | License file (see [Section 4](#4-license)). |

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
while the default option returns the ICT sector footprint.

## 4. License

This project is licensed under the **GNU General Public License v3.0**
([GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)).
