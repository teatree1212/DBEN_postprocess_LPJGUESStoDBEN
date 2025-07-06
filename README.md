# DBEN
LPJ-GUESS post-processing scripts for the demographic benchmarking initiative.

This script reads raw LPJ-GUESS output files (e.g. veg_structure_*.out,cmass_*.out) for different simulations and sites, processes them (e.g., patch averaging, unit conversion), and prepares NetCDF-compatible summaries.


├── 1_raw/
│   └── <CO2LEVEL>/
│       └── <LOCATION>/
│           └── veg_structure_<SIM>.out
├── 2_processed/
│   └── <CO2LEVEL>/
├── create_netcdfs_clean.R

where SIM is P0, and LOCATION is FI, BIA; BCI. CO2LEVEL in this publications are PS_412ppm
