# DBEN
LPJ-GUESS post-processing scripts for the demographic benchmarking initiative.

This script reads raw LPJ-GUESS output files (e.g. veg_structure_<>.out,cmass_<>.out) for different simulations and sites, processes them (e.g., patch averaging, unit conversion), and prepares NetCDF-compatible summaries.


```
<base.path>/
├── 1_raw/
│   └── PS_412ppm/
│       ├── FIN1/
│       │   └── *.out
│       ├── BIA/
│       │   └── *.out
│       └── BCI/
│           └── *.out
├── 2_processed/
│   └── PS_412ppm/
│       └── (output files written here)
├── create_netcdfs_clean.R
├── check_demographic_carbon_budget.R
```


## Run instructions:

1) from terminal:
navigate to the relevant folder, then run:

```
# convert LPJ-GUESS ouput to "DBEN"-output:
Rscript create_netcdfs_clean.R 1_raw/ a

# check the demographic carbon budget is adhered to:
Rscript check_demographic_carbon_budget.R 2_processed/

```


