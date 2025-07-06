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
```


## Run instructions:
from terminal:
navigate to the necessary location, then run:

```
Rscript create_netcdfs_clean.R
```
