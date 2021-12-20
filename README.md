# Hydro-grade-aggregation

These scripts are for importing IMAGE team's hydro researce data from different scenarios and converting them to useful formats for uses in MESSAGEix parametrization.

## Workflow
This assumes the raw datasets are available on the P drive (`file.path("p:", "ene.model", "NEST", "energy_potentials_Gernaat", "MESSAGE")`).

When the raw datasets are updated, do the followings:
1. Run aggregate_message_regions.R
    - This aggregates the country-level raw data into MESSAGE R12-CHN level and creates excel output files.
2. Run cost_curves.R
    - This generages the step function-like cost curves and load factor curves. More explanations are given in the description for [Hydro.Global.MESSAGE](https://github.com/iiasa/Hydro.Global.MESSAGE).
    - It reads the excel input files and creates a CSV file for each MESSAGE region, which will be used in the [Hydro.Global.MESSAGE](https://github.com/iiasa/Hydro.Global.MESSAGE).
