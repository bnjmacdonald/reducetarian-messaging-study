# README

This repository contains the code and data for replicating the results of a messaging experiment conducted by the [Animal Welfare Action Lab (AWAL)](http://www.awalab.org) with the support of the [Reducetarian Foundation](http://reducetarian.org/lab/). 

The working paper, pre-registration, and surveys for this study are available at: [https://osf.io/44z7j/](https://osf.io/44z7j/).

We welcome contributions to the existing set of analyses (see `TODO.md` for a list of analyses that have not been completed yet). If you wish to contribute, follow the instructions [here](http://kbroman.org/github_tutorial/pages/fork.html) to fork the repository, make changes, and submit a pull request.

Citation: 

Macdonald, BNJ, KD Caldwell, and GD Boese. 2016. The effects of "reduce" and "eliminate" appeals on individual meat consumption. *Unpublished manuscript*. Available at: https://osf.io/nxrx3/. 

## Replicating the analyses

1. Download the entire repository.
2. To replicate the data cleaning (i.e. converting raw data files in `raw-data-shareable` into clean data files in `cleaned-data-shreable`), open a command line session and run: `$ bash clean.sh`
3. To replicate the data analysis, open a new command line session and run: `$ bash analysis.sh`

This will run the R scripts in order and reproduce the main results contained in the `results`, `tables`, and `figures` directories.

## Main R Scripts

* `01-wave1-cleaning.R` cleans the wave 1 Qualtrics data and saves it as a csv file.
* `02-blocking.R` was used for block randomized assignment to treatment.
* `03-wave2-cleaning.R` cleans the wave 2 Qualtrics data and saves it to a csv file.
* `04-wave3-cleaning.R` cleans the wave 3 Qualtrics data and saves it to a csv file.
* `05-merging-all-waves.R` merges the data from waves 1, 2, and 3 together and does some additional cleaning.
* `06-main-analysis.R` contains the analyses for producing the main treatment effects.
* `07-subgroup-analyses.R` contains sub-group analyses examining how the treatment effects vary across groups in the study sample.
* `08-alternative-analyses.R` contains analyses for examining treatment effects on other outcomes of interest.

## Data

* `cleaned-data-shareable/all_waves_cleaned.csv` contains the cleaned data output by `05-merging-all-waves.R` that is used to produce the main results.
* `raw-data-shareable/` contains the raw unprocessed data.
* see `other-scripts/variables.r` for a description of variables in the cleaned data files. See `var-dicts/` for the interpretation of variable names in the raw data files.
* all MTurk IDs have been replaced with hashed IDs to ensure anonymity.

## Other folders

* `var-dicts/` contains variable dictionaries for the raw data files.
* `functions/` contains functions that are called in the main R scripts.
* `other-scripts/` contains additional R scripts that are called in the main R scripts.
