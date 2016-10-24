#!/bin/bash
# This script runs all files for data analysis.

INPUT_PATH='cleaned-data-shareable'
OUTPUT_PATH='results'

# conducts analyses.
# NOTE: 06_main_analysis.r is unnecessarily slow because of the inclusion of
# block covariates, which dramatically slow down the implementation 
# of vcovHC() in effectTable().
Rscript 06_main_analysis.r --input=$INPUT_PATH --output=$OUTPUT_PATH
Rscript 07_subgroup_analyses.r --input=$INPUT_PATH --output=$OUTPUT_PATH
