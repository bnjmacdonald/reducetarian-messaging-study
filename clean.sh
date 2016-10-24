#!/bin/bash
# This script runs all files for data cleaning.
INPUT_PATH='raw-data-shareable'
OUTPUT_PATH='cleaned-data-shareable'
TREATMENTS_PATH='treatment-assignments-shareable'

# cleans each of the survey waves.
Rscript 01_wave1_cleaning.r --input=$INPUT_PATH --output=$OUTPUT_PATH
Rscript 03_wave2_cleaning.r --input=$INPUT_PATH --output=$OUTPUT_PATH --treatments=$TREATMENTS_PATH
Rscript 04_wave3_cleaning.r --input=$INPUT_PATH --output=$OUTPUT_PATH

# merges together all waves.
Rscript 05_merging_all_waves.r --input=$OUTPUT_PATH --output=$OUTPUT_PATH
