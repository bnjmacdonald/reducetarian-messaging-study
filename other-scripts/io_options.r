library(optparse)
option_list <- list(
    make_option(c("-i", "--input"), type="character", default=NULL, help="input directory", metavar="character"),
    make_option(c("-o", "--output"), type="character", default=NULL, help="output directory", metavar="character"),
    make_option(c("-t", "--treatments"), type="character", default=NULL, help="treatment assignments directory", metavar="character")
    )

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
INPUT_PATH <- opt$input
OUTPUT_PATH <- opt$output
TREATMENTS_PATH <- opt$treatments

if (is.null(INPUT_PATH) | is.null(OUTPUT_PATH)) stop('input and output directories must both be specified.')