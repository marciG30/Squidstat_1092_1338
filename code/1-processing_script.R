## R processing script for SquidStat

## YOU ONLY HAVE TO SET THE CHRONO/ CV FILE PATHS AND THE OUTPUT FIGURE FILE PATHS (STEP 0)
## DO NOT CHANGE ANYTHING ELSE IN THIS SCRIPT.

## This script "sources" the processing functions in "code/processing_functions.R", and will run them here.
## Just run this script (Step 1, Step 2) as is, and it will process your data, and plot and save the graphs

## 2021-02-05 Kaizad F. Patel, Marci Garcia
## 2021-03-05 update: converted into functions for a more streamlined workflow

###################### #
###################### #

## STEP 0: Set the input file paths and the output figure paths

CHRONO_DATA_PATH = "data/chrono/Chrono_2021_02_25"
CHRONO_FIGURE_PATH = "figures/chrono_2021-02-25.png"

CV_DATA_PATH = "data/cv/cv_2021_03_04"
CV_FIGURE_PATH = "figures/cv_2021-03-04.png"

#
# step 1: process the data -----------------------------------------------
source("code/processing_functions.R")

chrono_processed = process_chrono_files(CHRONO_DATA_PATH)
(chrono_graph = plot_chrono_graph(chrono_processed))

cv_processed = process_cv_files(CV_DATA_PATH)
(cv_graph = plot_cv_graph(cv_processed))


# step 2: save the graphs -------------------------------------------------
ggsave(plot = chrono_graph, CHRONO_FIGURE_PATH)
ggsave(plot = cv_graph, CV_FIGURE_PATH)
