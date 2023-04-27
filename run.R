# run diagnostics
# remove all files from local environment
rm(list= ls())
source("main.R")
plot_overview(plot_df)
# run shell
run_shells(df)
# clear environment
