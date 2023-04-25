# run diagnostics
file_filter <- "music"
source("main.R")
plot_overview(plot_df)
# run shell
run_shells(df)
# clear environment
# rm(list= ls())
