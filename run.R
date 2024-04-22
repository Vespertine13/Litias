# run diagnostics
# remove all files from local environment
rm(list= ls())

# source main program
source("main.R")

# get plot of missing and decayed files
plot_overview(plot_df)

# run shell commands
run_shells(df)


