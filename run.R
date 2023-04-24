# run diagnostics
file_filter <- "test"
source("main.R")
plot_overview(plot_df)
# run shell
# run_shells(df)
# clear environment
# rm(list= ls())


# template
shell("copy /y e:\\folder_c\\test\\flytt.txt f:\\folder_a\\test\\flytt.txt")
