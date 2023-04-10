# this R code can be used to check if files R alike

library(digest)
library(tidyverse)
library(glue)

# source functions
source("funcs.R")

# paths to different folders
folder_a_path <- "e:/folder_a/"
folder_b_path <- "e:/folder_b/"
folder_c_path <- "e:/folder_c/"

folders <- c("folder_a", "folder_b", "folder_c")

folders_df <- create_df(folder_a_path, folder_b_path, folder_c_path)
overview <- fill_hash(folders_df, folder_a_path, folder_b_path, folder_c_path)


freq_df <- calculate_hash_freq(overview)

shell_df <- create_shell_cmd(freq_df)

run_shells(shell_df)

