# this R code can be used to check if files R alike
print("-------------------------- Litias --------------------------")
print("Loading libraries...")
library(digest)
library(tidyverse)
library(glue)
library(reshape2)
print("Done")

# source functions
print("Sourcing functions...")
source("funcs.R")
print("Done")
print("------------------------------------------------------------")

# paths to different folders
source("config.R")
print("Current folders")
print(folder_a_path)
print(folder_b_path)
print(folder_c_path)

print("------------------------------------------------------------")

folders <- c("folder_a", "folder_b", "folder_c")
folders_df <- create_df(folder_a_path, folder_b_path, folder_c_path)
print("Calculating hash values")
overview <- fill_hash(folders_df, folder_a_path, folder_b_path, folder_c_path)
freq_df <- calculate_hash_freq(overview)
print("Done")

plot_df <- get_overview(freq_df)
print("Generating Shell Commands")
shell_df <- create_shell_cmd(freq_df)
print("Done")

n_broken <- sum(shell_df$n_max_hash == 1)
print(glue("Number of broken or new files: {n_broken}"))
if(sum(shell_df$n_max_hash == 1) >0){print(shell_df$files[shell_df$n_max_hash == 1])}

total_cmd <- sum(!is.na(shell_df$shell_cmd_a)) + 
    sum(!is.na(shell_df$shell_cmd_b)) + 
    sum(!is.na(shell_df$shell_cmd_c))

print(glue("Number of suggested commands: {total_cmd}"))
if(sum(!is.na(shell_df$shell_cmd_a)) >0){print(shell_df$shell_cmd_a[!is.na(shell_df$shell_cmd_a)])}
if(sum(!is.na(shell_df$shell_cmd_b)) >0){print(shell_df$shell_cmd_b[!is.na(shell_df$shell_cmd_b)])}
if(sum(!is.na(shell_df$shell_cmd_c)) >0){print(shell_df$shell_cmd_c[!is.na(shell_df$shell_cmd_c)])}

print("------------------------------------------------------------")

