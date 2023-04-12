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
print("Calculating hash values...")
overview <- fill_hash(folders_df)
print("Done")

print("Calculating statistics...")
freq_df <- calculate_hash_freq(overview)
df_with_new_files <- check_new_file(freq_df)
df_with_broken_files <- check_broken_file(df_with_new_files)
print("Done")


plot_df <- get_overview(df_with_broken_files)
print("Generating Shell Commands...")
shell_df <- create_shell_cmd(df_with_broken_files)
print("Done")

n_broken <- sum(shell_df$broken_file)
print(glue("Number of broken files: {n_broken}"))
if(sum(shell_df$broken_file) >0){print(shell_df$files[shell_df$broken_file])}

n_new <- sum(shell_df$new_file)
print(glue("Number of new files: {n_new}"))

if(sum(shell_df$new_file) >0){print(shell_df$files[shell_df$new_file])}

total_cmd <- sum(!is.na(shell_df$shell_cmd_a)) + 
    sum(!is.na(shell_df$shell_cmd_b)) + 
    sum(!is.na(shell_df$shell_cmd_c))

print(glue("Number of suggested commands: {total_cmd}"))
if(sum(!is.na(shell_df$shell_cmd_a)) >0){print(shell_df$shell_cmd_a[!is.na(shell_df$shell_cmd_a)])}
if(sum(!is.na(shell_df$shell_cmd_b)) >0){print(shell_df$shell_cmd_b[!is.na(shell_df$shell_cmd_b)])}
if(sum(!is.na(shell_df$shell_cmd_c)) >0){print(shell_df$shell_cmd_c[!is.na(shell_df$shell_cmd_c)])}

print("------------------------------------------------------------")

