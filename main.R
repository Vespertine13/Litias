# this R code can be used to check if files R alike
print("-------------------------- Litias --------------------------")
# rm(list= ls())

print("Loading libraries...")
suppressPackageStartupMessages(library(tidyverse))
library(digest)
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

provide_folders()

print("------------------------------------------------------------")

print("Creating df...")

for(i in 1:length(folders)){
    assign(paste0(substr(folders[i], start = 8, stop = 8),"_files"),
           list.files(get(folders[i]), recursive = TRUE))
}

files_lst_names <- paste0(substr(folders, start = 8, stop = 8), "_files")
totfiles <- c()
for(name in files_lst_names){
    totfiles <- c(totfiles, get(name))
}
uniqfiles <- unique(totfiles)
df <- data.frame(files = uniqfiles)
for(i in folders){
    df[[i]] <- NA
}

folders_df <- df

print("done")


print("Calculating hash values...")
overview <- fill_hash(folders_df, folders)
print("Done")


print("Calculating statistics...")
overview$max <- NA
overview$n_max <- NA
overview$new_file <- NA
overview$broken_file <- NA

for(i in 1:nrow(overview)){
    hash_set <- overview[i, folders] %>% as.character()
    overview$max[i] <- max_hash(hash_set)
    overview$n_max[i] <- n_max_hash(hash_set)
    overview$new_file[i] <- check_new_file(hash_set)
    overview$broken_file[i] <- check_broken_file(hash_set)
}

print("Done")

overview
# I am here!

plot_df <- get_overview(overview)
print("Generating Shell Commands...")
shell_df <- create_shell_cmd(overview)
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

