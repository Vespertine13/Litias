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
    assign(paste0(extract_letter(folders[i]),"_files"),
           list.files(get(folders[i]), recursive = TRUE))
}

files_lst_names <- paste0(extract_letter(folders), "_files")
totfiles <- c()
for(name in files_lst_names){
    totfiles <- c(totfiles, get(name))
}
uniqfiles <- unique(totfiles)
df <- data.frame(files = uniqfiles)
for(i in folders){
    df[[i]] <- NA
}



print("done")


print("Calculating hash values...")
df <- fill_hash(df, folders)

print("Done")

print("Calculating statistics...")
df$max <- NA
df$n_max <- NA
df$new_file <- NA
df$broken_file <- NA

for(i in 1:nrow(df)){
    hash_set <- df[i, folders] %>% as.character()
    df$max[i] <- max_hash(hash_set)
    df$n_max[i] <- n_max_hash(hash_set)
    df$new_file[i] <- check_new_file(hash_set)
    df$broken_file[i] <- check_broken_file(hash_set)
}

for(i in 1:length(folders)){
    df[[extract_letter(folders[i])]] <- get_match(df[[folders[i]]], df$max)
    df[[extract_letter(folders[i])]][df$broken_file] <- FALSE
}

plot_df <- df[,nchar(colnames(df)) == 1]
plot_df$files <- df$files
print("Done")


print("Generating Shell Commands...")

for(i in 1:length(folders)){
    df[[paste0("shell_cmd_",extract_letter(folders[i]))]] <- NA
}


# I am here!
idx <- grep(pattern = "folder_", colnames(df))
n <- 1
folder_source <- sample(folders[which(df[n, idx] == df$max[n])], 1)
source <- paste0(get(paste0(folder_source)), df$files[n])


idx <- grep(pattern = "folder_", colnames(df))
for(i in 1:nrow(df)){
    folder_source <- sample(folders[which(df[n, idx] == df$max[n])], 1)
    source <- paste0(get(paste0(folder_source)), df$files[n])
    for(n in 1:length(folders)){
        if(df[[folders[n]]][i] != df$max[i]){
            target <- paste0(get(folders[n]), df$files[n])
            df[[paste0("shell_cmd_",extract_letter(folders[n]))]][i] <- glue('xcopy "{source}" "{target}"')
        }
    }
}

print("Done")

n_broken <- sum(df$broken_file)
print(glue("Number of broken files: {n_broken}"))
if(sum(df$broken_file) >0){print(df$files[df$broken_file])}

n_new <- sum(df$new_file)
print(glue("Number of new files: {n_new}"))

if(sum(df$new_file) >0){print(df$files[df$new_file])}

total_cmd <- sum(!is.na(df$shell_cmd_a)) + 
    sum(!is.na(df$shell_cmd_b)) + 
    sum(!is.na(df$shell_cmd_c))

print(glue("Number of suggested commands: {total_cmd}"))
if(sum(!is.na(df$shell_cmd_a)) >0){print(df$shell_cmd_a[!is.na(df$shell_cmd_a)])}
if(sum(!is.na(df$shell_cmd_b)) >0){print(df$shell_cmd_b[!is.na(df$shell_cmd_b)])}
if(sum(!is.na(df$shell_cmd_c)) >0){print(df$shell_cmd_c[!is.na(df$shell_cmd_c)])}

print("------------------------------------------------------------")

