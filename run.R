# this R code can be used to check if files R alike

library(digest)
library(tidyverse)
library(glue)

# source functions
source("funcs.R")

# paths to different folders
folder_a_path <- "C:/Users/eivin/AppData/Roaming/folder_a/"
folder_b_path <- "C:/Users/eivin/AppData/Roaming/mega/folder_b/"
folder_c_path <- "e:/folder_c/"

folders <- c("A", "B", "C")

overview <- create_df(folder_a_path, folder_b_path, folder_c_path)

overview <- fill_hash(overview, folder_a_path, folder_b_path, folder_c_path)

folder_idx <- colnames(overview) %>% grep(pattern ="folder_")
hash_df <- overview[,folder_idx]
hash_df[hash_df == "missing"] <- NA
hash_table <- apply(hash_df, 1, table)
max_hash <- as.character(lapply(hash_table, function(x) names(x)[which.max(x)])) # denne funker ikke og må byttes
n_max_hash <- as.numeric(lapply(hash_table, max))
overview$max_hash <- max_hash
overview$n_max_hash <- n_max_hash

hash_df
hash_table 



folder_idx <- colnames(overview) %>% grep(pattern ="folder_")
overview$message <- NA
for(n in 1:nrow(overview)){
    filename <- overview$files[n]
    print(filename)
    folder_without <- folders[!(overview[n,folder_idx] %in% overview$max_hash[n])]
    if(overview$n_max_hash[n] == 2){
        overview$message[n] <- glue("move file: {filename} into folder {folder_without}")
    }else if(overview$n_max_hash[n] == 1){
        overview$message[n] <- glue("Check all versions of {filename}")
    }else if(overview$n_max_hash[n] == 3){
        overview$message[n] <- glue("{filename} ok")
    }else{overview$message[n] <- "Something is wrong"}
}

overview$folder_a_ok <- overview$max_hash == overview$folder_a
overview$folder_b_ok <- overview$max_hash == overview$folder_b
overview$folder_c_ok <- overview$max_hash == overview$folder_c

overview

# coded from phone
overview$shell_cmd <- NA
source_folder <- folder_with # must be determined in the algorith above
source <- paste0(source_folder, overview$filename[n])

target_folder <- folder_without # how does one solve multiple folders without?
target <- paste0(target_folder, overview$filename[n])
# copy file
shell_cmd <- glue("cp {source} {target}")
