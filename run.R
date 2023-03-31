# this R code can be used to check if files R alike

library(digest)
library(tidyverse)

# source functions
source("funcs.R")

# paths to different folders
folder_a_path <- "C:/Users/eivin/AppData/Roaming/folder_a/"
folder_b_path <- "C:/Users/eivin/AppData/Roaming/mega/folder_b/"
folder_c_path <- "e:/folder_c/"



files_a <- list.files(folder_a_path)
files_b <- list.files(folder_b_path)
files_c <- list.files(folder_c_path)

for(i in 1:length(files_a)){
    print(paste0("checking file: ", files_a[i]))
    print(check_3_strings(files_a[i], files_b[i], files_c[i]))
    hash_a <- compute_hash(paste0(folder_a_path, files_a[i]))
    hash_b <- compute_hash(paste0(folder_b_path, files_b[i]))
    hash_c <- compute_hash(paste0(folder_c_path, files_c[i]))
    print(check_3_strings(hash_a, hash_b, hash_c))
}

