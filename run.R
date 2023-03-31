# this R code can be used to check if files R alike

library(digest)
library(tidyverse)

# source functions
source("funcs.R")

# paths to different folders
folder_a_path <- "C:/Users/eivin/AppData/Roaming/folder_a/"
folder_b_path <- "C:/Users/eivin/AppData/Roaming/mega/folder_b/"
folder_c_path <- "e:/folder_c/"


overview <- create_df(folder_a_path, folder_b_path, folder_c_path)

overview <- fill_hash(overview, folder_a_path, folder_b_path, folder_c_path)

overview
