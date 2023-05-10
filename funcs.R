# Compute the MD5 hash of a file
compute_hash <- function(file_path){
  return(digest(file = file_path, algo = "sha256"))
}

# To compute the hash for a file at a given path and handle any errors that may occur,  you can call the "try_compute_hash" function and pass the file path as an argument, The "hash_result" variable will either contain the hash computed for the file, or the string "missing" if an error occurred during the computation.
try_compute_hash <- function(path){
    result <- try(compute_hash(path))
    if (inherits(result, "try-error")) {
        return("missing")
    } else {
        return(result)
    }
}

# This R function "create_df" takes three arguments: "folder_a_path", 
# "folder_b_path", and "folder_c_path", which are the paths to three 
# different folders. The function uses the "list.files" function to 
# obtain a list of all the files in each of these folders, recursively
# (i.e., it includes all subdirectories). It then combines these three
# lists of files, removing any duplicates, and creates an empty data 
# frame with columns for "files", "folder_a", "folder_b", and "folder_c". 
# Finally, the function returns this data frame.
create_df <- function(folder_a_path, folder_b_path, folder_c_path){
    files_a <- list.files(folder_a_path, recursive = TRUE)
    files_b <- list.files(folder_b_path, recursive = TRUE)
    files_c <- list.files(folder_c_path, recursive = TRUE)
    tot_items <- unique(c(files_a, files_b, files_c))
    df <- data.frame(files=tot_items, folder_a=NA, folder_b=NA, folder_c=NA)
    return(df)
}

# creates a folder variable with the folders given in config
provide_folders <- function(){
    if(!("folders" %in% ls(envir=.GlobalEnv))){
        print("creating variable folders")
        folders <<- ls(envir=.GlobalEnv)[grep(pattern = "folder_",ls(envir=.GlobalEnv))]
    }
}

# takes row of hash and returns the most common
max_hash <- function(x){
    x[x == "missing"] <- NA
    table_x <- table(x)
    y <- names(table_x)[which.max(table_x)]
    if(is.character(y)){return(y)
    }else{return("missing")}
}

# takes row of hash and returns the count of the most common hash
n_max_hash <- function(x){
    x[x == "missing"] <- NA
    table_x <- table(x)
    y <- max(as.numeric(table_x))
    if(is.numeric(y) & y > 0){return(y)
    }else{return(0)}
}

# takes row of hash and returns TRUE if the file is new
check_new_file <- function(x){
    if(sum(x == "missing") == (length(folders)-1)){return(TRUE)
    }else(return(FALSE))
}

# checks if a file is broken
# that is: all files are present, but different
# meaning that it is unknown which is like the original
check_broken_file <- function(x){
    if(sum(x == "missing") == 0 & n_max_hash(x) <= 1){return(TRUE)
    }else if(n_max_hash(x) == 0){return(TRUE)
    }else(return(FALSE))
}

# uses the function try_compute_hash to get the hash value of all files listed in df
fill_hash <- function(df,folders){
    pb <- txtProgressBar(min = 0, max = nrow(df)*length(folders), style = 3)
    counter <- 0
    for(i in 1:length(folders)){
        for(n in 1:nrow(df)){
            df[[folders[i]]][n] <- try_compute_hash(paste0(get(folders[i]), df$files[n]))
            counter <- counter + 1
            setTxtProgressBar(pb, counter)
        }
    }
    close(pb)
    return(df)
}


# checks if x and y are the same
get_match <- function(x, y){
    return(x == y)
}

# extracts the letter that identifies a folder
extract_letter <- function(x){
    return(substr(x, start = 8, stop = 8))
}

# creates a heatmap like overview over missing and changed files
plot_overview <- function(df){
    plot_df <- melt(df, id.vars = "files")

    if(FALSE %in% unlist(df)){
        fig <- ggplot(plot_df, aes(x = variable,
                                   y = files,
                                   fill = as.factor(value))) +
            geom_tile() + 
            labs(x = "Folder", fill = "File OK") +
            scale_fill_manual(values = c("red", "green"))
    }else{
        fig <- ggplot(plot_df, aes(x = variable,
                                   y = files,
                                   fill = as.factor(value))) +
            geom_tile() + 
            labs(x = "Folder", fill = "File OK") +
            scale_fill_manual(values = c("green", "red"))
    }
    return(fig)
}

# takes character path to file or folder and converts it into a windows style path
to_windows <- function(x){
    return(str_replace_all(x, pattern = "/", replacement = "\\\\"))
}

# runs all shells in shell columns (in variable all_shells)
# specifically it 
# first check if folder is created and creates it if not
# copies files into the folder overwriting existing file if any 
run_shells  <- function(df){
    for(i in 1:nrow(df)){
        for(n in 1:length(folders)){
            j <- extract_letter(folders[n])
            if(!is.na(df[[all_shells[n]]][i])){
                current_dir <- df[[paste0("target_folder_", j)]][i]
                if(!dir.exists(current_dir)){
                    dirname_windows <- to_windows(current_dir)
                    shell(glue('mkdir "{dirname_windows}"'))
                }
                shell(df[[all_shells[n]]][i])
            }
        }
    }
}

