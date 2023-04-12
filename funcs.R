
# Compute the MD5 hash of a file
compute_hash <- function(file_path) {
  digest(file = file_path, algo = "md5")
}

# To compute the hash for a file at a given path and handle any errors that may occur,  you can call the "try_compute_hash" function and pass the file path as an argument, The "hash_result" variable will either contain the hash computed for the file, or the string "missing" if an error occurred during the computation.
try_compute_hash <- function(path) {
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

# Calculates the frequency of hash values for each folder in the input 
# data frame and adds the maximum hash value and its frequency to each 
# row of the data frame.
calculate_hash_freq <- function(df){
    folder_idx <- colnames(df) %>% grep(pattern ="folder_")
    hash_df <- df[,folder_idx]
    hash_df[hash_df == "missing"] <- NA
    max_hash <- rep(NA, nrow(hash_df))
    n_max_hash <- rep(NA, nrow(hash_df))
    all_diff <- rep(NA, nrow(hash_df))
    for(i in 1:nrow(hash_df)){
        table_i <- hash_df[i,] %>% as.character() %>% table()
        max_hash[i] <- names(table_i)[which.max(table_i)]
        n_max_hash[i] <- table_i %>% as.numeric() %>% max()
    }
    df$max_hash <- max_hash
    df$n_max_hash <- n_max_hash
    return(df)
}

fill_hash <- function(df){
    pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
    for(n in 1:nrow(df)){
            df$folder_a[n] <- try_compute_hash(paste0(folder_a_path, df$files[n]))
            df$folder_b[n] <- try_compute_hash(paste0(folder_b_path, df$files[n]))
            df$folder_c[n] <- try_compute_hash(paste0(folder_c_path, df$files[n]))
            setTxtProgressBar(pb, n)
    }
    close(pb)
    return(df)
}

create_shell_cmd <- function(df){
    df$shell_cmd_a <- NA
    df$shell_cmd_b <- NA
    df$shell_cmd_c <- NA
    for(n in 1:nrow(df)){
        folder_source <- folders[first(which(c(df$folder_a[n], df$folder_b[n], df$folder_c[n]) ==df$max_hash[n]))]
        source <- paste0(get(paste0(folder_source, "_path")), df$files[n])
        if(df$folder_a[n] != df$max_hash[n]){
            target <- paste0(folder_a_path, df$files[n])
            df$shell_cmd_a[n] <- glue('xcopy "{source}" "{target}"')
        }
        if(df$folder_b[n] != df$max_hash[n]){
            target <- paste0(folder_b_path, df$files[n])
            df$shell_cmd_b[n] <- glue('xcopy "{source}" "{target}"')
        }
        if(df$folder_c[n] != df$max_hash[n]){
            target <- paste0(folder_c_path, df$files[n])
            df$shell_cmd_c[n] <- glue('xcopy "{source}" "{target}"')
        }
    }
    df$shell_cmd_a <- gsub("/", "\\\\", df$shell_cmd_a)
    df$shell_cmd_b <- gsub("/", "\\\\", df$shell_cmd_b)
    df$shell_cmd_c <- gsub("/", "\\\\", df$shell_cmd_c)
    df$shell_cmd_a[df$broken_file] <- NA
    df$shell_cmd_b[df$broken_file] <- NA
    df$shell_cmd_c[df$broken_file] <- NA
    return(df)
}

# This function takes in a data frame and adds a new column 'new_file' to the data frame, with default value FALSE.
# It then looks for all columns whose names match the pattern "folder_" and saves their indexes in a variable 'folder_idx'.
# The function then iterates through each row of the data frame and checks if the values in the columns whose indexes are 
# in 'folder_idx' are at least 2 "missing".
# If the values in those columns are at least 2 "missing", then the value of the 'new_file' column for that row is set to TRUE.
# The function returns the modified data frame with the 'new_file' column.
check_new_file <- function(df){
    df$new_file <- FALSE
    folder_idx <- colnames(df) %>% grep(pattern ="folder_")
    for(i in 1:nrow(df)){
        if(sum(df[i, folder_idx] == "missing") == 2){df$new_file[i] <- TRUE}
    }
    return(df)
}

# This function takes in a data frame and adds a new column 'broken_file' to the data frame, with default value FALSE.
# It then looks for all columns whose names match the pattern "folder_" and saves their indexes in a variable 'folder_idx'.
# The function then iterates through each row of the data frame and checks if the values in the columns whose indexes are 
# in 'folder_idx' are not "missing" (none can be "missing") and if the value of the 'n_max_hash' column for that row is 1.
# If both conditions are met, then the value of the 'broken_file' column for that row is set to TRUE.
# The function returns the modified data frame with the 'broken_file' column.
check_broken_file <- function(df){
    df$broken_file <- FALSE
    folder_idx <- colnames(df) %>% grep(pattern ="folder_")
    for(i in 1:nrow(df)){
        if(sum(df[i, folder_idx] == "missing") == 0 & df$n_max_hash[i] == 1){df$broken_file[i] <- TRUE}
    }
    return(df)
}

get_overview <- function(df){
    match_a <- df$folder_a == df$max_hash
    match_b <- df$folder_b == df$max_hash
    match_c <- df$folder_c == df$max_hash
    overview <- data.frame(files = df$files,
                           a = match_a, 
                           b = match_b, 
                           c = match_c)
    
    overview$a[df$broken_file] <- FALSE
    overview$b[df$broken_file] <- FALSE
    overview$c[df$broken_file] <- FALSE
    return(overview)
}

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


run_shells  <- function(df){
    for(i in 1:nrow(df)){
        if(!is.na(df$shell_cmd_a[i])){shell(paste0(df$shell_cmd_a[i], " /Y /F"))}
        if(!is.na(df$shell_cmd_b[i])){shell(paste0(df$shell_cmd_b[i], " /Y /F"))}
        if(!is.na(df$shell_cmd_c[i])){shell(paste0(df$shell_cmd_c[i], " /Y /F"))}
    }
}
