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
    return(names(table_x)[which.max(table_x)])
}

# takes row of hash and returns the count of the most common hash
n_max_hash <- function(x){
    x[x == "missing"] <- NA
    table_x <- table(x)
    return(max(as.numeric(table_x)))
}

# takes row of hash and returns TRUE if the file is new
check_new_file <- function(x){
    if(sum(x == "missing") == (length(folders)-1)){return(TRUE)
    }else(return(FALSE))
}

check_broken_file <- function(x){
    if(sum(x == "missing") == 0 & n_max_hash(x) == 1){return(TRUE)
    }else(return(FALSE))
}


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

create_shell_cmd <- function(df){
    df$shell_cmd_a <- NA
    df$shell_cmd_b <- NA
    df$shell_cmd_c <- NA
    for(n in 1:nrow(df)){
        folder_source <- folders[first(which(c(df$folder_a[n], df$folder_b[n], df$folder_c[n]) ==df$max[n]))]
        source <- paste0(get(paste0(folder_source, "_path")), df$files[n])
        if(df$folder_a[n] != df$max[n]){
            target <- paste0(folder_a_path, df$files[n])
            df$shell_cmd_a[n] <- glue('xcopy "{source}" "{target}"')
        }
        if(df$folder_b[n] != df$max[n]){
            target <- paste0(folder_b_path, df$files[n])
            df$shell_cmd_b[n] <- glue('xcopy "{source}" "{target}"')
        }
        if(df$folder_c[n] != df$max[n]){
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

get_match <- function(x, y){
    return(x == y)
}

get_folder_letter <- function(x){
    return(substr(x, start = 8, stop = 8))
}

get_overview <- function(df){
    match_a <- df$folder_a == df$max
    match_b <- df$folder_b == df$max
    match_c <- df$folder_c == df$max
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
