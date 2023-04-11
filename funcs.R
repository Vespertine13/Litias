compute_hash <- function(file_path) {
  digest(file = file_path, algo = "md5")
}

try_compute_hash <- function(path) {
    result <- try(compute_hash(path))
    if (inherits(result, "try-error")) {
        return("missing")
    } else {
        return(result)
    }
}


create_df <- function(folder_a_path, folder_b_path, folder_c_path){
    files_a <- list.files(folder_a_path, recursive = TRUE)
    files_b <- list.files(folder_b_path, recursive = TRUE)
    files_c <- list.files(folder_c_path, recursive = TRUE)
    tot_items <- unique(c(files_a, files_b, files_c))
    df <- data.frame(files=tot_items, folder_a=NA, folder_b=NA, folder_c=NA)
    return(df)
}

# uses loop
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



fill_hash <- function(df, folder_a_path, folder_b_path, folder_c_path){
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
        folder_source <- folders[sample(which(c(df$folder_a[n], df$folder_b[n], df$folder_c[n]) ==df$max_hash[n]), 1)]
        source <- paste0(get(paste0(folder_source, "_path")), df$files[n])
        if(df$folder_a[n] != df$max_hash[n]){
            target <- paste0(folder_a_path, df$files[n])
            df$shell_cmd_a[n] <- glue("xcopy '{source}' '{target}'")
        }
        if(df$folder_b[n] != df$max_hash[n]){
            target <- paste0(folder_b_path, df$files[n])
            df$shell_cmd_b[n] <- glue("xcopy '{source}' '{target}'")
        }
        if(df$folder_c[n] != df$max_hash[n]){
            target <- paste0(folder_c_path, df$files[n])
            df$shell_cmd_c[n] <- glue("xcopy '{source}' '{target}'")
        }
    }
    df$shell_cmd_a <- gsub("/", "\\\\", df$shell_cmd_a)
    df$shell_cmd_b <- gsub("/", "\\\\", df$shell_cmd_b)
    df$shell_cmd_c <- gsub("/", "\\\\", df$shell_cmd_c)
    df$shell_cmd_a[df$n_max_hash == 1] <- NA
    df$shell_cmd_b[df$n_max_hash == 1] <- NA
    df$shell_cmd_c[df$n_max_hash == 1] <- NA
    return(df)
}



get_overview <- function(df){
    match_a <- df$folder_a == df$max_hash
    match_b <- df$folder_b == df$max_hash
    match_c <- df$folder_c == df$max_hash
    match_a[df$n_max_hash == 1] <- FALSE
    match_b[df$n_max_hash == 1] <- FALSE
    match_c[df$n_max_hash == 1] <- FALSE
    overview <- data.frame(files = df$files,
                           a = match_a, 
                           b = match_b, 
                           c = match_c)
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
        if(!is.na(df$shell_cmd_a[i])){shell(paste0(df$shell_cmd_a[i], " /y /i /f"))}
        if(!is.na(df$shell_cmd_b[i])){shell(paste0(df$shell_cmd_b[i], " /y /i /f"))}
        if(!is.na(df$shell_cmd_c[i])){shell(paste0(df$shell_cmd_c[i], " /y /i /f"))}
    }
}
