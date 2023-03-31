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




fill_hash <- function(df, folder_a_path, folder_b_path, folder_c_path){
    for(n in 1:nrow(df)){
            df$folder_a[n] <- try_compute_hash(paste0(folder_a_path, df$files[n]))
            df$folder_b[n] <- try_compute_hash(paste0(folder_b_path, df$files[n]))
            df$folder_c[n] <- try_compute_hash(paste0(folder_c_path, df$files[n]))
    }
    return(df)
}

check_3_strings <- function(string_a, string_b, string_c){
    if(string_a == string_b & string_a == string_c){
        return(NA)
    }else if(string_b != string_c & string_a == string_b){
        return("c")
    }else if(string_a != string_b & string_b == string_c){
        return("a")
    }else if(string_a != string_b & string_a == string_c){
        return("b")
    }else{
        return("all")
    }
}
