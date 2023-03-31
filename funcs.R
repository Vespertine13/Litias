# checks if one a string is didifferent from another
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

# creates a hash string from a file
compute_hash <- function(file_path) {
  digest(file = file_path, algo = "md5")
}

# check filenames and folders for disrepancies [spelling?]
check_folders_length <- function(path_a, path_b, path_c){
    files_a <- list.files(folder_a_path)
    files_b <- list.files(folder_b_path)
    files_c <- list.files(folder_c_path)
    a <- length(files_a) == length(files_b)
    b <- length(files_a) == length(files_c)
    c <- a & b
    return(c)
}

check_folders_filenames <- function(folder_a_path, folder_b_path, folder_c_path){
    content_a <- list.files(folder_c_path) %>% paste0(collapse = " ")
    content_b <- list.files(folder_c_path) %>% paste0(collapse = " ")
    content_c <- list.files(folder_c_path) %>% paste0(collapse = " ")
    a <- content_a == content_b
    b <- content_a == content_c
    c <- a & b
    return(c)
}

