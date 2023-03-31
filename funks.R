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
        return("broken")
    }
}

# creates a hash string from a file
compute_hash <- function(file_path) {
  digest(file = file_path, algo = "md5")
}
