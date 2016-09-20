
fix_column_names <- function(col_names) {
    col_names <- gsub(" ", "_", col_names)
    col_names <- tolower(col_names)

    return(col_names)
}
