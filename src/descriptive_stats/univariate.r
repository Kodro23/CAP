###Script for univariate analyses on dataset
#Summary of dataset
characteristics <- function(dataset) {
    #' To compute characteristics of a dataet
    #' @param dataset 'A dataframe'
    #' @return a dataframe with type, number of modalities, number of NA, min, mean, max and mode of each variable of the dataset


  carac <- data.frame(variable = colnames(dataset), stringsAsFactors = FALSE)
  
  # Types
  carac$types <- sapply(dataset, class)
  
  # Number of modalities
  carac$nombre_de_modalites <- sapply(dataset, function(col) length(unique(col)))
  carac$liste_modalites <- sapply(dataset, function(col) {
    vals <- unique(col)
    vals <- vals[!is.na(vals)]  # remove NA
    if (length(vals) <= 5) {
      paste(vals, collapse = ", ")
    } else {
      ""
    }
  })
  
  # Number of missing values
  carac$number_of_missing_values <- sapply(dataset, function(col) sum(is.na(col)))
  
  # Proportion of missing values
  carac$proportion_missing_values_percent <- round(carac$number_of_missing_values / nrow(dataset) * 100, 1)
  
  # Mean, max, min for numeric columns
  carac$moyenne <- sapply(dataset, function(col) {
    if (all(is.na(col))) {NA}
    else if (is.numeric(col) && !inherits(col, "haven_labelled")) {mean(col, na.rm = TRUE)}
    else {NA}
}) 
  carac$maximum <- sapply(dataset, function(col) {
    if (all(is.na(col))) {NA}
    else if (is.numeric(col) && !inherits(col, "haven_labelled")) {max(col, na.rm = TRUE)}
    else {NA}
})
  carac$minimum <- sapply(dataset, function(col) {
    if (all(is.na(col))) {NA}
    else if (is.numeric(col) && !inherits(col, "haven_labelled")) {min(col, na.rm = TRUE)}
    else {NA}
})
  
  # Mode
  get_mode <- function(col) {
    col <- col[!is.na(col)]
    if (length(col) == 0) return(NA)
    uniq_vals <- unique(col)
    freq <- tabulate(match(col, uniq_vals))
    uniq_vals[which.max(freq)]
  }
  
  carac$mode <- sapply(dataset, get_mode)
  
  # Proportion of mode
  prop_mode <- function(col, mode_val) {
  # Convert haven_labelled or factors to character first
  col_chr <- as.character(col)
  mode_chr <- as.character(mode_val)
  
  if (is.na(mode_val)) {
    return(round(sum(is.na(col)) / length(col) * 100, 3))
  } else {
    return(round(sum(col_chr == mode_chr, na.rm = TRUE) / length(col) * 100, 3))
  }
}
  
  carac$proportion_mode_percent <- mapply(prop_mode, dataset, carac$mode)
  
  return(carac)
}

#Check normality of numerical variables
seek_normality<-function(col) {
    #' Perform a shapiro-wilk test of normality to see if a numerical variable is normally distributed
    #' @param col 'The variable'
    #' @return a datafram with the veridict of normality 
  if (is.numeric(col) && !inherits(col, "haven_labelled")) {
    col_clean <- col[!is.na(col)]
    if (length(col_clean) < 3) {
      return(NA)  # Shapiro-Wilk requires at least 3 values
    }
    p_val <- shapiro.test(col_clean)$p.value
    if (p_val >= 0.05) {
      return("normal")
    } else {
      return("not normal")
    }
  } else {
    return(NA)
  }
}


