############################################################################################################################################
#                                  Script for univariate analyses on dataset
#############################################################################################################################################

########################## Summaries ####################################################################################

# 2x2 summary of categorical variables
summary_cat_by_group <- function(df, group_var_name = "Grupo") {
  #' To retrieve number of individuals and proportions of a modality of categorical variables of a dataframe, desagragated 
  #' according to another categorical variable
  #' @param df A dataframe
  #' @param group_var_name The variable of desagragation
  #' @return A dataframe with the overall numbers and proportions for each categorical variable of the dataset, in addition of 
  #' the desagragated results
  group_var <- df[[group_var_name]]
  
  # Ensure group_var is a factor
  group_var <- as.factor(group_var)
  group_levels <- levels(group_var)
  
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  cat_vars <- setdiff(cat_vars, group_var_name)  # exclude the grouping variable
  
  result_list <- list()
  
  for (var in cat_vars) {
    cat_var <- df[[var]]
    
    valid <- !is.na(cat_var) & !is.na(group_var)
    cat_var <- cat_var[valid]
    group_var_valid <- group_var[valid]
    
    overall_table <- table(cat_var)
    overall_prop <- prop.table(overall_table)
    all_levels <- names(overall_table)
    
    df_var <- data.frame(
      Variable = var,
      Category = all_levels,
      Overall_N_prop = paste0(
        as.integer(overall_table[all_levels]),
        " (", round(100 * as.numeric(overall_prop[all_levels]), 1), "%)"
      ),
      stringsAsFactors = FALSE
    )
    
    for (g in group_levels) {
      group_table <- table(cat_var[group_var_valid == g])
      group_prop <- prop.table(group_table)
      
      col_name <- paste0(g, "_N_prop")
      df_var[[col_name]] <- paste0(
        as.integer(group_table[all_levels]),
        " (", round(100 * as.numeric(group_prop[all_levels]), 1), "%)"
      )
      
      # Replace NAs with "0 (0%)"
      df_var[[col_name]][is.na(df_var[[col_name]])] <- "0 (0%)"
    }
    
    result_list[[var]] <- df_var
  }
  
  final_df <- do.call(rbind, result_list)
  rownames(final_df) <- NULL
  return(final_df)
}

# 2x2 summary of numerical variables
summary_num_by_group <- function(df, group_var_name = "Grupo") {
  #' To retrieve mean and SD of numerical variables of a dataframe, desagragated according to another categorical variable
  #' @param df A dataframe
  #' @param group_var_name The variable of desagragation
  #' @return A dataframe with the overall mean and SD for each numerical variable of the dataset, in addition of the desagragated results
  group_var <- df[[group_var_name]]
  group_var <- as.factor(group_var)
  group_levels <- levels(group_var)
  
  num_vars <- names(df)[sapply(df, is.numeric)]
  result_list <- list()
  
  for (var in num_vars) {
    x <- df[[var]]
    valid <- !is.na(x) & !is.na(group_var)
    x <- x[valid]
    group_var_valid <- group_var[valid]
    
    df_var <- data.frame(
      Variable = var,
      Overall_mean_SD = sprintf("%.1f (%.1f)", mean(x), sd(x)),
      Overall_med_IQR = sprintf("%.1f (%.1f - %.1f)", median(x), quantile(x, 0.25), quantile(x, 0.75)),
      Overall_min_max = sprintf("%.1f - %.1f", min(x), max(x)),
      stringsAsFactors = FALSE
    )
    
    for (g in group_levels) {
      x_g <- x[group_var_valid == g]
      mean_sd <- sprintf("%.1f (%.1f)", mean(x_g), sd(x_g))
      med_iqr <- sprintf("%.1f (%.1f - %.1f)", median(x_g), quantile(x_g, 0.25), quantile(x_g, 0.75))
      min_max <- sprintf("%.1f - %.1f", min(x_g), max(x_g))
      
      df_var[[paste0(g, "_mean_SD")]] <- mean_sd
      df_var[[paste0(g, "_med_IQR")]] <- med_iqr
      df_var[[paste0(g, "_min_max")]] <- min_max
    }
    
    result_list[[var]] <- df_var
  }
  
  final_df <- do.call(rbind, result_list)
  rownames(final_df) <- NULL
  return(final_df)
}


#################### Correlation tests ################################################################

compare_var_cat <- function(df, group_var_name) {
  #' To see if categorical variables are associated with the variable of interest, using chi2 or fisher test according to headcount
  #' @param df A dataframe
  #' @param group_var_name name of the variable of interest
  #' @return A data frame with categerical variables names, the name of the test, the p-value and the verdict (wheter or not the test is significant)
  

  #retrive categorical variables in the dataframe
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  cat_vars <- setdiff(cat_vars, group_var_name)
  result_list <- list()

  for (var in cat_vars) {
    #For each categorical variable compute contigency table...
    contingency <- table(df[[var]], df[[group_var_name]])
    expected <- chisq.test(contingency)$expected
    test_type <- ""
    pval <- NA

    #...then check headcounts
    if (all(expected >= 5)) {
      #...chi2 test if all headcounts >5...
      pval <- round(chisq.test(contingency)$p.value, 3)
      test_type <- "Chi2"
    } else {
      #...or else fisher test
      pval <- round(fisher.test(contingency)$p.value, 3)
      test_type <- "Fisher"
    }

    verdict <- if (pval < 0.1) "associated/significative difference" else "No significative difference"
    
    # Format results
    result_list[[var]] <- data.frame(
      Variable = var,
      Test = test_type,
      P_value = pval,
      Verdict = verdict,
      stringsAsFactors = FALSE
    )
  }

  final_df <- do.call(rbind, result_list)
  rownames(final_df) <- NULL
  return(final_df)
}


compare_var_num <- function(df, group_var_name) {
  #' To see if numerical variables are associated with the variable of interest, using student or mann-whitney test according to normality
  #' @param df A dataframe
  #' @param group_var_name name of the variable of interest
  #' @return A data frame with numerical variables names, the name of the test, the p-value and the verdict (wheter or not the test is significant)
  
  #Retrive numerical variabels in the dataframe
  num_vars <- names(df)[sapply(df, is.numeric)]
  result_list <- list()

  for (var in num_vars) {
    data_subset <- df[!is.na(df[[var]]) & !is.na(df[[group_var_name]]), ]
    
    # Skip if fewer than 3 non-NA values per group
    if (any(table(data_subset[[group_var_name]]) < 3)) next
    
    # Normality test on whole variable
    if (shapiro.test(data_subset[[var]])$p.value >= 0.05) {
      # Student t-test
      group0 <- data_subset[data_subset[[group_var_name]] == 0, var]
      group1 <- data_subset[data_subset[[group_var_name]] != 0, var]
      pval <- round(t.test(group0, group1)$p.value,3)
      test <- "Student"
    } else {
      # Wilcoxon test
      formula <- as.formula(paste(var, "~", group_var_name))
      pval <- round(wilcox.test(formula, data = data_subset)$p.value,3)
      test <- "Mann-Whitney"
    }

    verdict <- if (pval < 0.1) "associated/significative difference" else "No significative difference"

    #Format results
    result_list[[var]] <- data.frame(
      Variable = var,
      test=test,
      pvalue = pval,
      verdict = verdict,
      stringsAsFactors = FALSE
    )
  }

  final_df <- do.call(rbind, result_list)
  rownames(final_df) <- NULL
  return(final_df)
}

compare_proportions <- function(data, variables, group_var) {
  #' To see if proportion differ from one modality of a categorical variable to another,according to a variable of interest, using a Chi-squared test for proportions
  #' @param data A dataframe
  #' @param variables list of variables we want to test
  #' @param group_var name of the variable of interest
  #' @return A data frame with the variables names, the p-value of the test, the difference of proportions and the 95% CI
  
  results <- lapply(variables, function(var) {
    tab <- table(data[[group_var]], data[[var]])
    
    # Ensure the table is 2x2
    if (!all(dim(tab) == c(2, 2))) return(NULL)
    
    # Make sure rows of the table are in order: group 0 then group 1
    group_levels <- sort(unique(data[[group_var]]))
    if (!all(as.numeric(rownames(tab)) == group_levels)) {
      tab <- tab[as.character(group_levels), ]
    }
    
    # Extract success counts (column 1 = 0, column 2 = 1)
    successes <- tab[, "1"]
    totals <- rowSums(tab)

    # prop2 - prop1
    test <- prop.test(successes, totals, correct = FALSE)
    diff_prop <- round(100 * (test$estimate[1] - test$estimate[2]), 0)
    ci_low <- round(100 * test$conf.int[1], 0)
    ci_high <- round(100 * test$conf.int[2], 0)
    p_val <- round(test$p.value, 2)

    diff_ci_str <- paste0(diff_prop, " (", ci_low, "–", ci_high, ")")

    data.frame(
      Variable = var,
      P_value = p_val,
      Difference_CI = diff_ci_str,
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  do.call(rbind, results)
}

compare_medians <- function(data, variables, group_var) {
  #' To see if medians differ from one modality of a categorical variable to another, according to a variable of interest, using a Mann–Whitney U test
  #' @param data A dataframe
  #' @param variables list of variables we want to test
  #' @param group_var name of the variable of interest
  #' @return A data frame with the variables names, the p-value of the test, the difference of medians and the 95% CI


  results <- lapply(variables, function(var) {
    group0 <- data[data[[group_var]] == 0, var, drop = TRUE]
    group1 <- data[data[[group_var]] == 1, var, drop = TRUE]
    
    # Remove NA
    group0 <- group0[!is.na(group0)]
    group1 <- group1[!is.na(group1)]
    
    if (length(group0) == 0 || length(group1) == 0) return(NULL)

    # Median difference
    diff_median <- round(median(group1) - median(group0), 2)
    
    # P-value
    p_val <- round(wilcox.test(group0, group1, exact = FALSE)$p.value, 4)
    
    # Bootstrap CI for median difference
    boot_diffs <- replicate(1000, {
      med1 <- median(sample(group1, replace = TRUE))
      med0 <- median(sample(group0, replace = TRUE))
      med1 - med0
    })
    
    ci <- quantile(boot_diffs, probs = c((1 - 0.95)/2, 1 - (1 - 0.95)/2))
    ci_str <- paste0(round(ci[1], 2), " – ", round(ci[2], 2))
    
    data.frame(
      Variable = var,
      Median_Diff = diff_median,
      CI_95 = ci_str,
      P_value = p_val,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}


# Define CorrelationsStudy class
CorrelationsStudy <- setRefClass(
    #' A class of methods to perform correlation analysis
    #' @params A dataframe and a list of its numerical and categorical variables
  "CorrelationsStudy",
  fields = list(
    dataset = "data.frame",
    numerical_variables = "character",
    categorical_variables = "character"
  ),
  methods = list(
    initialize = function(dataset) {
      dataset <<- dataset
      numerical_variables <<- names(dataset)[sapply(dataset, is.numeric)] #retrieve list of numerical variables
      categorical_variables <<- setdiff(names(dataset), numerical_variables) #retrieve list of categorical variables
    },

    num_correlation_heatmap = function() {
        #' To compute pearson correlation scores among all numerical variables
        #' @return A heatmap displaying correlations

      corr_matrix <- cor(dataset[, numerical_variables], use = "pairwise.complete.obs")
      melted_corr <- melt(corr_matrix)
      ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), size = 4) +
        scale_fill_distiller(palette = "Spectral", direction = 1,limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Numerical Correlation Heatmap", fill = "Correlation")
    },

    cat_correlation_heatmap = function() {
        #' To compute Cramer's v correlation scores among all categorical variables
        #' @return A heatmap displaying correlations
        
      n_vars <- length(categorical_variables)
      assoc_matrix <- matrix(1, nrow = n_vars, ncol = n_vars)
      colnames(assoc_matrix) <- categorical_variables
      rownames(assoc_matrix) <- categorical_variables

      for (i in seq_len(n_vars)) {
        for (j in seq(i + 1, n_vars)) {
          x <- dataset[[categorical_variables[i]]]
          y <- dataset[[categorical_variables[j]]]
          assoc <- CramerV(table(x, y), bias.correct = TRUE)
          assoc_matrix[i, j] <- assoc
          assoc_matrix[j, i] <- assoc
        }
      }
      melted_assoc <- melt(assoc_matrix)
      ggplot(melted_assoc, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), size = 4) +
        scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Categorical Correlation Heatmap (Cramer's V)", fill = "Cramer's V")
    },

    kruskall_wallis = function() {
        #' To compute kruskall-wallis test (difference of means) to see how numerical and categorical variables are correlated
        #' @return A list of correlated couples (numerical variable, categorical variable)
        
      kruskal_correlation <- list()

      for (num_var in numerical_variables) {
        for (cat_var in categorical_variables) {
          df <- dataset %>% filter(!is.na(.data[[num_var]]), !is.na(.data[[cat_var]]))
          if (length(unique(df[[cat_var]])) < 2) next

          kw_test <- kruskal.test(df[[num_var]] ~ as.factor(df[[cat_var]]))
          if (kw_test$p.value <= 0.05) {
            kruskal_correlation <- append(kruskal_correlation, list(c(num_var, cat_var)))
          }
        }
      }

      return(kruskal_correlation)
    }
  )
)
