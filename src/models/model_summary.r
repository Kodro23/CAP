
####################################### Logistic regression ##############################################################
library(pROC)
library(ResourceSelection)
logit <- setRefClass(
    #' A class of methods to perform logistic regression 
    #' @params A dataframe and a list of binary variables to explain
  "logit",
  fields = list(
    dataset = "mids",
    vars_to_explain = "character"
  ),
  methods = list(
    initialize = function(dataset, vars_to_explain) {
      imp <<- dataset
      vars_to_explain <<- vars_to_explain
    },

    fit_logit = function(main_var="Grupo", other_variables=NULL,interraction_terms=NULL) {
        #' To fit the logistic model for each variable to explain with "Grupo" as main explicative variable
        #' @return A dataframe with odds ratio, pvalues and confidence intervals
        
        extract_stats <- function(pooled_fit, row_index) {
            tidy_fit <- tidy(pooled_fit, conf.int = TRUE, exponentiate = TRUE)
            row <- tidy_fit[row_index, ]

            if (nrow(row) == 0) {
                term <- NA
                or_ci <- NA
                p_val <- NA
            } else {
                term <- row$term
                or <- round(row$estimate, 2)
                ci_lower <- round(row$conf.low, 2)
                ci_upper <- round(row$conf.high, 2)
                p_val <- row$p.value
                or_ci <- paste0(or, " (", ci_lower, " - ", ci_upper, ")")
                }
  
            list(term = term, or_ci = or_ci, p_val = p_val)
        }


        results_list <- map(vars_to_explain, function(var) {
        # outcome ~ Grupo
            if (is.null(interraction_terms)){
                fit1 <- with(imp, glm(as.formula(paste(var, "~", main_var)), family = binomial))
                pool1 <- pool(fit1)
                rows_to_extract <- 2:nlevels(factor(data_dint[[main_var]]))
                stats1 <- map_dfr(rows_to_extract, ~extract_stats(pool1, .x))
                if (!is.null(other_variables)){
                    fit2 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var, other_variables), collapse = " + "))), family = binomial))
                    pool2 <- pool(fit2)
                    stats2 <- map_dfr(rows_to_extract, ~extract_stats(pool2, .x))
                    data.frame(
                    Variable_to_explain = var,
                    OR_CI_not_adjusted = stats1$or_ci,
                    p_value_1 = stats1$p_val,
                    OR_CI_adjusted = stats2$or_ci,
                    p_value_2 = stats2$p_val,
                    stringsAsFactors = FALSE
                )
                }else{
                    data.frame(
                        Variable_to_explain = var,
                        OR_CI= stats1$or_ci,
                        p_value= stats1$p_val,
                        stringsAsFactors = FALSE
                    )
            }}else{# outcome ~ Grupo*interraction_term
                interraction <-list()
                for (inter in interraction_terms){
                    interraction[[length(interraction)+1]] <-paste(main_var,":", inter)
                }
                fit1 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var, interraction_terms, interraction), collapse = " + "))), family = binomial))
                pool1 <- pool(fit1)

                #number of rows for main_var and interraction terms
                n_main<-nlevels(factor(data_dint[[main_var]])) + sum(sapply(data_dint[interraction_terms], function(x) nlevels(as.factor(x))))-(length(interraction_terms)+1)
                #number of rows for interraction
                n_inter_terms<-sum(sapply(data_dint[interraction_terms], function(x) (nlevels(as.factor(data_dint[[main_var]]))-1)*(nlevels(as.factor(x))-1) ))
                #total number of rows
                nrows<-1+n_main+n_inter_terms
                rows_to_extract<-c(2:nrows)
                stats1 <- map_dfr(rows_to_extract, ~extract_stats(pool1, .x))
                if (!is.null(other_variables)){
                    #number of rows for other variables
                    n_other<-sum(sapply(data_dint[other_variables], function(x) nlevels(as.factor(x))-1))
                    #skip other variables
                    n_inter<-n_main+n_other+1
                    #total number of rows
                    nrows<-1+n_main+n_inter_terms+n_other
                    rows_to_extract<-c(2:n_main, n_inter:nrows)
                    fit2 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var,interraction_terms, interraction, other_variables), collapse = " + "))), family = binomial))
                    pool2 <- pool(fit2)
                    stats2 <- map_dfr(rows_to_extract, ~extract_stats(pool2, .x))
                    data.frame(
                        Variable_to_explain = paste(var, "x", stats1$term),
                        OR_CI_not_adjusted = stats1$or_ci,
                        p_value_1 = stats1$p_val,
                        OR_CI_adjusted = stats2$or_ci,
                        p_value_2 = stats2$p_val,
                        stringsAsFactors = FALSE
                    )
                }else{
                    data.frame(
                        Variable_to_explain = paste(var, "x", stats1$term),
                        OR_CI= stats1$or_ci,
                        p_value= stats1$p_val,
                        stringsAsFactors = FALSE
                    )
                }
        }        
        })

        # Combine all into one dataframe
        results_df_log <- bind_rows(results_list)
        return(results_df_log)
    }
    ,

    performance_logit = function(main_var="Grupo", other_variables=NULL, interraction_terms=NULL) {
        #' Goodness of fit of logistic regression
        #' @return A dataframe with model performances
        
        # Function to extract model performances
        extract_performance <- function(fit,var) {
            pred_list <- lapply(fit$analyses, function(model) {
            predict(model, type = "response")
            })
            # Average predicted probabilities across imputations
            avg_probs <- Reduce("+", pred_list) / length(pred_list)
            # Actual values from the original data used for imputation
            actuals_f <- factor(imp$data[[var]],levels = c(0,1))
            avg_probs <- avg_probs[!is.na(actuals_f)]
            preds_f <- factor(ifelse(avg_probs >= 0.5, 1, 0), levels = c(0,1))
            # Confusion matrix anc ROC curve
            cm<-confusionMatrix(data = preds_f, reference = actuals_f, positive = "1")
            # roc_obj <- roc(actuals_f, avg_probs)
            #Deviance residuals
            residuals_list <- lapply(fit$analyses, function(mod) residuals(mod, type = "deviance"))
            residuals_matrix <- do.call(cbind, residuals_list)
            number_deviant_residuals <- length(unique(c(as.numeric(which(abs(residuals_list[[1]]) > 2)),
                                                as.numeric(which(abs(residuals_list[[2]]) > 2)),
                                                as.numeric(which(abs(residuals_list[[3]]) > 2)),
                                                as.numeric(which(abs(residuals_list[[4]]) > 2)),
                                                as.numeric(which(abs(residuals_list[[5]]) > 2)) 
                                                )))



            #Model performances
            accuracy <-as.numeric(cm$overall["Accuracy"])
            sensitivity <- as.numeric(cm$byClass["Sensitivity"])
            specificity <- as.numeric(cm$byClass["Specificity"])
            # auc <- as.numeric(auc(roc_obj))
            number_deviant_residuals<-number_deviant_residuals
            #predicted proportions vs true proportions
            diff_prop_placebo<-round((prop.table(table(imp$data[["Grupo"]], actuals_f), margin=1)[3] - prop.table(table(imp$data[["Grupo"]], preds_f),margin=1)[3])*100,2)
            diff_prop_traitement<-round((prop.table(table(imp$data[["Grupo"]], actuals_f), margin=1)[4] - prop.table(table(imp$data[["Grupo"]], preds_f),margin=1)[4])*100,2)

            list(accuracy = accuracy,
                sensitivity = sensitivity,
                specificity = specificity,
                # auc = auc,
                diff_prop_placebo =diff_prop_placebo,
                diff_prop_traitement = diff_prop_traitement,
                number_deviant_residuals=number_deviant_residuals)
        }

        results_list <- map(vars_to_explain, function(var){
        # formula for first regression: outcome ~ Grupo
        if (is.null(interraction_terms)){
            fit1 <- with(imp, glm(as.formula(paste(var, "~", main_var)), family = binomial))
            perf1 <- extract_performance(fit1,var)
            if (!is.null(other_variables)){
                fit2 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var, other_variables), collapse = " + "))), family = binomial))
                perf2 <- extract_performance(fit2,var)
                data.frame(
                    Variable_to_explain = var,
                    accuracy = perf1$accuracy,
                    sensitivity = perf1$sensitivity,
                    specificity = perf1$specificity,
                    diff_prop_placebo = perf1$diff_prop_placebo,
                    diff_prop_traitement = perf1$diff_prop_traitement,
                    # auc = perf1$auc,
                    number_deviant_residuals= perf1$number_deviant_residuals,
                    accuracy_adjusted = perf2$accuracy,
                    sensitivity_adjusted = perf2$sensitivity,
                    specificity_adjusted = perf2$specificity,
                    diff_prop_placebo_adjusted = perf2$diff_prop_placebo,
                    diff_prop_traitement_adjusted = perf2$diff_prop_traitement,
                    # auc_adjusted = perf2$auc,
                    number_deviant_residuals_adjusted= perf2$number_deviant_residuals,
                    stringsAsFactors = FALSE
                )
            } else{
                data.frame(
                    Variable_to_explain = var,
                    accuracy = perf1$accuracy,
                    sensitivity = perf1$sensitivity,
                    specificity = perf1$specificity,
                    diff_prop_placebo = perf1$diff_prop_placebo,
                    diff_prop_traitement = perf1$diff_prop_traitement,
                    # auc = perf1$auc,
                    number_deviant_residuals= perf1$number_deviant_residuals,
                    stringsAsFactors = FALSE
                )
            }}
        else{
            interraction <-list()
            for (inter in interraction_terms){
                interraction[[length(interraction)+1]] <-paste(main_var,":", inter)
            }
            fit1 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var,interraction,interraction_terms ), collapse = " + "))), family = binomial))
            perf1 <- extract_performance(fit1,var)
            if (!is.null(other_variables)){
                fit2 <- with(imp, glm(as.formula(paste(var, "~", paste(c(main_var,interraction, interraction_terms, other_variables), collapse = " + "))), family = binomial))
                perf2 <- extract_performance(fit2,var)
                data.frame(
                    Variable_to_explain = var,
                    accuracy = perf1$accuracy,
                    sensitivity = perf1$sensitivity,
                    specificity = perf1$specificity,
                    diff_prop_placebo = perf1$diff_prop_placebo,
                    diff_prop_traitement = perf1$diff_prop_traitement,
                    # auc = perf1$auc,
                    number_deviant_residuals= perf1$number_deviant_residuals,
                    accuracy_adjusted = perf2$accuracy,
                    sensitivity_adjusted = perf2$sensitivity,
                    specificity_adjusted = perf2$specificity,
                    diff_prop_placebo_adjusted = perf2$diff_prop_placebo,
                    diff_prop_traitement_adjusted = perf2$diff_prop_traitement,
                    # auc_adjusted = perf2$auc,
                    number_deviant_residuals_adjusted= perf2$number_deviant_residuals,
                    stringsAsFactors = FALSE
                )
            } else{
                data.frame(
                    Variable_to_explain = var,
                    accuracy = perf1$accuracy,
                    sensitivity = perf1$sensitivity,
                    specificity = perf1$specificity,
                    diff_prop_placebo = perf1$diff_prop_placebo,
                    diff_prop_traitement = perf1$diff_prop_traitement,
                    # auc = perf1$auc,
                    number_deviant_residuals= perf1$number_deviant_residuals,
                    stringsAsFactors = FALSE
                )
            }}    

        })
        # Combine all into one dataframe
        results_df <- bind_rows(results_list)
        return(t(results_df))
    }



  )
)

####################################### Cox model ##############################################################
library(survival)
library(survminer)
cox <- setRefClass(
    #' A class of methods to perform logistic regression 
    #' @params A dataframe and a list of binary variables to explain
  "cox",
  fields = list(
    dataset = "mids",
    time_event_pairs = "list"
  ),
  methods = list(
    initialize = function(dataset, time_event_pairs) {
      imp_data <<- dataset
      time_event_pairs <<- time_event_pairs
    },

    get_cox_results = function(main_var="Grupo", other_variables=NULL,interraction_terms=NULL) {
        # Helper to extract HR, CI, p-value
        extract_hr_stats <- function(pooled_fit, row_index) {
            tidy_fit <- tidy(pooled_fit, conf.int = TRUE, exponentiate = TRUE)
            row <- tidy_fit[row_index,]
    
            if (nrow(row) == 0) {
                term <- NA
                hr_ci <- NA
                p_val <- NA
            } else {
                term <- row$term
                hr <- round(row$estimate, 2)
                ci_lower <- round(row$conf.low, 2)
                ci_upper <- round(row$conf.high, 2)
                p_val <- row$p.value
                hr_ci <- paste0(hr, " (", ci_lower, " - ", ci_upper, ")")
            }
    
            list(term = term,hr_ci = hr_ci, p_val = p_val)
        }
  
        # Main mapping function
        results_list <- map(time_event_pairs, function(time_event_pair) {
        time_var <- time_event_pair[[1]]
        event_var <- time_event_pair[[2]]

        # outcome ~ Grupo
        if (is.null(interraction_terms)){
            events <- paste(event_var, "x", levels(as.factor(data_dint[[main_var]]))[-1])
            times  <- rep(time_var, length(events))
            # Fit Cox models
            fit1 <- with(imp_data, coxph(as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~", main_var))))
            pool1 <- pool(fit1)
            rows_to_extract <- 1:(nlevels(factor(data_dint[[main_var]]))-1)
            stats1 <- extract_hr_stats(pool1,rows_to_extract)
            # Perform PH test on the first model (unadjusted)
            fit_complete <- complete(imp_data, action = 1)
            surv_obj <- Surv(fit_complete[[time_var]], fit_complete[[event_var]])
            ph_model <- coxph(as.formula(paste0("surv_obj ~ ",main_var)), data = fit_complete)
            ph_test <- cox.zph(ph_model)
            ph_p <- ph_test$table[main_var, "p"]
            verdict <- ifelse(ph_p < 0.05, "PH assumption violated", "PH assumption met")
            if (!is.null(other_variables)){
                fit2 <- with(imp_data, coxph(as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~", paste(c(main_var, other_variables), collapse = " + ")))))
                pool2 <- pool(fit2)
                stats2 <- extract_hr_stats(pool2,rows_to_extract)
                data.frame(
                    Time = times,
                    Event = events,
                    HR_CI_not_adjusted = stats1$hr_ci,
                    p_value_1 = stats1$p_val,
                    HR_CI_adjusted = stats2$hr_ci,
                    p_value_2 = stats2$p_val,
                    ph_test_p_value = round(ph_p, 3),
                    ph_test_verdict = verdict,
                    stringsAsFactors = FALSE
                )
            } else{
                data.frame(
                    Time = times,
                    Event = events,
                    HR_CI = stats1$hr_ci,
                    p_value = stats1$p_val,
                    ph_test_p_value = round(ph_p, 3),
                    ph_test_verdict = verdict,
                    stringsAsFactors = FALSE
                )
                }
        }else{
            interraction <-list()
                for (inter in interraction_terms){
                    interraction[[length(interraction)+1]] <-paste(main_var,":", inter)
                }
            paste(event_var, "x", levels(as.factor(data_dint[[main_var]]))[-1])

            
            events <- c(paste(event_var, "x", levels(as.factor(data_dint[[main_var]]))[-1]),
                        paste(event_var, "x", unlist(sapply(data_dint[interraction_terms], function(x) levels(as.factor(x))[-1]))),
                        paste(event_var, "x", 
                                                paste( unlist(sapply(data_dint[interraction_terms], function(x) levels(as.factor(x))[-1])), ":" ,
                                                unlist(sapply(data_dint[main_var], function(x) levels(as.factor(x))[-1]))
                                                ))
                        )
            times  <- rep(time_var, length(events))
            # Fit Cox models
            fit1 <- with(imp_data, coxph(as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~", paste(c(main_var, interraction_terms, interraction), collapse = " + ")))))
            pool1 <- pool(fit1)
            #number of rows for main_var and interraction terms
            n_main<-nlevels(factor(data_dint[[main_var]])) +sum(sapply(data_dint[interraction_terms], function(x) nlevels(as.factor(x))))-(length(interraction_terms)+1)
            #number of rows for interraction
            n_inter_terms<-sum(sapply(data_dint[interraction_terms], function(x) (nlevels(as.factor(data_dint[[main_var]]))-1)*(nlevels(as.factor(x))-1) ))
            #total number of rows
            nrows<-n_main+n_inter_terms
            rows_to_extract<-c(1:nrows)
            stats1 <- extract_hr_stats(pool1,rows_to_extract)
            # Perform PH test on the first model (unadjusted)
            fit_complete <- complete(imp_data, action = 1)
            surv_obj <- Surv(fit_complete[[time_var]], fit_complete[[event_var]])
            ph_model <- coxph(as.formula(paste0("surv_obj ~ ",paste(c(main_var, interraction_terms, interraction), collapse = " + "))), data = fit_complete)
            ph_test <- cox.zph(ph_model)
            ph_p <- ph_test$table[main_var, "p"]
            verdict <- ifelse(ph_p < 0.05, "PH assumption violated", "PH assumption met")
            if (!is.null(other_variables)){
                #number of rows for other variables
                n_other<-sum(sapply(data_dint[other_variables], function(x) nlevels(as.factor(x))-1))
                #skip other variables
                n_inter<-n_main+n_other+1
                #total number of rows
                nrows<-n_main+n_inter_terms+n_other
                rows_to_extract<-c(1:n_main, n_inter:nrows)
                fit2 <- with(imp_data, coxph(as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~", paste(c(main_var,interraction_terms, interraction, other_variables), collapse = " + ")))))
                pool2 <- pool(fit2)
                stats2 <- extract_hr_stats(pool2,rows_to_extract)
                data.frame(
                    Time = times,
                    Event = events,
                    HR_CI_not_adjusted = stats1$hr_ci,
                    p_value_1 = stats1$p_val,
                    HR_CI_adjusted = stats2$hr_ci,
                    p_value_2 = stats2$p_val,
                    ph_test_p_value = rep(round(ph_p, 3), length(events)),
                    ph_test_verdict = rep(verdict, length(events)),
                    stringsAsFactors = FALSE
                )
            } else{
                data.frame(
                    Time = times,
                    Event = events,
                    HR_CI = stats1$hr_ci,
                    p_value = stats1$p_val,
                    ph_test_p_value = rep(round(ph_p, 3), length(events)),
                    ph_test_verdict = rep(verdict, length(events)),
                    stringsAsFactors = FALSE
                )
                }}

    # martingale_resid <- residuals(pool2, type = "martingale")
    # print(var)
    # print(plot(imp_data$data[["Grupo"]], martingale_resid))
     })
    results<-bind_rows(results_list)
    return(results)
    }
  )
)