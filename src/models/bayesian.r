library(rjags)
library(coda)
library(lattice)


# Define BayesianModel class
BayesianModel <- setRefClass(
  "BayesianModel",
  fields = list(
    data = "data.frame",
    Y = "character",
    X = "character",
    modelString = "character",
    jags_data = "list"
  ),
  methods = list(

    initialize = function(data, Y, X, modelString) {
      data <<- data
      Y <<- Y
      X <<- X
      modelString <<- modelString

      # Create data list for JAGS
      jags_data <<- list(
        Y = data[[Y]],
        X = data[[X]],
        N = nrow(data)
      )
    },

    model = function(nchains = 3, nadapt = 1000, burnin = 5000) {
      mod <- jags.model(
        file = textConnection(modelString),
        data = jags_data,
        n.chains = nchains,
        n.adapt = nadapt
      )
      update(mod, burnin)
      return(mod)
    },

    sample = function(model, variable_names, niter = 10000, thin = 15) {
      #' To retrieve generated samples from the model
      #' @param model the model, 
      #' @param variable_names the list of variables to display in the sample
      #' @return the sample
      samp <- coda.samples(
        model = model,
        variable.names = variable_names,
        n.iter = niter,
        thin = thin
      )
      return(samp)
    },
    likelihood = function(model, likelihood_name, niter = 10000, thin = 15) {
      #' To retrieve generated samples from the model
      #' @param model the model, 
      #' @param likelihood_name name of your likelihood function specified in "modelstring"
      #' @return the sample
      samp <- coda.samples(
        model = model,
        variable.names = c(likelihood_name),
        n.iter = niter,
        thin = thin
      )
      return(samp)
    },
    graphs = function(sample, likelihood = NULL, type_of_graph = "traceplot", apriori = NULL) {
      #' To display some graphs for model diagnostic
      #' @param type_of_graph character in the list ["traceplot", "autocorr", "density","comparison"]
      #' @param sample the MCMC sample
      #' @param apriori list of prior distributions for parameters: list(c("param1", prior1), c("param2", prior2), ...)
      #' @param likelihood log-likelihood samples (optional, used in "comparison")
      
      if (type_of_graph == "traceplot") {
        traceplot(sample)
        } 
      else if (type_of_graph == "autocorr") {
        autocorr.plot(sample)
        } 
      else if (type_of_graph == "density") {
        densityplot(sample)
        } 
      else if (type_of_graph == "comparison") {
        samp_mat <- as.matrix(sample)
        for (l in apriori) {
          param_name <- l[[1]]
          prior_func <- l[[2]]
          dens <- density(samp_mat[, param_name])
      
          # Plot posterior
          plot(dens, main = paste("Posterior vs Prior", 
              if (!is.null(likelihood)) "vs Likelihood", 
                              ":", param_name),
              lwd = 2, col = "blue", xlab = param_name)
      
          # Add prior
          curve(prior_func(x), add = TRUE, col = "red", lwd = 2, lty = 2)
      
          # Add likelihood if provided
          if (!is.null(likelihood)) {
            lik_mat <- as.matrix(likelihood)
            lines(density(lik_mat), lwd = 2, col = "green")
            legend("topright", legend = c("Posterior", "Likelihood", "Prior"),col = c("blue", "green", "red"), lwd = 2, lty = c(1, 1, 2))
                  } 
          else {
            legend("topright", legend = c("Posterior", "Prior"),col = c("blue", "red"), lwd = 2, lty = c(1, 2))
            }
      }} 
      else {message("I do not understand your request")}
                        },

    metrics = function(sample, type_of_metric = "gelman", apriori = NULL) {
      #' To display some graphs for model diagnostic
      #' @param type_of_graph character in the list ["gelman", "autocorr", "effectivesize"]
      #' @param sample the MCMC sample
      
      if (type_of_metric == "gelman"){
        return(gelman.diag(sample))
      }
      else if (type_of_metric == "autocorr"){
        return(autocorr.diag(sample))
      }
      else if (type_of_metric == "effectivesize"){
        return(effectiveSize(sample))
      }
      else {message("I do not understand your request")}
      },

    summ = function(sample) {
      return(summary(sample))
    },

    predictions = function(model, variable_names = c("Y_pred"), niter = 10000, thin = 15,ylim=c(0, 1)) {
      samp_pred <- coda.samples(
      model = model,
      variable.names = variable_names,
      n.iter = niter,
      thin = thin
      )
      Y_pred_mean <- summary(samp_pred)$statistics[grep("Y_pred\\[", rownames(summary(samp_pred)$statistics)), "Mean"]
      patient_index <- 1:length(Y_pred_mean)
      Y_true <- data[[Y]]
      plot(patient_index, Y_true, col = "blue", pch=19,
       ylim = ylim, xlab = "Patient index", ylab = "Y",
       main = "Observed vs predicted")
       points(patient_index, Y_pred_mean,col = "red", pch = 17)
       legend("topright", legend = c("Observed vs predicted"),
       col = c("blue", "red"), pch = c(19, 17))
       }

  )
)
