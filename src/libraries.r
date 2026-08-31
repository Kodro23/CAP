#Intall packages
install_packages<-function(path_to_requirement){
    packages <- readLines(paste(path_to_requirement))
    install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, repos= "https://cloud.r-project.org", dependencies = TRUE)
    }
    }
    invisible(lapply(packages, install_if_missing))
    install.packages(c("recipes", "hardhat", "sparsevctrs","sjmisc","survminer","knitr","markdown","performance","glmmTMB"),repos= "https://cloud.r-project.org", type = "binary")
}

