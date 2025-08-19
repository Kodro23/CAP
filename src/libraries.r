#Intall packages
install_packages<-function(path_to_requirement){
    packages <- readLines(paste(path_to_requirement, "requirements.txt"))
    install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
    }
    invisible(lapply(packages, install_if_missing))
    install.packages(c("recipes", "hardhat", "sparsevctrs","sjmisc","survminer","knitr","markdown",), type = "binary")
}
# Required packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(DescTools)
library(purrr)
library(mice)
library(haven)
library(tidyverse)
library(dplyr)
library(tidyr)
library(writexl)
library(naniar)
library(ggpubr)
library(openxlsx)
library(labelled)
library(lava)
library(recipes)
library(caret)
library(broom)
library(broom.helpers)
library(nnet)
library(gamlss)
library(MASS)
library(brant)
library(lubridate)
library(patchwork) 
library(maxLik)
library(pracma)
library(rjags)
library(coda)
library(lattice)
