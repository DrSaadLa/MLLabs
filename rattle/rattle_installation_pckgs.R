# Install the following packages 

required_packages <- c("tibble", "bitops", "magrittr", "stringi", "Hmisc", "R6",
                       "scales", "lazyeval", "ggplot2", "corrplot", "RGtk2",
                       "cairoDevice", "rattle", "tidyselect", "doBy", "ellipse", 
           "fpc", "randomForest", "DAAG", "arules", "mlbench", "amap", 
           "ggdendro")
installed_packages <- required_packages %in% rownames(installed.packages())

# Install missing packages

if (any(installed_packages == FALSE)){
  install.packages(required_packages[!installed_packages], dependencies = TRUE)
} else{
  message("All the packages are installed")
  message("You are good to Go, and happy learning!")
}


# Download GGobi and then install.

browseURL("http://ggobi.org/downloads/")