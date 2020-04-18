package_list <- c(
  "tidyverse",
  "Rlab",
  "Matrix",
  "ggplot2",
  "yaml",
  "doParallel", 
  "digest"
)


#install dependencies
if(FALSE){
  install.packages(package_list)
}

lapply(package_list, function(x) library(x, character.only = T))

rm(package_list)
