#load packages
library(tidyverse)
library(magrittr)

#import data
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") ) #not working?? why??
data <- read.csv('celio_retro_LP_GG_23.csv', 
                 nrows=608,
                 colClasses=c("myDate","integer","factor",
                                                          "numeric", "numeric", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "numeric", "integer", "factor",
                                                          "integer", "integer", "factor",
                                                          "factor", "factor", "factor"
                                                          ),
                 na.strings=c("", " "))
str(data)
model1 <- 
