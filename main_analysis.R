#load packages
library(tidyverse)
library(magrittr)

#import data
data <- read.csv('celio_retro_LP_GG_23.csv', 
                 nrows=608,
                 colClasses=c("Date","integer","factor",
                                                          "numeric", "numeric", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "numeric", "integer", "factor",
                                                          "integer", "integer", "factor",
                                                          "factor", "factor", "factor"
                                                          ))
str(data)
tail(data)
levels(data$surv_dismis)
