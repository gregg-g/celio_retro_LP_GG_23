     #load packages
library(tidyverse)
library(magrittr)

     #import data
data <- read.csv('celio_retro_LP_GG_23.csv', 
                 nrows=608,
                 colClasses=c("character","integer","factor",
                                                          "numeric", "numeric", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "factor", "factor", "factor",
                                                          "numeric", "integer", "factor",
                                                          "integer", "integer", "factor",
                                                          "factor", "factor", "factor"
                                                          ),
                 na.strings=c("", " "))
data$date_pres <- as.Date(data$date_pres, format="%m/%d/%y")
str(data)
     # summarize data

     # Model everything together
model1 <- 
