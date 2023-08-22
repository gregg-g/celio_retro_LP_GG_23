     #load packages
library(tidyverse)
library(magrittr)
library(logistf)


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
model_all <- logistf(incis_infect ~ enterot + bowel_resect + preop_antibio + intraop_antibio +
                       anes_time + recov_time + recov_qual, data = data)
summary(model_all)
     # Tried backwards elimination, everything was removed from model...

     # Model individual components that are of interest
model1 <- logistf(incis_infect ~ enterot + bowel_resect,
                  data = data)
summary(model1)

model2 <- logistf(incis_infect ~ preop_antibio + intraop_antibio,
                  data = data)
summary(model2)

model3 <- logistf(incis_infect ~ intraop_antibio_type + intraop_antibio_time,
                  data = data)
summary(model3)

model4 <- logistf(incis_infect ~ anes_time + recov_time + recov_qual,
                  data = data)
summary(model4)

model5 <- logistf(incis_infect ~ postop_nsaid_num + postop_nsaid_days,
                  data = data)
summary(model5)

     # Try modeling reflux?
model6 <- logistf(postop_reflux ~ anes_time + bowel_resect + enterot + preop_antibio +
                    intraop_antibio + postop_antibio_days + postop_antibio_addnl +
                    postop_nsaid_num + postop_nsaid_days + postop_lido +
                    postop_alpha2 + postop_butor + postop_ket, data=mydata)
summary(model6)
mydata <- data
drop1(model6, data = data)
model6a <- update(model6, .~. - anes_time - postop_antibio_days - postop_nsaid_days -
                    postop_butor - postop_ket)
drop1(model6a, data = data)
model6b <- update(model6a, .~. - intraop_antibio - postop_nsaid_num)
drop1(model6b, data = data)
model6c <- update(model6b, .~. - bowel_resect - preop_antibio - postop_antibio_addnl)
summary(model6c)
model6d <- update(model6c, .~. - enterot)
summary(model6d)
log(model6d$coefficients)

frequency_reflux <- ftable(reflux_yes = data$postop_reflux, lidocaine = data$postop_lido, alpha2 = data$postop_alpha2)
frequency_reflux

or <- exp(model6d$coefficients)
or.lower <- exp(model6d$ci.lower)
or.upper <- exp(model6d$ci.upper)
or.table <- round(cbind (or, or.lower, or.upper),3)
or.table

model7 <- logistf(other_comp ~ anes_time + recov_time +
                    recov_qual + bowel_resect + enterot + preop_antibio +
                    intraop_antibio + postop_antibio_days + postop_antibio_addnl +
                    postop_nsaid_num + postop_nsaid_days + postop_lido +
                    postop_alpha2 + postop_butor + postop_ket, data=mydata)
summary(model7)
drop1(model7, data = data)
model7a <- update(model7, .~. - recov_quality - preop_antibio - postop_lido - postop_butor)
drop1(model7a, data = data)
model7b <- update(model7a, .~. - anes_time - postop_antibio_days)
drop1(model7b, data = data)
summary(model7b)
model7c <- update(model7b, .~. - recov_time - postop_nsaid_days)
drop1(model7c, data = data)
model7d <- update(model7c, .~. - intraop_antibio)
drop1(model7d, data = data)
model7e <- update(model7d, .~. - bowel_resect)
drop1(model7e, data = data)
summary(model7e)

or2 <- exp(model7e$coefficients)
or.lower2 <- exp(model7e$ci.lower)
or.upper2 <- exp(model7e$ci.upper)
or.table2 <- round(cbind (or2, or.lower2, or.upper2),3)
or.table2

model8 <- logistf(surv_dismis ~ days_hosp, data = data)
summary(model8)

or3 <- exp(model8$coefficients)
or.lower3 <- exp(model8$ci.lower)
or.upper3 <- exp(model8$ci.upper)
or.table3 <- round(cbind (or3, or.lower3, or.upper3),3)
or.table3

surv_dismis_data <- data[,c(2:12, 14, 17:24)] %>% drop_na()

model9 <- logistf(surv_dismis ~ .,
                  data = surv_dismis_data)
summary(model9)
drop1(model9, data = surv_dismis_data)

model9a <- logistf(surv_dismis ~ recov_time + enterot + other_comp, data = surv_dismis_data)
summary(model9a)
drop1(model9a, data = surv_dismis_data)
model9b <- update(model9a, .~. - recov_time)
summary(model9b)
model9c <- update(model9b, .~. - enterot)
summary(model9c) #too much separation in the data


model.null <- logistf(surv_dismis ~ 1, data = data)
step.model <- MASS::stepAIC(model9, scope = c(lower = model.null, upper = model9), trace = TRUE)
summary(step.model) #agrees - nothing here is significant on its own
