     #load packages
library(tidyverse)
library(magrittr)
library(logistf)


     #import data
data <- read.csv('celio_retro_LP_GG_23.csv',
                 nrows=608,
                 colClasses=c("character","integer","factor",
                              "numeric", "numeric", rep("factor",11),
                              "numeric", "integer",
                              "factor", "integer", "integer",
                              "factor", "factor", "factor", "factor"),
                 na.strings=c("", " "))
data$date_pres <- as.Date(data$date_pres, format="%m/%d/%y")
str(data)

     # summarize data
summary_table <- Hmisc::describe(data[,-1])    # The -1 so date is removed
summary_table

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

model5a <- logistf(incis_infect ~ postop_nsaid_days, data = data)
summary(model5a)

or3 <- exp(model5a$coefficients)
or.lower3 <- exp(model5a$ci.lower)
or.upper3 <- exp(model5a$ci.upper)
or.table3 <- as_tibble(round(cbind (or3, or.lower3, or.upper3),3),
                      rownames = "predictor") %>%
  slice(-1) %>%
  rename('or'=or3, 'or.lower'=or.lower3, 'or.upper'=or.upper3)#tibble created for later plotting
or.table3


     # Try modeling reflux?
model6 <- logistf(postop_reflux ~ anes_time + bowel_resect + enterot + preop_antibio +
                    intraop_antibio + postop_antibio_days + postop_antibio_addnl +
                    postop_nsaid_num + postop_nsaid_days + postop_lido +
                    postop_alpha2 + postop_butor + postop_ket, data=data)
summary(model6)

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
     # Is there an interaction between lidocaine and alpha2s?
model6e <- logistf(postop_reflux ~ postop_lido*postop_alpha2, data = data)
summary(model6e)
     #Interaction is not significant, so will drop and use 6d

frequency_reflux <- ftable(reflux = data$postop_reflux, lidocaine = data$postop_lido, alpha2 = data$postop_alpha2)
frequency_reflux

frequency_reflux2 <- ftable(reflux = data$postop_reflux, lidocaine = data$postop_lido)
frequency_reflux2

or <- exp(model6d$coefficients)
or.lower <- exp(model6d$ci.lower)
or.upper <- exp(model6d$ci.upper)
or.table <- as_tibble(round(cbind (or, or.lower, or.upper),3),
                      rownames = "predictor") %>%
                       slice(-1)                    #tibble created for later plotting
or.table

other_comp_data <- data[,c(4:12, 14, 17:24)] %>% drop_na()

model7 <- logistf(other_comp ~ ., data = other_comp_data) 
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
     # some less significant predictors, can remove by hand or try automatic backward
     # selection with MASS and AIC

model.null.other <- logistf(other_comp ~ 1, data = other_comp_data) # I know, terrible syntax for naming
step.model.other <- MASS::stepAIC(model.null.other, scope = c(lower = model.null.other, upper = model7), trace = TRUE)
     # Just removes everything
model7f <- update(model7e, .~. - postop_ket - postop_alpha2)
summary(model7f)
     # All terms are now significant in the model
or2 <- exp(model7f$coefficients)
or.lower2 <- exp(model7f$ci.lower)
or.upper2 <- exp(model7f$ci.upper)
or.table2 <- round(cbind (or2, or.lower2, or.upper2),3) %>%
  as_tibble(rownames="predictor") %>%
  slice(-1) %>%
  slice(2,1,3:6) %>%
  rename('or'=or2, 'or.lower'=or.lower2, 'or.upper'=or.upper2)
or.table2

     # Is the number of days in the hospital predictive of dismissal?
model8 <- logistf(surv_dismis ~ days_hosp, data = data)
summary(model8)
     # The answer is 'possibly'...
or3 <- exp(model8$coefficients)
or.lower3 <- exp(model8$ci.lower)
or.upper3 <- exp(model8$ci.upper)
or.table3 <- round(cbind (or3, or.lower3, or.upper3),3) %>%
  as_tibble(rownames = "predictor") %>%
  slice(-1) %>%
  rename('or'=or3, 'or.lower'=or.lower3, 'or.upper'=or.upper3)
or.table3

surv_dismis_data <- data[,c(2:12, 14, 17:24)] %>% drop_na()
     # Is anything in the data predictive of survival to dismissal?
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

# added recurrent colic separately
rec.colic.data <- data[,c(7:8, 11, 13, 15, 18:25)] %>% drop_na()
model10 <- logistf(rec_colic ~ ., data = rec.colic.data)
summary(model10)
model.null.colic <- logistf(rec_colic ~ 1, data = rec.colic.data)
step.model.colic <- MASS::stepAIC(model10, scope = c(lower = model.null.colic, upper = model10),
                                  trace = TRUE, direction = "both")
#nothing is predictive in this model

model11 <- logistf(rec_colic ~ anes_time + recov_qual, data = data)
summary(model11)
model12 <- update(model11, .~. - recov_qual)
summary(model12)

or4 <- exp(model12$coefficients)
or.lower4 <- exp(model12$ci.lower)
or.upper4 <- exp(model12$ci.upper)
or.table4 <- round(cbind (or4, or.lower4, or.upper4),3) %>%
  as_tibble(rownames = "predictor") %>%
  slice(-1) %>%
  rename('or'=or4, 'or.lower'=or.lower4, 'or.upper'=or.upper4)
or.table4