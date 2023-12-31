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
     # add column for any preop or intraop antibiotics
data <- data %>% mutate(pre_intra_antibio = if_else(preop_antibio=='Y'& intraop_antibio=='Y', "Y", "N"),
                        .after = 17)
     # add column for pen/gent vs any other combination
data <- data %>% mutate(pen_gent_alone = if_else(preop_antibio_type=='pen gent'& intraop_antibio_type=='pen gent' & postop_antibio_addnl=='N', "Y", "N"),
                        .after = 18)
     # add column for preop antibiotics - pen/gent vs other
data <- data %>% mutate(preop_antibio_type_short = if_else(preop_antibio_type == 'pen gent', "pen gent", "other"),
                        .after = "preop_antibio_type")
     # add column for intraop antibiotics - pen gent vs other
data <- data %>% mutate(intraop_antibio_type_short = if_else(intraop_antibio_type == 'pen gent', "pen gent", "other"),
                        .after = "intraop_antibio_type")

     # summarize data
summary_table <- Hmisc::describe(data[,-1])    # The -1 so date is removed
summary_table

     # Model everything together
model_all <- logistf(incis_infect ~ enterot + bowel_resect + preop_antibio + intraop_antibio +
                       postop_antibio_days + anes_time + recov_time + recov_qual, data = data)
summary(model_all)
drop1(model_all, data = data)
model_all_step1 <- update(model_all, .~. - postop_antibio_days)
  drop1(model_all_step1, data = data)
model_all_step2 <- update(model_all_step1, .~. - bowel_resect)
  drop1(model_all_step2, data = data)
model_all_step3 <- update(model_all_step2, .~. - anes_time)
  drop1(model_all_step3)
model_all_step4 <- update(model_all_step3, .~. - intraop_antibio)
  drop1(model_all_step4)
model_all_step5 <- update(model_all_step4, .~. - recov_qual)
  drop1(model_all_step5)

     # Tried backwards elimination, everything was removed from model...

     #  model antibiotic combinations of interest
     # compare benefit of pre-op or intra-op antibiotics yes/no
antibio_model_a <- logistf(incis_infect ~ preop_antibio + intraop_antibio, data = data)
antibio_model_b <- logistf(postop_reflux ~ preop_antibio + intraop_antibio, data = data)
antibio_model_c <- logistf(other_comp ~ preop_antibio + intraop_antibio, data = data)
ms1 <- modelsummary(list(antibio_model_a, antibio_model_b, antibio_model_c), statistic = NULL,
             estimate = "{estimate}, [{conf.low}, {conf.high}]",
             coef_omit = 1,
             exponentiate = TRUE,
             output = "data.frame")

     # Compare pre- and intra- pen/gent vs others
antibio_model1a <- logistf(incis_infect ~ preop_antibio_type_short,
                          data = data)
summary(antibio_model1a)
antibio_model1b <- logistf(postop_reflux ~ preop_antibio_type_short,
                           data = data)
antibio_model1c <- logistf(other_comp ~ preop_antibio_type_short,
                           data = data)
antibio_model1d <- logistf(incis_infect ~ intraop_antibio_type_short,
                           data = data)
antibio_model1e <- logistf(postop_reflux ~ intraop_antibio_type_short,
                           data = data)
antibio_model1f <- logistf(other_comp ~ intraop_antibio_type_short,
                           data = data)
ms2 <- modelsummary(list(antibio_model1a, antibio_model1b, antibio_model1c,
                         antibio_model1d, antibio_model1e, antibio_model1f),
                    statistic = NULL,
                    estimate = "{estimate}, [{conf.low}, {conf.high}]",
                    coef_omit = 1,
                    exponentiate = TRUE,
                    output = "data.frame")

     # does giving antibiotics pre- or intra- confer a benefit overall?
antibio_model2a <- logistf(incis_infect ~ pre_intra_antibio, data = data)
summary(antibio_model2a)
antibio_model2b <- logistf(postop_reflux ~ pre_intra_antibio, data = data)
antibio_model2c <- logistf(other_comp ~ pre_intra_antibio, data = data)
ms3 <- modelsummary(list(antibio_model2a, antibio_model2b, antibio_model2c),
                    statistic = NULL,
                    estimate = "{estimate}, [{conf.low}, {conf.high}]",
                    coef_omit = 1,
                    exponentiate = TRUE,
                    output = "data.frame")

     # does pen/gent alone confer a benefit?
antibio_model3a <- logistf(incis_infect ~ pen_gent_alone, data = data)
summary(antibio_model3)
antibio_model3b <- logistf(postop_reflux ~ pen_gent_alone, data = data)
antibio_model3c <- logistf(other_comp ~ pen_gent_alone, data = data)
ms4 <- modelsummary(list(antibio_model3a, antibio_model3b, antibio_model3c),
                    statistic = NULL,
                    estimate = "{estimate}, [{conf.low}, {conf.high}]",
                    coef_omit = 1,
                    exponentiate = TRUE,
                    output = "data.frame")
str(ms4) # need to reverse the outcomes - 1/OR to model use of other antibiotics vs. just pen/gent

#####################################
     # Model other individual components that are of interest
model1 <- logistf(incis_infect ~ enterot + bowel_resect,
                  data = data)
summary(model1)

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
     # Filter out the 'boarding' patients to normalize the data
hosp_data <- data %>% filter(days_hosp < 16)
ggplot(data = data, aes(x=days_hosp)) + 
  geom_histogram(bins = 50)
summary(hosp_data$days_hosp)
     # Does this model say anything different?
model8a <- logistf(surv_dismis ~ days_hosp, data = hosp_data)
summary(model8a)
exp(model8a$coefficients)
     # not really different - will keep the original analysis

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