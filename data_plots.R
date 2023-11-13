
library(forestplot)
     #will need to put tables from main analysis into global environment prior
     # Figure 1
plot_data <- bind_rows(or.table3, or.table, or.table2)
plot_data$predictor <- c('# Days NSAIDs administered postop', 'Postop IV lidocaine administration', 'Postop alpha-2 agonist administration',
                      'Good recovery', 'Fair recovery', 'Poor recovery', 'Enterotomy performed',
                      'Additional antibiotics administered postop', 'Multiple NSAIDs administered (beyond 1)')

plot_data %<>% add_row(predictor = 'Incidence of Incisional Infection', .before = 1) %>%
  add_row(predictor = 'Postoperative Ileus', .before = 3) %>%
  add_row(predictor = 'Other Complications - Anesthetic Predictors', .before = 6) %>%
  add_row(predictor = "Excellent Recovery", .before = 7) %>%
  add_row(predictor = 'Other Complications - Non-anesthetic Predictors', .before = 11)

plot_data %<>% 
  add_column(odds_ratio_num = c('Odds Ratio', '1.14', NA, '21.5', '1.56',
                                NA, 'referrent', '2.01', '2.58', '4.69', NA,
                                '0.64', '3.63', '1.46'), .after = 1) %>%
  add_column(odds_ratio_ci = c('95% CI', '1.07-1.23', NA, '5.83-190.62', '1.04-2.34',
                               NA, NA, '0.787-10.74',
                               '0.63-8.26', '1.34-20.49', NA,
                               '0.45-0.91', '2.55-5.20', '1.04-2.06'), .after = 2)

png("figure1.png", units = "in", width = 8, height = 3, res = 300)
plot_data |>
  forestplot(labeltext = c('predictor','odds_ratio_num', 'odds_ratio_ci'),
             mean = or,
             lower = or.lower,
             upper = or.upper,
             zero = 1,
             is.summary = c(T, F, T, F, F, T, rep(F, 4), T, rep(F, 3)),
             clip = c(-Inf,30),
             lineheight = "auto",
             xlab = "Odds Ratio",
             hrzl_lines = list(NULL, gpar(columns = c(1:3)),
                               NULL, gpar(columns = c(1:3)),
                               NULL, NULL, gpar(columns = c(1:3)),
                               NULL, NULL, NULL, NULL, gpar(columns = c(1:3)),
                               NULL, NULL, NULL),
             align = c('l','l','l'),
             fn.ci_norm = fpDrawDiamondCI,
             boxsize = 0.2,
             vertices = TRUE,
             title = "Figure 1 - Forest plot of factors associated with equine celiotomies",
             txt_gp = fpTxtGp(cex=0.5))
dev.off()

     # Figure 2
plot2_data <- tribble(
  ~Outcome, ~or, ~or.lower, ~or.upper,
  #########
  'Incisional Infection', 0.819, 0.381, 1.664,
  'Postoperative Ileus', 1.137, 0.736, 1.741,
  'Other Complications', 0.918, 0.639, 1.315,
  'Incisional Infection', 1.242, 0.600, 2.474,
  'Postoperative Ileus', 1.597, 1.033, 2.453,
  'Other Complications', 1.788, 1.237, 2.590,
  'Incisional Infection', 2.570, 0.312, 334.547,
  'Postoperative Ileus', 0.922, 0.326, 3.138,
  'Other Complications', 0.747, 0.294, 1.962,
  'Incisional Infection', 1.120, 0.362, 3.384,
  'Postoperative Ileus', 0.721, 0.356, 1.429,
  'Other Complications', 0.607, 0.329, 1.114,
  'Incisional Infection', 0.269, 0.002, 1.996,
  'Postoperative Ileus', 1.884, 0.772, 4.269,
  'Other Complications', 2.323, 1.053, 5.327,
  'Incisional Infection', 1.004, 0.103, 142.857,
  'Postoperative Ileus', 3.663, 0.385, 500,
  'Other Complications', 2.967, 0.481, 31.25
)
head(plot2_data, 20)
plot2_data %<>% mutate(odds_ratio_num = as.character(or))
plot2_data %<>% unite(odds_ratio_ci, or.lower, or.upper, sep = ' - ', remove = FALSE)
plot2_data %<>% add_column(question = NA, .before = 1) %>%
  add_row(question = 'A: Pre- or intraoperative antibiotic administration vs. none?',
                        odds_ratio_num = 'Odds Ratio',
                        odds_ratio_ci = '95% CI',
                        .before = 1) %>%
  add_row(question = 'B: Penicillin/Gentamicin vs. any other combination?',
          .before = 8) %>%
  add_row(question = 'C: Administering any antibiotic pre- or intraoperatively vs. none?',
          .before = 15) %>%
  add_row(question = 'D: Administering any antibiotic combination other than Penicillin/Gentamicin?',
          .before = 19)
plot2_data$question[c(2, 9)] <- 'preoperative administration'
plot2_data$question[c(5, 12)] <- 'intraoperative administration'

png("figure2.png", units = "in", width = 8, height = 4, res = 300)
plot2_data %>%
  forestplot(labeltext = c('question', 'Outcome','odds_ratio_num', 'odds_ratio_ci'),
             mean = or,
             lower = or.lower,
             upper = or.upper,
             zero = 1,
             is.summary = c(T, rep(F, 6), T, rep(F, 6), T, rep(F, 3), T, rep(F, 3)),
             clip = c(-Inf,10),
             lineheight = "auto",
             xlab = "Odds Ratio",
             hrzl_lines = list(NULL, gpar(columns = c(1:4)),
                               NULL, NULL, gpar(columns = c(1:4)),
                               NULL, NULL, NULL, gpar(columns = c(1:4)),
                               NULL, NULL, gpar(columns = c(1:4)),
                               NULL, NULL, NULL, gpar(columns = c(1:4)),
                               NULL, NULL, NULL, gpar(columns = c(1:4)),
                               NULL, NULL, NULL),
            # align = c('l','l','l'),
             fn.ci_norm = fpDrawDiamondCI,
             boxsize = 0.2,
             vertices = TRUE,
             title = "Figure 2 - Forest plot of antibiotic factors associated with varioius outcomes after equine celiotomies",
             txt_gp = fpTxtGp(cex=0.4),
            colgap = unit(3, 'mm'))
dev.off()


####### Not used ########
forestdata <- structure(list(
  mean = c(NA, 0.143, 71.787, 0.097),
  lower = c(NA, 0.001, 3.039, 0.001),
  upper = c(NA, 6.531, 6392.058, 3.167)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -4L), 
  class = "data.frame")
forestdata

foresttext <- cbind(
  c("Factor", "Duration of Antimicrobial Use < 3 days",
    "Post-Operative Regional Limb Perfusion",
    "Duration of Anesthesia > 3 hrs"),
  c(rep(NA, 4)),
  c("OR", "0.143", "71.787", "0.097"),
  c("95% CI", "(0.001 - 6.531)", "(3.039 - 6392.058)", "(0.001 - 3.167)"))

png("figureX", units = "in", width = 10, height = 3, res = 300)
forestplot(foresttext,
           is.summary = c(TRUE, rep(FALSE,3)),
           forestdata,
           xlog = FALSE,
           xlab = "Odds Ratio for Post-Operative Infection",
           clip = c(0.001, 80),
           hrzl_lines = list("2" = gpar(lty=2)),
           vertices = TRUE,
           graph.pos = 3,
           boxsize = 0.1,
           zero = 1,
           xticks = c(0,1,20,40,60,80),
           lineheight = unit(1.2, "cm"))
dev.off()