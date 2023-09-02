
library(forestplot)
     #will need to put tables from main analysis into global environment prior
plot_data <- bind_rows(or.table3, or.table, or.table2)
plot_data$predictor <- c('# Days NSAIDs administered postop', 'Postop IV lidocaine administration', 'Postop alpha-2 agonist administration',
                      'Good recovery', 'Fair recovery', 'Poor recovery', 'Enterotomy performed',
                      'Additional antibiotics administered postop', 'Multiple NSAIDs administered (beyond 1)')

plot_data %<>% add_row(predictor = 'Incidence of Incisional Infection', .before = 1) %>%
  add_row(predictor = 'Postoperative Reflux', .before = 3) %>%
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