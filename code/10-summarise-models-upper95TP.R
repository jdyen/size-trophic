# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("outputs/fitted-models-upper95tp.RData")

# load some helper functions
source("code/helpers.R")

# need to extract some correlations
naive_vals <- cbind(apply(naive_lm_pred1, 2, median),
                    apply(naive_lm_pred2, 2, median),
                    apply(naive_lm_pred3, 2, median),
                    predict(mod_ctree1),
                    predict(mod_ctree2),
                    predict(mod_ctree3))
r2_naive <- apply(naive_vals, 2, cor, sp_data$tl) ** 2
r2_lm_cv <- apply(predictions_lm, 2, cor, sp_data$tl) ** 2
r2_ml_cv1 <- apply(predictions, 2, cor, sp_data$tl) ** 2
r2_ml_cv2 <- apply(predictions_reduced, 2, cor, sp_data$tl) ** 2
r2_ml_cv3 <- apply(predictions_reduced2, 2, cor, sp_data$tl) ** 2

# pull out guild-specific predictions
r2_rf_guild_cv <- list()
r2_rf_guild <- r2_lm_guild <- r2_lm_guild_cv <- matrix(NA, nrow = 4, ncol = 3)
for (k in seq_along(guilds)) {
  
  r2_rf_guild_cv[[k]] <- matrix(NA, nrow = 3, ncol = 6)
  
  # create a data subset for this guild
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  
  # naive fit
  r2_rf_guild[k, ] <- apply(fitted_rf_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  r2_lm_guild[k, ] <- apply(fitted_lm_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  
  # holdout fit
  r2_lm_guild_cv[k, ] <- apply(predictions_lm_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  r2_rf_guild_cv[[k]][1, ] <- apply(predictions_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  r2_rf_guild_cv[[k]][2, ] <- apply(predictions_reduced_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  r2_rf_guild_cv[[k]][3, ] <- apply(predictions_reduced2_guilds[[k]], 2, cor, sp_guild$tl) ^ 2
  
}
rownames(r2_rf_guild) <- rownames(r2_lm_guild) <- rownames(r2_lm_guild_cv) <- names(r2_rf_guild_cv) <- guilds

# calculate r2 value for jlhd model
naive_jlhd <- posterior_predict(mod_jlhd)
naive_tmp <-  apply(naive_jlhd, 2, median)
r2_jlhd_naive <- cor(naive_tmp, sp_data$jlhd) ** 2
ctree_naive_jlhd <- predict(mod_ctree_jlhd)
r2_jlhd_ctree <- cor(ctree_naive_jlhd, sp_data$jlhd) ** 2

# calculate relative importance of all variables
regression_imp <- ctree_vimp$importance / sum(ctree_vimp$importance)
regression_imp2 <- ctree_vimp2$importance / sum(ctree_vimp2$importance)
regression_imp3 <- ctree_vimp3$importance / sum(ctree_vimp3$importance)
imp_all <- round(regression_imp, 2)
imp_all2 <- round(regression_imp2, 2)
imp_all3 <- round(regression_imp3, 2)
imp_combined <- imp_all[order(imp_all, decreasing = TRUE), ]
names(imp_combined) <- rownames(imp_all)[order(imp_all, decreasing = TRUE)]
imp_combined <- cbind(imp_combined, imp_all2[names(imp_combined), ])
imp_combined <- cbind(imp_combined, imp_all3[rownames(imp_combined), ])
colnames(imp_combined) <- c("full", "length", "length_in_order")
write.csv(imp_combined, file = "outputs/importance_estimates-upper95TP.csv")

# mean slope estimates between TP and length
mean_slopes_order <- apply(posterior_interval(mod_stan1, regex_pars = c("len ord"), prob = 0.01), 1, mean)
mean_slopes_ecoregion <- apply(posterior_interval(mod_stan1, regex_pars = c("len ecoregion"), prob = 0.01), 1, mean)
slopes_by_order <- data.frame(
  mean = mean_slopes_order,
  posterior_interval(mod_stan1, regex_pars = c("len ord"), prob = 0.8),
  posterior_interval(mod_stan1, regex_pars = c("len ord"), prob = 0.95)
)
slopes_by_ecoregion <- data.frame(
  mean = mean_slopes_ecoregion,
  posterior_interval(mod_stan1, regex_pars = c("len ecoregion"), prob = 0.8),
  posterior_interval(mod_stan1, regex_pars = c("len ecoregion"), prob = 0.95)
)

write.csv(round(slopes_by_order, 3), file = "outputs/slopes_by_order-upper95TP.csv")
write.csv(round(slopes_by_ecoregion, 3), file = "outputs/slopes_by_ecoregion-upper95TP.csv")
