# plot jlhd model
# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("outputs/fitted-models.RData")

# load some helper functions
source("code/helpers.R")

# analyse JlHd as a function of length
# mod_jlhd <- stan_lmer(jlhd ~ (len | ord) +
#                         (len | ecoregion) +
#                         (len | fresh) +
#                         stream * len,
#                       data = sp_data,
#                       iter = 5000,
#                       cores = 1)

# Fig 1: length effects by order and ecoregion
jpeg(file = "outputs/figs/Fig1-jlhd.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(1, 2), mar = c(4.2, 8.2, 1.1, 1.1))
real_tp <- tapply(sp_data$jlhd, sp_data$ecoregion, identity)
real_tp <- lapply(real_tp, function(x) (-0.3449639 + 0.2334624 * x))
real_tp <- lapply(real_tp, function(x) 10 ^ x)
boxplot_fn(mod_jlhd, regex_pars = c(") ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion), xline = 2.5,
           ylab = "Ecoregion",
           xlab = "JlHd",
           offset = "(Intercept)",
           transform = TRUE,
           secondary_points = real_tp,
           rescale = list(int = -0.3449639, sd = 0.2334624))
real_tp <- tapply(sp_data$jlhd, sp_data$ord, identity)
real_tp <- lapply(real_tp, function(x) (-0.3449639 + 0.2334624 * x))
real_tp <- lapply(real_tp, function(x) 10 ^ x)
boxplot_fn(mod_jlhd, regex_pars = c(") ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 0.75,
           xline = 2.5,
           ylab = "Order",
           xlab = "JlHd",
           offset = "(Intercept)",
           transform = TRUE,
           secondary_points = real_tp,
           rescale = list(int = -0.3449639, sd = 0.2334624))
dev.off()
