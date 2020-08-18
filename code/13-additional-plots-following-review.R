# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("outputs/fitted-models-body-mass-rm-low-tp.RData")

# load some helper functions
source("code/helpers.R")

# plot Fig. 7: residuals from fitted models
stan_predictions <- posterior_predict(mod_stan1)
stan_plot <- apply(stan_predictions, 2, median)
ctree_plot <- predict(mod_ctree1)
jpeg(file = "outputs/figs/Fig7-mass-rm-low-tp.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(2, 1), mar = c(5.1, 5.1, 2.4, 1.1))
to_plot1 <- 10 ^ stan_plot
xplot <- 10 ^ sp_data$tl
plot(to_plot1 ~ xplot,
     ylim = c(2, 5.0), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic position",
     ylab = "Modelled trophic position",
     pch = 16, col = scales::alpha("black", 0.5))
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Linear model", side = 3, line = 1, adj = 1, cex = 1.25)
to_plot2 <- 10 ^ ctree_plot
plot(to_plot2 ~ xplot,
     ylim = c(2, 5.0), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic position",
     ylab = "Modelled trophic position",
     pch = 16, col = scales::alpha("black", 0.5))
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Random forest", side = 3, line = 1, adj = 1, cex = 1.25)
dev.off()
