# source helper functions
source("code/helper-functions.R")

# load all data and model outputs
stan_plot <- readRDS("outputs/stan_predictions.rds")
ctree_plot <- readRDS("outputs/ctree_predictions.rds")
sp_data <- readRDS("data/full-data-set.rds")
mod_stan <- readRDS("outputs/mod_stan.rds")
pd_regress <- readRDS("outputs/pd_regress.rds")
pd_class <- readRDS("outputs/pd_class.rds")

# plot Fig. 1: residuals from fitted models
pdf(file = "outputs/figs/Fig1.pdf")
par(mfrow = c(2, 1), mar = c(5.1, 5.1, 2.4, 1.1))
plot(stan_plot ~ sp_data$tl,
     ylim = c(2, 4.6), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic level",
     ylab = "Modelled trophic level",
     pch = 16, col = ggplot2::alpha("black", 0.5))
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Linear model", side = 3, line = 1, adj = 1, cex = 1.25)
plot(ctree_plot ~ sp_data$tl,
     ylim = c(2, 4.6), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic level",
     ylab = "Modelled trophic level",
     pch = 16, col = ggplot2::alpha("black", 0.5))
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Random forest", side = 3, line = 1, adj = 1, cex = 1.25)
dev.off()

# plot Figs 2-4: estimated coefficients from stan model
# 1: coefs for continuous preds
pdf(file = "outputs/figs/Fig2.pdf")
plot(mod_stan, regex_pars = c("edhd", "mobd", "jlhd", "cfdcpd", "invasive", "fresh"),
     prob = 0.8, prob_outer = 0.95)
dev.off()

# 2: orders
pdf(file = "outputs/figs/Fig3.pdf")
par(mfrow = c(1, 2))
plot(mod_stan, regex_pars = c(") ord"),
     prob = 0.8, prob_outer = 0.95)
dev.off()

# 3: length effects by order
pdf(file = "outputs/figs/Fig4.pdf")
plot(mod_stan, regex_pars = c("len ord"),
     prob = 0.8, prob_outer = 0.95)
dev.off()

# plot Fig. 5: partial regression coefficients
var_list <- c("len", "edhd", "mobd", "jlhd", "cfdcpd")
var_names <- c("Body length", "EdHd", "MoBd", "JlHd", "CFdCPd")
pdf(file = "outputs/figs/Fig5.pdf")
par(mfrow = c(3, 2))
for (i in seq_along(var_list)) {
  pd_plot(pd_regress[[i]], xlab = var_names[i], ylab = "Trophic level",
          mean = attributes(sp_data[, var_list[i]])$`scaled:center`,
          sd = attributes(sp_data[, var_list[i]])$`scaled:scale`)
  mtext(var_names[i], side = 3, line = 1, adj = 1, cex = 1.25)
}
dev.off()

pdf(file = "outputs/figs/Fig6.pdf")
par(mfrow = c(3, 2))
for (i in seq_along(var_list)) {
  pd_plot(pd_class[[i]], xlab = var_names[i], ylab = "Trophic level",
          mean = attributes(sp_data[, var_list[i]])$`scaled:center`,
          sd = attributes(sp_data[, var_list[i]])$`scaled:scale`)
  mtext(var_names[i], side = 3, line = 1, adj = 1, cex = 1.25)
}
plot(c(0, 1), c(0, 1), type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
col_pal <- RColorBrewer::brewer.pal(4, "Set2")
legend(0.5, 0.5,
       legend = c("Herbivore/Detritivore", "Omnivore", "Secondary consumer", "Top predator"),
       col = col_pal, lty = 1, lwd = 2, bty = "n",
       xjust = 0.5, yjust = 0.5, xpd = TRUE)
dev.off()