# where am I working?
setwd("~/Dropbox/research/size-trophic/")

# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("fitted-mods.RData")

# plot Fig. 1: residuals from fitted models
stan_predictions <- posterior_predict(mod_stan1)
stan_plot <- apply(stan_predictions, 2, median)
ctree_plot <- predict(mod_ctree1)
jpeg(file = "outputs/figs/Fig1.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(2, 1), mar = c(5.1, 5.1, 2.4, 1.1))
to_plot1 <- 10 ^ stan_plot
xplot <- 10 ^ sp_data$tl
plot(to_plot1 ~ xplot,
     ylim = c(2, 5.0), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic position",
     ylab = "Modelled trophic position",
     pch = 16, col = scales::alpha("black", 0.5))
for (k in seq_along(guilds)) {
  yplot <- 10 ^ fitted_lm_guilds[[k]][, 1]
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  xplot_sub <- 10 ^ sp_guild$tl
  points(yplot ~ xplot_sub,
         pch = 16, col = scales::alpha("darkred", 0.3))
}
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Linear model", side = 3, line = 1, adj = 1, cex = 1.25)
to_plot2 <- 10 ^ ctree_plot
plot(to_plot2 ~ xplot,
     ylim = c(2, 5.0), xlim = c(2, 4.6),
     las = 1, bty = "l",
     xlab = "Observed trophic position",
     ylab = "Modelled trophic position",
     pch = 16, col = scales::alpha("black", 0.5))
for (k in seq_along(guilds)) {
  yplot <- 10 ^ fitted_rf_guilds[[k]][, 1]
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  xplot_sub <- 10 ^ sp_guild$tl
  points(yplot ~ xplot_sub,
         pch = 16, col = scales::alpha("darkred", 0.3))
}
lines(c(1, 5), c(1, 5), lty = 2, lwd = 2)
mtext("Random forest", side = 3, line = 1, adj = 1, cex = 1.25)
dev.off()

# plot Figs 2, 3, S1: estimated coefficients from stan model
pdf(file = "outputs/figs/Fig2.pdf")
par(mfrow = c(1, 1), mar = c(4.5, 8.2, 1.1, 1.1))
boxplot_fn(mod_stan1, regex_pars = c("edhd", "mobd", "jlhd", "cfdcpd", "fresh", "streamLonely", "streamSonly"),
           prob = 0.8, prob_outer = 0.95,
           labels = c("EdHd", "MoBd", "JlHd", "CFdCPd", "Freshwater", "Lake", "Stream"),
           xlab = "Coefficient",
           intercept = FALSE)
dev.off()

# Plot Fig. 3 with embedded pie charts
jpeg(file = "outputs/figs/Fig3.jpg", width = 8.5, height = 7, units = "in", res = 150)
idx <- order(prop_guilds[, 1], decreasing = TRUE)
nlevel <- length(levels(sp_data$ord))
layout(matrix(c(1, 1, 1, 2, 2, 2, 2), nrow = 1))
par(mar = c(5.5, 8.2, 1.6, 1.1))
boxplot_fn(mod_stan1, regex_pars = c("len ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion),
           ylab = "Ecoregion",
           intercept = FALSE)
boxplot_fn(mod_stan1, regex_pars = c("len ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 0.75,
           ylab = "Order", order = idx,
           intercept = FALSE,
           xlim = c(-0.2, 0.2))
col_pal <- viridis::inferno(4)
to_plot <- prop_guilds[idx, ]
guild_num_sorted <- guild_num[idx]
for (i in rev(seq_len(nlevel))) {
  floating.pie(xpos = -0.119, ypos = i, x = to_plot[30 - i + 1, ], col = col_pal[which(to_plot[30 - i + 1, ] > 0)], radius = 0.01)
  text(x = -0.18, y = i, paste0("n = ", guild_num_sorted[30 - i + 1]), xpd = TRUE, cex = 0.9)
}
legend(x = 0.129, y = 31, legend = c("Herb./detrit.", "Omni.", "Sec. cons.", "Top pred."),
       fill = col_pal, border = NULL, xpd = TRUE, bty = "n", horiz = FALSE,
       xjust = 0.5, cex = 1.0)
dev.off()

# Fig S1: length effects by order and ecoregion
pdf(file = "outputs/figs/FigS1.pdf", width = 8.5)
par(mfrow = c(1, 2), mar = c(4.2, 8.2, 1.1, 1.1))
boxplot_fn(mod_stan1, regex_pars = c(") ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion), xline = 2.5,
           ylab = "Ecoregion",
           xlab = "Trophic position")
boxplot_fn(mod_stan1, regex_pars = c(") ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 0.75,
           xline = 2.5,
           ylab = "Order",
           xlab = "Trophic position")
dev.off()

# plot Fig. 4: partial regression coefficients
var_list <- c("len", "edhd", "mobd", "jlhd", "cfdcpd")
var_names <- c("log10(Body length)", "EdHd", "MoBd", "JlHd", "CFdCPd")
jpeg(file = "outputs/figs/Fig4.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(3, 2), mar = c(5.1, 5.1, 3.1, 1.1))
tl_plot <- 10 ^ sp_data$tl
for (i in seq_along(var_list)) {
  xmean <- attributes(sp_data[, var_list[i]])$`scaled:center`
  xsd <- attributes(sp_data[, var_list[i]])$`scaled:scale`
  tmp <- pd_regress[[i]]
  xplot <- xmean + xsd * sp_data[, var_list[i]]
  pd_plot(tmp, xlab = var_names[i], ylab = "Trophic position",
          mean = xmean,
          sd = xsd, tl = tl_plot, var = xplot)
  mtext(var_names[i], side = 3, line = 1, adj = 1, cex = 1.25)
}
dev.off()
