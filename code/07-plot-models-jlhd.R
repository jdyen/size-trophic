# plot jlhd model
# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace (body mass models)
load("outputs/fitted-models-body-mass.RData")

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

# repaet figure with body mass
jpeg(file = "outputs/figs/Fig1-jlhd-bodymass.jpg", units = "in", width = 7, height = 7, res = 150)
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

# calculate proportion herb/detrit in each order
prop_guilds <- tapply(sp_data$guild, sp_data$ord, table)
prop_guilds <- do.call(rbind, prop_guilds)
guild_num <- apply(prop_guilds, 1, sum)
prop_guilds <- sweep(prop_guilds, 1, apply(prop_guilds, 1, sum), "/")

# pull out details on number of observations in each ecoregion
necoregion <- length(levels(sp_data$ecoregion))
ecoregion_num <- table(sp_data$ecoregion)

# Plot Fig. 2 with embedded pie charts
jpeg(file = "outputs/figs/Fig2-jlhd-bodymass.jpg", width = 8.5, height = 7, units = "in", res = 300)
idx <- order(prop_guilds[, 1], decreasing = TRUE)
nlevel <- length(levels(sp_data$ord))
main_effect <- apply(posterior_interval(mod_jlhd, par = "len", prob = 0.01), 1, mean)
layout(matrix(c(1, 1, 1, 2, 2, 2, 2), nrow = 1))
par(mar = c(5.5, 8.2, 1.6, 1.1))
boxplot_fn(mod_jlhd, regex_pars = c("len ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion),
           ylab = "Ecoregion",
           offset = "len",
           zero_line = FALSE,
           cex.axis = 1.25,
           xlim = c(-0.05, 0.55),
           yline = 6.8)
for (i in rev(seq_len(necoregion))) {
  text(x = 0, y = i, paste0("n = ", ecoregion_num[necoregion - i + 1]), xpd = TRUE, cex = 0.9)
}
lines(rep(main_effect, 2), c(0, 10), lty = 2)
boxplot_fn(mod_jlhd, regex_pars = c("len ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 1,
           ylab = "Order", order = idx,
           offset = "len",
           xlim = c(-1.0, 1.5),
           zero_line = FALSE)
col_pal <- viridisLite::inferno(4)
to_plot <- prop_guilds[idx, ]
guild_num_sorted <- guild_num[idx]
for (i in rev(seq_len(nlevel))) {
  floating.pie(xpos = -0.65, ypos = i, x = to_plot[30 - i + 1, ], col = col_pal[which(to_plot[30 - i + 1, ] > 0)], radius = 0.06)
  text(x = -0.95, y = i, paste0("n = ", guild_num_sorted[30 - i + 1]), xpd = TRUE, cex = 0.9)
}
lines(rep(main_effect, 2), c(-10, 40), lty = 2)
legend(x = 1.3, y = 31, legend = c("Herb./detrit.", "Omni.", "Sec. cons.", "Top pred."),
       fill = col_pal, border = NULL, xpd = TRUE, bty = "n", horiz = FALSE,
       xjust = 0.5, cex = 1.0)
dev.off()

# plot Fig. 3: estimated coefficients from stan JLHD model
pdf(file = "outputs/figs/Fig3-jlhd.pdf")
par(mfrow = c(1, 1), mar = c(4.5, 10, 1.1, 1.1))
boxplot_fn(mod_jlhd,
           regex_pars = c("b\\[len fresh:Fonly\\]", "b\\[len fresh:FandM\\]",
                          "streamLonely:len", "streamSonly:len"),
           prob = 0.8, prob_outer = 0.95,
           labels = c("Freshwater:MBM", "Marine:MBM",
                      "Lake:MBM", "River:MBM"),
           xlab = "Coefficient",
           offset = "len",
           yline = 8.5)
dev.off()
