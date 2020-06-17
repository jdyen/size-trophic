# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("outputs/fitted-models-body-mass.RData")

# load some helper functions
source("code/helpers.R")

# Fig 1: length effects by order and ecoregion
jpeg(file = "outputs/figs/Fig1-mass.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(1, 2), mar = c(4.2, 8.2, 1.1, 1.1))
real_tp <- tapply(sp_data$tl, sp_data$ecoregion, identity)
real_tp <- lapply(real_tp, function(x) 10 ^ x)
boxplot_fn(mod_stan1, regex_pars = c(") ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion), xline = 2.5,
           ylab = "Ecoregion",
           xlab = "Trophic position",
           offset = "(Intercept)",
           transform = TRUE,
           secondary_points = real_tp)
real_tp <- tapply(sp_data$tl, sp_data$ord, identity)
real_tp <- lapply(real_tp, function(x) 10 ^ x)
boxplot_fn(mod_stan1, regex_pars = c(") ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 0.75,
           xline = 2.5,
           ylab = "Order",
           xlab = "Trophic position",
           offset = "(Intercept)",
           transform = TRUE,
           secondary_points = real_tp)
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
jpeg(file = "outputs/figs/Fig2-mass.jpg", width = 8.5, height = 7, units = "in", res = 300)
idx <- order(prop_guilds[, 1], decreasing = TRUE)
nlevel <- length(levels(sp_data$ord))
layout(matrix(c(1, 1, 1, 2, 2, 2, 2), nrow = 1))
par(mar = c(5.5, 9.2, 1.6, 1.1))
boxplot_fn(mod_stan1, regex_pars = c("len ecor"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ecoregion),
           ylab = "Ecoregion",
           offset = "len",
           cex.axis = 1.25,
           xlim = c(-0.04, 0.035),
           yline = 6.8)
for (i in rev(seq_len(necoregion))) {
  text(x = -0.032, y = i, paste0("n = ", ecoregion_num[necoregion - i + 1]), xpd = TRUE, cex = 0.9)
}
boxplot_fn(mod_stan1, regex_pars = c("len ord"),
           prob = 0.8, prob_outer = 0.95,
           labels = levels(sp_data$ord), cex.axis = 1,
           ylab = "Order", order = idx,
           offset = "len",
           xlim = c(-0.2, 0.2),
           yline = 7.8)
col_pal <- viridis::inferno(4)
to_plot <- prop_guilds[idx, ]
guild_num_sorted <- guild_num[idx]
for (i in rev(seq_len(nlevel))) {
  floating.pie(xpos = -0.119, ypos = i, x = to_plot[nlevel - i + 1, ], col = col_pal[which(to_plot[30 - i + 1, ] > 0)], radius = 0.01)
  text(x = -0.18, y = i, paste0("n = ", guild_num_sorted[30 - i + 1]), xpd = TRUE, cex = 0.9)
}
legend(x = 0.129, y = 31, legend = c("Herb./detrit.", "Omni.", "Sec. cons.", "Top pred."),
       fill = col_pal, border = NULL, xpd = TRUE, bty = "n", horiz = FALSE,
       xjust = 0.5, cex = 1.0)
dev.off()

# plot Fig. 3: estimated coefficients from stan model
pdf(file = "outputs/figs/Fig3-mass.pdf")
par(mfrow = c(1, 1), mar = c(4.5, 10, 1.1, 1.1))
boxplot_fn(mod_stan1,
           regex_pars = c("edhd", "mobd", "jlhd", "cfdcpd",
                          "b\\[len fresh:Fonly\\]", "b\\[len fresh:FandM\\]",
                          "streamLonely:len", "streamSonly:len"),
           prob = 0.8, prob_outer = 0.95,
           labels = c("Eye diameter", "Position of the mouth", "Jaw length",
                      "Caudal fin aspect", "Freshwater:MBM", "Marine:MBM",
                      "Lake:MBM", "River:MBM"),
           xlab = "Coefficient",
           offset = "len",
           yline = 8.5,
           offset_id = c(5:8))
dev.off()

# plot Fig. 5: partial regression coefficients
var_list_full <- c("len", "edhd", "mobd", "jlhd", "cfdcpd")
var_list <- c("len", "jlhd", "cfdcpd")
var_names <- c("log10(Body mass (BM))", "log10(Jaw length (JlHd))", "Caudal fin aspect (CFdCPd)")
jpeg(file = "outputs/figs/Fig5-mass.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(2, 2), mar = c(4.1, 4.1, 1.1, 1.1))
tl_plot <- 10 ^ sp_data$tl
log_xset <- c(FALSE, FALSE, FALSE)
for (i in seq_along(var_list)) {
  xmean <- attributes(sp_data[, var_list[i]])$`scaled:center`
  xsd <- attributes(sp_data[, var_list[i]])$`scaled:scale`
  tmp <- pd_regress[[match(var_list, var_list_full)[i]]]
  xplot <- xmean + xsd * sp_data[, var_list[i]]
  if (log_xset[i])
    xplot <- 10 ^ xplot
  pd_plot(tmp, xlab = var_names[i], ylab = "Trophic position",
          mean = xmean,
          sd = xsd, tl = tl_plot, var = xplot,
          log_x = log_xset[i])
}
dev.off()

# plot Fig. 5 for JlHd model: partial regression coefficients
jpeg(file = "outputs/figs/Fig5-jlhd_bodymass.jpg", units = "in", width = 7, height = 7, res = 150)
par(mfrow = c(1, 1), mar = c(4.1, 4.1, 1.1, 1.1))
xmean <- attributes(sp_data[, "len"])$`scaled:center`
xsd <- attributes(sp_data[, "len"])$`scaled:scale`
pd_tmp <- pd_jlhd
tl_plot <- sp_data$jlhd
pd_tmp$.outcome <- (pd_tmp$.outcome * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
tl_plot <- (tl_plot * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
tl_plot <- tl_plot
xplot <- (xmean + xsd * sp_data[, "len"])
pd_plot(pd_tmp, xlab = "log10(Body mass (BM))",
        ylab = "log10(Jaw length (JlHd))",
        mean = xmean,
        sd = xsd, tl = tl_plot, var = xplot,
        ylim = c(-2, 2),
        log_x = FALSE,
        log_y = FALSE)
dev.off()

# plot Fig. 6: residuals from fitted models
stan_predictions <- posterior_predict(mod_stan1)
stan_plot <- apply(stan_predictions, 2, median)
ctree_plot <- predict(mod_ctree1)
jpeg(file = "outputs/figs/Fig6-mass.jpg", units = "in", width = 7, height = 7, res = 150)
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

# extract intercept values and write to a CSV
len_intercept <- apply(posterior_interval(mod_stan1, par = "len", prob = 0.01), 1, mean)

length_effects <- cbind(
  posterior_interval(mod_stan1,
                     regex_pars = c("len ecor", "^streamSonly:len$", "^streamLonely:len$"),
                     prob = 0.95) + len_intercept,
  posterior_interval(mod_stan1,
                     regex_pars = c("len ecor", "^streamSonly:len$", "^streamLonely:len$"),
                     prob = 0.8) + len_intercept,
  apply(
    posterior_interval(mod_stan1,
                       regex_pars = c("len ecor", "^streamSonly:len$", "^streamLonely:len$"),
                       prob = 0.01) + len_intercept,
    1,
    mean
  )
)
length_effects <- length_effects[, c(5, 1:4)]
colnames(length_effects)[1] <- "Median"
rownames(length_effects) <- c("Afrotropic", "Australasia", "IndoMalay", "Nearctic", "Neotropic",
                              "Oceania", "Palearctic", "Stream", "Lake")
write.csv(length_effects, file = "outputs/length_coefficients-mass.csv")
