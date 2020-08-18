# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# load fitted model workspace
load("outputs/fitted-models-body-mass.RData")

# load some helper functions
source("code/helpers.R")

pd_jlhd <- readRDS("outputs/pd_jlhd.RDS")

# plot Fig. 3 for JlHd model: partial regression coefficients
jpeg(file = "outputs/figs/Fig3-jlhd_bodymass.jpg", units = "in", width = 7, height = 7, res = 300)
par(mfrow = c(1, 1), mar = c(4.1, 4.1, 1.1, 1.1))
xmean <- attributes(sp_data[, "len"])$`scaled:center`
xsd <- attributes(sp_data[, "len"])$`scaled:scale`
pd_tmp <- pd_jlhd
tl_plot <- sp_data$jlhd
pd_tmp$.outcome <- (pd_tmp$.outcome * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
tl_plot <- (tl_plot * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
xplot <- sp_data$len
xplot <- (xplot * attributes(sp_data$len)$`scaled:scale`) + attributes(sp_data$len)$`scaled:center`
xplot <- 10 ^ xplot
pd_plot(pd_tmp, xlab = "Body mass (BM)", ylab = "log10(Jaw length (JlHd))",
        mean = xmean,
        sd = xsd, tl = tl_plot, var = xplot,
        ylim = c(-1.5, 1),
        log_x = TRUE, log_y = FALSE)
dev.off()

# plot Fig. 3 for JlHd model: partial regression coefficients
#   without low TP species (herb/detritivores)

# load fitted model workspace
rm(sp_data)
load("outputs/fitted-models-body-mass-rm-low-tp.RData")

pd_jlhd_rmlowtp <- readRDS("outputs/pd_jlhd-rm-low-tp.RDS")

jpeg(file = "outputs/figs/Fig3-jlhd_bodymass-rm-low-tp.jpg", units = "in", width = 7, height = 7, res = 300)
par(mfrow = c(1, 1), mar = c(4.1, 4.1, 1.1, 1.1))
xmean <- attributes(sp_data[, "len"])$`scaled:center`
xsd <- attributes(sp_data[, "len"])$`scaled:scale`
pd_tmp <- pd_jlhd_rmlowtp
tl_plot <- sp_data$jlhd
pd_tmp$.outcome <- (pd_tmp$.outcome * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
tl_plot <- (tl_plot * attributes(sp_data$jlhd)$`scaled:scale`) + attributes(sp_data$jlhd)$`scaled:center`
xplot <- sp_data$len
xplot <- (xplot * attributes(sp_data$len)$`scaled:scale`) + attributes(sp_data$len)$`scaled:center`
xplot <- 10 ^ xplot
pd_plot(pd_tmp, xlab = "Body mass (BM)", ylab = "log10(Jaw length (JlHd))",
        mean = xmean,
        sd = xsd, tl = tl_plot, var = xplot,
        ylim = c(-1.5, 1),
        log_x = TRUE, log_y = FALSE)
dev.off()

