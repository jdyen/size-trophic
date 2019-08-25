boxplot_fn <- function(obj, regex_pars,
                       prob, prob_outer,
                       labels = NULL, cex.axis = 1,
                       xline = 2.5, yline = 6.5,
                       xlim = NULL,
                       order = NULL,
                       xlab = "Coefficient", ylab = "Parameter",
                       offset = "len",
                       transform = FALSE) {
  
  if (!is.null(offset))
    int_val <- apply(posterior_interval(obj, par = offset, prob = 0.01), 1, mean)
  
  outer <- posterior_interval(obj, regex_pars = regex_pars, prob = prob_outer)
  inner <- posterior_interval(obj, regex_pars = regex_pars, prob = prob)
  coefs <- apply(posterior_interval(obj, regex_pars = regex_pars, prob = 0.01), 1, mean)
  ncoef <- length(coefs)
  
  if (!is.null(offset)) {
    outer <- outer + int_val
    inner <- inner + int_val
    coefs <- coefs + int_val
  }
  
  if (transform) {
    outer <- 10 ^ outer
    inner <- 10 ^ inner
    coefs <- 10 ^ coefs
  }
  
  if (!is.null(order)) {
    coefs <- coefs[order]
    outer <- outer[order, ]
    inner <- inner[order, ]
  }
  
  ylims <- c(0.5, ncoef + 0.5)
  yplot <- rev(seq_len(ncoef))
  if (is.null(xlim))
    xlim <- range(c(outer, inner, coefs))
  plot(yplot ~ coefs,
       ylim = ylims, xlim = xlim,
       las = 1, bty = "l",
       xlab = "", xaxt = "n",
       ylab = "", yaxt = "n",
       pch = 16, cex = 2,
       col = "black")
  for (i in seq_len(ncoef)) {
    lines(c(yplot[i], yplot[i]) ~ c(outer[i, 1], outer[i, 2]),
          lwd = 1.5)
    lines(c(yplot[i], yplot[i]) ~ c(inner[i, 1], inner[i, 2]),
          lwd = 3.0)
  }
  
  if (is.null(labels))
    labels <- names(coefs)
  if (!is.null(order))
    labels <- labels[order]
  
  axis(1)
  axis(2, at = yplot, labels = labels, las = 1, cex.axis = cex.axis)
  
  mtext(xlab, side = 1, line = xline, adj = 0.5, cex = 1.25)
  mtext(ylab, side = 2, line = yline, adj = 0.5, cex = 1.25)
  
  lines(c(0, ncoef + 1) ~ c(0, 0), lty = 2, lwd = 1.5)
  
}

pd_plot <- function(pd, xlab = NULL, ylab = NULL, mean = 0, sd = 1, col_pal = NULL, tl, var) {
  
  col_pal <- RColorBrewer::brewer.pal(4, "Set2")
  
  if (is.null(xlab))
    xlab <- "Predictor"
  if (is.null(ylab))
    ylab <- "Response"
  
  pd[, 1] <- pd[, 1] * sd + mean
  pd[, 2] <- 10 ^ pd[, 2]
  
  if (ncol(pd) == 2) {

    plot(pd[, 2] ~ pd[, 1],
         type = "n", las = 1, bty = "l",
         xlab = "", ylab = "",
         ylim = c(2, 4.5))
    
  } else {
    
    plot(pd[, 2] ~ pd[, 1],
         type = "n", las = 1, bty = "l",
         xlab = xlab, ylab = ylab,
         ylim = c(2, 4.5))    
    for (i in 2:5)
      lines(pd[, i] ~ pd[, 1], col = col_pal[i - 1], lty = 1, lwd = 2)
    
  }
  
  points(tl ~ var, pch = 16, col = scales::alpha("darkred", 0.2))
  lines(pd[, 2] ~ pd[, 1], lwd = 3)
  
  mtext(xlab, side = 1, cex = 1, adj = 0.5, line = 2.8)
  mtext(ylab, side = 2, cex = 1, adj = 0.5, line = 3.5)
  
}

# convert predictions_lm to guilds
convert_tl_to_guild <- function(x) {
  
  out <- rep(NA, length(x))
  out[x <= 2.19] <- "Herb_detrite"
  out[x > 2.19 & x < 2.8] <- "Omni"
  out[x >= 2.8] <- "secCons"
  out[x >= 4] <- "Toppred"
  
  out
  
}

create_confusion_matrix <- function(predicted, observed) {
  
  x_lev <- unique(observed)
  out <- matrix(NA, length(x_lev), length(x_lev))
  
  for (i in seq_along(x_lev)) {
    out_tmp <- table(predicted[observed == x_lev[i]])
    out[i, match(names(out_tmp), x_lev)] <- c(out_tmp)
  }
  
  colnames(out) <- rownames(out) <- x_lev
  out[is.na(out)] <- 0
  
  out
  
}
