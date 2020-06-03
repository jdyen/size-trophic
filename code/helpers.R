boxplot_fn <- function(obj, regex_pars,
                       prob, prob_outer,
                       labels = NULL, cex.axis = 1,
                       xline = 2.5, yline = 6.5,
                       xlim = NULL,
                       order = NULL,
                       xlab = "Coefficient", ylab = "Parameter",
                       offset = "len",
                       transform = FALSE,
                       secondary_points = NULL,
                       rescale = NULL,
                       zero_line = TRUE
) {
  
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
  
  if (!is.null(rescale)) {
    outer <- rescale$int + rescale$sd * outer
    inner <- rescale$int + rescale$sd * inner
    coefs <- rescale$int + rescale$sd * coefs
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
  if (!is.null(secondary_points))
    xlim <- range(c(xlim, unlist(secondary_points)))
  plot(yplot ~ coefs,
       ylim = ylims, xlim = xlim,
       type = "n",
       las = 1, bty = "l",
       xlab = "", xaxt = "n",
       ylab = "", yaxt = "n")
  if (!is.null(secondary_points)) {
    npoints <- sapply(secondary_points, length)
    points(rep(yplot, npoints) ~ unlist(secondary_points),
           pch = 16, cex = 0.7, col = scales::alpha("darkred", 0.1))
  }

  for (i in seq_len(ncoef)) {
    lines(c(yplot[i], yplot[i]) ~ c(outer[i, 1], outer[i, 2]),
          lwd = 1.5)
    lines(c(yplot[i], yplot[i]) ~ c(inner[i, 1], inner[i, 2]),
          lwd = 3.5)
  }
  points(yplot ~ coefs, pch = 16, cex = 1.5, col = "black")
  
  if (is.null(labels))
    labels <- names(coefs)
  if (!is.null(order))
    labels <- labels[order]
  
  axis(1)
  axis(2, at = yplot, labels = labels, las = 1, cex.axis = cex.axis)
  
  mtext(xlab, side = 1, line = xline, adj = 0.5, cex = 1.25)
  mtext(ylab, side = 2, line = yline, adj = 0.5, cex = 1.25)
  
  if (zero_line)
    lines(c(0, ncoef + 1) ~ c(0, 0), lty = 2, lwd = 1.5)
  
}

pd_plot <- function(pd, xlab = NULL, ylab = NULL, mean = 0, sd = 1, col_pal = NULL, tl, var, ylim = c(2, 4.5), log_x = FALSE, log_y = TRUE) {
  
  col_pal <- RColorBrewer::brewer.pal(4, "Set2")
  
  if (is.null(xlab))
    xlab <- "Predictor"
  if (is.null(ylab))
    ylab <- "Response"

  pd[, 1] <- pd[, 1] * sd + mean
  if (log_y)
    pd[, 2] <- 10 ^ pd[, 2]
  if (log_x) {
    pd[, 1] <- 10 ^ pd[, 1]
  }
  
  if (ncol(pd) == 2) {

    plot(pd[, 2] ~ pd[, 1],
         type = "n", las = 1, bty = "l",
         xlab = "", ylab = "",
         ylim = ylim)
    
  } else {
    
    plot(pd[, 2] ~ pd[, 1],
         type = "n", las = 1, bty = "l",
         xlab = xlab, ylab = ylab,
         ylim = ylim)    
    for (i in 2:5)
      lines(pd[, i] ~ pd[, 1], col = col_pal[i - 1], lty = 1, lwd = 2)
    
  }
  
  points(tl ~ var, pch = 16, col = scales::alpha("black", 0.2))
  lines(pd[, 2] ~ pd[, 1], lwd = 3)
  
  mtext(xlab, side = 1, cex = 1, adj = 0.5, line = 2.6)
  mtext(ylab, side = 2, cex = 1, adj = 0.5, line = 2.9)
  
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
