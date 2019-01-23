pd_plot <- function(pd, xlab = NULL, ylab = NULL, mean = 0, sd = 1, col_pal = NULL) {
  
  col_pal <- RColorBrewer::brewer.pal(4, "Set2")
  
  if (is.null(xlab))
    xlab <- "Predictor"
  if (is.null(ylab))
    ylab <- "Response"
  
  pd[, 1] <- pd[, 1] * sd + mean
  
  if (ncol(pd) == 2) {

    plot(pd[, 2] ~ pd[, 1],
         type = "l", las = 1, bty = "l",
         xlab = xlab, ylab = ylab)
    
  } else {
    
    plot(pd[, 2] ~ pd[, 1],
         type = "n", las = 1, bty = "l",
         xlab = xlab, ylab = ylab,
         ylim = range(pd[, 2:5]))    
    for (i in 2:5)
      lines(pd[, i] ~ pd[, 1], col = col_pal[i - 1], lty = 1, lwd = 2)
    
  }
  
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
