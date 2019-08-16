setwd("~/Dropbox/research/size-trophic/")

# use the greta.fda package to fit models
library(greta)
library(greta.fda)

# we need the data
data_set <- read.csv("data/FWspeciesTopredictTP.csv")

# create functional response
hist_fn <- function(x, ...) {
  hist(x, plot = FALSE, ...)$counts
}
size_breaks <- exp(seq(log(0.9), log(max(data_set$Length)), length = 21))

### basins are fully nested in ecoregions and lat cats
size_fn <- tapply(data_set$Length, list(data_set$X1_Basin_Name), hist_fn, breaks = size_breaks)
size_fn <- t(matrix(unlist(size_fn), nrow = length(size_fn[[1]])))

# define latcats and ecoregions for each basin (confirm they're nested)
basin <- levels(data_set$X1_Basin_Name)
ecoreg <- tapply(data_set$X3_Ecoregion, data_set$X1_Basin_Name, function(x) as.character(unique(x)))
latcat <- tapply(data_set$Latcat, data_set$X1_Basin_Name, unique)

# pull out some continuous predictors
pred_set <- data.frame(runoff = tapply(data_set$maRunoff, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       npp = tapply(data_set$nppAvAnn01to15, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       temp_ave = tapply(data_set$tempYearAverage1900to2017, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       temp_cv = tapply(data_set$tempYearCoV1900to2017, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       vascular = tapply(data_set$vascular, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       discharge_ave = tapply(data_set$avYearDischarge, data_set$X1_Basin_Name, mean, na.rm = TRUE),
                       discharge_cv = tapply(data_set$covYearDischarge, data_set$X1_Basin_Name, mean, na.rm = TRUE))

# fill missing values with means (can use mice later on)
pred_set <- apply(pred_set, 2, function(x) ifelse(is.na(x) | is.nan(x), mean(x, na.rm = TRUE), x))
pred_set <- scale(pred_set)

# define greta linear predictor
data_short <- list(isd = size_fn,
                   lat = factor(latcat),
                   ecoreg = factor(ecoreg),
                   basin = factor(basin),
                   runoff = pred_set[, 1],
                   npp = pred_set[, 2],
                   temp_ave = pred_set[, 3],
                   temp_cv = pred_set[, 4],
                   vasc = pred_set[, 5],
                   disch_ave = pred_set[, 6],
                   disch_cv = pred_set[, 7])

fda_model <- fda_response(isd ~ lat + runoff + npp + temp_ave + temp_cv +
                            vasc + disch_ave + disch_cv + (1 | ecoreg),
                          data = data_short,
                          spline_settings = list(df = 8, degree = 3),
                          priors = list(alpha_sd = 1, beta_sd = 1, sigma_max = 1)) 

mu <- fda_model$mu
alpha <- fda_model$alpha
beta <- fda_model$beta
gamma <- fda_model$gamma
sigma_main <- fda_model$sigma_main
sigma_bins <- fda_model$sigma_bins
sigma_gamma <- fda_model$sigma_gamma

# set likelihood
distribution(data_short$isd) <- poisson(exp(mu))

# compile model
mod <- model(mu, alpha, beta, sigma_main, sigma_bins, sigma_gamma)

# sample from model
samples <- mcmc(mod, n_samples = 10000, warmup = 10000, thin = 5, chains = 3)

# summarise fitted model
mod_summary <- summary(samples)

samples_averaged <- do.call(abind, list(samples, along = 3))
samples_averaged <- apply(samples_averaged, c(1, 2), mean)
samples_averaged <- samples_averaged[, grep('mu\\[', colnames(samples_averaged), invert = TRUE)]

# extract mean fitted values
fitted_mean <- exp(matrix(mod_summary$statistics[grep('mu\\[', rownames(mod_summary$statistics)), 'Mean'], ncol = ncol(data_short$isd)))

# r2 variants
r2 <- cor(c(fitted_mean), c(data_short$isd)) ** 2
bayes_r2 <- var(c(fitted_mean)) /
  (var(c(fitted_mean)) + var(c(fitted_mean) - c(data_short$isd)))
int_term <- c(data_short$isd) / c(fitted_mean)
int_term <- ifelse(int_term == 0, 1, int_term)
int_term2 <- c(data_short$isd) / mean(c(data_short$isd))
int_term2 <- ifelse(int_term2 == 0, 1, int_term2)
dev_full <- 2 * sum(c(data_short$isd) * log(int_term) - (c(data_short$isd) - c(fitted_mean)))
dev_null <- 2 * sum(c(data_short$isd) * log(int_term2) - (c(data_short$isd) - mean(c(data_short$isd))))
deviance_r2 <- 1 - dev_full / dev_null

