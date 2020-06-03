# load some packages
library(caret)
library(rstanarm)

# read in and collapse the full data set to species
data_set <- read.csv("data/osm-raw-data.csv", stringsAsFactors = FALSE)
data_set <- data.frame(
  length = tapply(data_set$Length, data_set$SpecCode, unique),
  sp_code = tapply(data_set$SpecCode, data_set$SpecCode, unique),
  genus = tapply(data_set$Genus, data_set$SpecCode, unique),
  species = tapply(data_set$species, data_set$SpecCode, unique)
)

# load additional data on traits and mass
elongation <- read.csv("data/osm-data-fish-elongation.csv", stringsAsFactors = FALSE)
body_mass <- read.csv("data/osm-data-species-with-mass.csv", stringsAsFactors = FALSE)

# match up full data and additional data
idx <- match(paste(data_set$genus, data_set$species, sep = "_"),
             paste(elongation$Genus, elongation$species, sep = "_"))
data_set$BlBd <- elongation$BlBd[idx]

idy <- match(data_set$sp_code, body_mass$SpecCode)
data_set$body_mass <- body_mass$Wmax[idy]
data_set$log_length <- log10(data_set$length)
data_set$log_body_mass <- log10(data_set$body_mass)
data_set$log_BlBd <- log10(data_set$BlBd)

data_trimmed <- data_set[!apply(data_set, 1, function(x) any(is.na(x))), ]

# fit full model
mod <- stan_glm(log_body_mass ~ log_length + log_BlBd,
                data = data_trimmed,
                family = gaussian)

# summarise model fit
r2_full <- cor(mod$fitted.values, mod$data$log_body_mass)
slope_full <- lm(mod$fitted.values ~ -1 + mod$data$log_body_mass)$coefficients

# split up test and train data sets for this fold
n_cv <- 10
holdouts <- createFolds(data_trimmed$len, k = n_cv, list = TRUE, returnTrain = FALSE)
preds <- rep(NA, nrow(data_trimmed))
for (i in seq_len(n_cv)) {
  
  training <- data_trimmed[-holdouts[[i]], ]
  testing <- data_trimmed[holdouts[[i]], ]
  
  mod_tmp <- stan_glm(log_body_mass ~ log_length + log_BlBd,
                      data = training,
                      family = gaussian)
  
  stan_pred <- posterior_predict(mod_tmp, newdata = testing)
  preds[holdouts[[i]]] <- apply(stan_pred, 2, median)
  
}

r2_cv <- cor(preds, data_trimmed$log_body_mass)
slope_cv <- lm(preds ~ -1 + data_trimmed$log_body_mass)$coefficients

# fill body masses in full data set
data_sub <- data_set[!is.na(data_set$log_length) & !is.na(data_set$log_BlBd), ]
data_sub$body_mass <- NULL
data_sub$log_body_mass <- NULL
full_pred <- posterior_predict(mod, newdata = data_sub)
data_sub$body_mass_predicted <- apply(full_pred, 2, median)

# save predicted values
write.csv(data_sub, file = "data/osm-mass-data.csv")

