# load some packages
library(caret)
library(rstanarm)
library(edarf)
library(plotrix)

# Set Java settings (needed for some of the ML methods)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/")
Sys.setenv(LD_LIBRARY_PATH = "/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/")

# load some helper functions
source("code/helpers.R")

# start with all the data
data_set <- read.csv("data/raw-data-44k-records-with-stream-info.csv", stringsAsFactors = FALSE)
body_mass <- read.csv("data/osm-mass-data.csv", stringsAsFactors = FALSE)

# filter out species with TP in 2-2.19
data_set <- data_set[data_set$FoodTroph > 2.19, ]

# replace "Indo-Malay" with single word
data_set$X3_Ecoregion <- gsub("-", "", data_set$X3_Ecoregion)

# make a data frame with unique values of each variable for each species
sp_data <- data.frame(
  sp_code = c(tapply(data_set$SpecCode, data_set$SpecCode, unique)),
  tl = log10(c(tapply(data_set$FoodTroph, data_set$SpecCode, mean))),
  guild = c(tapply(data_set$Trophicguild, data_set$SpecCode, function(x) as.character(unique(x)))),
  len = log10(c(tapply(data_set$LengthcmTL, data_set$SpecCode, mean, na.rm = TRUE))),
  ord = c(tapply(data_set$Order_, data_set$SpecCode, function(x) as.character(unique(x)))),
  ecoregion = factor(tapply(data_set$X3_Ecoregion, data_set$SpecCode,
                            function(x) sort(unique(x))[which.max(tapply(rep(1, length(x)), x, sum))])),
  stream = c(tapply(data_set$Stream, data_set$SpecCode, function(x) as.character(unique(x)))),
  fresh = c(tapply(data_set$Fresh, data_set$SpecCode, function(x) as.character(unique(x)))),
  invasive = c(tapply(data_set$Invasive, data_set$SpecCode, function(x) as.character(unique(x)))),
  npp = c(tapply(data_set$nppAvAnn01to15, data_set$SpecCode, mean, na.rm = TRUE)),
  discharge = c(tapply(data_set$avYearDischarge, data_set$SpecCode, mean, na.rm = TRUE)),
  edhd = c(tapply(data_set$EdHd, data_set$SpecCode, mean, na.rm = TRUE)),
  mobd = c(tapply(data_set$MoBd, data_set$SpecCode, mean, na.rm = TRUE)),
  jlhd = log10(c(tapply(data_set$JlHd, data_set$SpecCode, mean, na.rm = TRUE)) + min(data_set$JlHd[data_set$JlHd > 0], na.rm = TRUE)),
  cfdcpd = c(tapply(data_set$CFdCPd, data_set$SpecCode, mean, na.rm = TRUE))
)

# add body mass
idx <- match(sp_data$sp_code, body_mass$sp_code)
matched_masses <- body_mass$body_mass_predicted[idx]
sp_data$len <- log10(matched_masses)

# there are some NAs, let's get rid of them
sp_data <- sp_data[apply(sp_data, 1, function(x) !any(is.na(x))), ]

# we don't want crazy big values of npp and discharge messing with things
sp_data$npp <- scale(sp_data$npp)
sp_data$discharge <- scale(sp_data$discharge)
sp_data$len <- scale(sp_data$len)
sp_data$edhd <- scale(sp_data$edhd)
sp_data$mobd <- scale(sp_data$mobd)
sp_data$jlhd <- scale(sp_data$jlhd)
sp_data$cfdcpd <- scale(sp_data$cfdcpd)

# try a general train-and-test partition
set.seed(2018-12-14)
n_cv <- 10
holdouts <- createFolds(sp_data$len, k = n_cv, list = TRUE, returnTrain = FALSE)

# how do we want to tune the model?
fitControl <- trainControl(
  method = "cv",
  number = 10)

# fit some simple regression models
mod_list <- c("bagEarth", "treebag",
              "xgbTree",
              "gaussprRadial",
              "bstTree", "cforest")
predictions <- matrix(NA, nrow(sp_data), length(mod_list))
predictions_reduced <- matrix(NA, nrow(sp_data), length(mod_list))
predictions_reduced2 <- matrix(NA, nrow(sp_data), length(mod_list))
for (i in seq_len(n_cv)) {
  
  # split up test and train data sets for this fold
  training <- sp_data[-holdouts[[i]], ]
  testing <- sp_data[holdouts[[i]], ]
  
  # there are a few different models, loop over each in turn
  for (j in seq_along(mod_list)) {
    
    # fit a model trained with general settings
    mod_tmp <- train(tl ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                       fresh +
                       ecoregion +
                       stream,
                     data = training,
                     method = mod_list[j],
                     trControl = fitControl)
    mod_tmp2 <- train(tl ~ len +
                        fresh +
                        ecoregion + stream,
                      data = training,
                      method = mod_list[j],
                      trControl = fitControl)
    mod_tmp3 <- train(tl ~ len + ord +
                        fresh +
                        ecoregion + stream,
                      data = training,
                      method = mod_list[j],
                      trControl = fitControl)
    
    # give me the predictions
    predictions[holdouts[[i]], j] <- predict(mod_tmp, newdata = testing)
    predictions_reduced[holdouts[[i]], j] <- predict(mod_tmp2, newdata = testing)
    predictions_reduced2[holdouts[[i]], j] <- predict(mod_tmp3, newdata = testing)
    
  }
  
}

# fit the cforest models to all data to extract variable importance
mod_ctree1 <- train(tl ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                      fresh +
                      ecoregion + stream,
                    data = sp_data,
                    method = mod_list[6],
                    trControl = fitControl)
ctree_vimp <- varImp(mod_ctree1, scale = FALSE)
mod_ctree2 <- train(tl ~ len +
                      fresh +
                      ecoregion + stream,
                    data = sp_data,
                    method = mod_list[6],
                    trControl = fitControl)
ctree_vimp2 <- varImp(mod_ctree2, scale = FALSE)
mod_ctree3 <- train(tl ~ len + ord +
                      fresh +
                      ecoregion + stream,
                    data = sp_data,
                    method = mod_list[6],
                    trControl = fitControl)
ctree_vimp3 <- varImp(mod_ctree3, scale = FALSE)

# re-run the above models but within guilds
guilds <- unique(sp_data$guild)
predictions_guilds <- predictions_reduced_guilds <-
  predictions_reduced2_guilds <- vector("list", length = length(guilds))
for (k in seq_along(guilds)) {
  
  # create a data subset for this guild
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  holdouts_guild <- createFolds(sp_guild$len, k = n_cv, list = TRUE, returnTrain = FALSE)
  
  # create some empty output matrices
  predictions_guilds[[k]] <- matrix(NA, nrow(sp_guild), length(mod_list))
  predictions_reduced_guilds[[k]] <- matrix(NA, nrow(sp_guild), length(mod_list))
  predictions_reduced2_guilds[[k]] <- matrix(NA, nrow(sp_guild), length(mod_list))
  
  for (i in seq_len(n_cv)) {
    
    # split up test and train data sets for this fold
    training <- sp_guild[-holdouts_guild[[i]], ]
    testing <- sp_guild[holdouts_guild[[i]], ]
    
    # there are a few different models, loop over each in turn
    for (j in seq_along(mod_list)) {
      
      # fit a model trained with general settings
      mod_tmp <- train(tl ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                         fresh +
                         ecoregion + stream,
                       data = training,
                       method = mod_list[j],
                       trControl = fitControl)
      mod_tmp2 <- train(tl ~ len +
                          fresh +
                          ecoregion + stream,
                        data = training,
                        method = mod_list[j],
                        trControl = fitControl)
      mod_tmp3 <- train(tl ~ len + ord +
                          fresh +
                          ecoregion + stream,
                        data = training,
                        method = mod_list[j],
                        trControl = fitControl)
      
      # give me the predictions
      predictions_guilds[[k]][holdouts_guild[[i]], j] <- predict(mod_tmp, newdata = testing)
      predictions_reduced_guilds[[k]][holdouts_guild[[i]], j] <- predict(mod_tmp2, newdata = testing)
      predictions_reduced2_guilds[[k]][holdouts_guild[[i]], j] <- predict(mod_tmp3, newdata = testing)
      
    }
    
  }
}

# partial dependence
var_list <- c("len", "edhd", "mobd", "jlhd", "cfdcpd")
pd_regress <- pd_class <- vector("list", length = length(var_list))
for (i in seq_along(var_list)) {
  data_pd <- sp_data[, colnames(sp_data) %in% var_list]
  pd_regress[[i]] <- partial_dependence(mod_ctree1$finalModel, vars = var_list[i],
                                        data = data_pd)
}

# fit the lm versions of these models
mod_stan1 <- stan_lmer(tl ~ (len | ord) + (len | ecoregion) +
                         (len | fresh) +
                         edhd + mobd + jlhd + cfdcpd +
                         stream * len,
                       data = sp_data,
                       iter = 5000,
                       cores = 1)
mod_stan2 <- stan_lmer(tl ~ (len | ecoregion) +
                         (len | fresh) +
                         stream * len,
                       data = sp_data,
                       iter = 5000,
                       cores = 1)
mod_stan3 <- stan_lmer(tl ~ (len | ord) + (len | ecoregion) +
                         (len | fresh) + stream * len,
                       data = sp_data,
                       iter = 5000,
                       cores = 1)
naive_lm_pred1 <- posterior_predict(mod_stan1)
naive_lm_pred2 <- posterior_predict(mod_stan2)
naive_lm_pred3 <- posterior_predict(mod_stan3)

# run 10-fold CV on the lm models
predictions_lm <- matrix(NA, nrow = nrow(sp_data), ncol = 3)
for (i in seq_len(n_cv)) {
  
  # split up test and train data sets for this fold
  training <- sp_data[-holdouts[[i]], ]
  testing <- sp_data[holdouts[[i]], ]
  
  # fit lm models
  mod_tmp1 <- stan_lmer(tl ~ (len | ord) + (len | ecoregion) +
                          (len | fresh) +
                          edhd + mobd + jlhd + cfdcpd +
                          stream * len,
                        data = training,
                        iter = 5000,
                        cores = 1)
  mod_tmp2 <- stan_lmer(tl ~ (len | ecoregion) +
                          (len | fresh) +
                          stream * len,
                        data = training,
                        iter = 5000,
                        cores = 1)
  mod_tmp3 <- stan_lmer(tl ~ (len | ord) + 
                          (len | ecoregion) +
                          (len | fresh) +
                          stream * len,
                        data = training,
                        iter = 5000,
                        cores = 1)
  
  # give me the predictions
  stan_pred_tmp <- posterior_predict(mod_tmp1, newdata = testing, re.form = NA)
  predictions_lm[holdouts[[i]], 1] <- apply(stan_pred_tmp, 2, median)
  stan_pred_tmp <- posterior_predict(mod_tmp2, newdata = testing, re.form = NA)
  predictions_lm[holdouts[[i]], 2] <- apply(stan_pred_tmp, 2, median)
  stan_pred_tmp <- posterior_predict(mod_tmp3, newdata = testing, re.form = NA)
  predictions_lm[holdouts[[i]], 3] <- apply(stan_pred_tmp, 2, median)
  
}

# re-run the linear models but within guilds
guilds <- unique(sp_data$guild)
predictions_lm_guilds <- vector("list", length = length(guilds))
for (k in seq_along(guilds)) {
  
  # create a data subset for this guild
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  holdouts_guild <- createFolds(sp_guild$len, k = n_cv, list = TRUE, returnTrain = FALSE)
  
  # create an output matrix
  predictions_lm_guilds[[k]] <- matrix(NA, nrow = nrow(sp_guild), ncol = 3)
  
  for (i in seq_len(n_cv)) {
    
    # split up test and train data sets for this fold
    training <- sp_guild[-holdouts_guild[[i]], ]
    testing <- sp_guild[holdouts_guild[[i]], ]
    
    # fit lm models
    mod_tmp1 <- stan_lmer(tl ~ (len | ord) + (len | ecoregion) +
                            (len | fresh) +
                            edhd + mobd + jlhd + cfdcpd +
                            stream * len,
                          data = training,
                          iter = 5000,
                          cores = 1)
    mod_tmp2 <- stan_lmer(tl ~ (len | ecoregion) +
                            (len | fresh) +
                            stream * len,
                          data = training,
                          iter = 5000,
                          cores = 1)
    mod_tmp3 <- stan_lmer(tl ~ (len | ord) +
                            (len | ecoregion) +
                            (len | fresh) +
                            stream * len,
                          data = training,
                          iter = 5000,
                          cores = 1)
    
    # give me the predictions
    stan_pred_tmp <- posterior_predict(mod_tmp1, newdata = testing, re.form = NA)
    predictions_lm_guilds[[k]][holdouts_guild[[i]], 1] <- apply(stan_pred_tmp, 2, median)
    stan_pred_tmp <- posterior_predict(mod_tmp2, newdata = testing, re.form = NA)
    predictions_lm_guilds[[k]][holdouts_guild[[i]], 2] <- apply(stan_pred_tmp, 2, median)
    stan_pred_tmp <- posterior_predict(mod_tmp3, newdata = testing, re.form = NA)
    predictions_lm_guilds[[k]][holdouts_guild[[i]], 3] <- apply(stan_pred_tmp, 2, median)
    
  }
}

# re-run the main (non-CV) models within guilds
fitted_lm_guilds <- fitted_rf_guilds <- vector("list", length = length(guilds))
for (k in seq_along(guilds)) {
  
  # create a data subset for this guild
  sp_guild <- sp_data[sp_data$guild == guilds[k], ]
  holdouts_guild <- createFolds(sp_guild$len, k = n_cv, list = TRUE, returnTrain = FALSE)
  
  # create an output matrix
  fitted_lm_guilds[[k]] <- matrix(NA, nrow = nrow(sp_guild), ncol = 3)
  fitted_rf_guilds[[k]] <- matrix(NA, nrow = nrow(sp_guild), ncol = 3)
  
  # fit lm models
  mod_tmp1 <- stan_lmer(tl ~ (len | ord) + 
                          (len | ecoregion) +
                          (len | fresh) +
                          edhd + mobd + jlhd + cfdcpd +
                          stream * len,
                        data = sp_guild,
                        iter = 5000,
                        cores = 1)
  mod_tmp2 <- stan_lmer(tl ~ (len | ecoregion) +
                          (len | fresh) +
                          stream * len,
                        data = sp_guild,
                        iter = 5000,
                        cores = 1)
  mod_tmp3 <- stan_lmer(tl ~ (len | ord) +
                          (len | ecoregion) +
                          (len | fresh) +
                          stream * len,
                        data = sp_guild,
                        iter = 5000,
                        cores = 1)
  
  # give me the predictions
  stan_pred_tmp <- posterior_predict(mod_tmp1)
  fitted_lm_guilds[[k]][, 1] <- apply(stan_pred_tmp, 2, median)
  stan_pred_tmp <- posterior_predict(mod_tmp2)
  fitted_lm_guilds[[k]][, 2] <- apply(stan_pred_tmp, 2, median)
  stan_pred_tmp <- posterior_predict(mod_tmp3)
  fitted_lm_guilds[[k]][, 3] <- apply(stan_pred_tmp, 2, median)
  
  # fit the cforest models to all data to extract variable importance
  mod_ctmp1 <- train(tl ~ len +
                       ord +
                       edhd + mobd + jlhd + cfdcpd +
                       fresh +
                       ecoregion +
                       stream,
                     data = sp_guild,
                     method = mod_list[6],
                     trControl = fitControl)
  mod_ctmp2 <- train(tl ~ len +
                       fresh +
                       ecoregion + 
                       stream,
                     data = sp_guild,
                     method = mod_list[6],
                     trControl = fitControl)
  mod_ctmp3 <- train(tl ~ len +
                       ord +
                       fresh +
                       ecoregion +
                       stream,
                     data = sp_guild,
                     method = mod_list[6],
                     trControl = fitControl)
   
  fitted_rf_guilds[[k]][, 1] <- predict(mod_ctmp1)
  fitted_rf_guilds[[k]][, 2] <- predict(mod_ctmp2)
  fitted_rf_guilds[[k]][, 3] <- predict(mod_ctmp3)
  
}

# analyse JlHd as a function of length
mod_jlhd <- stan_lmer(jlhd ~ (len | ord) +
                        (len | ecoregion) +
                        (len | fresh) +
                        stream * len,
                      data = sp_data,
                      iter = 5000,
                      cores = 1)

# jlhd model but with cforest
mod_ctree_jlhd <- train(jlhd ~ len + ord + 
                          fresh + ecoregion + stream,
                        data = sp_data,
                        method = mod_list[6],
                        trControl = fitControl)
ctree_jlhd_vimp <- varImp(mod_ctree_jlhd, scale = FALSE)

# partial dependence
var_list <- c("len")
data_pd <- sp_data[, colnames(sp_data) == "len"]
pd_jlhd <- partial_dependence(mod_ctree_jlhd$finalModel,
                              vars = "len",
                              data = data_pd)

save.image(file = "outputs/fitted-models-body-mass-rm-low-tp.RData")
