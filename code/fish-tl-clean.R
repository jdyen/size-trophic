# where am I working?
setwd("~/Dropbox/research/size-trophic/")

# the caret package should make our life easy
library(caret)
library(rstanarm)
library(edarf)

# source helper functions
source("code/helper-functions.R")

# load a pre-compiled data set
sp_data <- readRDS("data/full-data-set.rds")

# try a general train-and-test partition
set.seed(2018-12-14)
n_cv <- 10
holdouts <- createFolds(sp_data$len, k = n_cv, list = TRUE, returnTrain = FALSE)

# how do we want to tune the model?
fitControl <- trainControl(method = "cv", number = 10)

# fit a random forest regression model
# alternative models: "bagEarth", "treebag", "xgbTree", "gaussprRadial", "bstTree"
method <- "cforest"
predictions <- rep(NA, nrow(sp_data))
for (i in seq_len(n_cv)) {
  
  # split up test and train data sets for this fold
  training <- sp_data[-holdouts[[i]], ]
  testing <- sp_data[holdouts[[i]], ]
  
  # fit a model trained with general settings
  mod_tmp <- train(tl ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                     invasive + fresh,
                   data = training,
                   method = method,
                   trControl = fitControl)
  
  # give me the predictions
  predictions[holdouts[[i]]] <- predict(mod_tmp, newdata = testing)
  
}

# fit a random forest classification model
predictions_categorical <- rep(NA, nrow(sp_data))
for (i in seq_len(n_cv)) {
  
  # split up test and train data sets for this fold
  training <- sp_data[-holdouts[[i]], ]
  testing <- sp_data[holdouts[[i]], ]
  
  # fit a model trained with general settings
  mod_tmp <- train(guild ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                     invasive + fresh,
                   data = training,
                   method = method,
                   trControl = fitControl)
  
  # give me the predictions
  predictions_categorical[holdouts[[i]]] <- as.character(predict(mod_tmp, newdata = testing))
  
}

# fit full random forest models to estimate in-sample fit and variable importance
mod_ctree <- train(tl ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                     invasive + fresh,
                   data = sp_data,
                   method = method,
                   trControl = fitControl)
mod_ctree_guild <- train(guild ~ len + ord + edhd + mobd + jlhd + cfdcpd +
                           invasive + fresh,
                         data = sp_data,
                         method = method,
                         trControl = fitControl)

# calculate variable importance for both random forest models
ctree_vimp <- varImp(mod_ctree, scale = FALSE)
ctree_guild_vimp <- varImp(mod_ctree_guild, scale = FALSE)

# estimate partial dependence on each continuous variable in random forest models
var_list <- c("len", "edhd", "mobd", "jlhd", "cfdcpd")
pd_regress <- pd_class <- vector("list", length = length(var_list))
for (i in seq_along(var_list)) {
  data_pd <- sp_data[, colnames(sp_data) %in% var_list]
  pd_regress[[i]] <- partial_dependence(mod_ctree$finalModel, vars = var_list[i],
                                        data = data_pd)
  pd_class[[i]] <- partial_dependence(mod_ctree_guild$finalModel, vars = var_list[i],
                                      data = data_pd)
}

# fit a Bayesian mixed model of tl against all predictors
mod_stan <- stan_lmer(tl ~ len + (len | ord) + edhd + mobd + jlhd + cfdcpd + invasive + fresh,
                      data = sp_data,
                      iter = 5000,
                      cores = 4)

# run 10-fold CV on the stan models
predictions_stan <- rep(NA, nrow(sp_data))
for (i in seq_len(n_cv)) {
  
  # split up test and train data sets for this fold
  training <- sp_data[-holdouts[[i]], ]
  testing <- sp_data[holdouts[[i]], ]
  
  # fit lm models
  mod_tmp <- stan_lmer(tl ~ len + (len | ord) + edhd + mobd + jlhd + cfdcpd + invasive + fresh,
                       data = training,
                       iter = 5000,
                       cores = 4)
  
  # give me the predictions
  stan_pred_tmp <- posterior_predict(mod_tmp, newdata = testing, re.form = NA)
  predictions_stan[holdouts[[i]]] <- apply(stan_pred_tmp, 2, median)
  
}

# calculate in-sample r2 values for models of tl
ctree_predictions <- predict(mod_ctree)
r2_ctree <- cor(ctree_predictions, sp_data$tl) ** 2
naive_stan_pred <- posterior_predict(mod_stan)
stan_predictions <- apply(naive_stan_pred, 2, median)
r2_stan <- cor(stan_predictions, sp_data$tl) ** 2

# convert continuous TL to guilds
predictions_stan_converted <- convert_tl_to_guild(predictions_stan)
predictions_converted <- convert_tl_to_guild(predictions)
predictions_naive_converted <- cbind(convert_tl_to_guild(stan_predictions),
                                     convert_tl_to_guild(ctree_predictions),
                                     as.character(predict(mod_ctree_guild)))

# create confusion matrices from predicted and true guilds
out <- create_confusion_matrix(predictions_categorical,
                               sp_data$guild)
out_converted <- create_confusion_matrix(predictions_converted,
                                         sp_data$guild)
out_stan_converted <- create_confusion_matrix(predictions_stan_converted,
                                              sp_data$guild)
out_naive <- vector("list", length = ncol(predictions_naive_converted))
for (i in seq_len(ncol(predictions_naive_converted))) {
  out_naive[[i]] <- create_confusion_matrix(predictions_naive_converted[, i],
                                            sp_data$guild)
}

# calculate proportion correct from confusion matrices
prop_correct_full <- round(diag(out) / apply(out, 1, sum), 2)
prop_correct_tl <- round(diag(out_converted) / apply(out, 1, sum), 2)
prop_correct_stan <- round(diag(out_stan_converted) / apply(out, 1, sum), 2)
prop_correct_naive <- round(sapply(out_naive, function(x) diag(x) / apply(x, 1, sum)), 2)
table1_data <- cbind(prop_correct_naive[, 1], prop_correct_stan,
                     prop_correct_naive[, 2], prop_correct_tl,
                     prop_correct_naive[, 3], prop_correct_full)
colnames(table1_data) <- c("naive_stan", "cv_stan",
                           "naive_tl", "cv_tl",
                           "naive_class", "cv_class")

# calculate relative importance of all variables
regression_imp <- ctree_vimp$importance / sum(ctree_vimp$importance)
classification_imp <- ctree_guild_vimp$importance / sum(ctree_guild_vimp$importance)
imp_all <- cbind(round(regression_imp, 2), round(classification_imp, 2))
imp_all <- imp_all[order(imp_all[, 1], decreasing = TRUE), ]

# save some outputs for plotting
write.csv(table1_data, file = "outputs/tables/table1_data_r1.csv")
write.csv(imp_all, file = "outputs/tables/importance_values_r1.csv")
saveRDS(stan_predictions, file = "outputs/stan_predictions_r1.rds")
saveRDS(ctree_predictions, file = "outputs/ctree_predictions_r1.rds")
saveRDS(sp_data, file = "data/full-data-set_r1.rds")
saveRDS(mod_stan, file = "outputs/mod_stan_r1.rds")
saveRDS(pd_regress, file = "outputs/pd_regress_r1.rds")
saveRDS(pd_class, file = "outputs/pd_class_r1.rds")
