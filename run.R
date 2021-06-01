setwd("~/R/bike")
library(knitr)
library(doParallel)
library(dplyr)
library(checkmate)
library(xtable)
library(reshape2)
library(data.table)
library(ggplot2)
library(h2o)
library(randomForest)

library(lubridate)

source("prepare.R")

#pdfName <- "analysis_raw_data"

#get data

data <- read.csv2("bike-sharing-demand/train.csv", sep=",")
#predictData <- read.csv2("datadrivenproject/data/rawData/testSetValues.csv", sep=",")

data <- as.data.table(data)

colSums(is.na(data))

data <- prepareData(data)

#model----------------------------------

h2o.init()

autoModel <- h2o.automl(x = names(data)[-c(1,10,11)], y = "count",
                        training_frame = as.h2o(data[,c(-1,-11)]),
                        seed = 1)

lb <- autoModel@leaderboard
print(lb, n = nrow(lb)) 

model <- h2o.getModel(autoModel@leaderboard[3, 1])

model <- h2o.gbm(x = names(data)[-c(1,10,11)], y = "count",
                 training_frame = as.h2o(data[,c(-1,-11)]),nfolds = 5,
                 col_sample_rate = 0.82, learn_rate = 0.12,
                 max_depth = 9, ntrees = 75
                 )

#randomGrid------------------------------------

hyper_params = list(learn_rate = seq(0.05, 0.2, 0.01),
                    max_depth = seq(8, 12, 1),
                    col_sample_rate = seq(0.8, 1.0, 0.01),
                    ntrees = seq(60, 74, 2)
                    )

search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 1000, stopping_metric = "AUTO", stopping_tolerance = 0.00001, stopping_rounds = 5, seed = 123476)


drf.grid <- h2o.grid("gbm",
                     grid_id = "mygrid",
                     x = names(data)[-c(1,10,11)],
                     y = "count",
                     
                     # faster to use a 80/20 split
                     training_frame = as.h2o(data[,c(-1,-11)]),
                     #validation_frame = as.h2o(v),
                     nfolds = 5,
                     
                     # alternatively, use N-fold cross-validation
                     #training_frame = train,
                     #nfolds = 5,
                     
                     ## stop as soon as mse doesn't improve by more than 0.1% on the validation set,
                     ## for 2 consecutive scoring events
                     #stopping_rounds = 2,
                     #stopping_tolerance = 1e-3,
                     #stopping_metric = "MSE",
                     
                     #score_tree_interval = 100, ## how often to score (affects early stopping)
                     seed = 123476,  ## seed to control the sampling of the Cartesian hyper-parameter space
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

drf.sorted.grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "r2")
print(drf.sorted.grid)





#prediction------------------------------

pred <- as.data.frame(h2o.predict(model,as.h2o(data[,c(-1,-10,-11)])))

pred[pred <= 0] <- 0
pred <- round(pred)
