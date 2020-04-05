# TA: Leslie Huang
# Course: Text as Data
# Date: 03/26/2020

# Lab adapted from: Kevin Munger, Patrick Chester, Leslie Huang, Pedro L. Rodríguez

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

set.seed(1234)

# load required libraries
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)

# set working directory
setwd("/Users/lesliehuang/TextasDataLabSpring2020/W8_03_26_20")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds") # same data as last week, but we will look at different categories
table(news_data$category)

# let's work with 2 categories
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  sample_n(500) %>%  # sample 500 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# remember that the order of operations matters! We first select category, group by, and then sample 500 obs

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------
library(quanteda)

# create document feature matrix, actually a MATRIX object this time!
# keep tokens that appear in at least 5 headlines
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% 
  dfm_trim(min_termfreq = 5) %>% 
  convert("matrix")

ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
mtry <- sqrt(ncol(train_x))  # number of features to sample at each split
ntree <- 51  # num of trees to grow
# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")

system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree, mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

?predict.randomForest

predict_test <- predict(rf.base, newdata = test_x)
confusionMatrix(data = predict_test, reference = test_y)

# to tune hyperparameters, use:
# ?tuneRF


#----------------------------------------
# 4. 5-Fold CV RandomForest Using Caret            ---
#----------------------------------------
# note that the RF model in caret calls randomForest, but it's wrapped in caret

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree <- 51  
tunegrid <- expand.grid(.mtry = mtry)
system.time(rf.caret <- train(x = train_x, y = train_y, 
                              method = "rf", metric = metric, 
                              tuneGrid = tunegrid, trControl = trainControl,
                              ntree = ntree)
            )

# print results
print(rf.caret)

rf_predict <- predict(rf.caret, newdata = test_x)
confusionMatrix(rf_predict, reference = test_y)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")


#----------------------------------------
# 4.5 Follow up from last time
#----------------------------------------
# how is the CV best model selected?

# function used to select the optimal tuning parameter
trainControl$selectionFunction
?best

#----------------------------------------
# 5. RandomForest Using Caret + tuning   ---
#----------------------------------------
# we are going to gridsearch over 1 parameter: mtry

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  # at the moment caret only allows tuning of mtry (partly b/c ntree is just a matter of computational constratints)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = ntree)
            )
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)

#----------------------------------------
# 6. RandomForest Using Caret + manual tuning ---
#----------------------------------------
# we have one value for mtry and we will train 3 models with different values for ntree

tunegrid <- expand.grid(.mtry = mtry)

# ntree = 1
system.time(rf.man1 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 1))

# ntree = 5
system.time(rf.man2 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 5))

# ntree = 51
system.time(rf.man3 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 51))

# collect results & summarize
results <- resamples(list(rf1 = rf.man1, rf5 = rf.man2, rf51 = rf.man3))
summary(results)

# test set accuracy
(cm <- confusionMatrix(predict(rf.man1, newdata = test_x), test_y))
# access the components of the results with the $ operator
cm$table
cm$overall

confusionMatrix(predict(rf.man2, newdata = test_x), test_y)
confusionMatrix(predict(rf.man3, newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

# reminder: Kappa = Cohen's Kappa, compares observed accuracy with expected accuracy (think: baseline accuracy)
