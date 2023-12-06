###Data import, cleaning, and manipulation

# import libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggcorrplot)

# import data
df <- read.csv("archive/cbb.csv")

# clean data

# seperate data for teams that made the tournament and those that did not
df_tourney <- df %>% 
        filter(POSTSEASON != "N/A")
        tail(df_tourney)

df_no_tourney <- df %>%
        filter(POSTSEASON == "N/A")
        head(df_no_tourney)

# statistical summary of all teams
summary(df)

# create correlation matrix for all teams
cor_matrix <- df %>% 
  select(-c(TEAM, CONF, POSTSEASON)) %>% 
  mutate_if(is.character, as.numeric) %>% 
  cor(use = "complete.obs")

#visualize correlation matricies
print(cor_matrix)

#plot correlation matrix
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle", colors = c("tomato2", "white", "springgreen3"), title = "Correlation Matrix of Tournament Teams")


================================================================================
# regression analysis
###Regression analysis of tournament teams

# import data
df <- read.csv("archive/cbb.csv")

# clean data

# seperate data for teams that made the tournament and those that did not
#add a column for seed as a integer
df$SEED <- as.integer(df_tourney$SEED)
head(df_tourney)

#add a column for postseason as a factor
df$POSTSEASON <- factor(df_tourney$POSTSEASON)
head(df_tourney)

#create a column for made post season or not (1 or 0)
df$MDE <- ifelse(is.na(df$POSTSEASON) | df$POSTSEASON %in% c("N/A", "NA"), 0, 1)

# statistical summary of all teams
summary(df)

# create correlation matrix for all teams
cor_matrix <- df %>% 
  select(-c(TEAM, CONF, POSTSEASON, YEAR)) %>% 
  mutate_if(is.character, as.numeric) %>% 
  cor(use = "complete.obs")

#plot correlation matrix
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle", colors = c("tomato2", "white", "springgreen3"), title = "Correlation Matrix of Tournament Teams")

================================================================================

# regression analysis

#convert post season to factor
df$MDE <- factor(df$MDE)

print(df$MDE)

# create logistic regression model
logit_model <- glm(MDE ~ ADJOE + ADJDE + DRB + ADJ_T  , data = df, family = "binomial")

# summary of logistic regression model
summary(logit_model)

#display logistic regression model as table
library(stargazer)
stargazer(logit_model, type = "text")

================================================================================

#Create a prediction model by creating a training and test set
#set seed
set.seed(123)

#split data into training and test set
train <- sample(1:nrow(df), 0.8*nrow(df))
train_set <- df[train,]
test_set <- df[-train,]

# create logistic regression model
logit_model <- glm(MDE ~ ADJOE + ADJDE + DRB + ADJ_T, data = train_set, family = "binomial")

# summary of logistic regression model
summary(logit_model)


#display logistic regression model as table
library(stargazer)
stargazer(logit_model, type = "text")

#predict on test set
pred <- predict(logit_model, test_set, type = "response")

#convert to binary
pred <- ifelse(pred > 0.5, 1, 0)

print(pred)

#confusion matrix
table(test_set$MDE, pred)

#label confusion matrix
library(caret)
confusionMatrix(table(test_set$MDE, pred))

#accuracy
mean(test_set$MDE == pred)

#precision
mean(test_set$MDE == pred & pred == 1)

#recall
mean(test_set$MDE == pred & test_set$MDE == 1)

#f1 score
f1_score <- function(x, y){
  precision <- mean(x == y & y == 1)
  recall <- mean(x == y & x == 1)
  return(2 * precision * recall / (precision + recall))
}

f1_score(test_set$MDE, pred)

#ROC curve
library(ROCR)
pred <- prediction(pred, test_set$MDE)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#AUC
auc <- performance(pred, "auc")

#plot ROC curve with AUC
plot(perf)
abline(a = 0, b = 1)
text(0.5, 0.5, paste("AUC = ", round(auc, 2)))

================================================================================

#create a classification tree
library(rpart)

#set seed
set.seed(123)

#split data into training and test set
train <- sample(1:nrow(df), 0.8*nrow(df))

train_set <- df[train,]
test_set <- df[-train,]

# create classification tree
tree <- rpart(MDE ~ ADJOE + ADJDE + DRB + ADJ_T, data = train_set, method = "class")

#plot tree
plot(tree)
text(tree)
#label tree with numbers

#plot tree with rpart.plot
library(rpart.plot)
rpart.plot(tree)

#summary of tree
summary(tree)



#predict on test set
pred <- predict(tree, test_set, type = "class")

#confusion matrix
table(test_set$MDE, pred)

#label confusion matrix
library(caret)
confusionMatrix(table(test_set$MDE, pred))

#accuracy
mean(test_set$MDE == pred)

#precision
mean(test_set$MDE == pred & pred == 1)

#recall
mean(test_set$MDE == pred & test_set$MDE == 1)

#f1 score
f1_score <- function(x, y){
  precision <- mean(x == y & y == 1)
  recall <- mean(x == y & x == 1)
  return(2 * precision * recall / (precision + recall))
}

f1_score(test_set$MDE, pred)

#ROC curve
library(ROCR)

pred <- prediction(pred, test_set$MDE)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#AUC
auc <- performance(pred, "auc")

#plot ROC curve with AUC
plot(perf)
abline(a = 0, b = 1)
text(0.5, 0.5, paste("AUC = ", round(auc, 2)))

================================================================================

#random forest
library(randomForest)

#set seed
set.seed(123)

#split data into training and test set
train <- sample(1:nrow(df), 0.8*nrow(df))

train_set <- df[train,]
test_set <- df[-train,]

# create random forest
rf <- randomForest(MDE ~ ADJOE + ADJDE + DRB + ADJ_T, data = train_set, ntree = 100)

#summary of random forest
summary(rf)

#predict on test set
pred <- predict(rf, test_set, type = "class")

#confusion matrix
table(test_set$MDE, pred)

#label confusion matrix
library(caret)
confusionMatrix(table(test_set$MDE, pred))

#accuracy
mean(test_set$MDE == pred)

#precision
mean(test_set$MDE == pred & pred == 1)

#recall
mean(test_set$MDE == pred & test_set$MDE == 1)

#f1 score
f1_score <- function(x, y){
  precision <- mean(x == y & y == 1)
  recall <- mean(x == y & x == 1)
  return(2 * precision * recall / (precision + recall))
}

f1_score(test_set$MDE, pred)

