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

#add a column for seed as a integer
df_tourney$SEED <- as.integer(df_tourney$SEED)
head(df_tourney)

#add a column for postseason as a factor
df_tourney$POSTSEASON <- factor(df_tourney$POSTSEASON)
head(df_tourney)

# statistical summary of all teams
summary(df)

# create correlation matrix for all teams
cor_matrix <- df %>% 
  select(-c(TEAM, CONF, POSTSEASON)) %>% 
  mutate_if(is.character, as.numeric) %>% 
  cor(use = "complete.obs")

#visualize correlation matricies

#plot correlation matrix
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle", colors = c("tomato2", "white", "springgreen3"), title = "Correlation Matrix of Tournament Teams")

# regression analysis

#convert post season to factor
df_tourney$POSTSEASON <- factor(df_tourney$POSTSEASON)

# create logistic regression model
logit_model <- glm(POSTSEASON ~ ADJOE + ADJDE + BARTHAG + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + WAB + SEED, data = df_tourney, family = "binomial")

# summary of logistic regression model
summary(logit_model)

#display logistic regression model as table
library(stargazer)
stargazer(logit_model, type = "text")



