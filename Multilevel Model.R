# Libraries required 
library(lme4)
library(tidyverse)
library(dplyr)
library(broom.mixed)
library(ggplot2)
library(lmerTest)

# Reading Merged Dataset
data <- read_csv("all_movies_and_cast.csv")

# Exploratory Data Analysis on Movies Dataset
str(data)
summary(data)

## Removing missing values 
colSums(is.na(data))
data = na.omit(data)
colSums(is.na(data))

## Invalid movies (i.e. with a runtime of 0)
nrow(data)
data <- data[!(data$runtime == 0),]
nrow(data)

## Replacing zero values in budget and revenue columns with its median
data$budget[data$budget == 0] <- median(data$budget, na.rm = TRUE)
data$revenue[data$revenue == 0] <- median(data$revenue, na.rm = TRUE)

## Remove Gender with value 0 (Not Specified)
nrow(data)
data <- data[!(data$gender == 0),]
nrow(data)

## Set Gender as Factors
data$gender <- as.factor(ifelse(data$gender == 1, "Female", "Male"))

## Convert Column Vote_average as Factors
data$vote_average <- as.factor(data$vote_average)
class(data$vote_average)

# Data Visualisation 
ggplot(data = data, aes(x = runtime, y = cast_popularity, col = vote_average, group = vote_average)) +
  geom_point(size = 1.2, alpha = 0.8, position = "jitter") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8) +
  labs(title = "Cast Popularity against Runtime",
       subtitle = "add regression lines for different vote averages")

ggplot(data = data, aes(x = budget, y = cast_popularity, col = vote_average, group = vote_average)) +
  geom_point(size = 1.2, alpha = 0.8, position = "jitter") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8) +
  labs(title = "Cast Popularity against Budget",
       subtitle = "add regression lines for different vote averages")

ggplot(data = data, aes(x = popularity, y = cast_popularity, col = vote_average, group = vote_average)) +
  geom_point(size = 1.2, alpha = 0.8, position = "jitter") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8) +
  labs(title = "Cast Popularity against Movie Popularity",
       subtitle = "add regression lines for different vote averages")

ggplot(data = data, aes(x = order, y = cast_popularity, col = vote_average, group = vote_average)) +
  geom_point(size = 1.2, alpha = 0.8, position = "jitter") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8) +
  labs(title = "Cast Popularity against Billing Order",
       subtitle = "add regression lines for different vote averages")

# Multilevel Model Analysis 
## Intercept only model 
interceptonlymodel <- lmer(cast_popularity ~ 1 + (1 | vote_average), data = data)
summary(interceptonlymodel)
### Residual variance on the movie (vote_average) level is 0.7019
### Residual variance on the first level (cast level) is 17.7029
### Estimate of the intercept is 3.0634

## Model 1 
lmer_model1 <- lmer(cast_popularity ~ gender + order + (1 | vote_average), data = data)
summary(lmer_model1)


## Rescale budget (and revenue) to make the model more stable 
scaleddata <- data %>%
  mutate(budget_scaled = scale(budget), revenue_scaled = scale(revenue))

## Model 2 
lmer_model2 <- lmer(cast_popularity ~ order + runtime + popularity + budget_scaled + revenue_scaled + (1 | vote_average), data = scaleddata)
summary(lmer_model2)

## Model 3 
lmer_model3 <- lmer(cast_popularity ~ order + runtime + budget_scaled + revenue_scaled + (1 | vote_average), data = scaleddata)
summary(lmer_model3)

## Model 4 - Model 2 with random slopes 
lmer_model4 <- lmer(cast_popularity ~ order + runtime + popularity + budget_scaled + revenue_scaled + (1 + order | vote_average), data = scaleddata)
summary(lmer_model4)


## Model 5 - Model 3 with random slopes 
lmer_model5 <- lmer(cast_popularity ~ order + runtime + budget_scaled + revenue_scaled + (1 + order | vote_average), data = scaleddata)
summary(lmer_model5)


## Model 6 (Random Slopes)
lmer_model6 <- lmer(cast_popularity ~ order + runtime + budget_scaled + revenue_scaled + (1 + order + runtime + budget_scaled + revenue_scaled | vote_average), data = scaleddata)
summary(lmer_model6)


## Model 7 (Random Slopes)
lmer_model7 <- lmer(cast_popularity ~ order + runtime + budget_scaled + revenue_scaled + (1 + budget_scaled + revenue_scaled | vote_average), data = scaleddata)
summary(lmer_model7)
coef(lmer_model7)

## Model 8 (Cross-level interaction model) 
lmer_model8 <- lmer(cast_popularity ~ order + runtime + budget_scaled + revenue_scaled + order*runtime + order*budget_scaled + order*revenue_scaled + (1 + budget_scaled + revenue_scaled | vote_average), data = scaleddata)
summary(lmer_model8)

# Conclusion
ggplot(data = scaleddata, aes(x = budget, y = cast_popularity, col = vote_average)) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.7, alpha = 0.8, position = "jitter") +
  geom_smooth(method = lm, se = FALSE, size = 2, alpha = 0.8) +
  theme_minimal()

ggplot(data = scaleddata, aes(x = runtime, y = cast_popularity, col = vote_average)) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.7, alpha = 0.8, position = "jitter") +
  geom_smooth(method = lm, se = FALSE, size = 2, alpha = 0.8) +
  theme_minimal()
