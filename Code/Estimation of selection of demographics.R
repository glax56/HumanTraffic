library(dplyr)
library(stargazer)

setwd("C:/Users/user/OneDrive/Desktop/Masters/Semester 05 - MGT-6203-Data Analytics in Business/Project/Code")

df <- read.csv("logit_data.csv")

head(df)

#Filtering desired genders
df <- df %>%
  filter(gender == "Male" | gender == "Female") %>%
  mutate(isSexualExploit = ifelse(is.na(isSexualExploit), 0, isSexualExploit)) %>%
  mutate(isForcedLabour = ifelse(is.na(isForcedLabour), 0, isForcedLabour)) 

model1 <- glm(isSexualExploit ~ gender + majorityStatusAtExploit + gender*majorityStatusAtExploit , df, family = "binomial")
summary(model1)

# getting the odds ratio from each coefficient, except the intercept
(exp(model1$coefficients[-1])-1) * 100
