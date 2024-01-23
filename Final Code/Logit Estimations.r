if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# set the working directory
setwd("C:/Users/user/OneDrive/Desktop/Masters/Semester 05 - MGT-6203-Data Analytics in Business/Project/Code")

#data file needs to be in the wd
df <- read.table(file = 'CTDC_synthetic_20210825.tsv', sep = '\t', header = TRUE)
head(df)

df <- df %>%
  mutate(index = row_number()) %>%
  mutate_all(~ifelse(. == "", NA, .)) %>%
  drop_na(yearOfRegistration) %>%
  mutate(index = row_number()) %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(majorityStatusAtExploit)) %>%
  mutate(index = row_number()) %>%
  filter(gender == "Male" | gender == "Female") %>%
  mutate(isSexualExploit = ifelse(is.na(isSexualExploit), 0, isSexualExploit)) %>%
  mutate(isForcedLabour = ifelse(is.na(isForcedLabour), 0, isForcedLabour)) %>%
  mutate(recruiterRelationIntimatePartner = ifelse(is.na(recruiterRelationIntimatePartner), 0, recruiterRelationIntimatePartner)) %>%
  mutate(recruiterRelationFriend = ifelse(is.na(recruiterRelationFriend), 0, recruiterRelationFriend)) %>%
  mutate(recruiterRelationFamily = ifelse(is.na(recruiterRelationFamily), 0, recruiterRelationFamily))

head(df)

heatmap_data <- df %>%
  filter(!is.na(citizenship)) %>%
  filter(!is.na(CountryOfExploitation)) %>%
  group_by(citizenship, CountryOfExploitation) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(heatmap_data, aes(x = citizenship, y = CountryOfExploitation, fill = log(count))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#000080") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

model1 <- glm(isSexualExploit ~ gender + majorityStatusAtExploit + gender*majorityStatusAtExploit , df, family = "binomial")
summary(model1)

# getting the odds ratio from each coefficient, except the intercept
(exp(model1$coefficients[-1])-1) * 100

model2 <- glm(isForcedLabour ~ gender + majorityStatusAtExploit + gender*majorityStatusAtExploit , df, family = "binomial")
summary(model2)

#We don't have labels for all the cases so for this case we filtered to have a df with valid data
df2 <- df %>%
  filter(recruiterRelationIntimatePartner==1|recruiterRelationFriend==1|recruiterRelationFamily==1)

head(df2)

model3 <- glm(recruiterRelationIntimatePartner ~ gender + majorityStatusAtExploit , df2, family = "binomial")
summary(model3)

model4 <- glm(recruiterRelationFriend ~ gender + majorityStatusAtExploit + gender*majorityStatusAtExploit , df2, family = "binomial")
summary(model4)

model5 <- glm(recruiterRelationFamily ~ majorityStatusAtExploit , df2, family = "binomial")
summary(model5)


