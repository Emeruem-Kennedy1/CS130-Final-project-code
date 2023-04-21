library(stargazer)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(texreg)
library(separationplot)
library(MatchIt)
library(cem)
library(stringr)
library(cobalt)
library(performance)
library(see)
library(lubridate)
library(dplyr)
library(lmtest)
library(sandwich)
library(modelsummary)
library(hrbrthemes)
library(ggpubr)
library(readxl)
library(xtable)

## Load Data
read.csv("Downloads/dataverse_files (6)/final_dataset_submission.csv") -> backlashdata


## DESCRIPTIVE ANALYSIS

#see also file "Figures_Resubmission.RMD"


#Data summary

backlashdata %>% dplyr::select(backlash, leader_arrest, mass_arrest_12, organization_membership, Repression_10days, Repression_50days, share_income_food_log, cult_date, pre_election_period, post_election_period, context_protest_quant, ongoing_armedconflict, avg_income_pc_logged, fs_Security_Apparatus) -> backlashdata_reduced

backlashdata_reduced <- as.data.frame(backlashdata_reduced)
# stargazer(backlashdata_reduced)

### Arrests by group
arrestsbygroup <- backlashdata %>% 
  group_by(identitygroup) %>% 
  count()

### Unique identity groups
identitygroups <- backlashdata$identitygroup %>% unique() %>% sort()
# stargazer(identitygroups)

### Backlashes by identitygroups
onlyback <- filter(backlashdata, backlash == 1)
backlashes_by_group <- onlyback  %>% group_by(identitygroup) %>% count()
onlynoback <- filter(backlashdata, backlash == 0)
nobacklashes_by_group <- onlynoback   %>% group_by(identitygroup) %>% count()
identitygroup <- unique(backlashdata$identitygroup)
identitygroup <- as.data.frame(identitygroup)
identitygroup$test <- 1
add1 <- merge(identitygroup, backlashes_by_group, by = "identitygroup", all.x = T, all.y = F)
add1$test <- NULL
add1 <- rename(add1, backlashes = n)
add1$backlashes <- ifelse(is.na(add1$backlashes), 0, add1$backlashes)
add2 <- merge(add1, nobacklashes_by_group, by = "identitygroup", all.x = T)
add2 <- rename(add2, no_backlashes = n)
add2$no_backlashes <- ifelse(is.na(add2$no_backlashes), 0, add2$no_backlashes)
xtable(add2, digits = 0)
uyears <- onlyback %>% group_by(identitygroup, year) %>% count()

### Dependent variable
table(backlashdata$backlash)
backlashdata$backlash_factor <- as.factor(backlashdata$backlash)
levels(backlashdata$backlash_factor) <- c("No Backlash", "Backlash")
## pdf(file = "descriptive.pdf")
p1 <- ggplot(data = backlashdata, mapping = aes(x = backlash_factor)) +
  geom_bar(width = 0.6) +
  ylab("# of arrests") +
  xlab("") +
  theme_minimal() 
p_dt <- layer_data(p1)
p1 + annotate(geom = "text", label = p_dt$count, x = p_dt$x, y = p_dt$y + 20)
## dev.off()

### Stacked bar chart
backlashdata_notna <- backlashdata[!is.na(backlashdata$cowc), ]
## pdf(file = "stackedbarchart.pdf", width=18, height=8)
ggplot(data = backlashdata_notna) + ylab("Share of arrests with backlash by country") +
  xlab("") + scale_fill_manual(values = c("skyblue1","red")) + 
  geom_bar(mapping = aes(x = cowc, fill = backlash_factor), position = "fill") +
  theme_minimal() + theme(legend.title = element_blank())
## dev.off()

# Cases that are neither mass arrest nor leader arrest
backlashdata$different_arrest <- ifelse(backlashdata$mass_arrest_12 == 0 & 
                                          backlashdata$leader_arrest == 0, 1, 0)
backlashdata$both_arrests <- ifelse(backlashdata$mass_arrest_12 == 1 & 
                                      backlashdata$leader_arrest == 1, 1, 0)

table(backlashdata$different_arrest)
table(backlashdata$mass_arrest_12)
table(backlashdata$leader_arrest)
table(backlashdata$both_arrests)
crosstab <- table(backlashdata$mass_arrest_12, backlashdata$leader_arrest)
crosstab
addmargins(crosstab)

# Contigency table arrest types by backlash
t1 <- table(backlash = backlashdata$backlash, leader_arrest = backlashdata$leader_arrest)
addmargins(t1)

t1 <- xtabs(~ mass_arrest_25+leader_arrest + backlash, data=backlashdata)
ftable(t1)
# stargazer(ftable(t1)) 




## DATA PREP



#### Prepare variables for analysis
class(backlashdata$share_income_food_log)
backlashdata$share_income_food_log <- as.numeric(backlashdata$share_income_food_log)
class(backlashdata$cult_date)
backlashdata$cult_date <- as.numeric(backlashdata$cult_date)
class(backlashdata$sharerecent_repression_1)
backlashdata$sharerecent_repression_1 <- as.numeric(backlashdata$sharerecent_repression_1)
class(backlashdata$sharerecent_repression_2)
backlashdata$sharerecent_repression_2 <- as.numeric(backlashdata$sharerecent_repression_2)
class(backlashdata$sharerecent_repression_3)
backlashdata$sharerecent_repression_3 <- as.numeric(backlashdata$sharerecent_repression_3)
class(backlashdata$fs_index_total)
backlashdata$fs_index_total <- as.numeric(backlashdata$fs_index_total)
class(backlashdata$fs_Security_Apparatus)
backlashdata$fs_Security_Apparatus <- as.numeric(backlashdata$fs_Security_Apparatus)
class(backlashdata$fs_Economic_Inequality)
backlashdata$fs_Economic_Inequality<- as.numeric(backlashdata$fs_Economic_Inequality)
class(backlashdata$fs_PublicServices)
backlashdata$fs_PublicServices <- as.numeric(backlashdata$fs_PublicServices)
class(backlashdata$fs_DemographicPressures)
backlashdata$fs_DemographicPressures <- as.numeric(backlashdata$fs_DemographicPressures)
class(backlashdata$v2x_polyarchy)
backlashdata$v2x_polyarchy <- as.numeric(backlashdata$v2x_polyarchy)
class(backlashdata$fs_StateLegitimacy)
backlashdata$fs_StateLegitimacy <- as.numeric(backlashdata$fs_StateLegitimacy)
class(backlashdata$v2peapssoc)
backlashdata$v2peapssoc <- as.numeric(backlashdata$v2peapssoc)

# Create binary organization membership-variable 
backlashdata$organization_membership_binary <- ifelse(backlashdata$organization_membership > 0, 1, 0)



#rename variables


rename(backlashdata, context_protest = context_protest_quant) -> backlashdata

rename(backlashdata, democracy = v2x_polyarchy) -> backlashdata

rename(backlashdata, exclusion = v2peapssoc) -> backlashdata

rename(backlashdata, stateness = fs_StateLegitimacy) -> backlashdata

rename(backlashdata, cult_event = cult_date) -> backlashdata

rename(backlashdata, security_apparatus = fs_Security_Apparatus) -> backlashdata

rename(backlashdata, repression_10days = Repression_10days) -> backlashdata

rename(backlashdata, repression_50days = Repression_50days) -> backlashdata

### 5 models from the paper

pooled1 <- glm(backlash ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period  + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict, family = binomial(link = logit), data = backlashdata)

pooled1_cl <- coeftest(pooled1, vcov = vcovCL, cluster = ~identitygroup)



#country fe

countryfe1 <- glm(backlash ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + factor(country), family = binomial(link = logit), data = backlashdata)

countryfe1_cl <- coeftest(countryfe1, vcov = vcovCL, cluster = ~identitygroup)


#group fe
groupfe1 <- glm(backlash ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + factor(identitygroup), family = binomial(link = logit), data = backlashdata)

groupfe1_cl <- coeftest(groupfe1, vcov = vcovCL, cluster = ~identitygroup)


#country-year fe
countryearfe1 <- glm(backlash ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + factor(country) + factor(year), family = binomial(link = logit), data = backlashdata)

countryearfe1_cl <- coeftest(countryearfe1, vcov = vcovCL, cluster = ~identitygroup)


#group-year fe
groupyearfe1 <- glm(backlash ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + factor(identitygroup) + factor(year), family = binomial(link = logit), data = backlashdata)

groupyearfe1_cl <- coeftest(groupyearfe1, vcov = vcovCL, cluster = ~identitygroup)


#or modelsummary
models <- list(pooled1, countryfe1, groupfe1, countryearfe1, groupyearfe1)




#### Using a tree based method to predict backlash

#### remove NAs from the data for the important variables

library(tree)

# split the data into a training and test set
set.seed(123)
train <- sample(1:nrow(backlashdata), 0.8*nrow(backlashdata))
backlashdata_train <- backlashdata[train,]
backlashdata_test <- backlashdata[-train,]

# translating all 5 models to a tree based method
pooled1_tree <- tree(as.factor(backlash) ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period  + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict, data = backlashdata_train)

countryfe1_tree <- tree(as.factor(backlash) ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + country, data = backlashdata_train)

groupfe1_tree <- tree(as.factor(backlash) ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + identitygroup, data = backlashdata_train)

countryearfe1_tree <- tree(as.factor(backlash) ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + country + year, data = backlashdata_train)

groupyearfe1_tree <- tree(as.factor(backlash) ~ leader_arrest + mass_arrest_12 + repression_10days + repression_50days + share_income_food_log + cult_event + pre_election_period + post_election_period + context_protest + democracy + security_apparatus + avg_income_pc_logged + ongoing_armedconflict + identitygroup + year, data = backlashdata_train)

# Showing the accuracy of the models using the bootstrap sample
pooled1_tree_boot <- predict(pooled1_tree, backlashdata_test, type = "class")
print(paste("pooled model training accuracy:", mean(pooled1_tree_boot == backlashdata_train$backlash)))
print(paste("pooled model test accuracy:", mean(pooled1_tree_boot == backlashdata_test$backlash)))

countryfe1_tree_boot <- predict(countryfe1_tree, backlashdata_test, type = "class")
print(paste("country fe model training accuracy:", mean(countryfe1_tree_boot == backlashdata_train$backlash)))
print(paste("country fe model test accuracy:", mean(countryfe1_tree_boot == backlashdata_test$backlash)))

groupfe1_tree_boot <- predict(groupfe1_tree, backlashdata_test, type = "class")
print(paste("group fe model training accuracy:", mean(groupfe1_tree_boot == backlashdata_train$backlash)))
print(paste("group fe model test accuracy:", mean(groupfe1_tree_boot == backlashdata_test$backlash)))

countryearfe1_tree_boot <- predict(countryearfe1_tree, backlashdata_test, type = "class")
print(paste("country-year fe model training accuracy:", mean(countryearfe1_tree_boot == backlashdata_train$backlash)))
print(paste("country-year fe model test accuracy:", mean(countryearfe1_tree_boot == backlashdata_test$backlash)))

groupyearfe1_tree_boot <- predict(groupyearfe1_tree, backlashdata_test, type = "class")
print(paste("group-year fe model training accuracy:", mean(groupyearfe1_tree_boot == backlashdata_train$backlash)))
print(paste("group-year fe model accuracy:", mean(groupyearfe1_tree_boot == backlashdata_test$backlash)))