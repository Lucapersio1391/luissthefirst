# import the dataset
library(rattle)
library(caret)
library(rpart)
library(randomForest)
library(C50)
library(tidyverse)
library(mlbench)
library(gmodels)
# let's import the dataset
churn <- read_delim('Churn_Modelling.csv', delim = ',', col_types = cols(
  RowNumber = col_integer(),
  CustomerId = col_integer(),
  Surname = col_character(),
  CreditScore = col_integer(),
  Geography = col_character(),
  Gender = col_character(),
  Age = col_integer(),
  Tenure = col_integer(),
  Balance = col_double(),
  NumOfProducts = col_integer(),
  HasCrCard = col_integer(),
  IsActiveMember = col_integer(),
  EstimatedSalary = col_double(),
  Exited = col_integer()
))

head(churn)
##### Exploring data

churn %>% 
  group_by(Geography) %>% 
  summarise(n = n(),
            avg_credit_score = mean(CreditScore),
            sd_credit_score = sd(CreditScore))

churn %>% 
  group_by(Exited) %>% 
  summarise(n = n(),
            avg_credit_score = mean(CreditScore),
            sd_credit_score = sd(CreditScore))

churn %>% 
  group_by(NumOfProducts) %>%
  summarise(n = n(),
            avg_credit_score = mean(CreditScore),
            sd_credit_score = sd(CreditScore))

churn %>% 
  group_by(Gender) %>% 
  summarise(n = n(),
            avg_credit_score = mean(CreditScore),
            sd_credit_score = sd(CreditScore))

churn %>% 
  group_by(IsActiveMember) %>%
  summarise(n = n(),
           avg_credit_score = mean(CreditScore),
           sd_credit_score = sd(CreditScore))

churn %>%
  group_by(Geography) %>%
  summarise(n = n(),
  avg_credit_score = mean(CreditScore),
  sd_credit_score = sd(CreditScore))

churn %>%
  group_by(HasCrCard) %>% 
  summarise(n = n(),
  avg_credit_score = mean(CreditScore),
  sd_credit_score = sd(CreditScore))
##### Data Preprocessing (class imbalance, missing values, outliers)

sum(is.na(churn)) #there are no missing values

str(churn)
dim(churn)
summary(churn[c('CreditScore', 'EstimatedSalary')])
IQR(churn$CreditScore)
IQR(churn$EstimatedSalary)

churn %>% ggplot(aes(factor(Exited), CreditScore)) + 
  geom_boxplot(color = 'blue', outlier.colour = 'red', notch = TRUE, notchwidth = 0.1)

churn %>% ggplot(aes(factor(Exited), EstimatedSalary)) + 
  geom_boxplot(color = 'blue', outlier.colour = 'red', notch = TRUE, notchwidth = 0.1)

##### t test for see if the difference between credit score, balance and EstimatedSalary (between guys who exited and guys who did not) is significative

t.test(CreditScore ~ Exited, churn)
t.test(Balance ~ Exited, churn)
t.test(EstimatedSalary ~ Exited, churn)
levels(churn$Exited)


###### categorical variables

CrossTable(churn$Tenure, churn$Exited)
CrossTable(churn$Geography, churn$Exited)
CrossTable(churn$Gender, churn$Exited)


# calculate correlation matrix
cormat <- cor(churn[,c(-2, -3, -7, -8, -9)])
print(cormat)

highlycorrelated <- findCorrelation(cormat, cutoff = 0.5)
# no correlation among features, so no linear model in this case

  
#### set seed and feature selection
# churn <- as.data.frame(churn)
# churn$NumOfProducts <- as.numeric(churn$NumOfProducts)
# churn$Gender <- as.numeric(churn$Gender)
# churn$Geography <- as.numeric(churn$Geography)
# churn$Age <- as.numeric(churn$Age)
# churn$Tenure <- as.numeric(churn$Tenure)
# churn$HasCrCard <- as.numeric(churn$HasCrCard)
# churn$IsActiveMember <- as.numeric(churn$IsActiveMember)
# churn$CreditScore <- as.numeric(churn$CreditScore)
set.seed(12345)
churn <- churn[,4:14]
churn <- as.data.frame(churn)
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model <- train(factor(Exited) ~ ., data = churn, method = 'lvq', trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

control2 <- rfeControl(functions = rfFuncs, method = 'cv', number = 10)
results <- rfe(churn[,1:10], churn$Exited,sizes = c(1:10), rfeControl = control2)
results
plot(results, type = c('g', 'o'))
churn <- churn[4:14]
## set numerical factors
churn$Geography <- factor(churn$Geography,
                          levels = c('France', 'Spain', 'Germany'),
                          labels = c(1,2,3))

churn$Gender <- factor(churn$Gender,
                          levels = c('Female', 'Male'),
                          labels = c(0,1))

#### Divide in train and train
churn_train <- churn %>% sample_frac(0.75)
churn_test <- churn %>% anti_join(churn_train)
head(churn_train)
#### Logistic regression

mod1 <- glm(Exited ~ ., data = churn_train, family = binomial())
summary(mod1)

pr1 <- predict(mod1, type = 'response', newdata = churn_test)

summary(pr1)

response_hat_insample <- pr1 > 0.5 ## Predict 1 if pr1>0.5

cm  <- confusionMatrix(factor(response_hat_insample), factor(churn_test$Exited))

accuracy_cm <- cm$overall['Accuracy']

##### Decision tree
mod2 <- rpart(factor(Exited) ~., data = churn_train)

mod2

pr2 <- predict(mod2, newdata = churn_test[-11], type = 'class')
summary(pr2)

cm2  <- table(pr2, churn_test$Exited)
cm21  <- ?confusionMatrix(pr2, factor(churn_test$Exited))
accuracy_dt <- (cm2[1,1] + cm2[2,2]) / (cm2[1,1] + cm2[1,2] + cm2[2,1] + cm2[2,2])

fancyRpartPlot(mod2, palettes = c('Reds'))

###### Random Forests

churn$Exited <- factor(churn$Exited, levels = c(0,1), labels = c(FALSE, TRUE))

churn_train$Exited <- factor(churn_train$Exited, levels = c(0,1), labels = c(FALSE, TRUE))
churn_test$Exited <- factor(churn_test$Exited, levels = c(0,1), labels = c(FALSE, TRUE))
mod3 <- randomForest(Exited ~., data = churn_train, ntree = 200)

pr3 <- predict(mod3, newdata = churn_test[-11])
summary(pr3)

cm3  <- confusionMatrix(pr3, factor(churn_test$Exited))

accuracy_rf <- (cm3[1,1] + cm3[2,2]) / (cm3[1,1] + cm3[1,2] + cm3[2,1] + cm3[2,2])

##### Boosting with C5.0 (memory problem...solution AWS)

boost10 <- C5.0(churn_train[c(-2,-3,-11)], churn_train$Exited, trials = 10)

boost10




churn_train$Exited <- factor(churn_train$Exited, levels = c(0,1), labels = c('FALSE', 'TRUE'))
churn_test$Exited <- factor(churn_test$Exited, levels = c(0,1), labels = c('FALSE', 'TRUE'))
churn_train$Geography <- as.numeric(churn_train$Geography)
churn_train$Gender <- as.numeric(churn_train$Gender)

matrix_dimensions <- list(c('FALSE', 'TRUE'), c('FALSE', 'TRUE'))
names(matrix_dimensions) <- c('predicted', 'actual')

error_cost <- matrix(c(0,1,4,0), ncol = 2, dimnames = matrix_dimensions)

churn_cost <- C5.0(churn_train[c(-2,-3,-11)], churn_train$Exited, costs = error_cost, rules = TRUE)
churn_cost_pred <- predict(churn_cost, churn_test[c(-2,-3,-11)])
CrossTable(churn_test$Exited, churn_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual churners', 'predicted churners'))


 
#####





#### Conclusions
