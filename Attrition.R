# Loading the excel

library(readxl)

df <- read_excel("D:\\GM\\Test_GeneralMills.xlsx", sheet = "DataforQ1")

# EDA

dim(df)

str(df)

summary(df)

sum(is.na(df))

library(mlr)

library(dplyr)

library(explore)

df %>% explore_tbl()

df %>% select (-c(Plants, month)) %>% explore_all()

df %>% select(-c(Plants, month)) %>% explain_tree(target = "Turnover_Rate")

dev.off()

par(mfrow = c(3,2))

boxplot(df$Turnover_Rate, horizontal = T, xlab = 'Turnover Rate')

boxplot(df$Over_Time_Hours, horizontal = T, xlab = 'Over time hours')

boxplot(df$Headcount, horizontal = T, xlab = 'Headcount')

boxplot(df$Open_Postions, horizontal = T, xlab = 'Open positions')

boxplot(df$Unplanned_leaves, horizontal = T, xlab = 'Unplanned leaves')

boxplot(df$Monthly_Production, horizontal = T, xlab = 'Monthly production')

# Fixing the column names

names(df) <- gsub("Turover Rate", "Turnover Rate", names(df))

names(df) <- gsub(" ", "_", names(df))

names(df)

# Creating a new data frame with only required columns

df_new <- df[-c(1, 3)]

# Capping outliers

df_new = capLargeValues(df_new, target = "Turnover_Rate", cols = c("Over_Time_Hours"),threshold = 25000)

boxplot(df_new$Over_Time_Hours, horizontal = T, xlab = 'Overtime hours')

df_new = capLargeValues(df_new, target = "Turnover_Rate", cols = c("Headcount"),threshold = 350)

boxplot(df_new$Headcount, horizontal = T, xlab = 'Headcount')

df_new = capLargeValues(df_new, target = "Turnover_Rate", cols = c("Unplanned_leaves"),threshold = 3200)

boxplot(df_new$Unplanned_leaves, horizontal = T, xlab = 'Unplanned leaves')

(IQR(df_new$Monthly_Production) + quantile(df_new$Monthly_Production))

df_new = capLargeValues(df_new, target = "Turnover_Rate", cols = c("Monthly_Production"),threshold = 58038744)

boxplot(df_new$Monthly_Production, horizontal = T, xlab = 'Monthly production')

boxplot(df_new)

# Checking correlation

datacor <- df_new

library(corrplot)

cor <- cor(datacor)

corrplot(cor, method = "number", type = "lower")

# Scaling 

df_scaled = data.frame(scale(df_new))

# Creating regression model

Lin_reg=lm(Turnover_Rate ~ ., data=df_scaled)

summary(Lin_reg)

# Option 1 - Using VIF to remove multicollinearity 

library(car)

vif(Lin_reg)

# Removing 'headcount' column to eliminate multicollinearity

df_scaled1 = df_scaled[-3]

# Running linear regression on scaled dataframe

Lin_reg3 =lm(Turnover_Rate ~ ., data=df_scaled1)

summary(Lin_reg3)

vif(Lin_reg3)

# Option 2 - Performing factor analysis

library(nFactors)

library(psych)

parallel = fa.parallel(df_scaled, fm = 'minres', fa = 'fa')

fa1 = fa(r=df_scaled, nfactors = 2, rotate="varimax",fm="pa")

print(fa1)

fa.diagram(fa1)

regdata <- cbind(df_scaled[1], fa1$scores)

Model=lm(Turnover_Rate ~.,data = regdata)

summary(Model)

# Ridge Regularization

library(glmnet)

library(caret)

library(Rcpp) 

x <- data.matrix(df_scaled[, 2:6])

y <- df_scaled$Turnover_Rate

model <- glmnet(x, y, alpha = 0)

summary(model)

cv_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- cv_model$lambda.min

best_lambda

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)

coef(best_model)

y_predicted <- predict(model, s = best_lambda, newx = x)

# Finding R^2 for Ridge regression

sst <- sum((y - mean(y))^2)

sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst

rsq

# Lasso Regression

model1 <- glmnet(x, y, alpha = 1)

summary(model1)

cv_model <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_model$lambda.min

best_lambda

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

coef(best_model)

y_predicted <- predict(model1, s = best_lambda, newx = x)

# Finding R^2 for Lasso Regression

sst <- sum((y - mean(y))^2)

sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst

rsq
