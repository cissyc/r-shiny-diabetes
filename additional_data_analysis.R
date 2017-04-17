library(mlbench)        # - for original data set
library(ggplot2)        # - for nice plots
library(e1071)          # - for naive bayes
library(stats)          # - for principal component analysis and glm
library(MASS)           # - for akaike information criterion
library(scatterplot3d)  # - for 3d scatter plot
library(boot)           # - for cross validation
library(pROC)           # - for ROC and AUC
library(ROCR)           # - for ROC and AUC
library(glmnet)         # - for lasso
library(dplyr)          # - data manipulation 

# - get data
data(PimaIndiansDiabetes)
df_data <- PimaIndiansDiabetes

# - data set with 768 observations, 8 features, and 2 classes
dim(df_data)

# - split into training set and test set
set.seed(3)
sample_index <- sample(1:nrow(df_data), 0.1*nrow(df_data), FALSE)
df_test <- df_data[sample_index, ]
df_train <- df_data[-sample_index, ]

df_train_old <- df_train

# - plot each feature vector
df_melted <- reshape2::melt(df_train, id.vars = "diabetes")

ggplot(data = df_melted,
       aes(x = value,
           group = diabetes,
           colour = diabetes)) + 
  geom_density() + 
  facet_wrap(~ variable, scales  =  "free") +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black"))


#---------------------------------------------------------------------------------------------------
# missing data imputation
#---------------------------------------------------------------------------------------------------

# - variables with missing data: pressure, triceps, insulin, mass
# - variables without missing data: pregnant, glucose, pedigree, age

pressure_model <- stats::glm(pressure ~ pregnant + glucose + pedigree + age, data = df_train[df_train$pressure != 0, ])
pressure_predict <- stats::predict(pressure_model, newdata = df_train[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

triceps_model <- stats::glm(triceps ~ pregnant + glucose + pedigree + age, data = df_train[df_train$triceps != 0, ])
triceps_predict <- stats::predict(triceps_model, newdata = df_train[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

insulin_model <- stats::glm(insulin ~ pregnant + glucose + pedigree + age, data = df_train[df_train$insulin != 0, ])
insulin_predict <- stats::predict(insulin_model, newdata = df_train[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

mass_model <- stats::glm(mass ~ pregnant + glucose + pedigree + age, data = df_train[df_train$mass != 0, ])
mass_predict <- stats::predict(mass_model, newdata = df_train[, c("pregnant", "glucose", "pedigree", "age")], type = "response")

df_train_new <- cbind(df_train, pressure_predict, triceps_predict, insulin_predict, mass_predict) %>%
  dplyr::mutate(
    pressure = ifelse(pressure == 0, pressure_predict, pressure),
    triceps = ifelse(triceps == 0, triceps_predict, triceps),
    #insulin = ifelse(insulin == 0, insulin_predict, insulin),
    mass = ifelse(mass == 0, mass_predict, mass)
  ) %>%
  dplyr::select(
    -pressure_predict,
    -triceps_predict,
    -insulin_predict,
    -mass_predict
  )

# - triceps and insulin bad
df_melted_new <- reshape2::melt(df_train_new, id.vars = "diabetes")

ggplot(data = df_melted_new,
       aes(x = value,
           group = diabetes,
           colour = diabetes)) + 
  geom_density() + 
  facet_wrap(~ variable, scales  =  "free") +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black"))


#---------------------------------------------------------------------------------------------------
# binomial logistic regression
#---------------------------------------------------------------------------------------------------

df_train <- df_train_old
df_train <- df_train_new

logit_model <- stats::glm(diabetes ~ .*., data = df_train, family = "binomial")

# - check leave-one-out CV
# - 15.9% error rate
boot::cv.glm(df_train, logit_model)$delta

# - predict on new data
logit_predict <- stats::predict(logit_model, newdata = df_test[, 1:8], type = "response")
logit_predict_factor <- as.factor(ifelse(round(logit_predict, 0) == 0, "pred_neg", "pred_pos"))

# - confusion matrix
table(
  model_prediction = logit_predict_factor,
  actual_class = df_test$diabetes
)

library(ROCR)
# - plot ROC curve for false positive rate vs true positive rate
rocr_predict <- ROCR::prediction(logit_predict, df_test[, 9]=="positive")
roc_perf <- ROCR::performance(rocr_predict, measure = "tpr", x.measure = "fpr")
plot(roc_perf, main = "ROC Curve")
abline(a = 0, b = 1)

# - area under curve
auc_perf <- ROCR::performance(rocr_predict, measure = "auc")
auc_perf@y.values

# - give a cost and find optimal cut-off point
cost_perf <- ROCR::performance(rocr_predict, measure = "cost", cost.fp = 1, cost.fn = 2)
cutoff_optimal <- pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]

# - confusion matrix with new cut-off point
logit_predict_factor_new <- ifelse(logit_predict <= cutoff_optimal, "pred_neg", "pred_pos")

table(
  model_prediction = logit_predict_new,
  actual_class = df_test$diabetes
)


#---------------------------------------------------------------------------------------------------
# apply lasso
#---------------------------------------------------------------------------------------------------


fit <- glmnet::glmnet(model.matrix(diabetes ~ .*., df_train), as.matrix(as.numeric(df_train$diabetes=="pos")), family = "binomial")
plot(fit, xvar = "dev", label = TRUE)

fit <- glmnet::glmnet(model.matrix(diabetes ~ ., df_train), as.matrix(as.numeric(df_train$diabetes=="pos")), family = "binomial")


# - plot this prediction on most features with lowest p values
ggplot(data = df_logit_predict, 
       aes(x = glucose,
           y = log(mass),
           shape = diabetes,
           size = diabetes,
           colour = diabetes)) + 
  geom_point() +
  scale_shape_manual(values = c(1, 1, 17, 17)) +
  scale_size_manual(values = c(2, 2, 4 , 4)) +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "chartreuse4", "red1"))


# - select model using akaike information criteria
# - as a result only the intercept, glucose, mass, pedigree and age remain
logit_model_aic <- MASS::stepAIC(logit_model, direction = "backward", trace = 0)

# - check leave-one-out CV
# - 15.7% error rate
boot::cv.glm(df_train, logit_model_aic)$delta

# - predict on new data
logit_predict_aic <- round(stats::predict(logit_model_aic, newdata = df_test[, 1:8], type = "response"), 0)
logit_predict_aic <- as.factor(ifelse(logit_predict_aic == 0, "pred_neg", "pred_pos"))



# - other stuff

with(df_train, plot(x = triceps, y = log(insulin), col = diabetes))
with(df_train, plot(x = glucose, y = log(mass), col = diabetes))


table(
  model_prediction  =  df_data,
  actual_class  =  df_test$diabetes
)




