library(mlbench)        # - for original data set
library(ggplot2)        # - for nice plots
library(e1071)          # - for naive bayes
library(stats)          # - for principal component analysis and glm
library(MASS)           # - for akaike information criterion
library(scatterplot3d)  # - for 3d scatter plot
library(boot)           # - for cross validation

# - get data
data(PimaIndiansDiabetes)
df_data <- PimaIndiansDiabetes

# - data set with 768 observations, 8 features, and 2 classes
dim(df_data)

# - split into training set and test set
set.seed(3)
sample_index <- sample(1:nrow(df_data), 0.1*nrow(df_data), FALSE)
dt_test <- df_data[sample_index, ]
dt_train <- df_data[-sample_index, ]

# - plot each feature vector
df_melted <- reshape2::melt(df_data, id.vars = "diabetes")

ggplot(data = df_melted,
       aes(x = value,
           group = diabetes,
           colour = diabetes)) + 
  geom_density() + 
  facet_wrap(~ variable, scales  =  "free") +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black"))

with(dt_train, plot(x = triceps, y = log(insulin), col = diabetes))
with(dt_train, plot(x = glucose, y = log(mass), col = diabetes))

#---------------------------------------------------------------------------------------------------
# naive bayes
#---------------------------------------------------------------------------------------------------

nb_model <- e1071::naiveBayes(diabetes ~ .,data = dt_train)

# - back-test with training data
nb_predict_train <- stats::predict(nb_model, dt_train[, 1:8])

# - confusion matrix to get true and false negatives
table(
  model_prediction  =  nb_predict_train,
  actual_class  =  dt_train$diabetes
)

# - now try test data
nb_predict_test <- stats::predict(nb_model, dt_test[, 1:8])
table(
  model_prediction  =  nb_predict_test,
  actual_class  =  dt_test$diabetes
)


#---------------------------------------------------------------------------------------------------
# principal component analysis
#---------------------------------------------------------------------------------------------------

pr_train <- stats::prcomp(x = dt_train[, 1:8], center = TRUE, scale = TRUE)

# - first 3 PCs explain 61% of variance
plot(pr_train, type = "l")
summary(pr_train)

plot(x = pr_train$x[, 1], y = pr_train$x[, 2], col = dt_train$diabetes)

scatterplot3d(x = pr_train$x[, 1], y = pr_train$x[, 2], z = pr_train$x[, 3], 
              color = c("chartreuse4", "red")[dt_train$diabetes])

#---------------------------------------------------------------------------------------------------
# binomial logistic regression
#---------------------------------------------------------------------------------------------------

logit_model <- stats::glm(diabetes ~ ., data = dt_train, family = "binomial")

# - check leave-one-out CV
# - 15.9% error rate
boot::cv.glm(dt_train, logit_model)$delta

# - predict on new data
logit_predict <- stats::predict(logit_model, newdata = dt_test[, 1:8], type = "response")
logit_predict <- as.factor(ifelse(round(logit_predict, 0) == 0, "pred_neg", "pred_pos"))
dt_logit_predict <- rbind(dt_train, data.frame(dt_test[, 1:8], diabetes = logit_predict))

# - plot this prediction on most features with lowest p values
ggplot(data = dt_logit_predict, 
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
boot::cv.glm(dt_train, logit_model_aic)$delta

# - predict on new data
logit_predict_aic <- round(stats::predict(logit_model_aic, newdata = dt_test[, 1:8], type = "response"), 0)
logit_predict_aic <- as.factor(ifelse(logit_predict_aic == 0, "pred_neg", "pred_pos"))


# - feature extraction with PCA
# - set up data first, with PC instead of original features
dt_train_pca <- data.frame(
  diabetes = dt_train$diabetes,
  pr_train$x
)

logit_model_pca <- stats::glm(diabetes ~ ., data = dt_train_pca, family = "binomial")

# - check leave-one-out CV
# - 15.9% error rate
boot::cv.glm(dt_train_pca, logit_model_pca)$delta

# - predict on new data
# - generate principal components for test set
pr_test <- stats::predict(pr_train, newdata = dt_test[, 1:8])
logit_predict_pca <- round(stats::predict(logit_model_pca, newdata = data.frame(pr_test), type = "response"), 0)
logit_predict_pca <- as.factor(ifelse(logit_predict_pca == 0, "pred_neg", "pred_pos"))









