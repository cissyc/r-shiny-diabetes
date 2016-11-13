library(mlbench)
library(ggplot2)
library(stats)
library(e1071)

# get data
data(PimaIndiansDiabetes)
df_data <- PimaIndiansDiabetes

# data set with 768 observations, 8 features, and 2 classes
dim(df_data)

# split into training set and test set
set.seed(3)
sample_index <- sample(1:nrow(df_data), 0.1*nrow(df_data), FALSE)
dt_test <- df_data[sample_index, ]
dt_train <- df_data[-sample_index, ]

# plot each feature vector
df_melted <- reshape2::melt(df_data, id.vars = "diabetes")

new_data <- data.frame(
  pregnant = 10,
  glucose = 100,
  pressure = 75,
  triceps = 3,
  insulin = 600,
  mass = 40,
  pedigree = 1,
  age = 20
)

new_data <- data.frame(new_values = t(new_data))
new_data$variable <- rownames(new_data)

new_data_merged <- merge(df_melted, new_data, by.x = "variable") 

ggplot(data = new_data_merged,
       aes(x = value,
           group = diabetes,
           colour = diabetes)) + 
  geom_density() + 
  facet_wrap(~ variable, scales  =  "free") +
  geom_vline(aes(xintercept = new_values)) +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "black"))

with(dt_train, plot(x = triceps, y = log(insulin), col = diabetes))
with(dt_train, plot(x = glucose, y = log(mass), col = diabetes))

#---------------------------------------------------------------------------------------------------
# naive bayes
#---------------------------------------------------------------------------------------------------

nb_model <- e1071::naiveBayes(diabetes ~ .,data = dt_train)

# back-test with training data
nb_predict_train <- stats::predict(nb_model, dt_train[, 1:8])

# confusion matrix to get true and false negatives
table(
  model_prediction  =  nb_predict_train,
  actual_class  =  dt_train$diabetes
)

# now try test data
nb_predict_test <- stats::predict(nb_model, dt_test[, 1:8])
table(
  model_prediction  =  nb_predict_test,
  actual_class  =  dt_test$diabetes
)


#---------------------------------------------------------------------------------------------------
# principal component analysis
#---------------------------------------------------------------------------------------------------

pr_train <- stats::prcomp(x = dt_train[, 1:8], center = TRUE, scale = TRUE)

# first 3 PCs explain 61% of variance
plot(pr_train, type = "l")
summary(pr_train)

plot(x = pr_train$x[, 1], y = pr_train$x[, 2], col = dt_train$diabetes)

scatterplot3d(x = pr_train$x[, 1], y = pr_train$x[, 2], z = pr_train$x[, 3], 
              color = c("chartreuse4", "red")[dt_train$diabetes])

#---------------------------------------------------------------------------------------------------
# binomial logistic regression
#---------------------------------------------------------------------------------------------------

logit_model <- stats::glm(diabetes ~ ., data = dt_train, family = "binomial")

# predict on new data
logit_predict <- round(stats::predict(logit_model, newdata = dt_test[, 1:8], type = "response"), 0)
logit_predict <- as.factor(ifelse(logit_predict == 0, "pred_neg", "pred_pos"))
dt_logit_predict <- rbind(dt_train, data.frame(dt_test[, 1:8], diabetes = logit_predict))

# plot this prediction on most statistically significant variables
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


# with PCA
pca_logit_model <- stats::glm(dt_train$diabetes ~ ., data = data.frame(pr_train$x), family = "binomial")

pr_test <- stats::predict(pr_train, newdata = dt_test[, 1:8])

pca_logit_predict <- round(stats::predict(pca_logit_model, newdata = data.frame(pr_test), type = "response"), 0)
pca_logit_predict <- as.factor(ifelse(pca_logit_predict == 0, "pred_neg", "pred_pos"))
table(
  model_prediction  =  pca_logit_predict,
  actual_class  =  dt_test$diabetes
)

dt_logit_predict <- rbind(data.frame(pr_train$x, diabetes = dt_train$diabetes),
                          data.frame(pr_test, diabetes = pca_logit_predict))
ggplot(data = dt_logit_predict, 
       aes(x = PC1,
           y = PC2,
           shape = diabetes,
           colour = diabetes)) + 
  geom_point() +
  scale_shape_manual(values = c(1, 1, 17, 17)) +
  scale_colour_manual(name = "diabetes",values = c("chartreuse3", "red", "chartreuse4", "red1"))








