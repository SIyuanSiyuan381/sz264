cat("==================DATA CHECK==================")
library("readxl")
library("dplyr")
library("data.table")
winequality<- fread("winequality-red.csv", sep = ";")
View(winequality)
library("tidyverse")
glimpse(df)
names(df) <- gsub("\\.", " ", names(df))
library(tidyverse)
library(skimr)  
library(patchwork) 
cat("========Data Type========")
summary(df)
cat("======== Missing Value Inspection ========")
print(colSums(is.na(df))) 
skimr::skim(df)
cat("==================EDA==================")
cat("==========Response Variable==========")
df$quality <- as.numeric(as.character(df$quality))
p_hist_q <- ggplot(df, aes(quality)) +
geom_bar(fill = "orange", width = 0.7) +
scale_x_continuous(breaks = min(df$quality):max(df$quality)) +
labs(title = "Quality ", subtitle = " Output variable based on sensory data ") +
theme_minimal(base_size = 14)
p_box_q <- ggplot(df, aes(y = quality)) +
geom_boxplot(fill = "orange", alpha = 0.6, outlier.color = "red", width = 0.25) +
labs(title = "Quality ", subtitle = " Outliers ") +
theme_minimal(base_size = 14)

print(p_hist_q)
print(p_box_q)
#Most quality ratings fall between 5 and 7,# 
#accounting for roughly 70–80 % of the scores, # 
#which aligns with the typical pattern of wine sensory evaluation.#

cat("==========Explanatory Variable==========")
p_alcohol <- ggplot(df, aes(x = alcohol)) +
geom_histogram(aes(y = ..density..), bins = 20, fill = "orange", alpha = 0.5) +
geom_density(color = "darkorange", linewidth = 1) +  
geom_vline(xintercept = c(8, 22), linetype = "dashed", color = "red") + 
labs(title = "Alcohol content distribution
(extreme values marked: <8 % and >22 %)", x = "Alcohol content %", y = "Density") +
theme_minimal(base_size = 14)
print(p_alcohol)

p_volatile_acidity <-ggplot(df, aes(x = `volatile acidity`))+             
geom_histogram(aes(y = after_stat(density)),bins = 20,fill = "orange",alpha = 0.5)+ 
geom_density(color = "darkorange", linewidth = 1)+ 
geom_vline(xintercept = c(0, 2), linetype = "dashed", color = "red")+ 
labs(title = "Distribution of volatile acidity",x = "Volatile acidity",y = "Density")+ 
theme_minimal(base_size = 14)
print(p_volatile_acidity )

p_fixed_acidity <-ggplot(df, aes(x = `fixed acidity`))+             
geom_histogram(aes(y = after_stat(density)),bins = 20,fill = "orange",alpha = 0.5)+ 
geom_density(color = "darkorange", linewidth = 1)+ 
geom_vline(xintercept = c(0, 20), linetype = "dashed", color = "red")+ 
labs(title = "Distribution of fixed acidity",x = "Fixed acidity",y = "Density")+ 
theme_minimal(base_size = 14)
print(p_fixed_acidity)

p_citric_acid <-ggplot(df, aes(x = `citric acid`))+             
geom_histogram(aes(y = after_stat(density)),bins = 20,fill = "orange",alpha = 0.5)+ 
geom_density(color = "darkorange", linewidth = 1)+geom_vline(xintercept = c(0, 1), linetype = "dashed", color = "red")+ 
labs(title = "Distribution of citric acid",x = "Citric acid",y = "Density")+ 
theme_minimal(base_size = 14)
print(p_citric_acid)

library("gridExtra")
grid.arrange(p_alcohol, p_volatile_acidity,p_fixed_acidity,p_citric_acid ,ncol = 4,top = "Univariate Analysis of Key Physicochemical Indicators")

library("RColorBrewer") 
library("corrplot")
library("ggcorrplot")
corrplot(cor_matrix,method = "color", type = "full",          
col = rev(brewer.pal(11, "YlOrRd")),  
tl.cex = 0.8,           
tl.srt = 90, 
tl.col="darkorange",
addCoef.col = "black",   
number.cex = 0.6,        
title = "Heatmap of Input Variable Correlation Matrix",
mar = c(0, 0, 2, 0))  

p_corr <- ggcorrplot(
  cor_matrix,
  hc.order = TRUE,         
  type = "lower",
  lab = TRUE,             
  lab_size = 3,
  colors = c("YlOrRd"),  
  title = " Input Variable Correlation Matrix ",
  ggtheme = theme_minimal(base_size = 12))  
theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_corr)

library(GGally)
library(ggplot2)
cont_var <- c(1:11)  
df11 <- df[ , ..cont_var]
var_lab <- c("fixed acidity","volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","pH","sulphates","alcohol")

my_upper <- wrap("cor",
                 size = 2.2,        
                 colour = "orange")
my_lower <- wrap("points",          
                 size= 0.35,      
                 alpha = 0.25,
                 colour = "darkorange")  
my_diag  <- wrap("densityDiag",
                 colour = "red",
                 size = 0.25)    

pairs <- ggpairs(
  df11,
  columnLabels = var_lab,      
  aes(color = factor(df$quality), alpha = 0.55),
  upper = list(continuous = my_upper),
  lower = list(continuous = my_lower),
  diag  = list(continuous = my_diag)
) +
  theme_bw(base_size = 7) +      
  theme(
    axis.text.x = element_text(size = 5.5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom")
print(pairs)

library(ggplot2)
library(gridExtra)
library(grid)
plot_list <- lapply(names(input_vars), function(var) {
  ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "orange", alpha = 0.6, outlier.color = "darkorange", width = 0.5,outlier.size = 0.3) +
    labs(title = paste(var), y = var) +
    theme_minimal(base_size = 7)})
grid.arrange(grobs = plot_list, ncol = 3, top = " Input Variable Outlier Detection (Red Dots Indicate Outliers)")

z_scores <- as.data.frame(lapply(input_vars, function(x) (x - mean(x))/sd(x)))
outliers_z <- sapply(z_scores, function(x) sum(abs(x) > 3))
cat("======= Number of outliers by the Z-score method=======")
print(outliers_z)

library(dplyr)
quality_summary <- df %>%count(quality) %>%mutate(percentage = n / sum(n) * 100)
cat("=======Distribution of quality scores=======")
print(quality_summary)

cor_with_quality <- cor(df[, 1:11], df$quality) %>% 
  as.data.frame() %>%tibble::rownames_to_column("variable") %>%
  arrange(desc(abs(V1))) %>%head(3)
cat("======= Variables strongly related to quality =======")
print(cor_with_quality)

strong_cor_pairs <- which(abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1, arr.ind = TRUE)
strong_cor_df <- data.frame(
  var1 = rownames(cor_matrix)[strong_cor_pairs[, 1]],
  var2 = colnames(cor_matrix)[strong_cor_pairs[, 2]],
  cor = cor_matrix[strong_cor_pairs])
cat("======= Strongly correlated input variable pairs=======")
print(strong_cor_df)

cat("===================GLM===================")
library(tidyverse)   
library(car)         
library(MASS) 
pairs(df[, c(1:11, 12)], pch = 20,cex=0.1)  
cat("Variable"); print(names(df))
df_clean <- df %>%filter(`alcohol` >= 8 & `alcohol` <= 22,`volatile acidity` <= 1.5,
                         `residual sugar` <= 65)

df_clean <- df_clean %>%mutate(
  log_residual_sugar = log(`residual sugar` + 1),  
  log_chlorides = log(`chlorides` + 0.001))

glm_formula <- quality ~ 
  `fixed acidity` + `volatile acidity` + `citric acid` + log_residual_sugar + 
  log_chlorides + `free sulfur dioxide` + `total sulfur dioxide` + 
  density + pH + sulphates + alcohol

glm_model <- glm(formula = glm_formula,data = df_clean,
                 family = gaussian(link = "identity"), na.action = na.exclude )

cat("======= GLM model summary=======")
summary(glm_model)

qq_plot <- ggplot(data.frame(residuals = residuals(glm_model)), aes(sample = residuals)) +
  stat_qq() + 
  stat_qq(size = 1)+
  stat_qq_line(color = "orange") +
  labs(title = " GLM Residual QQ Plot", x = " Quantiles ", y = " Residuals ") +
  theme_minimal()
print(qq_plot)

shapiro_test <- shapiro.test(residuals(glm_model))
cat("======= Normality Test for Residuals=======")
print(shapiro_test)

resid_fit_plot <- ggplot(data.frame(fitted = fitted(glm_model), 
                                    resid = residuals(glm_model)), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = " Residuals vs Fitted Values", x = " Fitted Values ", y = " Residuals ") +
  theme_minimal()
print(resid_fit_plot)

library(lmtest)
bp_test <- bptest(glm_model)
cat("======= Breusch-Pagan =======")
print(bp_test)

vif_values <- car::vif(glm_model)
cat("======= VIF values for each variable =======")
print(vif_values)

glm_step <- MASS::stepAIC(glm_model,direction = "both",  trace = FALSE )
cat("======= Optimized model results after stepwise regression=======")
summary(glm_step)

sig_vars <- summary(glm_step)$coefficients %>%as.data.frame() %>%
  rownames_to_column("variable") %>%
  filter(`Pr(>|t|)` < 0.05) %>%
  mutate(effect = ifelse(Estimate > 0, " positive effect ", " negative effect "),
         magnitude = abs(Estimate)) %>%
  arrange(desc(magnitude))
cat("======= Variables that have a significant impact on quality =======")
print(sig_vars[, c("variable", "effect", "Estimate", "Pr(>|t|)")])

cat("===================Deep Learning===================")
library(keras)
library(tidyverse)
library(caret)
library(readr)
library(tensorflow)

set.seed(123)
X <- df[, 1:11] 
y <- df$quality 

preprocess_params <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preprocess_params, X)

X_matrix <- as.matrix(X_scaled)
y_vector <- as.numeric(y)

train_index <- createDataPartition(y_vector, p = 0.8, list = FALSE)
cat("======= Build a neural network model =======")

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.3) %>%
  
  layer_dense(units = 64, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.3) %>%
  
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

model %>% compile(optimizer = optimizer_adam(learning_rate = 0.001),loss = "mse",
                  metrics = c("mae", "mse"))

cat("=======Model architecture=======")
summary(model)

callbacks_list <- list(callback_early_stopping(monitor = "val_loss",
                                               patience = 15,restore_best_weights = TRUE),
                       callback_reduce_lr_on_plateau(monitor = "val_loss",factor = 0.5,patience = 8,
                                                     min_lr = 0.0001),
                       callback_model_checkpoint(filepath = "best_wine_model.h5",save_best_only = TRUE,
                                                 monitor = "val_loss"))

cat("======= Start training the model =======")
history <- model %>% fit(x_train, y_train,epochs = 200,batch_size = 64,
                         validation_split = 0.2,verbose = 1,callbacks = callbacks_list)

cat("======= Plot training history =======")
plot(history)

cat("======= Model Evaluation =======")
test_evaluation <- model %>% evaluate(x_test, y_test, verbose = 0)
cat("Test loss (MSE)", test_evaluation[[1]], "\n")
cat("est Mean Absolute Error (MAE", test_evaluation[[2]], "\n")
cat("Test Mean Squared Error ", test_evaluation[[3]], "\n")

at("=======Prediction=======")
predictions <- model %>% predict(x_test)
predictions <- as.numeric(predictions)

cat("Root Mean Squared Error (RMSE)", rmse, "\n")
cat("Mean Absolute Error (MAE)", mae, "\n")
cat("Coefficient of Determination (R²)", r_squared, "\n")
results_df <- data.frame(ActualQuality = y_test,
                         PredictedQuality = round(predictions, 2),
                         AbsoluteError = abs(y_test - predictions),
                         RelativeError = abs(y_test - predictions) / y_test * 100)

cat("=======Predicted results=======")
print(head(results_df, 10))

quality_performance <- results_df %>%
  mutate(QualityGrade = as.factor(ActualQuality)) %>%
  group_by(QualityGrade) %>%
  summarise(SampleCount = n(),
            MeanAbsoluteError = mean(AbsoluteError),
            Accuracy = sum(round(PredictedQuality) == ActualQuality) / n() * 100) %>%arrange(QualityGrade)

cat("\nPerformance Analysis by Quality Grade:\n")
print(quality_performance)

cat("===================Random Forest===================")
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

names(df) <- make.names(names(df))
df$quality <- as.factor(df$quality)
table(df$quality)

set.seed(123)
train_index <- createDataPartition(df$quality, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

train_data$quality <- factor(train_data$quality)
test_data$quality <- factor(test_data$quality, levels = levels(train_data$quality))

set.seed(123)
rf_model <- randomForest(quality ~ .,
                         data = train_data,
                         ntree = 500,
                         mtry = sqrt(ncol(train_data)-1),
                         importance = TRUE,
                         na.action = na.omit)
print(rf_model)
predictions <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$quality)
print(conf_matrix)
importance_plot <- varImpPlot(rf_model, main = " Variable Importance Plot")
print(importance_plot)

control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10),
                         splitrule = "gini",min.node.size = c(1, 5, 10))
rf_tune <- train(quality ~ .,data = train_data,method = "ranger",trControl = control,
                 tuneGrid = tune_grid,importance = "impurity")

print(rf_tune)

final_predictions <- predict(rf_model, test_data)
final_predictions <- factor(final_predictions, levels = levels(test_data$quality))

final_conf_matrix <- confusionMatrix(final_predictions, test_data$quality)
print(final_conf_matrix)

accuracy <- final_conf_matrix$overall['Accuracy']
cat("model accuracy:", round(accuracy, 3), "\n")

print(final_conf_matrix$byClass[, c("Precision", "Recall", "F1")])


