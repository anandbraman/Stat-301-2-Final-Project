library(MASS)
library(tidyverse)
library(skimr)
library(janitor)
library(broom)
library(modelr)
library(glmnet)
library(glmnetUtils)
library(leaps)
library(rpart)
library(rpart.utils)
set.seed(301)

hc_red <- read_csv("data/asec_reduced.csv")

# removing cheater preds

hc_red <- hc_red %>% dplyr::select(-now_hcov, -hcov_recode, -hpub, -now_hpub, -hpriv, -now_hpriv,
                           -hmcaid, -now_hmcaid, -hcov, -hwcval, -hwsval)

hc_red_train <- hc_red %>% sample_frac(0.85)
hc_red_test <- hc_red %>% setdiff(hc_red_train)



# classification tree -----------------------------------------------------

hc_red_tree <- rpart(data = hc_red_train, formula = now_hcov_recode ~ ., method = "class")

printcp(hc_red_tree)


# subset sel --------------------------------------------------------------

hc_red_full <- glm(now_hcov_recode ~ ., data = hc_red_train, family = binomial)

hc_red_fwd <- regsubsets(now_hcov_recode ~ ., data = hc_red_train, 
                         method = "forward")
rm(hc_red_back)

predict_regsubset <- function(object, fmla , new_data, model_id)
{
  # Not a dataframe? -- handle resample objects/k-folds
  if(!is.data.frame(new_data)){
    new_data <- as_tibble(new_data)
  }
  
  # Get formula
  obj_formula <- as.formula(fmla)
  
  # Extract coefficients for desired model
  coef_vector <- coef(object, model_id)
  
  # Get appropriate feature matrix for new_data
  x_vars <- names(coef_vector)
  mod_mat_new <- model.matrix(obj_formula, new_data)[ , x_vars]
  
  # Get predicted values
  pred <- as.numeric(mod_mat_new %*% coef_vector)
  pred <- ifelse(pred > 0.5, 1, 0)
  return(pred)
}

test_error_regsubset <- function(object, fmla , test_data){
  
  # Number of models
  num_models <- object %>% summary() %>% pluck("which") %>% dim() %>% .[1]
  
  # Set up storage
  test_mse <- rep(NA, num_models)
  
  # observed targets
  obs_target <- test_data %>% 
    as_tibble() %>% 
    pull(!!as.formula(fmla)[[2]])
  
  # Calculate test MSE for each model class
  for(i in 1:num_models){
    pred <- predict_regsubset(object, fmla, test_data, model_id = i)
    test_mse[i] <- mean((obs_target - pred)^2)
  }
  
  # Return the test errors for each model class
  tibble(model_index = 1:num_models,
         test_error    = test_mse)
}


# ridge and lasso ---------------------------------------------------------

ridge_red_lambda <- hc_red_train %>% cv.glmnet(now_hcov_recode ~ ., alpha = 0, 
                                               nfolds = 10, data = ., family = "binomial")

ridge_red_lambda_min <- ridge_red_lambda$lambda.min
ridge_red_lambda_1se <- 