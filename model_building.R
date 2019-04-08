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

hc <- read_csv("data/asec_clean.csv") %>% clean_names()

#removing cheater predictors

hc <- hc %>% dplyr::select(-now_hcov, -hcov_recode, -hpub, -now_hpub, -hpriv, -now_hpriv,
                    -hmcaid, -now_hmcaid, -hcov, -hwcval, -hwsval)


hc_test <- hc %>% sample_frac(0.15)

hc_train <- hc %>% setdiff(hc_test)


# ridge and lasso logistic regression --------------------------------------------

ridge_cv <- hc_train %>% cv.glmnet(now_hcov_recode ~ ., nfolds = 10, 
                                   alpha = 0, data = ., family = "binomial")

ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

plot(ridge_cv)

lasso_cv <- hc_train %>% cv.glmnet(now_hcov_recode ~ ., nfolds = 10,
                                   alpha = 1, data = ., family = "binomial")

lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

plot(lasso_cv)

hcov_glmnet <- tibble(train = hc_train %>% list(),
                      test = hc_test %>% list()) %>% 
  mutate(ridge_min = map (train, ~glmnet(now_hcov_recode ~ ., data = .x, alpha = 0,
                                        lambda = ridge_lambda_min, family = "binomial")),
         ridge_1se = map(train, ~glmnet(now_hcov_recode ~ ., data = .x, alpha = 0,
                                        lambda = ridge_lambda_1se, family = "binomial")),
         lasso_min = map(train, ~glmnet(now_hcov_recode ~ ., data = .x, alpha = 1,
                                        lambda = lasso_lambda_min, family = "binomial")),
         lasso_1se = map(train, ~glmnet(now_hcov_recode ~ ., data = .x, alpha = 1,
                                        lambda = lasso_lambda_min, family = "binomial"))) %>% 
  gather(key = method, value = fit, -train, -test)

hcov_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y,
         lasso_min = s0.x.x,
         lasso_1se = s0.y.y) %>% 
  knitr::kable(digits = 3)


glmnet_error <- hcov_glmnet %>% 
  mutate(preds = map2(fit, test, predict),
         pred_dir = map(preds, ~ifelse(.x > 0.5, 1, 0)),
         error = map2(test, pred_dir, ~ifelse(.x$now_hcov_recode != .y, 1, 0)),
         error_rate = map(error, mean))

glmnet_error %>% dplyr::select(method, error_rate) %>% unnest() %>% 
  arrange(error_rate)

hc_test %>% pull(now_hcov_recode) %>% tabyl

# stepwise regression -----------------------------------------------------

hc_fwd <- regsubsets(now_hcov_recode ~ ., data = hc_train, method = "forward")

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
    test_mse[i] <- mean((obs_target - pred)^2) #this is the same as test error for binom
  }
  
  # Return the test errors for each model class
  tibble(model_index = 1:num_models,
         test_error    = test_mse)
}

regsubset_error <- tibble(train = list(hc_train), 
                        test = list(hc_test)) %>% 
  mutate(fit = list(hc_fwd),
         test_error = map2(fit, test, ~test_error_regsubset(object = .x, fmla = "now_hcov_recode ~ .",
                                                            test_data = .y)))

regsubset_error %>% 
  select(fit, test_error) %>% unnest(test_error) %>% arrange(test_error)

regsubset_error %>% 
  pluck("fit") %>% 
  map( ~ coef(.x, id = 8)) %>% 
  map2(c("fwd"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  knitr::kable(digits = 3)


# tree based classification -----------------------------------------------


hc_tree <- rpart(as.factor(now_hcov_recode) ~ ., data = hc_train, method = "class")

printcp(hc_tree)





