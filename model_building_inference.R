library(MASS)
library(tidyverse)
library(skimr)
library(janitor)
library(broom)
library(modelr)
library(glmnet)
library(glmnetUtils)
library(leaps)
set.seed(301)

hc_inf <- read_csv("data/asec_clean.csv")



# removing all "cheater" predictors except for healthcare last 12 mo

hc_inf <- hc_inf %>% dplyr::select(-now_hcov, -hpub, -now_hpub, -hpriv, -now_hpriv,
                               -hmcaid, -now_hmcaid, -hcov, -hwcval, -hwsval)

hc_inf_test <- hc_inf %>% sample_frac(0.15)

hc_inf_train <- hc_inf %>% setdiff(hc_inf_test)

## forward selection

hc_inf_fwd <- regsubsets(now_hcov_recode ~ ., data = hc_inf_train, method = "forward")

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

regsubset_inf <- tibble(train = list(hc_inf_train), 
                        test = list(hc_inf_test)) %>% 
  mutate(fit = list(hc_inf_fwd),
         test_error = map2(fit, test, ~test_error_regsubset(object = .x, fmla = "now_hcov_recode ~ .",
                                                            test_data = .y)))

regsubset_inf %>% 
  select(fit, test_error) %>% unnest(test_error) %>% arrange(test_error)

regsubset_inf %>% 
  pluck("fit") %>% 
  map( ~ coef(.x, id = 1)) %>% 
  map2(c("fwd"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  knitr::kable(digits = 3)



