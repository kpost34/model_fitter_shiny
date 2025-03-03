# Polynomial Regression

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, janitor, ggeasy, rsample, broom, ggfortify, tidymodels)
select <- dplyr::select



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_iris <- iris %>%
  as_tibble() %>%
  clean_names() %>%
  select(sepal_length, petal_length, species) %>%
  labelled::set_variable_labels(.labels=(c("sepal_length"="Sepal length (cm)",
                                           "petal_length"="Petal length (cm)",
                                           "species"="species")))
#x = sepal length (in cm) of iris flowers
#y = petal length (in cm) of iris flowers


## Split into training and test sets
set.seed(13)
iris_split <- initial_split(df_iris, prop=0.8)
df_iris_train <- training(iris_split)
df_iris_test <- testing(iris_split)



# Plot Data and Add Smoother========================================================================
df_iris_train %>%
  ggplot(aes(x=sepal_length, y=petal_length)) +
  geom_point(shape=16, size=3, alpha=0.8, color="darkblue") +
  geom_smooth(method = "lm", 
              formula=y ~ poly(x, 2, raw=TRUE),
              se = TRUE, color = "purple") +
  # facet_wrap(~species) +
  easy_labs() +
  theme_bw()



# Fit Model, Display Summary (Analysis), and View Diagnostics=======================================
## Fit model
mod_iris <- lm(petal_length ~ poly(sepal_length, 2), data = df_iris_train)


## Display summary
mod_iris #dist = -2.744(sepal_length^2) + 17.672(sepal_length) + 3.805

#traditional
summary(mod_iris) 
#p-values: 
  #B0 = <2e-16
  #B1 = <2e-16
  #B2 = 0.000844

#tibble
tidy(mod_iris)
#p-values significant for all terms (intercept and x show specific p-values)

#more detailed model info
glance(mod_iris)
#AIC = 306
#null deviance = 83.9

#extract coefficient info
coef_iris <- summary(mod_iris)$coefficients %>%
  signif(4) %>%
  as.data.frame() %>%
  as_tibble(rownames="parameter") 


## Plot diagnostic info
#base R
par(mfrow=c(2, 2))
plot(mod_iris)
par(mfrow=c(1, 1))

#ggplot2
autoplot(mod_iris, which = c(1:3, 5)) +
  theme_bw()

#interpretation:
#residuals vs fitted: linear vs non-linear patterns of residuals; although the best fit line
  #straddles the 0 line throughtout the range of fitted values, the points vary widely
#normal q-q: shows if residuals are normally distributed: decent; points straddle line well except
  #for ends where there's more deviation
#scale-location: shows if residuals are spread equally along the ranges of predictors (equal var):
  #spread is consistent through most of range of predicted values except higher ones at middle of
  #fitted values
#residuals vs leverage: id highly influential cases (upper right and lower right)
  #49, 88, 52
#overall: good diagnostics


# Plot Data and Model===============================================================================
## Use model to generate predicted values and CI bands
df_iris_pred <- df_iris_train %>%
  bind_cols(predict(mod_iris, interval="confidence")) %>%
  rename(petal_length_pred="fit", ci_lwr="lwr", ci_upr="upr")


## Plot actual and predicted values and CI band from model
#extract formula and r2
iris_poly_formula <- coef(mod_iris) %>%
  signif(4) %>%
  set_names(c("(Intercept)", "sepal_length", "(sepal_length)^2")) %>%
  paste0(., names(.), collapse=" + ") %>%
  str_remove("\\(Intercept\\)") %>%
  paste("petal_length =", .)
iris_poly_r2 <- summary(mod_iris)[[8]] %>% round(3)

#plot values
df_iris_pred %>%
  ggplot() +
  geom_point(aes(x=sepal_length, y=petal_length), shape=21, size=3, fill="blue") +
  geom_line(aes(x=sepal_length, y=petal_length_pred), color="purple") +
  geom_ribbon(aes(x=sepal_length, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +
  annotate("text", x=4, y=7.5, label=iris_poly_formula, hjust=0, size=3) +
  annotate("text", x=4, y=7, label=paste0("r2 = ", iris_poly_r2), hjust=0, size=3) +
  #smoother (which is effectively identical to the model-generated values)
  # geom_smooth(aes(x=sepal_length, y=petal_length),
  #             method = "lm",
  #             formula=y ~ poly(x, 2, raw=TRUE),
  #             se = TRUE, color = "red") +
  easy_labs() +
  labs(title="Iris petal length by sepal length") +
  theme_bw()



# Model Prediction and Assessment===================================================================
## Define model
iris_poly_mod <- linear_reg(mode="regression") %>%
  set_engine("lm") %>%
  translate()


## Construct workflow
iris_poly_wf <- workflow() %>%
  add_model(iris_poly_mod, formula=petal_length ~ poly(sepal_length, 2)) %>%
  add_formula(petal_length ~ sepal_length)


## Fit model
iris_poly_fit <- iris_poly_wf %>%
  fit(data=df_iris_train)


## Evaluate model
df_iris_test_pred<- iris_poly_fit %>%
  predict(new_data=df_iris_test) %>%
  bind_cols(df_iris_test) %>%
  select(sepal_length, petal_length, petal_length_pred=".pred")

iris_poly_metrics <- df_iris_test_pred %>%
  metrics(truth=petal_length, estimate=petal_length_pred)

#RMSE: average as it is 0.973 and the range of petal_length is 4.3, so it's between 20-30%
#RSQ: average as it is only 0.588
#MAE: good as it's 0.735 and thus is between 10-20% of the target range



# Plots Related to Actual and Predicted Test Data===================================================
## Incidents versus service using both predicted and actual values
df_iris_test_pred %>%
  rename(petal_length_actual="petal_length") %>%
  pivot_longer(cols=!sepal_length, names_to="petal_length_type", values_to="petal_length", 
               names_pattern="petal_length_(.*$)") %>%
  ggplot() +
  geom_point(aes(x=sepal_length, y=petal_length, color=petal_length_type), shape=16, size=3, alpha=0.7) +
  scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
  theme_bw()
#many actual values fall close to predicted values but a few stray far


## Actual (y) vs predicted (x) plot
df_iris_test_pred %>%
  rename(petal_length_actual="petal_length") %>%
  ggplot(aes(x=petal_length_pred, y=petal_length_actual)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  theme_bw()
#similar to above plot where many points deviate little from regression line but a handful stray
  #a decent distance away


## Residual (y) vs predicted (x) plot
df_iris_test_pred %>%
  mutate(residual=petal_length_pred-petal_length) %>%
  ggplot() +
  geom_point(aes(x=petal_length_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()
#a few that deviate considerably but most are close to 0 line




