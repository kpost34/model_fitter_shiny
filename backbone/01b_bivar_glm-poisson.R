# Generalized Linear Models: Poisson Regression

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, ggeasy, rsample, broom, ggfortify, tidymodels, poissonreg)

#data
data("ships", package = "MASS")
#y=incidents and x=service



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_ships <- ships %>% 
  select(service, incidents) %>%
  labelled::set_variable_labels(.labels=(c("service"="Service (months)",
                                           "incidents"="No. of damage incidents")))
#x = months of service, y = number of damage incidents


## Split into training and test sets
set.seed(33)
ships_split <- initial_split(df_ships, prop=0.8)
df_ships_train <- training(ships_split)
df_ships_test <- testing(ships_split)



# Plot Training Data================================================================================
df_ships_train %>%
  ggplot(aes(x=service, y=incidents)) +
  geom_point(shape=16, size=3, alpha=0.8, color="darkblue") +
  # geom_smooth(method="glm", method.args = list(family = "poisson"), se=TRUE, color="purple") +
  easy_labs() +
  # labs(x="Service (months)", y="No. of damage incidents") +
  theme_bw() 



# Fit Model, Display Summary (Analysis), and View Diagnostics=======================================
## Fit model
mod_ships <- glm(incidents ~ service, family=poisson(), data=df_ships_train)


## Display summary
mod_ships #incidents = 0.001196(service) + 2.962124

#traditional
summary(mod_ships) 
#p-values: 
  #B0 = .0219
  #B1 = 7.62e-11

#tibble
tidy(mod_ships)
#p-values significant for both terms

#more detailed model info
glance(mod_ships)
#AIC = 214
#null deviance = 5200

#extract coefficient info
coef_ships <- summary(mod_ships)$coefficients %>%
  signif(4) %>%
  as.data.frame() %>%
  as_tibble(rownames="parameter") 


## Plot diagnostic info
#base R
par(mfrow=c(2, 2))
plot(mod_ships)
par(mfrow=c(1, 1))

#ggplot2
autoplot(mod_ships, which = c(1:3, 5)) +
  theme_bw()

#interpretation:
#residuals vs fitted: linear vs non-linear patterns of residuals; inconsistent--points below, then
  #above, and finally below 0
#normal q-q: shows if residuals are normally distributed: decent; points straddle line towards
  #middle and end of range but deviate towards start
#scale-location: shows if residuals are spread equally along the ranges of predictors (equal var):
  #spread tends to increase as x increases
#residuals vs leverage: id highly influential cases (upper right and lower right)
  #14, 22, and 8



# Plot Data and Model===============================================================================
## Use model to generate predicted values and CI bands
df_ships_pred <- df_ships_train %>%
  bind_cols(incidents_pred=predict(mod_ships, type="response", se.fit=TRUE)$fit) %>%
  bind_cols(incidents_se=predict(mod_ships, type="response", se.fit=TRUE)$se.fit) %>%
  mutate(ci_lwr=incidents_pred - 1.96 * incidents_se,
         ci_upr=incidents_pred + 1.96 * incidents_se) %>%
  select(-incidents_se)


## Plot actual and predicted values and CI band from model
#extract formula and calculate pseudo r2
ships_pois_formula <- coef(mod_ships) %>%
  signif(4) %>%
  paste0(., names(.), collapse=" + ") %>%
  str_remove("\\(Intercept\\)") %>%
  paste0("incidents = exp(", ., ")")
ships_pois_r2 <- 1 - (mod_ships$deviance/mod_ships$null.deviance) %>%
  round(3) #psuedo r2

#plot values
df_ships_pred %>%
  ggplot() +
  geom_point(aes(x=service, y=incidents), shape=21, size=3, fill='green') +
  geom_line(aes(x=service, y=incidents_pred), color="purple") +
  geom_ribbon(aes(x=service, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +
  annotate("text", x=0, y=85, label=ships_pois_formula, hjust=0) +
  annotate("text", x=0, y=80, label=paste0("psuedo r2 = ", ships_pois_r2), hjust=0) +
  #smoother (which is close to the model-generated values)
  # geom_smooth(aes(x=service, y=incidents), method="glm", method.args = list(family = "poisson"),
  #             se=TRUE, color="red") +
  xlim(0, NA) +
  easy_labs() +
  # labs(x="Service (months)", y="No. of damage incidents") +
  theme_bw()



# Model Prediction and Assessment===================================================================
## Define model
ships_pois_mod <- poisson_reg() %>%
  set_engine("glm") %>%
  translate()


## Construct workflow
ships_pois_wf <- workflow() %>%
  add_model(ships_pois_mod, formula=incidents ~ service) %>%
  add_formula(incidents ~ service)


## Fit model
ships_pois_fit <- ships_pois_wf %>%
  fit(data=df_ships_train)


## Evaluate model
df_ships_test_pred <- ships_pois_fit %>%
  predict(new_data=df_ships_test) %>%
  bind_cols(df_ships_test) %>%
  select(service, incidents, incidents_pred=".pred")

ships_pois_metrics <- df_ships_test_pred %>%
  metrics(truth=incidents, estimate=incidents_pred)
  
#RMSE: sqrt of the avg squared diffs between predicted and actual values 
  #poor as it is greater than 10-15 (for a range of 0-60)
#RSQ: proportion of variance in target variable explained by the model
  #strong as it is above 0.8
#MAE: average of the abs diff between pred and actual values
  #poor as it is greater than 8-10

# Plots Related to Actual and Predicted Test Data===================================================
## Incidents versus service using both predicted and actual values
df_ships_test_pred %>%
  rename(incidents_actual="incidents") %>%
  pivot_longer(cols=!service, names_to="incidents_type", values_to="incidents", 
               names_pattern="incidents_(.*$)") %>%
  ggplot() +
  geom_point(aes(x=service, y=incidents, color=incidents_type), shape=16, size=3, alpha=0.7) +
  scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
  theme_bw()
#shows poor fit as service increases from intermediate to large values


## Actual (y) vs predicted (x) plot
df_ships_test_pred %>%
  rename(incidents_actual="incidents") %>%
  ggplot(aes(x=incidents_pred, y=incidents_actual)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  theme_bw()
#fairly strong fit...one point deviates from line considerably


## Residual (y) vs predicted (x) plot
df_ships_test_pred %>%
  mutate(residual=incidents_pred-incidents) %>%
  ggplot() +
  geom_point(aes(x=incidents_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()
#large deviations at intermediate and high values of predicted incidents

