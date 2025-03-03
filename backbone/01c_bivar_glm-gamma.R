# Generalized Linear Models: Gamma Regression

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, janitor, ggeasy, rsample, broom, ggfortify, tidymodels)
select <- dplyr::select

#data
# data("airquality")



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_airquality <- airquality %>%
  as_tibble() %>%
  na.omit() %>%
  clean_names() %>%
  select(temp, ozone) %>%
  labelled::set_variable_labels(.labels=(c("temp"="Temperature (oF)",
                                           "ozone"="Ozone (ppb)")))
#x = temperature in NY from May-Sep 1973
#y = ozone levels in NY from May-Sep 1973


## Split into training and test sets
set.seed(83)
airquality_split <- initial_split(df_airquality, prop=0.8)
df_airquality_train <- training(airquality_split)
df_airquality_test <- testing(airquality_split)



# Plot Data and Add Smoother========================================================================
df_airquality_train %>%
  ggplot(aes(x=temp, y=ozone)) +
  geom_point(shape=16, size=3, alpha=0.8, color="darkblue") +
  geom_smooth(method = "glm", 
              method.args = list(family = Gamma(link = "log")),  
              se = TRUE, color = "purple") +
  easy_labs() +
  # labs(x="Temperature (oF)", y="Ozone (ppb)") +
  theme_bw()



# Fit Model, Display Summary (Analysis), and View Diagnostics=======================================
## Fit model
mod_airquality <- glm(ozone ~ temp, family=Gamma(link="log"), data=df_airquality_train)


## Display summary
mod_airquality #ozone = 0.06316(temp) - 1.36803

#traditional
summary(mod_airquality) 
#p-values: 
  #B0 = 0.00979
  #B1 = 2.95e-15

#tibble
tidy(mod_airquality)
#p-values significant for both terms

#more detailed model info
glance(mod_airquality)
#AIC = 757
#null deviance = 58

#extract coefficient info
coef_airquality <- summary(mod_airquality)$coefficients %>%
  signif(4) %>%
  as.data.frame() %>%
  as_tibble(rownames="parameter")


## Plot diagnostic info
#base R
par(mfrow=c(2, 2))
plot(mod_airquality)
par(mfrow=c(1, 1))

#ggplot2
autoplot(mod_airquality, which = c(1:3, 5)) +
  theme_bw()

#interpretation:
#residuals vs fitted: linear vs non-linear patterns of residuals; points straddle the 0 line well
  #throughout the range of predicted values
#normal q-q: shows if residuals are normally distributed: decent; points straddle line well except
  #for ends where there's more deviation
#scale-location: shows if residuals are spread equally along the ranges of predictors (equal var):
  #spread is consistent through most of range of predicted values except higher ones where it turns
  #negative 
#residuals vs leverage: id highly influential cases (upper right and lower right)
  #36, 81, and 9
#overall: good diagnostics


# Plot Data and Model===============================================================================
## Use model to generate predicted values and CI bands
df_airquality_pred <- df_airquality_train %>%
  bind_cols(ozone_pred=predict(mod_airquality, type="response", se.fit=TRUE)$fit) %>%
  bind_cols(ozone_se=predict(mod_airquality, type="response", se.fit=TRUE)$se.fit) %>%
  mutate(ci_lwr=ozone_pred - 1.96 * ozone_se,
         ci_upr=ozone_pred + 1.96 * ozone_se) %>%
  select(-ozone_se)


## Plot actual and predicted values and CI band from model
#extract formula and calculate pseduo r2
airquality_gamma_formula <- coef(mod_airquality) %>%
  signif(4) %>%
  paste0(., names(.), collapse=" + ") %>%
  str_remove("\\(Intercept\\)") %>%
  paste0("ozone = exp(", ., ")")

airquality_gamma_r2 <- 1 - (mod_airquality$deviance/mod_airquality$null.deviance) %>%
  round(3) #psudeo r2

#plot values
df_airquality_pred %>%
  ggplot() +
  geom_point(aes(x=temp, y=ozone), shape=21, size=3, fill="red") +
  geom_line(aes(x=temp, y=ozone_pred), color="purple") +
  geom_ribbon(aes(x=temp, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +
  annotate("text", x=55, y=180, label=airquality_gamma_formula, hjust=0) +
  annotate("text", x=55, y=170, label=paste0("pseudo r2 = ", airquality_gamma_r2), hjust=0) +
  #smoother (which is effectively identical to the model-generated values)
  # geom_smooth(aes(x=temp, y=ozone),
  #             method = "glm", 
  #             method.args = list(family = Gamma(link = "log")),  
  #             se = TRUE, color = "red", linetype='dashed') +
  easy_labs() +
  labs(title="Ozone levels by temperature in NY from May-Sep 1973") +
  theme_bw()



# Model Prediction and Assessment===================================================================
## Define model
airquality_gamma_mod <- linear_reg(mode="regression") %>%
  set_engine("glm", family=Gamma(link="log")) %>%
  translate()


## Construct workflow
airquality_gamma_wf <- workflow() %>%
  add_model(airquality_gamma_mod, formula=ozone ~ temp) %>%
  add_formula(ozone ~ temp)


## Fit model
airquality_gamma_fit <- airquality_gamma_wf %>%
  fit(data=df_airquality_train)


## Evaluate model
df_airquality_test_pred<- airquality_gamma_fit %>%
  predict(new_data=df_airquality_test) %>%
  bind_cols(df_airquality_test) %>%
  select(temp, ozone, ozone_pred=".pred")

airquality_gamma_metrics <- df_airquality_test_pred %>%
  metrics(truth=ozone, estimate=ozone_pred)

#RMSE: average as it is 23.5 and the range of ozone is 113 units, so it's between 15-30%
#RSQ: average as it is only 0.548
#MAE: good as it's 16.5 and thus is between 6-17 (5-15%)



# Plots Related to Actual and Predicted Test Data===================================================
## Incidents versus service using both predicted and actual values
df_airquality_test_pred %>%
  rename(ozone_actual="ozone") %>%
  pivot_longer(cols=!temp, names_to="ozone_type", values_to="ozone", 
               names_pattern="ozone_(.*$)") %>%
  ggplot() +
  geom_point(aes(x=temp, y=ozone, color=ozone_type), shape=16, size=3, alpha=0.7) +
  scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
  theme_bw()
#many actual values fall close to predicted values but a few stray far


## Actual (y) vs predicted (x) plot
df_airquality_test_pred %>%
  rename(ozone_actual="ozone") %>%
  ggplot(aes(x=ozone_pred, y=ozone_actual)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  theme_bw()
#similar to above plot where many points deviate little from regression line but a handful stray
  #a decent distance away

## Residual (y) vs predicted (x) plot
df_airquality_test_pred %>%
  mutate(residual=ozone_pred-ozone) %>%
  ggplot() +
  geom_point(aes(x=ozone_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()
#a few that deviate considerably but most are close to 0 line






