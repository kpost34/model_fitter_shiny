# Generalized Additive Model

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, janitor, ggeasy, rsample, broom, ggfortify, tidymodels, mgcv)
select <- dplyr::select



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_mtcars <- mtcars %>%
  as_tibble() %>%
  clean_names() %>%
  select(wt, mpg) %>%
  labelled::set_variable_labels(.labels=(c("wt"="Car weight (1000 lb)",
                                           "mpg"="Miles per (US) gallon")))

#x = car weight (in 1000 lb)
#y = car fuel efficiency (mpg)
#data are from 1974 issue of Motor Trend magazine on 32 automobiles from 1973-1974


## Split into training and test sets
set.seed(22)
mtcars_split <- initial_split(df_mtcars, prop=0.8)
df_mtcars_train <- training(mtcars_split)
df_mtcars_test <- testing(mtcars_split)



# Plot Data and Add Smoother========================================================================
df_mtcars_train %>%
  ggplot(aes(x=wt, y=mpg)) +
  geom_point(shape=16, size=3, alpha=0.8, color="darkblue") +
  geom_smooth(method = "gam", 
              se = TRUE, color = "purple") +
  easy_labs() +
  theme_bw()



# Fit Model, Display Summary (Analysis), and View Diagnostics=======================================
## Fit model
mod_mtcars <- gam(mpg ~ s(wt), data=df_mtcars_train)


## Display summary
mod_mtcars 

#traditional
summary(mod_mtcars) 
#gaussian family, identity link function
#mpg ~ s(wt) #s is the smooth term
#for approx sig of smooth terms:
#edf: estimated df (higher values means more flexible and thus the smooth term can capture more
  #complex relationships)
#Ref.df #reference DF; number of basis functions used to represent the smooth term, which helps you 
  #understand how many basis functions were used to model the smooth term
#F: F-statistic for the smooth term, which tests whether the smooth term is significantly different
  #from 0
#p-value: significance of the smooth term (that x has a non-linear effect on y)

#for intercept:
#estimate: estimated value of the coef
#std. error: SE of the coef est
#t value: ratio of estimate: SE and tests whether the coef sig diffs from 0
#p-value: associated with the coef

#p-values: 
  #intercept = <2e-16; significantly differs from 0
  #s(wt) = <2e-16; strong evidence that x has a non-linear effect on y

#Deviance explained: prop of variation in y explained by model
#scale estimate: estimated residual standard deviation or dispersion parameter
#n: number of data points used in the model

#tibble
tidy(mod_mtcars)
#p.value = 0; edf = 2.07

#more detailed model info
glance(mod_mtcars)
#AIC = 126

#extract coefficient info--NEED TO REVISIT
# coef_mtcars <- bind_rows(
#   coef(mod_mtcars)
#   summary(mod_mtcars)$coefficients
#   
#   %>%
#   signif(4) %>%
#   as.data.frame() %>%
#   as_tibble(rownames="parameter") 


## Plot diagnostic info
#visualize smooth term and CI
par(mfrow=c(2, 2))
gam.check(mod_mtcars)
par(mfrow=c(1, 1))

#interpretation:
#1) QQ plot (deviance residuals vs theoretical quantiles): 
  #curvilinear pattern around suggests a non-normal pattern and a 
    #misspecification in the model
  #either a different distribution or skewness or kurtosis
  #possible outliers

#2) resids vs linear predictor: linear predictor represents combination of both the linear and
  #smooth terms before they are transformed through the linked function
  #fairly random pattern: indicative of homoscedasticity, appropriate model and fit

#3) histogram of residuals: ideally a bell-shaped curve for normally distributed residuals
  #appears right-skewed which is suggestive of missing parameters/interactions, adjustment of the
    #link function is needed, and there may be a non-linear relationship that the model is not
    #capturing

#4) response vs fitted values: assesses how well the model fits the data and whether there's any
  #issues such as overfitting, underfitting, or heteroscedasticity
  #points are along a 45-degree line which suggests a strong fit



# Plot Data and Model===============================================================================
## Use model to generate predicted values and CI bands
df_mtcars_pred <- df_mtcars_train %>%
  bind_cols(mpg_pred=predict(mod_mtcars, type="response", se.fit=TRUE)$fit) %>%
  bind_cols(mpg_se=predict(mod_mtcars, type="response", se.fit=TRUE)$se.fit) %>%
  mutate(ci_lwr=mpg_pred - 1.96 * mpg_se,
         ci_upr=mpg_pred + 1.96 * mpg_se) %>%
  select(-mpg_se)


## Plot actual and predicted values and CI band from model
#extract formula and r2
mtcars_gam_formula <- summary(mod_mtcars)$p.coeff %>%
  unname() %>%
  signif(4) %>%
  paste(deparse(summary(mod_mtcars)$formula), "+", .) %>%
  str_replace("~", "=")

mtcars_gam_r2 <- summary(mod_mtcars)[[14]] %>% 
  round(3)

#plot values
df_mtcars_pred %>%
  ggplot() +
  geom_point(aes(x=wt, y=mpg), shape=1, size=3) +
  geom_line(aes(x=wt, y=mpg_pred), color="purple") +
  geom_ribbon(aes(x=wt, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +
  annotate("text", x=4.5, y=35, label=mtcars_gam_formula, hjust=0) +
  annotate("text", x=4.5, y=34, label=paste0("pseudo r2 = ", mtcars_gam_r2), hjust=0) +
  #smoother (which is effectively identical to the model-generated values)
  # geom_smooth(aes(x=wt, y=mpg),
  #             method = "gam",
  #             se = TRUE, color = "red") +
  easy_labs() +
  labs(title="Miles per gallon by weight") +
  theme_bw()



# Model Prediction and Assessment===================================================================
## Define model
mtcars_gam_mod <- gen_additive_mod(mode="regression") %>%
  set_engine("mgcv") %>%
  translate()


## Construct workflow
mtcars_gam_wf <- workflow() %>%
  add_model(mtcars_gam_mod, formula=mpg ~ s(wt)) %>%
  add_formula(mpg ~ wt)


## Fit model
mtcars_gam_fit <- mtcars_gam_wf %>%
  fit(data=df_mtcars_train)


## Evaluate model
df_mtcars_test_pred <- mtcars_gam_fit %>%
  predict(new_data=df_mtcars_test) %>%
  bind_cols(df_mtcars_test) %>%
  select(wt, mpg, mpg_pred=".pred")

mtcars_gam_metrics <- df_mtcars_test_pred %>%
  metrics(truth=mpg, estimate=mpg_pred)

#RMSE: moderate as it's 3.15 and the target variable ranges from 14-30 (~20% of
  #target range)
#RSQ: strong as it is 0.922
#MAE: moderate as it's 2.53 which is about 15.8% of the target variable range



# Plots Related to Actual and Predicted Test Data===================================================
## Incidents versus service using both predicted and actual values
df_mtcars_test_pred %>%
  rename(mpg_actual="mpg") %>%
  pivot_longer(cols=!wt, names_to="mpg_type", values_to="mpg", 
               names_pattern="mpg_(.*$)") %>%
  ggplot() +
  geom_point(aes(x=wt, y=mpg, color=mpg_type), shape=16, size=3, alpha=0.7) +
  scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
  theme_bw()
#deviations are very small to moderate


## Actual (y) vs predicted (x) plot
df_mtcars_pred %>%
  rename(mpg_actual="mpg") %>%
  ggplot(aes(x=mpg_pred, y=mpg_actual)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  theme_bw()
#similar to above plot where many points deviate little from regression line but a handful stray
  #a decent distance away


## Residual (y) vs predicted (x) plot
df_mtcars_test_pred %>%
  mutate(residual=mpg_pred-mpg) %>%
  ggplot() +
  geom_point(aes(x=mpg_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()
#residuals are positive except one
#a few are close to line but remainder deviate some distance






