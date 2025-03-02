# Linear Models
# Plotting, model-fitting, model analysis, and prediction

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, rsample, janitor, ggeasy, broom, ggfortify, tidymodels)

#data
data(trees)



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_trees <- trees %>%
  clean_names() %>%
  select(girth, volume) %>%
  labelled::set_variable_labels(.labels=(c("girth"="Girth (in)",
                                          "volume"="Volume (cu ft)")))


## Split into training and test sets
set.seed(11)
trees_split <- initial_split(df_trees, prop=0.8)
df_trees_train <- training(trees_split)
df_trees_test <- testing(trees_split)



# Plot Training Data================================================================================
## Auto-scale
df_trees_train %>%
  ggplot(aes(x=girth, y=volume)) +
  geom_point(shape=1, size=3) +
  # geom_smooth(method="lm", se=TRUE, color="purple") +
  easy_labs() +
  # labs(x="Girth (in)", y="Volume (cu ft)") +
  theme_bw() 
#this appears to be a tight fit between the two variables


#Forced 0
df_trees_train %>%
  ggplot(aes(x=girth, y=volume)) +
  geom_point(shape=1, size=3) +
  # geom_smooth(method="lm", se=TRUE, color="purple") +
  xlim(0, NA) +
  ylim(0, NA) +
  easy_labs() +
  # labs(x="Girth (in)", y="Volume (cu ft)") +
  theme_bw() 



# Fit Model, Display Summary (Analysis), and View Diagnostics=======================================
## Fit model
mod_trees <- lm(volume ~ girth, data=df_trees_train)


## Display summary
mod_trees #volume = 5.193(girth) - 39.080

#traditional
summary(mod_trees) 
#p-values: 
  #B0 = 1.19e-9
  #B1 = 6.33e-15
#r^2: 0.94
#r^2(adj) = 0.9372

#tibble
tidy(mod_trees)
#p-values significant for both terms

#more detailed model info
glance(mod_trees)

#extract coefficient info
coef_trees <- summary(mod_trees)$coefficients %>%
  signif(4) %>%
  as.data.frame() %>%
  as_tibble(rownames="parameter") 


## Plot diagnostic info
#base R
par(mfrow=c(2, 2))
plot(mod_trees)
par(mfrow=c(1, 1))

#ggplot2
autoplot(mod_trees, which = c(1:3, 5)) +
  theme_bw()

#interpretation:
#residuals vs fitted: linear vs non-linear patterns of residuals; appears to be a non-liner pattern
#normal q-q: shows if residuals are normally distributed: decent; points straddle line and ends are
  #close to line
#scale-location: shows if residuals are spread equally along the ranges of predictors (equal var):
  #spread changes as values increase along x (first decrease then increase)
#residuals vs leverage: id highly influential cases (upper right and lower right)
  #here point 3 should be considered a highly influential point


# Plot Data and Model===============================================================================
## Use model to generate predicted values and CI bands
df_trees_pred <- df_trees_train %>%
  bind_cols(predict(mod_trees, interval="confidence")) %>%
  rename(volume_pred="fit", ci_lwr="lwr", ci_upr="upr")


## Plot actual and predicted values and CI band from model
#extract formula and r2
trees_lm_formula <- coef(mod_trees) %>%
  signif(4) %>%
  paste0(., names(.), collapse=" + ") %>%
  str_remove("\\(Intercept\\)") %>%
  paste("volume =", .)
trees_lm_r2 <- summary(mod_trees)[[8]] %>% round(3)

#plot values
df_trees_pred %>%
  ggplot() +
  geom_point(aes(x=girth, y=volume), shape=1, size=3) +
  geom_line(aes(x=girth, volume_pred), color="purple") +
  geom_ribbon(aes(x=girth, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +
  annotate("text", x=6, y=59, label=trees_lm_formula, hjust=0) +
  annotate("text", x=6, y=57, label=paste0("r2 = ", trees_lm_r2), hjust=0) +
  # xlim(0, NA) +
  easy_labs() +
  theme_bw() 



# Model Prediction and Assessment===================================================================
## Define model
trees_lm_mod <- linear_reg() %>%
  set_engine("lm") %>%
  translate()


## Construct workflow
trees_lm_wf <- workflow() %>%
  add_model(trees_lm_mod, formula=volume ~ girth) %>%
  add_formula(volume ~ girth)


## Fit model
trees_lm_fit <- trees_lm_wf %>%
  fit(data=df_trees_train)


## Evaluate model
df_trees_test_pred <- trees_lm_fit %>%
  predict(new_data=df_trees_test) %>%
  bind_cols(df_trees_test) %>%
  select(girth, volume, volume_pred=".pred")

trees_lm_metrics <- df_trees_test_pred %>%
  metrics(truth=volume, estimate=volume_pred)


#develop code to categorize metrics results
test_y_range <- max(df_trees_test_pred[['volume']]) - min(df_trees_test_pred[['volume']])

df_trees_test_pred %>%
  metrics(truth=volume, estimate=volume_pred) %>%
  rename(metric=".metric", estimate=".estimate") %>%
  mutate(rel_est=ifelse(metric %in% c("rmse", "rsq"),
                        (estimate/test_y_range) * 100,
                        estimate),
         estimate=signif(estimate, 3)) %>%
  mutate(
    strength=case_when(
      metric=="rsq" & rel_est > 0.7              ~ "high",
      metric=="rsq" & between(rel_est, 0.3, 0.7) ~ "medium",
      metric=="rsq" & rel_est < 0.3              ~ "low",
      metric=="mae" & rel_est < 5                ~ "high",
      metric=="mae" & between(rel_est, 5, 10)    ~ "medium",
      metric=="mae" & rel_est > 10               ~ "low",
      metric=="rmse" & rel_est < 5               ~ "high",
      metric=="rmse" & between(rel_est, 5, 20)   ~ "medium",
      metric=="rmse" & rel_est > 20              ~ "low")
    ) %>%
  select(metric, estimate, strength)

  
#MAE: 
  #weak (> 10% of range in y)
  #mod (5-10% of range in y)
  #strong (<5% of range in y)
#RMSE:
  #weak (> 20% of range of y)
  #mod (5-20% of range of y)
  #strong (< 5% of range of y)
#R^2:
  #weak (0-0.3)
  #mod (0.3-0.7)
  #strong (> 0.7)
  
  

# Plots Related to Actual and Predicted Test Data===================================================
## Volume vs girth using both predicted and actual values
df_trees_test_pred %>%
  rename(volume_actual="volume") %>%
  pivot_longer(cols=!girth, names_to="volume_type", values_to="volume", 
               names_pattern="volume_(.*$)") %>%
  ggplot() +
  geom_point(aes(x=girth, y=volume, color=volume_type), shape=16, size=3, alpha=0.7) +
  scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
  theme_bw()


## Actual (y) vs predicted (x) plot
df_trees_test_pred %>%
  rename(volume_actual="volume") %>%
  ggplot(aes(x=volume_pred, y=volume_actual)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  theme_bw()
#shows tight fit between actual and predicted values


## Residual (y) vs predicted (x) plot
df_trees_test_pred %>%
  mutate(residual=volume_pred-volume) %>%
  ggplot() +
  geom_point(aes(x=volume_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()
#deviate more as volume increase (as shown two plots above)



#table of residuals
df_trees_test_pred %>%
  mutate(resid=volume_pred-volume,
         resid_mean=mean(resid),
         resid_sd=sd(resid),
         resid_dev_4th=(resid - resid_mean)^4) %>%
  reframe(resid_mom_4th=sum(resid_dev_4th)/length(resid_dev_4th),
          sd_4th=resid_sd^4,
          kurtosis=resid_mom_4th/sd_4th) %>%
  distinct() %>%
  pull(kurtosis) -> kurtosis


df_trees_test_pred %>%
  mutate(resid=volume_pred-volume) %>%
  reframe(across(resid, list(min=min, 
                                mean=mean, 
                                median=median,
                                max=max,
                                sd=sd))) %>%
  mutate(resid_skewness=3*(resid_mean - resid_median)/resid_sd,
         resid_kurtosis=kurtosis) %>%
  rename_with(.cols=everything(), .fn=~str_remove(.x, "^resid_")) %>%
  pivot_longer(cols=everything(), names_to="metric", values_to="value") %>%
  mutate(value=signif(value, 3)) -> df_resid_summ

