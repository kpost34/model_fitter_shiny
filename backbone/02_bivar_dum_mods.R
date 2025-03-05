# Bivariate models with dummy variable
# Plotting, model-fitting, model analysis, and prediction

# Load Packages and Functions=======================================================================
## Load packages and data
#packages
require("pacman")
p_load(here, tidyverse, rsample, janitor, ggeasy, broom, ggfortify, tidymodels, mgcv)


## Functions
source("fns_objs/00_fn.R")



# Wrangle and Split Data============================================================================
## Wrangle dataset
df_mtcars <- mtcars %>%
  clean_names() %>%
  select(wt, mpg, vs) %>%
  mutate(vs=as.factor(vs)) %>%
  labelled::set_variable_labels(.labels=(c("wt"="Car weight (1000 lb)",
                                           "mpg"="Miles per (US) gallon",
                                           "vs"="Engine type")))


## Split into training and test sets
set.seed(11)
mtcars_split <- initial_split(df_mtcars, prop=0.8)
df_train <- training(mtcars_split)
df_test <- testing(mtcars_split)



# Outputs===========================================================================================
## Generate scatterplot of training data with model and CI bands--------------------
df_train %>%
  mutate(vs=as.factor(vs)) %>%
  ggplot() +
  geom_point(aes(x=wt, y=mpg, fill=vs),
             shape=21, size=3) + 
  geom_smooth(aes(x=wt, y=mpg, color=vs), method="lm", se=TRUE) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  easy_labs() +
  theme_bw() +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=13))


## Model info: estimate, SE, stat, p--------------------
### Fit model: 
#define formula
# type <- "lm"
# type <- "poly"
type <- "gam"


form <- if(type=="poly"){
  as.formula("mpg ~ poly(wt, 2) * vs")
} else if(type=="gam") {
  as.formula("mpg ~ s(wt, by=vs) + vs")
} else{as.formula("mpg ~ wt * vs")}


#define model
# mod <- linear_reg(mode="regression") %>%
#   set_engine("lm") %>%
#   translate()

mod <- gen_additive_mod(mode="regression") %>%
  set_engine("mgcv") 


tidymod_fit <- mod %>%
  fit(form, data=df_train)


#extract model object
mod_fit <- tidymod_fit %>%
  extract_fit_engine()


## Grab model summary
tidy(mod_fit)


## Model diagnostic plots--------------------
# autoplot(mod_fit, which = c(1:3, 5)) +
#   theme_bw()


par(mfrow=c(2, 2))
gam.check(mod_fit)
par(mfrow=c(1, 1))



## Prediction plot
### Generate fitted model and PI values for plot
x_rng_0 <- df_test %>%
  filter(vs==0) %>%
  pull(wt) %>%
  range()

x_rng_1 <- df_test %>%
  filter(vs==1) %>%
  pull(wt) %>%
  range()

df_x_range_0 <- seq(x_rng_0[1], x_rng_0[2], length.out=100) %>%
  enframe(name=NULL, value="wt") %>%
  mutate(vs=0)

df_x_range_1 <- seq(x_rng_1[1], x_rng_1[2], length.out=100) %>%
  enframe(name=NULL, value="wt") %>%
  mutate(vs=1)

df_x_range <- bind_rows(df_x_range_0, df_x_range_1)

#lm or poly
# df_test_mod <- mod_fit %>%
#   predict(newdata=df_x_range, interval="prediction") %>%
#   bind_cols(df_x_range, .) %>%
#   as_tibble()

#gam
z_score = qnorm(0.975)
preds <- mod_fit %>%
  predict(newdata=df_x_range, type="response", se.fit=TRUE) 

pred_lwr <- preds$fit - z_score * preds$se.fit
pred_upr <- preds$fit + z_score * preds$se.fit
pred_fit <- preds$fit

df_test_mod <- tibble(fit=pred_fit, lwr=pred_lwr, upr=pred_upr) %>%
  bind_cols(df_x_range, .) %>%
  as_tibble()

df_test_mod


### Plot results
df_test_mod %>%
  mutate(vs=as.factor(vs)) %>%
  ggplot() +
  geom_line(aes(x=wt, y=fit, color=vs)) +
  geom_ribbon(aes(x=wt, ymin=lwr, ymax=upr, fill=vs),
              alpha=0.3) +
  geom_point(data=df_test, 
             aes(x=wt, y=mpg, color=as.factor(vs))) +
  labs(y="mpg") +
  theme_bw()



### Get model metrics: rmse, rsq, mae
#create DF of actual and predicted test values
df_test_pred <- tidymod_fit %>%
  predict(new_data=df_test) %>%
  bind_cols(df_test) %>%
  relocate(mpg_pred = ".pred", .after=last_col())
  
#pivot to long version
df_test_pred_long <- df_test_pred %>%
  rename(mpg_actual = "mpg") %>%
  pivot_longer(cols=c("mpg_actual", "mpg_pred"), 
               names_to="mpg_type", 
               values_to="mpg", 
               names_pattern="mpg_(.*$)")
  
  
#get range of y var in test data
test_y_rng <- max(df_test["mpg"]) - min(df_test[["mpg"]])
  
tab_bivar_test_pred <- df_test_pred %>%
  metrics(truth="mpg", estimate="mpg_pred") %>%
  rename(metric=".metric", estimate=".estimate") %>%
  mutate(rel_est=ifelse(metric %in% c("rmse", "rsq"),
                        estimate/test_y_rng * 100,
                        estimate),
         estimate=signif(estimate, 3)) %>%
  categorize_metric() %>%
  select(metric, estimate, strength)


### Pred assessment: plot of y (actual & predicted) vs x of test data
df_test_pred_long %>%
  ggplot() +
  geom_point(aes(x=wt, y=mpg, color=as.factor(vs), shape=mpg_type), 
             size=3, alpha=0.7) +
  scale_color_viridis_d(end=0.6) +
  scale_shape_manual(values=c("actual"=1, "pred"=16)) +
  easy_labs() +
  labs(color="vs",
       shape="mpg type") +
  theme_bw() 
 # labs(title=paste("Actual and predicted", y_var(), "values versus", x_var()))


### Pred assessment: plot of y (actual) vs x (predicted)
df_test_pred %>%
  rename(mpg_actual = "mpg") %>%
  ggplot(aes(x=mpg_pred, y=mpg_actual, color=as.factor(vs))) +
  geom_point(alpha=0.5) +
  geom_abline(slope=1) +
  scale_color_viridis_d(end=0.6) +
  labs(color="vs") +
  theme_bw()


### Pred assessment: plot of residual vs x (predicted)
df_test_pred %>%
  mutate(residual=mpg_pred - mpg) %>%
  ggplot() +
  geom_point(aes(x=mpg_pred, y=residual)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_bw()


### Pred assessment: summary of residual info
df_test_pred %>%
  generate_resid_summ(y="mpg", y_pred="mpg_pred")




