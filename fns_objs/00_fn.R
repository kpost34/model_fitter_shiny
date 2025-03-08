# Model Fitter Shiny App
## Functions

# Load packages
pacman::p_load(tidyverse, ggeasy, ggiraph)



# UI function
build_ui <- function(id, vec_df, vec_col=vec_col_std, vec_axes=vec_axes_std, vec_mod=vec_mod_std) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(width=3,
        ## Select dataset
        selectInput(ns("sel_ds"), "Select dataset", 
                    choices = vec_df
        ),
        
        
        ## Show data sample
        checkboxInput(ns("chk_ds"), "Show data sample"),
        
        
        ## Plot training data
        #set axes
        selectInput(ns("sel_plot_train_axes"),
                  label="Select lower limits of axes",
                  choices=vec_axes),
        br(),
        #choose color
        selectInput(ns("sel_plot_train_color"),
                    label="Select color",
                    choices=vec_col),
        #choose model
        radioButtons(ns("rad_mod_select"),
                     label="Choose model",
                     choices=vec_mod,
                     selected="none")
      ),
      mainPanel(width=9,
        ## Create tabs within panel
        tabsetPanel(id=ns("tabset_output"), selected="Training Data", type="tabs",
                    
          ### Training data/modelling output
          tabPanel(title="Training Data",
            br(),
                
            #display sample of training data
            uiOutput(ns("ui_tab_train_samp")),
            
            #plot training data
            fluidRow(
              plotOutput(ns("plot_train")),
              width="70%", height="70%"
            ),
            
            
            #display model summary
            DTOutput(ns("tab_mod_summ"))
          ),
          
          ### Diagnostics plots
          tabPanel(title="Model Diagnostics",
            br(),
            plotOutput(ns("plot_train_diag"))
          ),
          
          ### Predictions
          tabPanel(title="Predictions",
            br(),
            fluidRow(
              plotOutput(ns("plot_mod_test")),
              width="60%", height="60%"
            ),
            DTOutput(ns("tab_mod_test_pred"))
          ),
          
          ### Prediction assessments
          tabPanel(title="Prediction assessments",
            br(),
            fluidRow(
              column(6, plotOutput(ns("plot_test_actual_pred_x"))),
              column(6, plotOutput(ns("plot_test_actual_pred")))
            ),
            br(),
            tags$hr(),
            br(),
            fluidRow(
              column(6, plotOutput(ns("plot_test_resid_pred"))),
              column(6, DTOutput(ns("tab_mod_resid")))
            )
          )
          # tabPanel(title="Prediction assessments",
          #   br(),
          #   plotOutput(ns("plot_test_actual_pred_x")),
          #   br(),
          #   plotOutput(ns("plot_test_actual_pred")),
          #   br(),
          #   plotOutput(ns("plot_test_resid_pred")),
          #   br(),
          #   DTOutput(ns("tab_mod_resid"))
          # )
        )
      )
    )
  )
}


# Data wrangling function
prep_df <- function(df, x, y, x_name, y_name, x2=NULL, x2_name=NULL) {
  x_chr <- deparse(substitute(x))
  y_chr <- deparse(substitute(y))
  x2_chr <- deparse(substitute(x2))
  
  var_labs <- if(x2_chr!="NULL") {
    c(x_chr=x_name, x2_chr=x2_name, y_chr=y_name)
  } else{
    c(x_chr=x_name, y_chr=y_name)
  }

  df_new <- df %>%
    as_tibble() %>%
    na.omit() %>%
    clean_names() %>%
    {if(x2_chr!="NULL") mutate(., {{x2}} := as.factor({{x2}})) else .} %>%
    select({{x}}, {{x2}}, {{y}}) %>%
    labelled::set_variable_labels(.labels=var_labs)

  return(df_new)
}



# Plotting Functions
## Simple scatterplot for training data
make_scatter_train <- function(df, x, y, x2=NA, forced0="none", col="black",
                               mod="none") {
  # Set objects
  x_var <- sym(x)
  y_var <- sym(y)
  if(!is.na(x2)) {
    x2_var <- sym(x2)
  } else{x2_var <- NULL}
  
  if(!forced0 %in% c("x", "y", "xy", "none")){
    stop("forced0 must be 'x', 'y', 'xy', or 'none'")
  }
  
  x_forced0 <- if(forced0 %in% c("x", "xy")) {
    TRUE
  } else{FALSE}
  
  y_forced0 <- if(forced0 %in% c("y", "xy")) {
    TRUE
  } else{FALSE}
  
  
  # Make plot
  p1 <- df %>%
    ggplot(aes(x=!!x_var, y=!!y_var, shape=!!x2_var)) +
    {if(is.na(x2)) geom_point(shape=21, size=3, fill=col)
        else geom_point(size=3, color=col)} +
    easy_labs() +
    theme_bw() +
    theme(axis.title=element_text(size=14),
          axis.text=element_text(size=13)) +
    #axis type
    (if(x_forced0) scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))) +
    (if(y_forced0) scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)))
  
  x_txt <- 1.025 * layer_scales(p1)$x$range$range[1]
  y_txt <- 0.975 * layer_scales(p1)$y$range$range[2]
  
  p2 <- p1 +
    #smoother
    (if(mod=="lm") geom_smooth(method="lm", se=TRUE, color="purple")) +
    (if(mod=="pois") geom_smooth(method="glm", method.args = list(family = "poisson"), 
                                 se=TRUE, color="purple")) +
    (if(mod=="gamma") geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log")),  
                                  se = TRUE, color = "purple")) +
    (if(mod=="poly") geom_smooth(method = "lm", formula=y ~ poly(x, 2, raw=TRUE), se = TRUE, 
                                 color = "purple")) +
    (if(mod=="gam") geom_smooth(method = "gam", se = TRUE, color = "purple")) +
    (if(mod=="gamma" & (sum(df[x] <= 0) > 0|sum(df[y] <= 0)>0)) 
      annotate('text', x=x_txt, y=y_txt, hjust=0, color="purple", 
               label="Non-positive are values not allowed"))
  
  
  # return(girafe(ggobj = p))
  return(p2)
}



# Model-fitting Function
fit_tidymodel <- function(type, df, formula_mod){

  # Define model
  if(type %in% c("lm", "poly")) {
    mod <- linear_reg(mode="regression") %>%
      set_engine("lm") %>%
      translate()
  } else if(type=="pois") {
    mod <- poisson_reg() %>%
      set_engine("glm") %>%
      translate()
  } else if(type=="gamma") {
    mod <- linear_reg(mode="regression") %>%
      set_engine("glm", family=Gamma(link="log")) %>%
      translate()
  } else if(type=="gam") {
    mod <- gen_additive_mod(mode="regression") %>%
      set_engine("mgcv") %>%
      translate()
  }

  # Fit model
  tidymod_fit <- mod %>%
    fit(formula_mod, data=df)

  return(tidymod_fit)
}



# fit_tidymodel <- function(type, x, y, df, formula_mod, x2=NA){
#   
#   # Define model
#   if(type %in% c("lm", "poly")) {
#     mod <- linear_reg(mode="regression") %>%
#       set_engine("lm") %>%
#       translate()
#   } else if(type=="pois") {
#     mod <- poisson_reg() %>%
#       set_engine("glm") %>%
#       translate()
#   } else if(type=="gamma") {
#     mod <- linear_reg(mode="regression") %>%
#       set_engine("glm", family=Gamma(link="log")) %>%
#       translate()
#   } else if(type=="gam") {
#     mod <- gen_additive_mod(mode="regression") %>%
#       set_engine("mgcv") %>%
#       translate()
#   }
# 
#   # Construct workflow
#   if(is.na(x2)) {
#     wf <- workflow() %>%
#       add_model(mod, formula=formula_mod) %>%
#       add_formula(as.formula(paste0(y, " ~ ", x)))
#   } else if(!is.na(x2)) {
#     wf <- workflow() %>%
#       add_model(mod, formula=formula_mod) %>%
#       add_formula(as.formula(paste(y, "~", x, "*", x2)))
#     
#   }
#   
#   # Fit model
#   tidymod_fit <- wf %>%
#     fit(data=df)
#   
#   return(tidymod_fit)
# }



# Get predicted values
make_pred_values <- function(type, mod, df_test, x, x2=NA) {
  df_x_range <- NULL
  
  if(!is.na(x2)) {
  
    levels <- df_test %>%
      mutate(!!x2 := as.character(!!sym(x2))) %>%
      pull(x2) %>%
      unique()
    
    for(lev in levels){
      rng_lev <- df_test %>%
        filter(!!sym(x2)==lev) %>%
        pull(!!sym(x)) %>%
        range() 
      
      df_tmp <- seq(rng_lev[1], rng_lev[2], length.out=100) %>%
        enframe(name=NULL, value=x) %>%
        mutate(!!x2 := lev)
      
      df_x_range <- bind_rows(df_x_range, df_tmp)
      
    }
  } else{
  
    # Create DF of test x range
    x_min <- min(df_test[, x])
    x_max <- max(df_test[, x])
    
    df_x_range <- seq(x_min, x_max, length.out=100) %>%
      enframe(name=NULL, value=x)
  }
                             
  # Conditional PIs for non-LMs
  if(type %in% c("pois", "gamma", "gam")) {
    z_score <- qnorm(0.975) #95% interval
    preds <- predict(mod, newdata=df_x_range, type="response", se.fit=TRUE)
    
    # The gamma & poisson functions have a log-link fn
    if(type=="gam"){
      pred_lwr <- preds$fit - z_score * preds$se.fit
      pred_upr <- preds$fit + z_score * preds$se.fit
      pred_fit <- preds$fit
    } else{
      pred_lwr <- exp(preds$fit - z_score * preds$se.fit)
      pred_upr <- exp(preds$fit + z_score * preds$se.fit)
      pred_fit <- exp(preds$fit)
    } 

    
    df_pred <- tibble(fit=pred_fit, lwr=pred_lwr, upr=pred_upr)
    
    df_test_pred <- df_x_range %>%
      bind_cols(df_pred)
    
  # Create DF of x range, y predictions, and PIs of predictions (for LMs)
  } else if(type %in% c("lm", "poly")) {
    df_test_pred <- mod %>%
      predict(newdata=df_x_range, interval="prediction") %>%
      bind_cols(df_x_range, .) %>%
      as_tibble()
  }
  
  return(df_test_pred)
}


# make_pred_values <- function(type, mod, df_test, x) {
#   
#   # Create DF of test x range
#   x_min <- min(df_test[, x])
#   x_max <- max(df_test[, x])
#   
#   df_x_range <- seq(x_min, x_max, length.out=100) %>%
#     enframe(name=NULL, value=x)
#                              
#   # Conditional PIs for non-LMs
#   if(type %in% c("pois", "gamma", "gam")) {
#     z_score <- qnorm(0.975) #95% interval
#     preds <- predict(mod, newdata=df_x_range, type="response", se.fit=TRUE)
#     
#     # The gamma & poisson functions have a log-link fn
#     if(type=="gam"){
#       pred_lwr <- preds$fit - z_score * preds$se.fit
#       pred_upr <- preds$fit + z_score * preds$se.fit
#       pred_fit <- preds$fit
#     } else{
#       pred_lwr <- exp(preds$fit - z_score * preds$se.fit)
#       pred_upr <- exp(preds$fit + z_score * preds$se.fit)
#       pred_fit <- exp(preds$fit)
#     } 
# 
#     
#     df_pred <- tibble(fit=pred_fit, lwr=pred_lwr, upr=pred_upr)
#     
#     df_test_pred <- df_x_range %>%
#       bind_cols(df_pred)
#     
#   # Create DF of x range, y predictions, and PIs of predictions (for LMs)
#   } else if(type %in% c("lm", "poly")) {
#     df_test_pred <- mod %>%
#       predict(newdata=df_x_range, interval="prediction") %>%
#       bind_cols(df_x_range, .) %>%
#       as_tibble()
#   }
#   
#   return(df_test_pred)
# }



# Assess strength of metrics when testng model
categorize_metric <- function(df){
  df %>%
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
        metric=="rmse" & rel_est > 20              ~ "low"
      )
    ) -> df1
  return(df1)
}



# Generate table of residuals
generate_resid_summ <- function(df, y, y_pred) {
  df %>%
    mutate(resid=!!sym(y_pred)-!!sym(y),
           resid_mean=mean(resid),
           resid_sd=sd(resid),
           resid_dev_4th=(resid - resid_mean)^4) %>%
    reframe(resid_mom_4th=sum(resid_dev_4th)/length(resid_dev_4th),
            sd_4th=resid_sd^4,
            kurtosis=resid_mom_4th/sd_4th) %>%
    distinct() %>%
    pull(kurtosis) -> kurtosis
  
  
  df %>%
    mutate(resid=!!sym(y_pred)-!!sym(y)) %>%
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
  
  return(df_resid_summ)
}








