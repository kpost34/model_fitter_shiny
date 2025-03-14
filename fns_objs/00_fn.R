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
        # Select dataset
        selectInput(ns("sel_ds"), "Select dataset", 
                    choices = vec_df
        ),
        
        
        # Show data sample
        checkboxInput(ns("chk_ds"), "Show data sample"),
        
        
        # Plot training data
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
        # Create tabs within panel
        tabsetPanel(id=ns("tabset_output"), selected="Training Data", type="tabs",
                    
          ## Training data/modelling output
          tabPanel(title="Training Data",
            br(),
                
            #display sample of training data
            uiOutput(ns("ui_tab_train_samp")),
            
            #plot training data
            fluidRow(
              plotOutput(ns("plot_train")),
                height="400px"
            ),
            
            
            #display model summary
            DTOutput(ns("tab_mod_summ"))
          ),
          
          ## Diagnostics plots
          tabPanel(title="Model Diagnostics",
            br(),
            plotOutput(ns("plot_train_diag"),
              height="600px")
          ),
          
          ## Predictions
          tabPanel(title="Predictions",
            br(),
            fluidRow(
              plotOutput(ns("plot_mod_test")),
              width="60%", height="60%"
            ),
            DTOutput(ns("tab_mod_test_pred"))
          ),
          
          ## Prediction assessments
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
        )
      )
    )
  )
}


# Data wrangling function
## Create variable labels
create_var_labs <- function(x, y, x_name, y_name, x2=NULL, x2_name=NULL) {
  
  var_labs <- if(!is.null(x2)) {
    c(x_name, x2_name, y_name) %>%
      set_names(c(x, x2, y))
  } else{
   c(x_name, y_name) %>%
      set_names(c(x, y))
  }

  return(var_labs)
}


## Perform initial data cleaning
prep_df <- function(df, x, y, x2=NULL) {
  x2_chr <- deparse(substitute(x2))
  
  df_new <- df %>%
    as_tibble() %>%
    na.omit() %>%
    clean_names() %>%
    {if(x2_chr!="NULL") mutate(., {{x2}} := as.factor({{x2}})) else .} %>%
    select({{x}}, {{x2}}, {{y}}) 

  return(df_new)
}



# Plotting Functions (for training and test data)
## Scatterplot w/optional smoother for training data
make_scatter_train <- function(df, x, y, x2=NA, forced0="none", col="black",
                               mod="none", r2_value=NULL) {
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
  ## Initial plot without smoother
  p1 <- df %>%
    ggplot(aes(x=!!x_var, y=!!y_var, shape=!!x2_var)) +
    {if(is.na(x2)) geom_point(shape=21, size=3, fill=col)
        else geom_point(size=3, color=col)} +
    easy_labs() +
    theme_bw() +
    theme_norm +
    #axis type
    (if(x_forced0) scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))) +
    (if(y_forced0) scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)))
  
  ## Create plotting objects
  x_txt <- 1.025 * layer_scales(p1)$x$range$range[1]
  y_txt <- 0.975 * layer_scales(p1)$y$range$range[2]
  
  legend_mod <- "Fitted line \u00B1 95% CI"
  
  r2_lab <- if(mod %in% c("lm", "poly")){
    "R2 ="
  } else if(mod %in% c("pois", "gamma", "gam")) {
    "psuedo R2 ="
  } else{NULL}
  
  ## Add annotation
  if(mod=="gamma" & (sum(df[x] <= 0) > 0|sum(df[y] <= 0)>0)){
    p2 <- p1 +
      annotate('text', x=x_txt, y=y_txt, hjust=0, size=7, color="darkblue", 
               label="Non-positive values are not allowed \n in gamma regression")
  } else{
  
  ## Add smoother
  p2 <- p1 +
    #smoother
    (if(mod=="lm") geom_smooth(method="lm", se=TRUE, aes(color=legend_mod))) +
    (if(mod=="pois") geom_smooth(method="glm", method.args = list(family = "poisson"), 
                                 se=TRUE, aes(color=legend_mod))) +
    (if(mod=="gamma") geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log")),  
                                  se = TRUE, aes(color=legend_mod))) +
    (if(mod=="poly") geom_smooth(method = "lm", formula=y ~ poly(x, 2, raw=TRUE), se = TRUE, 
                                 aes(color=legend_mod))) +
    (if(mod=="gam") geom_smooth(method = "gam", se = TRUE, aes(color=legend_mod))) +
    scale_color_manual(name="", values=c("purple")) +
    #display r2/psuedo r2
    ggtitle(label=NULL, subtitle=paste(r2_lab, r2_value))
  }
    
  ## Return plot
  return(p2)
}


## Scatterplot of test data with fitted model
make_scatter_test <- function(df, df2, var_labs, x, y, x2=NULL, col){

# Conditional logic to add geoms based on x2
  if (!is.null(x2)) {
    geom_layers <- list(
      geom_line(aes(x=!!sym(x), y=!!sym(y), group=!!sym(x2))),
      geom_ribbon(aes(x=!!sym(x), ymin=lwr, ymax=upr, group=!!sym(x2)), color='gray', alpha=0.1),
      geom_point(data=df2, aes(x=!!sym(x), y=!!sym(y), shape=!!sym(x2)), color=col)
    )
  } else {
    geom_layers <- list(
      geom_line(aes(x=!!sym(x), y=!!sym(y))),
      geom_ribbon(aes(x=!!sym(x), ymin=lwr, ymax=upr), color='gray', alpha=0.1),
      geom_point(data=df2, aes(x=!!sym(x), y=!!sym(y)), color=col)
    )
  }
  
  # Construct the ggplot
  p <- df %>%
    labelled::set_variable_labels(.labels=var_labs) %>%
    ggplot() +
    #conditional geom layers
    geom_layers +  
    easy_labs() +
    ggtitle(paste("Test data of", 
                  str_replace_all(y, "_", " "),
                  "plotted against", str_replace_all(x, "_", " "), 
                  "with fitted line \u00B1 95% PI")) +
    theme_bw() +
    theme_norm
  
  return(p)
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



# Get predicted values
make_pred_values <- function(type, mod, df_test, x, y, x2=NA) {
  df_x_range <- NULL
  
  # Bivariate model + dummy var
  if(!is.na(x2)) {
    ## Find levels of dummy var
    levels <- df_test %>%
      mutate(!!x2 := as.character(!!sym(x2))) %>%
      pull(x2) %>%
      unique()
    
    ## Iterate over levels, extract x ranges, and combine into DF
    for(lev in levels){
      rng_lev <- df_test %>%
        filter(!!sym(x2)==lev) %>%
        pull(!!sym(x)) %>%
        range() 
      
      df_tmp <- seq(.975*rng_lev[1], 1.025*rng_lev[2], length.out=100) %>%
        enframe(name=NULL, value=x) %>%
        mutate(!!x2 := lev)
      
      df_x_range <- bind_rows(df_x_range, df_tmp)
      
    }
  # Bivariate model
  } else{ 
  
    ## Create DF of test x range
    x_min <- min(df_test[, x])
    x_max <- max(df_test[, x])
    
    df_x_range <- seq(.975*x_min, 1.025*x_max, length.out=100) %>%
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
      bind_cols(df_pred) %>%
      rename(!!y := "fit")
    
  # Create DF of x range, y predictions, and PIs of predictions (for LMs)
  } else if(type %in% c("lm", "poly")) {
    df_test_pred <- mod %>%
      predict(newdata=df_x_range, interval="prediction") %>%
      bind_cols(df_x_range, .) %>%
      as_tibble() %>%
      rename(!!y := "fit")
  }
  
  return(df_test_pred)
}



# Wrangling predicted/actual test values
## Combine test data with predictions
combine_test_pred_values <- function(tidymod, df, x, y, x2=NULL){
  y_pred <- paste0(y, "_predicted")
  
  df <- tidymod %>%
    predict(new_data=df) %>%
    bind_cols(df) %>%
    select(all_of(c(x, y, x2)), !!y_pred := ".pred")
  
  return(df)
}


## Pivot to long version
pivot_test_pred_long <- function(df, y){
  y_pred <- paste0(y, "_predicted")
  y_actual <- paste0(y, "_actual")
  y_type <- paste0(y, "_type")
  
  df1 <- df %>%
    rename(!!y_actual := y) %>%
    pivot_longer(cols=c(y_actual, y_pred), 
                 names_to=y_type, 
                 values_to=y, 
                 names_pattern=paste0(y, "_(.*$)"))%>%
    relocate(!!sym(y_type), .after=last_col())
  
  return(df1)
}


# Reporting metrics on tested model
## Assess strength of metrics when testng model
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


## Generate metrics table
generate_pred_metrics <- function(df, y, y_range){
  y_pred <- paste0(y, "_predicted")
  
  df1 <- df %>%
    metrics(truth=y, estimate=y_pred) %>%
    rename(metric=".metric", estimate=".estimate") %>%
    mutate(rel_est=ifelse(metric %in% c("rmse", "rsq"),
                          (estimate/y_range) * 100,
                          estimate),
           estimate=signif(estimate, 3)) %>%
    categorize_metric() %>%
    select(metric, estimate, strength) %>%
    mutate(metric=case_when(
      metric=='mae'  ~ "Mean absolute error",
      metric=="rmse" ~ "Root mean square error",
      metric=='rsq'  ~ "R-squared",
      TRUE           ~ "NEEDS CATEGORY")) %>%
    arrange(metric)
  
  return(df1)
}



# Generate table of residuals
generate_resid_summ <- function(df, y) {
  y_pred <- paste0(y, "_predicted")
  
  # Calculate kurtosis
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
  
  # Calculate residual metrics
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



# Prediction Assessment Plotting Functions
## Plot actual and predicted test values versus x
plot_actual_pred_y_vs_x <- function(df, var_labs, x, y, user_theme=theme_norm, 
                                    x2=NULL){
  
  # Create variants of y as objects
  y_type <- paste0(y, "_type")
  y_actual <- paste0(y, "_actual")
  y_pred <- paste0(y, "_predicted")
  
  # Create DF and apply labels
  df1 <- df %>%
    labelled::set_variable_labels(.labels=var_labs) 
  
  # Filter data for actual and predicted values
  df_actual <- df1 %>% filter(!!sym(y_type)=="actual")
  df_predicted <- df1 %>% filter(!!sym(y_type)=="predicted")
  df_merged <- inner_join(df_actual, df_predicted, 
                          by=c(x, x2),
                          suffix=c("_actual", "_predicted"))
    
  # Create the plot
  p <- df1 %>%
    ggplot() +
    {if(!is.null(x2)) geom_point(aes(x=!!sym(x), y=!!sym(y), color=!!sym(y_type),
                   shape=!!sym(x2)), size=3, alpha=0.7)
      else geom_point(aes(x=!!sym(x), y=!!sym(y), color=!!sym(y_type)), 
                      shape=16, size=3, alpha=0.7)} +
    geom_segment(data= df_merged,
                 aes(x=!!sym(x), xend=!!sym(x),
                     y = !!sym(y_actual),
                     yend=!!sym(y_pred)),
                 color="gray", linetype="dashed", size=0.5) +
    scale_color_manual(values=c("actual"="darkred", "predicted"="darkblue")) +
    labs(title=paste("Actual and predicted", str_replace_all(y, "_", " "), 
                     "values \nplotted against", str_replace_all(x, "_", " "))) +
    easy_labs() +
    theme_bw() +
    user_theme +
    theme(legend.box="vertical",
          legend.spacing=unit(0.05, "cm"),
          legend.box.spacing = unit(0.05, "cm")) +
    guides(color=guide_legend(nrow=1),
           shape=guide_legend(nrow=1))
  
  return(p)
}


## Plot actual (y) vs predicted (x) values
plot_actual_vs_pred_y <- function(df, y, col, x2=NULL, user_theme=theme_norm){
  # Create objects for predicted and actual y values
  y_actual <- paste0(y, "_actual")
  y_pred <- paste0(y, "_predicted")
  
  # Create the plot
  p <- df %>%
    rename(!!y_actual:=y) %>%
    ggplot() +
    {if(!is.null(x2)) geom_point(aes(x=!!sym(y_pred), y=!!sym(y_actual),
                               shape=!!sym(x2)), alpha=0.5, size=2, color=col)
      else geom_point(aes(x=!!sym(y_pred), y=!!sym(y_actual)),
                      alpha=0.5, size=2, color=col)} +
    geom_abline(slope=1) +
    ggtitle(paste("Actual versus predicted values of", 
                  str_replace_all(y, "_", " "),
                  "\nwith fitted 1:1 line")) +
    labs(y=paste(str_replace_all(str_to_sentence(y), "_", " "), 
                                 "(actual)"), 
         x=paste(str_replace_all(str_to_sentence(y), "_", " "), 
                                "(predicted)")) +
    easy_labs() +
    theme_bw() +
    user_theme
  
  return(p)
}


## Plot residuals (y) vs predicted values (x) 
plot_resids_vs_pred_y <- function(df, y, user_theme=theme_norm){
  y_pred <- paste0(y, "_predicted")
  
  p <- df %>%
    mutate(residual=!!sym(y_pred) - !!sym(y)) %>%
    ggplot() +
    geom_point(aes(x=!!sym(y_pred), y=residual)) +
    geom_hline(yintercept=0, color="red", linetype="dashed") +
    ggtitle(paste("Residuals against predicted values of", 
                  str_replace_all(y, "_", " "))) +
    labs(y="Residual", 
         x=paste(str_replace_all(str_to_sentence(y), "_", " "), 
                 "(predicted)")) +
    theme_bw() +
    user_theme
  
  return(p)
}
















