# Bivariate Module

# UI================================================================================================
bivarUI <- function(id) {
  build_ui(id=id, vec_df=vec_bivar_df)
  # build_ui(id="bivar_df", vec_df=vec_bivar_df)
}



# Server============================================================================================
bivarServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  ## Create reactives
  df_mod <- reactive({
    req(nchar(input$sel_ds) > 0)
    get(input$sel_ds) 
  })
  
  mod_split <- reactive({
    initial_split(df_mod(), prop=0.8) #later prop will be modifiable
  })
  
  df_mod_train <- reactive({
    training(mod_split())
  })
  
  df_mod_test <- reactive({
    testing(mod_split())
  })
  
  x_var <- reactive({
    switch(input$sel_ds,
      df_trees="girth",
      df_ships="service",
      df_airquality="temp",
      df_iris="sepal_length",
      df_mtcars="wt"
    )
  })

  y_var <- reactive({
    switch(input$sel_ds,
      df_trees="volume",
      df_ships="incidents",
      df_airquality="ozone",
      df_iris="petal_length",
      df_mtcars="mpg"
    )
  })
  
  y_var_pred <- reactive({
    req(y_var())
    
    paste0(y_var(), "_pred")
  })
  
  
  y_var_actual <- reactive({
    req(y_var())
    
    paste0(y_var(), "_actual")
  })
  
  y_var_type <- reactive({
    req(y_var())
    
    paste0(y_var(), "_type")
  })
  
  r2_train <-reactive({
    req(mod_train)
    
    switch(input$rad_mod_select,
      "lm"=summary(mod_train())[["r.squared"]] %>% 
        round(3),
      "pois"=1 - (mod_train()[["deviance"]]/mod_train()[["null.deviance"]]) %>%
        round(3),
      "gamma"=1 - (mod_train()[["deviance"]]/mod_train()[["null.deviance"]]) %>%
        round(3),
      "poly"=summary(mod_train())[["r.squared"]] %>%
        round(3),
      "gam"=summary(mod_train())[["dev.expl"]] %>%
        round(3)
    )
  })

  
  ## Generate sample table
  #render table conditionally
  output$ui_tab_train_samp <- renderUI({
    req(input$chk_ds)
    if(input$chk_ds) {
      DTOutput(session$ns("tab_train_samp"))
    }
  })
  
  #render table
  output$tab_train_samp <- renderDT({
    datatable(sample_n(df_mod_train(), 5),
      rownames=FALSE,
      options=list(dom="t")
    )
  })
  
  #hide/show table
  observeEvent(input$chk_ds, {
    toggle(session$ns("tab_train_samp"))
  })
  
  
  ## Generate scatterplot of training data
  output$plot_train <- renderPlot({
    req(df_mod_train())
    make_scatter_train(df=df_mod_train(),
                       x=x_var(),
                       y=y_var(),
                       forced0=input$sel_plot_train_axes,
                       col=input$sel_plot_train_color,
                       mod=input$rad_mod_select,
                       r2_value=r2_train())
                       
  })
  
  
  ## Fit model
  #formula
  form_train <- reactive({
    req(input$rad_mod_select!="none")
    
    if(input$rad_mod_select=="poly") {
      as.formula(paste0(y_var(), " ~ poly(", x_var(), ", 2)"))
    } else if(input$rad_mod_select=="gam") {
      as.formula(paste0(y_var(), " ~ s(", x_var(), ")"))
      } else{as.formula(paste0(y_var(), " ~ ", x_var()))}
  })
  
  #tidymodel
  tidymod_train <- reactive({
    req(input$rad_mod_select!="none")
    
    #for cases where there's 0 or - values and gamma model is selected
    result <- tryCatch({
      fit_tidymodel(type=input$rad_mod_select,
                    df=df_mod_train(),
                    formula_mod=form_train())
    }, error=function(e) {
      NULL
    })
    
    return(result)
  })
  
  #model
  mod_train <- reactive({
    req(tidymod_train())
    
    tidymod_train() %>%
      extract_fit_engine()
  })
  
  
  ## Model summary
  output$tab_mod_summ <- renderDT({
    req(df_mod_train(), mod_train())

    datatable(
      tidy(mod_train()) %>%
        mutate(across(where(is.numeric), ~signif(.x, 3))),
      rownames=FALSE,
      options=list(dom="t")
    )
  })
  
  
  ## Model diagnostics
  output$plot_train_diag <- renderPlot({
    req(mod_train())
    req(class(mod_train())!="character")
    
    if(input$rad_mod_select=="none"){
      NULL
    } else if(input$rad_mod_select %in% c("lm", "pois", "gamma", "poly")) {
      autoplot(mod_train(), which = c(1:3, 5)) +
        theme_bw() +
        theme_norm
    } else if(input$rad_mod_select=="gam") {
      par(mfrow=c(2, 2))
      gam.check(mod_train())
      par(mfrow=c(1, 1))
    }
  })
  
  
  ## Prediction
  ### Plot of test values against model
  #create reactive DF of test values and model
  df_mod_test_mod <- reactive({
    make_pred_values(type=input$rad_mod_select, mod=mod_train(), df=df_mod_test(), x=x_var())
  })
  
  
  #plot values--need to functionalize this
  output$plot_mod_test <- renderPlot({
    df_mod_test_mod() %>%
      ggplot() +
      geom_line(aes(x=!!sym(x_var()), y=fit)) +
      geom_ribbon(aes(x=!!sym(x_var()), ymin=lwr, ymax=upr),
                  color='gray', alpha=0.3) +
      geom_point(data=df_mod_test(), 
                 aes(x=!!sym(x_var()),
                     y=!!sym(y_var())),
                 color=input$sel_plot_train_color) +
      labs(y=y_var()) +
      theme_bw() +
      theme_norm
  })
  
  
  ### Generate DFs of actual/predicted test values
  #create DF of actual and predicted test values
  df_mod_test_pred <- reactive({
    
    tidymod_train() %>%
      predict(new_data=df_mod_test()) %>%
      bind_cols(df_mod_test()) %>%
      select(all_of(c(x_var(), y_var())), !!y_var_pred() := ".pred")
  })
  
  #pivot to long version
  df_mod_test_pred_long <- reactive({
    df_mod_test_pred() %>%
      rename(!!y_var_actual() := y_var()) %>%
      pivot_longer(cols=c(y_var_actual(), y_var_pred()), 
                   names_to=y_var_type(), 
                   values_to=y_var(), 
                   names_pattern=paste0(y_var(), "_(.*$)")) 
  })
  
  
  ### Generate metrics table
  #get range of y var in test data
  test_y_rng <- reactive({
    max(df_mod_test()[[y_var()]]) - min(df_mod_test()[[y_var()]])
  })
  
  output$tab_mod_test_pred <- renderDT(
    datatable(
      df_mod_test_pred() %>%
        metrics(truth=y_var(), estimate=y_var_pred()) %>%
        rename(metric=".metric", estimate=".estimate") %>%
        mutate(rel_est=ifelse(metric %in% c("rmse", "rsq"),
                              (estimate/test_y_rng()) * 100,
                              estimate),
               estimate=signif(estimate, 3)) %>%
        categorize_metric() %>%
        select(metric, estimate, strength),
      rownames=FALSE,
      options=list(dom="t")
    )
  )
  
  
  ### Generate plots and table
  #actual and predicted test values versus x
  output$plot_test_actual_pred_x <- renderPlot({
    df_mod_test_pred_long() %>%
      ggplot() +
      geom_point(aes(x=!!sym(x_var()), y=!!sym(y_var()), color=!!sym(y_var_type())), 
                 shape=16, size=3, alpha=0.7) +
      scale_color_manual(values=c("actual"="darkred", "pred"="darkblue")) +
      labs(title=paste("Actual and predicted", y_var(), "values versus", x_var())) +
      theme_bw() +
      theme_norm
  })
  
  #actual (y) vs predicted (x) plot
  output$plot_test_actual_pred <- renderPlot({
    df_mod_test_pred() %>%
      rename(!!y_var_actual():=y_var()) %>%
      ggplot(aes(x=!!sym(y_var_pred()), y=!!sym(y_var_actual()))) +
      geom_point(alpha=0.5) +
      geom_abline(slope=1) +
      theme_bw() +
      theme_norm
  })
  
  #residual (y) vs predicted (x) plot
  output$plot_test_resid_pred <- renderPlot({
    df_mod_test_pred() %>%
      mutate(residual=!!sym(y_var_pred()) - !!sym(y_var())) %>%
      ggplot() +
      geom_point(aes(x=!!sym(y_var_pred()), y=residual)) +
      geom_hline(yintercept=0, color="red", linetype="dashed") +
      theme_bw() +
      theme_norm
  })
  
  #table of summary residual data
  output$tab_mod_resid <-renderDT(
    datatable(
      df_mod_test_pred() %>%
        generate_resid_summ(y=y_var(), y_pred=y_var_pred()), 
        caption = htmltools::tags$caption(style='caption-side: top; text-align: left; color:black;  
                                          font-size:200% ;','Residual Summary Stats'),
      rownames=FALSE,
      options=list(dom="t")
    )
  )
  })
}
