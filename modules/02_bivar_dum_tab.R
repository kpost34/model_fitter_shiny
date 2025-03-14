# Bivariate + Dummy Module

# UI================================================================================================
bivarDumUI <- function(id) {
  build_ui(id=id, vec_df=vec_bivar_dum_df)
}



# Server============================================================================================
bivarDumServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  ## Create reactives
  ### Variable labels
  var_labs <- reactive({
    switch(input$sel_ds,
      df_trees=var_labs_trees,
      df_ships=var_labs_ships,
      df_airquality=var_labs_airquality,
      df_iris=var_labs_iris,
      df_mtcars=var_labs_mtcars
    )
  })
  
  
  var_labs_pred <- reactive({
    var_labs() %>%
      c("lwr"="lwr", "upr"="upr")
  })
  
  var_labs_pred_type <- reactive({
    var_labs() %>%
      c(set_names(str_replace_all(str_to_sentence(paste0(y_var(), "_type")), "_", " "),
                  paste0(y_var(), "_type")))
  })
    
    
  ### Data frames
  #full dataset
  df_mod <- reactive({
    req(nchar(input$sel_ds) > 0)
    get(input$sel_ds) %>%
      labelled::set_variable_labels(.labels=var_labs())
  })
  
  #split into train/test
  mod_split <- reactive({
    initial_split(df_mod(), prop=0.8) 
  })
  
  df_mod_train <- reactive({
    training(mod_split())
  })
  
  df_mod_test <- reactive({
    testing(mod_split())
  })
  
  
  ### Variables/metric
  x_var <- reactive({
    switch(input$sel_ds,
      df_ships="service",
      df_iris="sepal_length",
      df_mtcars="wt"
    )
  })
  
  x2_var <- reactive({
    switch(input$sel_ds,
      df_ships="type",
      df_iris="species",
      df_mtcars="vs")
  })

  y_var <- reactive({
    switch(input$sel_ds,
      df_ships="incidents",
      df_iris="petal_length",
      df_mtcars="mpg"
    )
  })
  
  
  r2_train <- reactive({

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
                       x2=x2_var(),
                       forced0=input$sel_plot_train_axes,
                       col=input$sel_plot_train_color,
                       mod=input$rad_mod_select,
                       r2_value=r2_train())
  })
  
  
  ## Fit model
  #formulas
  form_train <- reactive({
    req(input$rad_mod_select!="none")

    if(input$rad_mod_select=="poly") {
      as.formula(paste0(y_var(), " ~ poly(", x_var(), ", 2) * ", x2_var()))
    } else if(input$rad_mod_select=="gam") {
      as.formula(paste0(y_var(), " ~ s(", x_var(), ", by=", x2_var(), ") + ", x2_var()))
      } else{as.formula(paste(y_var(), "~", x_var(), "*", x2_var()))}
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


  ## Model summary table
  output$tab_mod_summ <- renderDT({
    req(df_mod_train(), mod_train())

    datatable(
      tidy(mod_train()) %>%
        mutate(across(where(is.numeric), ~signif(.x, 3))),
      rownames=FALSE,
      options=list(dom="t")
    )
  })
  

  ## Model diagnostics plot
  output$plot_train_diag <- renderPlot({
    req(mod_train())
    req(class(mod_train())!="character")
    
    #no plot if "none" selected
    if(input$rad_mod_select=="none"){
      NULL
    #autoplot if all models but gam selected
    } else if(input$rad_mod_select %in% c("lm", "pois", "gamma", "poly")) {
      autoplot(mod_train(), which = c(1:3, 5)) +
        theme_bw() +
        theme_norm
    #use gam.check() output if gam selected
    } else if(input$rad_mod_select=="gam") {
      par(mfrow=c(2, 2))
      par(cex.axis = 1.2)    #axis text size
      par(cex.lab = 1.5)     #axis label size
      par(cex.main = 1.5)    #title size
      gam.check(mod_train())
      par(mfrow=c(1, 1))
    }
  })
  

  ## Prediction
  ### Plot of test values against model
  #create reactive DF of test values and model
  df_mod_test_mod <- reactive({
    make_pred_values(type=input$rad_mod_select, 
                     mod=mod_train(), 
                     df=df_mod_test(), 
                     x=x_var(), 
                     x2=x2_var(),
                     y=y_var())
  })
  
  
  #plot values
  output$plot_mod_test <- renderPlot({
    req(mod_train())
    
    make_scatter_test(df=df_mod_test_mod(),
                      df2=df_mod_test(),
                      var_labs=var_labs_pred(),
                      x=x_var(),
                      y=y_var(),
                      col=input$sel_plot_train_color,
                      x2=x2_var())
  })
  

  ### Generate DFs of actual/predicted test values
  #create DF of actual and predicted test values
  df_mod_test_pred <- reactive({
    combine_test_pred_values(tidymod=tidymod_train(),
                             df=df_mod_test(),
                             x=x_var(),
                             y=y_var(),
                             x2=x2_var())
  })
  
  #pivot to long version
  df_mod_test_pred_long <- reactive({
    pivot_test_pred_long(df=df_mod_test_pred(), y=y_var())
  })
  
  
  ### Generate metrics table
  #get range of y var in test data
  test_y_rng <- reactive({
    max(df_mod_test()[[y_var()]]) - min(df_mod_test()[[y_var()]])
  })
  
  #create table
  output$tab_mod_test_pred <- renderDT({
    req(mod_train())
    
    datatable(
      generate_pred_metrics(df=df_mod_test_pred(),
                            y=y_var(),
                            y_range=test_y_rng()),
      rownames=FALSE,
      options=list(dom="t")
    )
  })


  ### Generate plots and table
  #actual and predicted test values versus x
  output$plot_test_actual_pred_x <- renderPlot({
    req(mod_train())
    
    plot_actual_pred_y_vs_x(df=df_mod_test_pred_long(),
                            var_labs=var_labs_pred_type(),
                            x=x_var(),
                            y=y_var(),
                            x2=x2_var(),
                            user_theme=theme_norm)
  })
  
  #actual (y) vs predicted (x) plot
  output$plot_test_actual_pred <- renderPlot({
    req(mod_train())
    
    plot_actual_vs_pred_y(df=df_mod_test_pred(),
                          y=y_var(),
                          col=input$sel_plot_train_color,
                          x2=x2_var(),
                          user_theme=theme_norm)
  })
  
  #residual (y) vs predicted (x) plot
  output$plot_test_resid_pred <- renderPlot({
    req(mod_train())
    
    plot_resids_vs_pred_y(df=df_mod_test_pred(),
                          y=y_var(),
                          user_theme=theme_norm)
  })
  
  #table of summary residual data
  output$tab_mod_resid <-renderDT({
    req(mod_train())
    
    datatable(
      generate_resid_summ(df=df_mod_test_pred(), y=y_var()), 
      caption = htmltools::tags$caption(style='caption-side: top; text-align: left; color:black;  
                                        font-size:200% ;','Residual Summary Stats'),
      rownames=FALSE,
      options=list(dom="t")
    )
  })
  })
}
  
  
