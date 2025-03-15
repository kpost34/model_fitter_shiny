# Model Fitter Shiny App
## App code

# Modularized structure=============================================================================
## Load packages
pacman:: p_load(shiny, shinyjs, here, tidyverse, janitor, DT, tidymodels, ggiraph, ggfortify, mgcv,
                poissonreg)


## Load and/or wrangle datasets====================================================================
### Load objects
source("fns_objs/00_setup.R")


### Load modules
here("modules") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


# App
modelFitterApp <- function() {
  ## UI
  ui <- navbarPage(
    theme=bslib::bs_theme(bootswatch="yeti"),
    tags$head(
      tags$style(HTML("
        hr {
          border: 2px solid black;  /* Darker and thicker line */
          margin-top: 20px;
          margin-bottom: 20px;
        }
      "))
    ),
    useShinyjs(),
    title="Model Fitter Shiny App", 
    id="modNav",
  
    ### Bivariate Tab
    tabPanel("Bivariate Data", 
      bivarUI("bivar_df")),
    
    ### Bivariate + Dummy Tab
    tabPanel("Bivariate with Dummy Variable Data",
      bivarDumUI("bivar_dum_df")),
    
    ### App Info
    tabPanel("App Info",
      appInfoUI("app_info")),
    
    ### Developer Info Tab
    tabPanel("Developer Info",
      devInfoUI("dev_info"))

  )

  ## Server
  server <- function(input, output, session) {
    
    ### Bivariate
    bivarServer("bivar_df")
    
    ### Bivariate + Dummy
    bivarDumServer("bivar_dum_df")
    
    ### App Instructions
    #placeholder
    
    ### Developer Info
    #placeholder
    
    
  }

shinyApp(ui, server)

}

modelFitterApp()



