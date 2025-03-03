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
    
    ### Multivariable
    tabPanel("Multivariable Data",
      multvarUI("multvar_df"))
  )

  ## Server
  server <- function(input, output, session) {
    
    ### Bivariate
    bivarServer("bivar_df")
    
    ### Bivariate + Dummy
    bivarDumServer("bivar_dum_df")
    
    ### Multivariable
    multvarServer("multvar_df")
    
  }

shinyApp(ui, server)

}

modelFitterApp()