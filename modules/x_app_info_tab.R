# App Ifo Module

# UI================================================================================================
appInfoUI <- function(id) {
  tagList(
    #div is to set up margins
    div(
      h4(strong("Application info")),
      tags$li(strong("App Overview")),
      app_overview,
      br(),
      br(),
      tags$li(strong("Important Notes")),
      app_notes,
      style="margin-left: 15%; margin-right: 15%"
    )
  )
}

