# Developer Info Module

# UI================================================================================================
devInfoUI <- function(id) {
  tagList(
    div(
      h4(strong("Keith Post")),
      br(),
      p("If you would like to see the code for this Shiny app, please visit my",
        tags$a(href="https://github.com/kpost34/model_fitter_shiny",
               "Github repo"),
        "for this project."
      ),
      p(tags$a(href="https://github.com/kpost34","GitHub Profile")),
      p(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn")),
      style="margin-left: 10%; margin-right: 10%"
    )
  )
}
