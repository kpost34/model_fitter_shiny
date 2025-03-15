# Model Fitter Shiny App
## Data Setup code

# Load Packages, Source Functions, and Load Data====================================================
pacman::p_load(tidyverse, janitor)
source(here("fns_objs/00_fn.R"))

data("trees")
data("ships", package = "MASS")

set.seed(39)



# Create DF vectors for UI==========================================================================
## Dataset names
vec_bivar_df <- c("Choose one"="",
                  "trees"="df_trees",
                  "ships"="df_ships",
                  "airquality"="df_airquality",
                  "iris"="df_iris",
                  "mtcars"="df_mtcars")

vec_bivar_dum_df <- c("Choose one"="",
                      "ships"="df_ships",
                      "iris"="df_iris",
                      "mtcars"="df_mtcars")


## Colors
vec_col_std <- c("black", "red", "green", "blue", "steelblue", "orange", 
                 "seagreen", "brown")


## Axis options
vec_axes_std <- c("Unspecified"="none",
                  "Forced 0 for x-axis"="x",
                  "Forced 0 for y-axis"="y",
                  "Forced 0 for both axes"="xy")


## Models
vec_mod_std <- c("none"="none",
                 "linear model"="lm", 
                 "poisson regression (glm)"="pois",
                 "gamma regression (glm)"="gamma",
                 "polynomial regression"="poly",
                 "generalized additive model (gam)"="gam")



# Wrangle Data======================================================================================
## Variable labels
var_labs_trees <- create_var_labs(x="girth", 
                                  y="volume", 
                                  x_name="Girth (in)",
                                  y_name="Volume (cu ft)")   


var_labs_ships <- create_var_labs(x="service",
                                  y="incidents",
                                  x2="type",
                                  x_name="Service (months)",
                                  y_name="Volume (cu ft)",
                                  x2_name="Type")


var_labs_airquality <- create_var_labs(x="temp",
                                       y="ozone",
                                       x_name="Temperature (oF)",
                                       y_name="Ozone (ppb)")


var_labs_iris <- create_var_labs(x="sepal_length",
                                 y="petal_length",
                                 x2="species",
                                 x_name="Sepal length (cm)",
                                 y_name="Petal length (cm)",
                                 x2_name="Iris species name")


var_labs_mtcars <- create_var_labs(x="wt",
                                   y="mpg",
                                   x2="vs",
                                   x_name="Car weight (1000 lb)",
                                   y_name="Miles per (US) gallon",
                                   x2_name="Engine type")


## Prep DFs
### trees
df_trees <- prep_df(df=trees, 
                    x=girth, 
                    y=volume)


### ships
df_ships <- prep_df(df=ships,
                    x=service,
                    y=incidents,
                    x2=type)


### airquality
df_airquality <- prep_df(df=airquality,
                         x=temp,
                         y=ozone)


### iris
df_iris <- prep_df(df=iris,
                   x=sepal_length,
                   y=petal_length,
                   x2=species)


### mtcars
df_mtcars <- prep_df(df=mtcars,
                     x=wt,
                     y=mpg,
                     x2=vs)



# Create Theme for Plotting=========================================================================
theme_norm <- theme(
  plot.title=element_text(face="bold", size=15),
  plot.subtitle=element_text(size=13),
  axis.title=element_text(size=14),
  axis.text=element_text(size=12),
  legend.title=element_text(size=14),
  legend.text=element_text(size=12),
  legend.position="bottom"
)



# String Objects for Application Instructions=======================================================
## App overview
app_overview1 <- "This app was developed as a simple way to explore and understand"
app_overview2 <- "relationships among variables in a dataset through modelling. The"
app_overview3 <- "app enables a user to choose one of two variable sets from multiple"
app_overview4 <- "datasets, select various plotting options, and explore fits by"
app_overview5 <- "different model types. Diagnostic information and performance"
app_overview6 <- "metrics of models are presented in multiple ways."
app_overview <- paste(app_overview1, app_overview2, app_overview3, app_overview4,
                      app_overview5, app_overview6)


## Notes
app_notes1 <- "Note that if no dataset is selected, then no output will be displayed"
app_notes2 <- "on any lower tab. If a dataset is selected but 'none' is chosen under"
app_notes3 <- "'Choose model' in the left menu, then no output will be presented on"
app_notes4 <- "all but the 'Training Data' tab. Finally, negative values and/or 0s"
app_notes5 <- "are not allowed for gamma regression models, and text indicated this"
app_notes6 <- "limitation will be presented in the 'Training Data' scatterplot."
app_notes <- paste(app_notes1, app_notes2, app_notes3, app_notes4, app_notes5, 
                   app_notes6)











