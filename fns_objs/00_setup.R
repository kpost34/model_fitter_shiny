# Model Fitter Shiny App
## Data Setup code

# Load Packages, Source Functions, and Load Data====================================================
pacman::p_load(tidyverse, janitor)
source(here("fns_objs/00_fn.R"))

data("trees")
data("ships", package = "MASS")

set.seed(39)



# Create DF vectors for UI==========================================================================
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

vec_col_std <- c("black", "red", "green", "blue", "steelblue", "orange", 
                 "seagreen", "brown")

vec_axes_std <- c("Unspecified"="none",
                  "Forced 0 for x-axis"="x",
                  "Forced 0 for y-axis"="y",
                  "Forced 0 for both axes"="xy")

vec_mod_std <- c("none"="none",
                 "linear model"="lm", 
                 "poisson regression (glm)"="pois",
                 "gamma regression (glm)"="gamma",
                 "polynomial regression"="poly",
                 "generalized additive model (gam)"="gam")



# Wrangle Data======================================================================================
## trees
df_trees <- prep_df(df=trees, 
                    x=girth, 
                    y=volume, 
                    x_name="Girth (in)",
                    y_name="Volume (cu ft)")                          


## ships
df_ships <- prep_df(df=ships,
                    x=service,
                    y=incidents,
                    x2=type,
                    x_name="Service (months)",
                    y_name="Volume (cu ft)",
                    x2_name="Type")


## airquality
df_airquality <- prep_df(df=airquality,
                         x=temp,
                         y=ozone,
                         x_name="Temperature (oF)",
                         y_name="Ozone (ppb)")


## iris
df_iris <- prep_df(df=iris,
                   x=sepal_length,
                   y=petal_length,
                   x2=species,
                   x_name="Sepal length (cm)",
                   y_name="Petal length (cm)",
                   x2_name="Iris species name")


## mtcars
df_mtcars <- prep_df(df=mtcars,
                     x=wt,
                     y=mpg,
                     x2=vs,
                     x_name="Car weight (1000 lb)",
                     y_name="Miles per (US) gallon",
                     x2_name="Engine type")



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





