

###Packages for this session-------------
#install.packages("dplyr")
#install.packages("psych")
#install.packages("sjlabelled")
#install.packages("interactions")
#install.packages("misty")
#install.packages("sjPlot")
#install.packages("broom")
#install.packages("parameters")
#install.packages("reghelper")

library(dplyr)
library(psych)
library(sjlabelled)
library(interactions)
library(misty)
library(sjPlot)
library(broom)
library(parameters)
library(reghelper)

###Get the data------------
data <- readr::read_csv("https://raw.githubusercontent.com/RKesberg/Skills_Lab/main/dat.csv")
describe(data)

### Centering data ------

# method 1
data <- data%>%
  dplyr::mutate(
    var1_c = var1 - mean(var1, na.rm = T),
    var2_c = var2 - mean(var2, na.rm = T)
  )

# making it faster 

#write function
centre <- function(var){
  var - mean(var, na.rm = T)
}

data <- data%>%
  dplyr::mutate(
    dplyr::across(c(var1, var2), list(cent = centre))
  )

# method 3

data <- data%>%
  dplyr::mutate(
    misty::center(var1),
    misty::center(var2))

data <- data%>%
  dplyr::mutate(
    misty::center(var1, value = 0),
    misty::center(var2, value = 0)) # Can be any value you'd like

####Creating interaction terms---------

#model: y = a + b + a*b
#model: relationship problems = alcohol consume + impulsiveness + alcohol consume*impulsiveness

#create interaction term

data <- data %>% 
  dplyr::mutate(
    interaction = var1_cent*var2_cent  #make sure to use the centered predictors
  )

#not necessary, but good to know

#describe(data)

###Running the model--------------

# method 1

model.a <- lm(DV ~ var1_cent + var2_cent + var1_cent:var2_cent, data = data)

# method 2

model.b <- lm(DV ~ var1_cent*var2_cent, data = data)

# method 3

model.c <- lm(DV ~ var1_cent + var2_cent + interaction, data = data) # if interaction is created by hand

### Check outputs

summary(model)

broom::glance(model)
broom::tidy(model, conf.int = T)%>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~round(.,3))
  )

parameters::model_parameters(model, robust = T, vcov.type = "HC4", digits = 3)

###Interpreting the interaction effect-----

# method 1

interactions::sim_slopes(
  model,
  pred = alc_cent,
  modx = impulsive_cent,
  jnplot = T,
  robust = T,
  confint = T
)

# method 2

reghelper::simple_slopes(model)

####Plotting the results----

# method 1

interactions::interact_plot(
  model,
  pred = var1_cent,
  modx = var2_cent,
  interval = T,
  x.label = "predictor",
  y.label = "criteria",
  legend.main = "moderator"
)

# method 2

sjPlot::plot_model(
  model,
  type = "int",
  mdrt.values = "meansd",
  axis.title = c("predictor", "criteria"),
  title = "title",
  legend.title = "moderator"
)

### Categorical predictors
# Run and inspect the model
model.2 <- lm(rel_prob ~ alc_cent*gender, data = data)
summary(model.2)

# Examine the simple effect
interactions::sim_slopes(
  model.2,
  pred = alc_cent,
  modx = gender,
  jnplot = T,  robust = T,  confint = T
)

# Plot the model
interactions::interact_plot(
  model.2,
  pred = alc_cent,
  modx = gender,  interval = T,
  x.label = "Alcohol consumption (in ml)",
  y.label = "Predicted relationship problems",
  legend.main = "Gender"
)