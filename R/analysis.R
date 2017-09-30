library(tidyverse)
library(caret)

source('R/aero.R')

unzip('inst/extdata/pfc_tra.zip', exdir = 'inst/extdata')

#datos análogos del aerogenerador 15
ae15 <- read_csv('inst/extdata/pfc_tra/ae15.csv')

#datos análogos del aerogenerador 19
ae19 <- read_csv('inst/extdata/pfc_tra/ae19.csv')

#datos análogos del aerogenerador 20
ae20 <- read_csv('inst/extdata/pfc_tra/ae20.csv')

#registro de eventos relacionados con fallo en el sistema de trasmisión de los aerogeneradores del parque
ae_tra <- read_csv('inst/extdata/pfc_tra/ae_tra.csv')

aero <- clean_aero(ae15)
aero <- clean_aero(ae19)
aero <- clean_aero(ae20)

analyze <- function(aero) {

  #split data set
  train_m <- round(aero %>% nrow * .7)
  training <- aero[1:train_m,]
  testing <- aero[(train_m+1):nrow(ae19_clean),]
  
  #linear regression  
  model <- lm(bearing_temp ~ ., data = training)
  summary(model)
  layout(matrix(1:4,2,2))
  plot(model)
  
  #We need to inspect the validity of the main assumptions of the linear regression model. This
  #refers, first of all, to the (conditional) distribution of the model’s errors terms 
  #variance, normality, and independence. Analysis of observed residuals ei may help to evaluate
  #the plausibility of these assumptions. Checking for unusual and influential observations is
  #another part of regression diagnostics. In addition, the validity of the structural model itself,
  #i.e., its linearity E(Y ) = Xβ and the selection of explanatory variables, should be examined.
  
  predictions <- predict(model, testing)
  new.modelvalues <- data.frame(obs = testing$bearing_temp, pred = predictions)
  defaultSummary(new.modelvalues)
  
  #observed vs predicted
  xyplot(new.modelvalues$obs ~ new.modelvalues$pred, type = c("p", "g"), xlab = "Predicted", ylab = "Observed")

  #anova
  anova(model)

  #shapiro test for normality, too sensitive for large samples
  shapiro.test(residuals(model)[1:5000])
  
  #variable importance
  varImp(model)
}