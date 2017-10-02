library(tidyverse)
library(caret)

aero20 <- aero %>% filter(aero == 'aero20') %>% select(-date_time, -aero)

#split data set
train_m <- round(aero20 %>% nrow * .7)
training <- aero20[1:train_m,]
testing <- aero20[(train_m+1):nrow(aero),]

#linear regression  
aero_model <- lm(bearing_temp ~ ., data = training)
aero_model %>% summary
layout(matrix(1:4,2,2))
plot(aero_model)

predictions <- predict(aero_model, testing)
new.modelvalues <- data.frame(obs = testing$bearing_temp, pred = predictions)
defaultSummary(new.modelvalues)

#observed vs predicted
xyplot(new.modelvalues$obs ~ new.modelvalues$pred,
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(0,1)
       },
       type = c("p", "g"),
       xlab = "Predicted",
       ylab = "Observed")
