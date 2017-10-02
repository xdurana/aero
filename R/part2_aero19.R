library(tidyverse)
library(caret)

aero19 <- aero %>% filter(aero == 'aero19') %>% select(-date_time, -aero)

#split data set
train_m <- round(aero19 %>% nrow * .7)
training <- aero19[1:train_m,]
testing <- aero19[(train_m+1):nrow(aero),]

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
