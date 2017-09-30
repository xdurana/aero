aero <- aero15

#split data set
train_m <- round(aero %>% nrow * .7)
training <- aero[1:train_m,]
testing <- aero[(train_m+1):nrow(aero),]

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
