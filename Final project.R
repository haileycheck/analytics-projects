feb_data <- tomslee_airbnb_las_vegas_0919_2017.02.25[,-12:-14]
feb_data$set = 0

august_data <- tomslee_airbnb_las_vegas_1507_2017.07.20[-c(2,5,6,13,16:19)]
august_data$set = 1

combinedset <- rbind(feb_data, august_data)

set.seed(1616)

num_obs <- nrow(combinedset)

train_obs <- sample(num_obs,0.67*num_obs)
rental_train <- combinedset[train_obs,]
rental_test <- combinedset[-train_obs,]

model1 <- lm(overall_satisfaction~set+bedrooms+price, data=combinedset)
summary(model1)

lin_preds1 <- predict(model1,newdata=rental_test)

RMSE_test1 <- sqrt(mean((lin_preds1-rental_test$overall_satisfaction)^2))
RMSE_test1

model2 <- lm(overall_satisfaction~set+reviews+accommodates+bedrooms+price, data=combinedset)
summary(model2)

lin_preds2 <- predict(model2,newdata=rental_test)

RMSE_test2 <- sqrt(mean((lin_preds2-rental_test$overall_satisfaction)^2))
RMSE_test2

model3 <- lm(overall_satisfaction~set+reviews+accommodates+bedrooms+price+(bedrooms*price)+(accommodates*price)+(bedrooms*accommodates)+(bedrooms*accommodates*price), data=combinedset)
summary(model3)

lin_preds3 <- predict(model3,newdata=rental_test)

RMSE_test3 <- sqrt(mean((lin_preds3-rental_test$overall_satisfaction)^2))
RMSE_test3

