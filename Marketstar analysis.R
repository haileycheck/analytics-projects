#Count of qualified leads by sales rep and conversion rates
data2$Stage1 <- 0
data2$Stage1 <- ifelse(data2$Stage=="Qualified",1,0)
tapply(data2$Stage1, data2$EmployeeId, sum)
aggregate(data2$Stage1,list(data2$EmployeeId), mean)
aggregate(data2$Stage1,list(data2$EmployeeId, data2$CreatedQuarter), mean)

aggregate(data2$Stage,list(data2$EmployeeId), mean)

#lead sources conversion rates and count of converted opportunities
data2$LeadSource1 <- as.factor(data2$LeadSource)
tapply(data2$Stage1, data2$LeadSource1, sum)
aggregate(data2$Stage1,list(data2$LeadSource1), mean)
aggregate(data2$Stage1,list(data2$LeadSource1, data2$CreatedQuarter), mean)

#conversion rates by activity
tapply(data2$Stage, data2$ActivityType, sum)
aggregate(data2$Stage, list(data2$ActivityType), mean)
aggregate(data2$Stage1, list(data2$ActivityType, data2$ActivityQuarter), mean)


summary(data2$DisqualifiedReason1)



data2$ActivityType1 <- as.factor(data2$ActivityType)
data2$Outcome1 <- as.factor(data2$Outcome)
data2$Title1 <- as.factor(data2$Title)
data2$Industry1 <- as.factor(data2$Industry)
data2$TimeZone1 <- as.factor(data2$TimeZone)
data2$DisqualifiedReason1 <- as.factor(data2$DisqualifiedReason)


library(glmnet)

converted_all <- lm(Stage1 ~ EmployeeId + YearsOfExperience + Employees + LeadSource1 + Amount + OwnerEmployeeId + TimeZone1 + ActivityQuarter + CreatedQuarter + ActivityType1 + Outcome1 + Title1 + Industry1 + DisqualifiedReason1, data = data2)
summary(converted_all)

converted_null <- lm(Stage1 ~ 1, data=data2)
summary(converted_null)

backward_converted <- step(converted_all, direction="backward")
summary(backward_converted)
 

forward_converted <- step(converted_null, scope=list(upper=converted_all), direction="forward", trace=1)
summary(forward_converted)

converted_both <- step(converted_null, scope=list(upper=converted_all), direction="both", trace=0)
summary(converted_both)


amount_all <- lm(Amount ~ EmployeeId + YearsOfExperience + Employees + LeadSource1 + Stage1 + OwnerEmployeeId + TimeZone1 + ActivityQuarter + CreatedQuarter + ActivityType1 + Outcome1 + Title1 + Industry1 + DisqualifiedReason1, data = data2)
summary(amount_all)

amount_null <- lm(Amount ~ 1, data=data2)
summary(converted_null)

backward_amount <- step(amount_all, direction="backward")
summary(backward_amount)


forward_amount <- step(amount_null, scope=list(upper=amount_all), direction="forward", trace=1)
summary(amount_converted)

amount_both <- step(amount_null, scope=list(upper=amount_all), direction="both", trace=1)
summary(amount_both)






amount_tree <- tree(Amount~EmployeeId + YearsOfExperience + Employees + LeadSource1 + Stage1 + OwnerEmployeeId + TimeZone1 + ActivityQuarter + CreatedQuarter + ActivityType1 + Outcome1 + DisqualifiedReason1, data = data2)
summary(amount_tree)
plot(amount_tree)
text(amount_tree)

test_instn <- sample(nrow(data2), 0.3*nrow(data2))
data_test <- data2[test_instn,]
data_rest <- data2[-test_instn,]

mean(opportunities$Amount)


