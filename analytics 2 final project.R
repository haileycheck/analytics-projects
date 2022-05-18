library("dplyr")
#remove rows that have NA values for average rating
Airbnb_Training$accommodates <- as.numeric(Airbnb_Training$accommodates)
Airbnb_Training$availability_30 <- as.numeric(Airbnb_Training$availability_30)
Airbnb_Training$availability_60 <- as.numeric(Airbnb_Training$availability_60)
Airbnb_Training$availability_90 <- as.numeric(Airbnb_Training$availability_90)
Airbnb_Training$bathrooms <- as.numeric(Airbnb_Training$bathrooms)
Airbnb_Training$bedrooms <- as.numeric(Airbnb_Training$bedrooms)
Airbnb_Training$cleaning_fee <- as.numeric(Airbnb_Training$cleaning_fee)
Airbnb_Training$host_identity_verified <- ifelse(Airbnb_Training$host_identity_verified=="t",1,0)
Airbnb_Training$host_is_superhost <- ifelse(Airbnb_Training$host_is_superhost=="t",1,0)
Airbnb_Training$host_has_profile_pic <- ifelse(Airbnb_Training$host_has_profile_pic=="t",1,0)
Airbnb_Training$host_listings_count <- as.numeric(Airbnb_Training$host_listings_count)
Airbnb_Training$host_total_listings_count <- as.numeric(Airbnb_Training$host_total_listings_count)
Airbnb_Training$instant_bookable<- ifelse(Airbnb_Training$instant_bookable=="t",1,0)
Airbnb_Training$is_business_travel_ready <- ifelse(Airbnb_Training$is_business_travel_ready=="t",1,0)
Airbnb_Training$is_location_exact <- ifelse(Airbnb_Training$is_location_exact=="t",1,0)
Airbnb_Training$requires_license <- ifelse(Airbnb_Training$requires_license=="t",1,0)
Airbnb_Training$bed_type <- ifelse(Airbnb_Training$bed_type=="Real Bed",1,0)
Airbnb_Training$cancellation_policy <- as.factor(Airbnb_Training$cancellation_policy)
Airbnb_Training$experiences_offered <- ifelse(Airbnb_Training$experiences_offered=="none",0,1)
Airbnb_Training$host_response_rate <- as.numeric(Airbnb_Training$host_response_rate)
Airbnb_Training$host_response_time <- ifelse(Airbnb_Training$host_response_time=="within an hour",1,0)
Airbnb_Training$property_type <- as.factor(Airbnb_Training$property_type)
Airbnb_Training$requires_guest_phone_verification <- ifelse(Airbnb_Training$requires_guest_phone_verification=="f",0,1)
Airbnb_Training$requires_guest_profile_picture <- ifelse(Airbnb_Training$requires_guest_profile_picture=="t",1,0)
Airbnb_Training$room_type <- as.factor(Airbnb_Training$room_type)

#impute the rest of the NA values
Airbnb_Training1 <- Airbnb_Training[-c(13584,16246,24317,29879),-c(1,4,13,14,16,21,28,30,31,35,36,40,42:44,46,48,49)]

summary(Airbnb_Training1)

Airbnb_Training1$bathrooms[is.na(Airbnb_Training1$bathrooms)]=1
Airbnb_Training1$beds[is.na(Airbnb_Training1$beds)]=1
Airbnb_Training1$cleaning_fee[is.na(Airbnb_Training1$cleaning_fee)]=54.92629
Airbnb_Training1$host_listings_count[is.na(Airbnb_Training1$host_listings_count)]= 1
Airbnb_Training1$host_response_rate[is.na(Airbnb_Training1$host_response_rate)]=1
Airbnb_Training1$host_response_time[is.na(Airbnb_Training1$host_response_time)]=0
Airbnb_Training1$host_total_listings_count[is.na(Airbnb_Training1$host_total_listings_count)]=1
Airbnb_Training1$monthly_price[is.na(Airbnb_Training1$monthly_price)]=2400
Airbnb_Training1$security_deposit[is.na(Airbnb_Training1$security_deposit)]=200
Airbnb_Training1$weekly_price[is.na(Airbnb_Training1$weekly_price)]=700
Airbnb_Training1$bedrooms[is.na(Airbnb_Training1$bedrooms)]=1

summary(Airbnb_Training1)

Airbnb_all <- lm(avg_rating~., data = Airbnb_Training1)
Airbnb_null <- lm(avg_rating~1, data= Airbnb_Training1)


#turn date into years since variable (compare to host_since?)

#partition into validation data

Airbnb_Testing$accommodates <- as.numeric(Airbnb_Testing$accommodates)
Airbnb_Testing$availability_30 <- as.numeric(Airbnb_Testing$availability_30)
Airbnb_Testing$availability_60 <- as.numeric(Airbnb_Testing$availability_60)
Airbnb_Testing$availability_90 <- as.numeric(Airbnb_Testing$availability_90)
Airbnb_Testing$bathrooms <- as.numeric(Airbnb_Testing$bathrooms)
Airbnb_Testing$bedrooms <- as.numeric(Airbnb_Testing$bedrooms)
Airbnb_Testing$cleaning_fee <- as.numeric(Airbnb_Testing$cleaning_fee)
Airbnb_Testing$host_identity_verified <- ifelse(Airbnb_Testing$host_identity_verified=="t",1,0)
Airbnb_Testing$host_is_superhost <- ifelse(Airbnb_Testing$host_is_superhost=="t",1,0)
Airbnb_Testing$host_has_profile_pic <- ifelse(Airbnb_Testing$host_has_profile_pic=="t",1,0)
Airbnb_Testing$host_listings_count <- as.numeric(Airbnb_Testing$host_listings_count)
Airbnb_Testing$host_total_listings_count <- as.numeric(Airbnb_Testing$host_total_listings_count)
Airbnb_Testing$instant_bookable<- ifelse(Airbnb_Testing$instant_bookable=="t",1,0)
Airbnb_Testing$is_business_travel_ready <- ifelse(Airbnb_Testing$is_business_travel_ready=="t",1,0)
Airbnb_Testing$is_location_exact <- ifelse(Airbnb_Testing$is_location_exact=="t",1,0)
Airbnb_Testing$requires_license <- ifelse(Airbnb_Testing$requires_license=="t",1,0)
Airbnb_Testing$bed_type <- ifelse(Airbnb_Testing$bed_type=="Real Bed",1,0)
Airbnb_Testing$cancellation_policy <- as.factor(Airbnb_Testing$cancellation_policy)
Airbnb_Testing$experiences_offered <- ifelse(Airbnb_Testing$experiences_offered=="none",0,1)
Airbnb_Testing$host_response_rate <- as.numeric(Airbnb_Testing$host_response_rate)
Airbnb_Testing$host_response_time <- ifelse(Airbnb_Testing$host_response_time=="within an hour",1,0)
Airbnb_Testing$property_type <- as.factor(Airbnb_Testing$property_type)
Airbnb_Testing$requires_guest_phone_verification <- ifelse(Airbnb_Testing$requires_guest_phone_verification=="f",0,1)
Airbnb_Testing$requires_guest_profile_picture <- ifelse(Airbnb_Testing$requires_guest_profile_picture=="t",1,0)
Airbnb_Testing$room_type <- as.factor(Airbnb_Testing$room_type)



Airbnb_Testing1 <- Airbnb_Testing[,-c(1,3,12,13,15,20,27,29,30,34,35,39,41:43,45,47,48)]
#replace NA values
summary(Airbnb_Testing1)

Airbnb_Testing1$bathrooms[is.na(Airbnb_Testing1$bathrooms)]=1
Airbnb_Testing1$beds[is.na(Airbnb_Testing1$beds)]=1
Airbnb_Testing1$cleaning_fee[is.na(Airbnb_Testing1$cleaning_fee)]=54.92629
Airbnb_Testing1$host_listings_count[is.na(Airbnb_Testing1$host_listings_count)]=1
Airbnb_Testing1$host_response_rate[is.na(Airbnb_Testing1$host_response_rate)]=1 
Airbnb_Testing1$host_response_time[is.na(Airbnb_Testing1$host_response_time)]=0
Airbnb_Testing1$host_total_listings_count[is.na(Airbnb_Testing1$host_total_listings_count)]=1
Airbnb_Testing1$monthly_price[is.na(Airbnb_Testing1$monthly_price)]=3054.747
Airbnb_Testing1$security_deposit[is.na(Airbnb_Testing1$security_deposit)]=305.2132
Airbnb_Testing1$weekly_price[is.na(Airbnb_Testing1$weekly_price)]=940.0916
Airbnb_Testing1$bedrooms[is.na(Airbnb_Testing1$bedrooms)]=1
Airbnb_Testing1$accommodates[is.na(Airbnb_Testing1$accommodates)]=3
Airbnb_Testing1$availability_30[is.na(Airbnb_Testing1$availability_30)]=8
Airbnb_Testing1$availability_60[is.na(Airbnb_Testing1$availability_60)]=22
Airbnb_Testing1$availability_90[is.na(Airbnb_Testing1$availability_90)]=41
Airbnb_Testing1$first_review[is.na(Airbnb_Testing1$first_review)]=42511
Airbnb_Testing1$maximum_nights[is.na(Airbnb_Testing1$maximum_nights)]=1125
Airbnb_Testing1$minimum_nights[is.na(Airbnb_Testing1$minimum_nights)]=2
Airbnb_Testing1$price[is.na(Airbnb_Testing1$price)]=109

summary(Airbnb_Testing1)

library(tidyverse)


library(dplyr)
Airbnb_Testing1$first_preds <- predict(Airbnb_all, newdata=Airbnb_Testing1)
no_na <- Airbnb_Testing1 %>% mutate(replace_mean_pred = ifelse(is.na(first_preds), 94.54, first_preds))
list(no_na$replace_mean_pred)
Predictions <- no_na[33]
install.packages("writexl")
library(writexl)
write.csv(Predictions, "C:\\Users\\haile\\OneDrive\\Documents\\Data analytics\\predictions.csv")

summary(Airbnb_all)

#backward model
Airbnb_back <- step(Airbnb_all, direction="backward", trace=0)
summary(Airbnb_back)
Airbnb_Testing1$back_preds <- predict(Airbnb_back, newdata=Airbnb_Testing1)
no_na <- Airbnb_Testing1 %>% mutate(replace_mean_back = ifelse(is.na(back_preds), 94.54, back_preds))
list(no_na$replace_mean_back)
Predictions <- no_na[34]
write.csv(Predictions, "C:\\Users\\haile\\OneDrive\\Documents\\Data analytics\\predictions.csv")

#forward model
Airbnb_forward <- step(Airbnb_null, scope = list(upper=Airbnb_all), direction = "forward", trace=0)
summary(Airbnb_forward)
Airbnb_Testing1$forward_preds <- predict(Airbnb_forward, newdata=Airbnb_Testing1)
no_na <- Airbnb_Testing1 %>% mutate(replace_mean_forward = ifelse(is.na(forward_preds), 94.54, forward_preds))
list(no_na$replace_mean_forward)
Predictions <- no_na[35]
write.csv(Predictions, "C:\\Users\\haile\\OneDrive\\Documents\\Data analytics\\predictions.csv")


#backward both
back_model_both <- step(Airbnb_all, scope=list(lower=Airbnb_null), direction="both", trace=0)
summary(back_model_both)
Airbnb_Testing1$both_preds <- predict(back_model_both, newdata=Airbnb_Testing1)
no_na <- Airbnb_Testing1 %>% mutate(replace_mean_both = ifelse(is.na(both_preds), 94.54, both_preds))
list(no_na$replace_mean_both)
Predictions <- no_na[35]
write.csv(Predictions, "C:\\Users\\haile\\OneDrive\\Documents\\Data analytics\\predictions.csv")

#forward both
forward_model_both <- step(Airbnb_null, scope= list(upper=Airbnb_all), direction="both", trace=0)
summary(forward_model_both)
Airbnb_Testing1$forboth_preds <- predict(forward_model_both, newdata=Airbnb_Testing1)
no_na <- Airbnb_Testing1 %>% mutate(replace_mean_forboth = ifelse(is.na(forboth_preds), 94.54, forboth_preds))
list(no_na$replace_mean_forboth)
Predictions <- no_na[36]
write.csv(Predictions, "C:\\Users\\haile\\OneDrive\\Documents\\Data analytics\\predictions.csv")
