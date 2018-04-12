library(ggplot2)
library(tidyr)
library(dplyr)

#Cleaning up data
#load("hedonic.Rda")
#hedonic_temp <- hedonic %>% gather(Area, "num", 42:50) 
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(bed_type, "num", 19:23) 
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(host_has_profile_pic, "num", 19:21)
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(host_is_superhost, "num", 19:21)
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(room_type, "num", 19:21)
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(instant_bookable, "num", 19:20)
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#hedonic_temp <- hedonic_temp %>% gather(cancellation_policy, "num", 19:21)
#hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)
#hedonic_temp$num <- NULL
#save(hedonic_temp, file ="hedonic_fixed.Rda")
load("hedonic_fixed.Rda")

#Quick summary of Sq Ft
summary(hedonic_temp$square_feet)

#Price to Sq Feet Plot with regression line
filtered <- hedonic_temp %>% filter(hedonic_temp$square_feet >= 70)
model <- lm(lm(filtered$price_total~filtered$square_feet))
ggplot(filtered, aes(filtered$square_feet, filtered$price_usd)) + geom_point() + ggtitle("Price to Square feet") + xlab("Square Feet") + ylab("Price (USD)") + xlim(c(0,2000)) + geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red")
rm(filtered)
rm(model)

#Table for Mean/Median per Area
hedonic_temp %>% group_by(Area) %>% filter(!is.na(price_total)) %>% summarise(AverageCost = mean(price_total), MedianCost = median(price_total)) %>% arrange(desc(AverageCost, MedianCost))


#Violin Plots
spliter <- c("Central Copenhagen", "East Copenhagen", "North Copenhagen", "South Copenhagen", "West Copenhagen", "Frederiksberg", "Greater Copenhagen", "NA", "Valby")
ggplot(hedonic_temp, aes(factor(Area), hedonic_temp$price_total)) + geom_violin(scale = "area") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Total Price (USD)") + scale_x_discrete(labels = spliter) + xlab("Area") + ggtitle("Violin Plots of the Total Price to Area")
rm(spliter)

#Listings Per Area
table(hedonic_temp$Area)


#Review Scores to price plot
ggplot(hedonic_temp, aes(review_scores_rating, price_total)) + geom_smooth() + geom_point() + ggtitle("Review scores to Price") + xlab("Review Scores") + ylab("Total Price(USD)")

#Density Plot
ggplot(hedonic_temp, aes(review_scores_rating)) + geom_density() + ggtitle("Density of Review Score") + xlab("Review Scores") + ylab("Density")

#Boxplot of Superhost
filtered <- hedonic_temp %>% filter(hedonic_temp$host_is_superhost != "host_is_superhost_NA")
SuperHostStatus <- c("Not a Superhost", "Is a SuperHost")
ggplot(filtered, aes(filtered$host_is_superhost, price_total)) + geom_boxplot() + ggtitle("Boxplots of Price to SuperHost Status") +xlab("Superhost Status") + ylab("Total Price (USD)") + scale_x_discrete(labels = SuperHostStatus)
rm(SuperHostStatus)
rm(filtered)


