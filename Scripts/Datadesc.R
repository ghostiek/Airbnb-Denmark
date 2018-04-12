library(ggplot2)
library(tidyr)
library(dplyr)
summary(hedonic)


hist(hedonic$accommodates)

hist(hedonic$beds)

hedonic_temp <- hedonic %>% gather(Area, "num", 42:50)

hedonic_temp <- hedonic_temp %>% filter(hedonic_temp$num==1)


View(hedonic_temp)

hedonic <- hedonic %>% mutate(Area = hedonic$Area_CopenhagenC+hedonic$Area_CopenhagenE+hedonic$Area_CopenhagenW+
                     hedonic$Area_CopenhagenN+hedonic$Area_CopenhagenS+hedonic$Area_Frederiksberg+
                     hedonic$Area_GreaterCopenhagen+hedonic$Area_Valby)


qplot(hedonic, aes(hedonic$accommodates, hedonic$Area_CopenhagenC))

ggplot(hedonic_temp, aes(x=hedonic_temp$Area, y=hedonic_temp$accommodates)) + geom_boxplot()

table(hedonic_temp$accommodates, hedonic_temp$Area)


ggplot(hedonic_temp, aes(x=hedonic_temp$Area, y=hedonic_temp$accommodates)) + geom_boxplot()

nrow(hedonic)
nrow(hedonic_temp)
