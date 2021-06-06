# DESC. STATISTICS OF DEATH RATE #
# ------------------------------ #
# Function to clear console
cat("\014")
# Learn R version
getRversion()
# Clearing all plots
graphics.off()
# To empty an environment
rm(list = ls())
# To generate same output for every run (random number related)
set.seed(1234)

#setwd("Location Here")

library(tidyverse)
library("ggplot2")
library(gridExtra)
library("ggthemes")

#https://ourworldindata.org/smoking#the-global-distribution-of-smoking-deaths
# SKEWNESS - KURTOSIS
#Skewness: This tells us whether the distribution is symetric (at 0 or closer to 0), 
#Right-Skewed which means that most of the values are to the left (Skewness statistic becomes larger) 
#and Left skewed most observations are to the right (this means that the skewness statistic is in the negative zone.)

#Kurtosis: Is a statistical measure that helps us understand how skew is the distribution and the 
#peakedness of our data. In this case we have a kurtosis greater than zero which means that the peak is 
#wider with heavier tails, meaning there are some outliers in the observation.
skewness <-  function(x) {
  m3 <- mean((x-mean(x))^3)
  skew <- m3/(sd(x)^3)
  skew
}
kurtosis <- function(x) {  
  m4 <- mean((x-mean(x))^4) 
  kurt <- m4/(sd(x)^4)  
  kurt
}

data <- read.csv("Turkey_Smoking_Death_Rate_1990_2017.csv", header = FALSE) # Read The File into data
data.tibble <- as_tibble(data) # Transform into tibble for easier analysis
str(data.tibble) # Check the Structure
my_data <- data.tibble %>% rename (           # Renaming The Columns
  Country = V1,  
  CT = V2,
  Year = V3,
  Rate = V4
)
my_data


# Mean = sum all number / into number of variables
# Median = middle number of list 
#mean(my_data$Rate) # Mean of death in Turkey between 2000 - 2017
median(my_data$Rate) # Median 
quantile(my_data$Rate) # Quantile of data
IQR(my_data$Rate) # 75% - 25%

five_num_sum <-  round(fivenum(my_data$Rate), 2) # Min, 1stQuartile, Median, 3rdQuartile, Maximum
five_num_sum
#ggplot(my_data$Rate, my_data$Year)
# BOXPLOT Of Death  between 00 - 17 of five_sum# 
# Variance = average of squared deviations from the mean - SS -
# Sd = How Rates are spread out 
my_data.sd <-  sd(my_data$Rate)
my_data.m <- mean(my_data$Rate)
my_data.skew <- skewness(my_data$Rate)
my_data.kurtosis <- kurtosis(my_data$Rate)


five_num_sum <- as.data.frame(five_num_sum)

five_num_sum <- rbind(five_num_sum, my_data.m)
five_num_sum <- rbind(five_num_sum, my_data.sd)
five_num_sum <- rbind(five_num_sum, my_data.skew)
five_num_sum <- rbind(five_num_sum, my_data.kurtosis)


five_num_sum

value <- factor(c("Minimum", "1stQuartile", "Median", "3rd Quartile", "Maximum", "Mean", "SD", "Skewness", "Kurtosis"),
                levels = c("Minimum", "1stQuartile", "Median", "3rd Quartile", "Maximum", "Mean", "SD", "Skewness", "Kurtosis"))

plot.stat <- ggplot(five_num_sum,aes(x = value , y =  five_num_sum, fill = "blue"))+
  geom_bar(position = "dodge2", stat = "identity", color = "black", width = .5)+
  xlab("Values")+
  ylab("Statistical Parameters")+
  geom_text(aes(label = round(five_num_sum, digits = 2)),
            size = 3.1, fontface = "bold", vjust = -.4)+
  ggtitle("Deaths Per 100 - Stat Graph")


plot.stat + theme_classic() + scale_color_solarized('lightblue') + theme(legend.position = "none")


#Plot of Death Above
#GGplotBar - A visualization of Death Rates Per 100.000 on Ages
#        geom_segment(position = "dodge", stat="identity", color="black", width=.5, fill = colors)+

colors <- c("orchid4")
# Legend Will Be Attached
p.v1 <- ggplot(my_data, aes(y = `Rate`, x = `Year`))+
  geom_segment(aes(x = `Year`,   xend = `Year`, y  = 0, yend = `Rate`), color = "grey")+
  geom_point(size=4, color="lightblue")+
  scale_y_continuous(breaks = seq(0, 150, 20))+
  scale_x_continuous(breaks = seq(2000, 2017, 1))+
  theme(legend.title = element_text(size=18), legend.position = "top", 
        legend.text = element_text(size = 16)) +
  guides(fill=guide_legend(nrow = 1, byrow = TRUE))+
  geom_text(aes(label = round(`Rate`, digits = 2)), size = 3.1, fontface = "bold", vjust = -1.1)+
  ggtitle("Death Per 100.000")



p.v1 + theme_classic() + scale_color_solarized('blue')

# # -----------------------------------------------------------------------
# https://ourworldindata.org/smoking#the-global-distribution-of-smoking-deaths
# Death Numbers on Age

library(hrbrthemes)
library(ggplot2)
library(dplyr)

data.v2 <- read.csv("smoking-deaths-by-age-1990to2017.csv", header = TRUE) # data creation

data.v2.tibble <- as.tibble(data.v2) # tibble structure

data.v2.tibble
# Renaming columns
names(data.v2.tibble)[names(data.v2.tibble) == "X15.49_years"] <- "15-49 Years"
names(data.v2.tibble)[names(data.v2.tibble) == "X50.69_years"] <- "50-69 Years"
names(data.v2.tibble)[names(data.v2.tibble) == "X70._years"] <- "70+ Years"


data.smry <- summary(data.v2.tibble$`15-49 Years`)
colnames(data.smry)
data.smry <- gather(data.smry, Stats, )                  


data.v2.tibble <-  select(data.v2.tibble, -Country) # We know its Turkey

data.v2.gt <- gather(data.v2.tibble, Ages, Deaths, `15-49 Years`:`70+ Years`)  
# GGplot of death dist. according to the ages
ggplot(data = data.v2.gt, mapping = aes(x = `Year`, y = `Deaths`, color = `Ages`, shape = `Ages`))+
  geom_line()+
  geom_point()+
  scale_y_continuous(breaks = seq(0, 40000, 5000))+
  scale_x_continuous(breaks = seq(2000, 2017, 1))+
  ggtitle("Deaths From Smoking By Age, Turkey")+
  theme(legend.title = element_text(size=20), legend.position = "bottom", 
        legend.text = element_text(size = 14))



##-------------------------------------------------------------------------
#Smoking Prevelance
# 2000, 2005, 2010, 2011, 2012, 2013, 2014, 2015, 2016

data.v3 <- read.csv("share-of-adults-who-smoke.csv", header = TRUE)

data.v3.tibble <- as.tibble(data.v3)

colnames(data.v3.tibble)

data.v3.tibble

data.v3.plot <- ggplot(data.v3.tibble, aes(x = `Year`, y = `SmokingPrevelence`))+
  geom_line(size = .2)+
  geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
  ggtitle("Smoking Prevelance in Turkey Between 2000 - 2016")+
  scale_x_continuous(breaks = seq(2000, 2017, 1))+
  scale_y_continuous(breaks = seq(25,  40, 2))
#                stat_smooth()

data.v3.plot + theme_classic() + scale_color_solarized('blue')




# Stat Calculation
five_num_sum.v2 <-  round(fivenum(data.v3.tibble$SmokingPrevelence), 2)
five_num_sum.v2


data.v3.tibble.sd <-  round(sd(data.v3.tibble$SmokingPrevelence), 2)
data.v3.tibble.m <- mean(data.v3.tibble$SmokingPrevelence)
data.v3.tibble.skew <- skewness(data.v3.tibble$SmokingPrevelence)
data.v3.tibble.kurtosis <- kurtosis(data.v3.tibble$SmokingPrevelence)

five_num_sum.v2 <- as.data.frame(five_num_sum.v2)

five_num_sum.v2 <- rbind(five_num_sum.v2, data.v3.tibble.m)
five_num_sum.v2 <- rbind(five_num_sum.v2, data.v3.tibble.sd)
five_num_sum.v2 <- rbind(five_num_sum.v2, data.v3.tibble.skew)
five_num_sum.v2 <- rbind(five_num_sum.v2, data.v3.tibble.kurtosis)

five_num_sum.v2


value.v3 <- factor(c("Minimum", "1stQuartile", "Median", "3rd Quartile", "Maximum", "Mean", "SD", "Skewness", "Kurtosis"),
                   levels = c("Minimum", "1stQuartile", "Median", "3rd Quartile", "Maximum", "Mean", "SD", "Skewness", "Kurtosis"))

#Plot Of Stat - Smoking Prevelance
plot.stat.v2 <- five_num_sum.v2 %>%
  ggplot(aes(x = value.v3 , y =  five_num_sum.v2, fill = "blue"))+
  geom_bar(position = "dodge", stat = "identity", color = "black", width = .5)+
  xlab("Values")+
  ylab("Statistical Parameters")+
  geom_text(aes(label = round(five_num_sum.v2, digits = 2)),
            size = 3.1, fontface = "bold", vjust = -.4)+
  ggtitle("Smoking Prevelance - Stat Graph")


plot.stat.v2 + theme_classic() + scale_color_solarized('lightblue') + theme(legend.position = "none")








#ReasonsToStartSmoking
#
library("readxl")

data.v4 <- read_xls("ReasonsToStartSmoking.xls")

str(data.v4)

data.v4


data.v4.con <- mutate_all(data.v4[-1, -1], .funs = as.numeric)

str(data.v4.con)

data.v4.con

rowMeans(data.v4.con[1, ])

group <- c(rep(c("Interest", "Desire", "Family Problems", "Personal Problems",
                 "Impact Of Friend", "For Fun", "No Special Reason"), 4))


Year  <- c(rep("2010", 7), rep("2012", 7), rep("2014", 7), rep("2016", 7))

valueS <- as.vector(c(data.v4.con[, 1], data.v4.con[, 4], data.v4.con[, 7], data.v4.con[, 10]))


comb.data <- data.frame(Year, group, valueS)


valueS <- unlist(valueS)

valueS

comb.data

plot.bar.v1 <- ggplot(comb.data, aes(y = valueS, x = Year, fill = group))+
  geom_bar(position = "stack", stat = 'identity')+
  ggtitle("Reasons Of Starting Smoking")+
  labs(fill = "Group Parameters")

plot.bar.v1 + theme_classic() + scale_color_solarized('lightblue') + theme(legend.title = element_text(size=15, family = 'bold'), legend.position = "bottom", 
                                                                           legend.text = element_text(size = 11))

#-------------------------------------------------------------------------------
#Smoking Distribution on gender in Turkey

library(xlsReadWrite)

data.v5 <- read_xls("TobaccoOnDistGender.xls")

data.v5 %>% group_by(Year) %>% mutate(highestY = max(Male, Female))

data.v5 <- data.v5[-5, ]
#data.v5.df <- data.frame(value <- c(data.v5$Male, data.v5$Female),
#                             type <- c(data.v5$Year))

year <- c(rep("2010" , 2) , rep("2012" , 2), rep("2014" , 2), rep("2016", 2))
group <- c(rep(c( "M", "F"), 4))
group
data.v5.s.rate <-  data.v5 %>% gather("GenderType", "SmokingR", 3:4)

data.v5
smoking.rate <- c(data.v5[1, 3], data.v5[1, 4], data.v5[2, 3], data.v5[2, 4],
                  data.v5[3, 3], data.v5[3, 4], data.v5[4, 3], data.v5[4, 4])

smoking.rate <- as.numeric(smoking.rate)

cdata <- data.frame(year, group, smoking.rate)

cdata$group <- factor(cdata$group , levels = c('F', 'M'))


colors <- c("deeppink2", "steelblue2")


plot.bar <- ggplot(cdata, aes(y= smoking.rate, x = year, fill = group))+
  geom_bar(position = "stack", stat = 'identity')+
  geom_text(aes(label = paste(round(smoking.rate, digits = 2),"%", sep = " ")), 
            size = 5, hjust = 0.5, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = colors)+
  ggtitle("Smoking Rate Distribution on Genders")+
  xlab("Years")+
  ylab("Population Rates")+
  labs(fill = "Gender")



plot.bar + theme_classic() + scale_color_solarized('lightblue') + theme(legend.title = element_text(size=20), legend.position = "bottom", 
                                                                        legend.text = element_text(size = 14))































