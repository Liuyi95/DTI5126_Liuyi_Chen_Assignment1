install.packages("tidyverse")
install.packages("lubridate")
install.packages("readr")
library(readr)
library(tidyverse)
library(lubridate)
###delay analysis and figures
getwd()

case_data <- read_csv("/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/case_data.csv")
transmission_pairs <- read_csv(file = "/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv")

case_data <- case_data %>%
  mutate(onset.date = dmy(onset.date),
         confirm.date = dmy(confirm.date),
         epi.date = dmy(epi.date))
#(1)Pick three columns of the dataframe. Use the function to get the following summaries of these columns
#1. The dataframe is onset.date column
data_confirm<-read.csv("/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/case_data.csv",
                      row.names = 1)
#put the object into a class 
class(data_confirm)
dmy(data_confirm$confirm.date)
#clean the data with NA 
na.omit(data_confirm$confirm.date)
sum(is.na(data_confirm$confirm.date)) 
#because the formation of the confirm data is %d%m%Y, Order the dates from first to last 
arrange=data_confirm[order(as.Date(data_confirm$confirm.date, format="%d/%m/%Y")),]

#The first day and last day
First_Day=sapply(arrange, function(arrange) arrange[1])
Last_Day=data_confirm$confirm.date[last(arrange)]
#1.2.age
age_group<- read.csv(file = "/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv")
age=age_group[,c("agegroup.infectee")]
Norm_age<-as.vector(scale(age)) 

max(age)
min(age)
mean(age)
na.omit(age)
sum(is.na(age))
#1.3 the delay days
#confirm.infectee-infector.onset
delay_infector<- read.csv(file = "/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv")
delay=delay_infector[,c("delay.infector")]
new_delay=na.omit(delay)
sum(is.na(delay))
max(new_delay)
min(new_delay)
mean(new_delay)

#(6),(7) Show a count of all missing values in each column
df<-read.csv(file = "/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv")
df1=delay_infector[,c("infector.case")] 
df2=delay_infector[,c("infectee.case")]
df3=delay_infector[,c("cluster.risk")]

df4=delay_infector[,c("cluster.generation")]
df5=delay_infector[,c("pair.type")]
df6=delay_infector[,c("infector.quarantine")]
df7=delay_infector[,c("infectee.quarantine")]
df8=delay_infector[,c("cluster.id")]
df9=delay_infector[,c("infector.onset")]
df10=delay_infector[,c("infectee.onset")]
df11=delay_infector[,c("onset.diff")]
df12=delay_infector[,c("confirm.infector")]
df13=delay_infector[,c("confirm.infectee")]
df14=delay_infector[,c("delay.infector")]
df15=delay_infector[,c("agegroup.infector")]
df16=delay_infector[,c("agegroup.infectee")]
df17=delay_infector[,c("infector.epi.date")]
df18=delay_infector[,c("infectee.epi.date")]

sum(is.na(df1)) 
sum(is.na(df2))
sum(is.na(df3))
sum(is.na(df4))
sum(is.na(df5))

sum(is.na(df6))
df6[is.na(df6)] <-"N"

sum(is.na(df7))
df7[is.na(df7)] <-"Y"


sum(is.na(df8))
sum(is.na(df9))
sum(is.na(df10))

df11_new=na.omit(df11)
mean(df11_new)
df11[is.na(df11)] <-5.8
df11

df14_new=na.omit(df14)
mean(df14_new)
df14[is.na(df14)] <-7.07
df14

df15_new=na.omit(df15)
mean(df15_new)
df15[is.na(df15)] <-9
df15


sum(is.na(df15))
sum(is.na(df16))
sum(is.na(df17))
sum(is.na(df18))
#(8)Z-score normalization (μ: mean, σ: standard deviation)
age_group<- read.csv(file = "/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv") 
age=age_group[,c("agegroup.infectee")] 
Norm_age<-as.vector(scale(age)) 
#(9)Pick one categorial column and convert it into several dummy columns. 
# Install fastDummies:
install.packages('fastDummies')
library('fastDummies')
# Create dummy variables:
df<-read.csv(file ="/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/transmission_pairs.csv")
dataf <- df(df, select_columns = 'cluster.risk')
# Make dummy variables of two columns:
dataf <- dummy_cols(dataf, select_columns = c('cluster.risk', 'discipline'))
#(10) Perform three different data preparation operations from those discussed in class	 
#1. clean data
case_data <- read.csv("/Users/liuyichen/Documents/R/Liuyi_Chen_Assignment1/Data_file_part2/case_data.csv")
drops <- case_data[,c("cluster.setting.1")]
drops <- case_data[,c("cluster.setting.2")]
drops <- case_data[,c("cluster.setting.3")]
drops <- case_data[,c("secondary.generation.1")]
drops <- case_data[,c("secondary.generation.2")]
#(11)Discretize another numeric column using binning. 	
library(dplyr)

#perform binning with specific number of bins
delay_infector %>% mutate(delay_phases = ntile(df14, n=3))
# 
#(12) Produce a matrix of scatter plots ( use pair function). 
data <- data.frame(df14, df15, df16) # Combine all variables to data.frame
pairs(data) 
# Apply pairs function
#(5) Figure 
case_data %>%
  filter(cluster.id != 0,
         cluster.category != "Cluster of imported cases") %>%
  mutate(delay = as.numeric(confirm.date - onset.date, na.rm = T)) %>%
  filter(!is.na(delay)) %>%
  ggplot() +
  geom_histogram(aes(x = delay, y = ..density..),  fill = '#dedede', colour = "black", binwidth = 1) +
  scale_y_continuous("Frequency", expand = c(0,0), limits = c(0,0.20)) + 
  scale_x_continuous("Delay from onset-to-confirmation (days)", expand = c(0,0), breaks = seq(0,27, by = 3)) +
  theme_classic() +
  theme(aspect.ratio = 0.3, legend.position = 'none')

case_data %>%
  filter(cluster.id != 0, 
         cluster.category != "Cluster of imported cases") %>%
  mutate(delay = as.numeric(confirm.date - onset.date, na.rm = T)) %>%
  group_by(cluster.id) %>%
  mutate(n = n()) %>% 
  dplyr::select(n, delay) %>%
  ungroup() %>%
  mutate(n = as_factor(n)) %>%
  group_by(n) %>%
  filter(!is.na(delay)) %>%
  summarise(ymin = min(delay),
            ymax = max(delay),
            middle = median(delay),
            lower = quantile(delay,0.25),
            upper = quantile(delay,0.75)) %>%
  ungroup() %>%
  ggplot() +
  geom_boxplot(aes(x = n, ymin = ymin,ymax = ymax,middle = middle,upper = upper,lower= lower),
               stat = 'identity', 
               fill = "#dedede",
               width = 0.8) +
  theme_classic() +
  theme(aspect.ratio=1) +
  scale_y_continuous("Delay from onset-to-confirmation (days)", limits = c(0, 27), expand = c(0,0), breaks = seq(0,27, by = 3)) +
  xlab("Cluster size (n)") +
  coord_flip()

#Figure 4B
transmission_pairs %>%
  group_by(infector.case, cluster.risk) %>%
  summarise(n = n(), delay = mean(delay.infector)) %>%
  filter(!is.na(delay)) %>%
  ggplot() +
  geom_histogram(aes(x = delay, y = ..density..),  fill = '#dedede', colour = "black", binwidth = 1) +
  scale_y_continuous("Frequency", expand = c(0,0), limits = c(0,0.20)) + 
  scale_x_continuous("Delay from onset-to-isolation of infector (days)", 
                     expand = c(0,0), limits = c(0,27), breaks = seq(0,27, by = 3)) +
  theme_classic() +
  theme(aspect.ratio = 0.3, legend.position = 'none')

transmission_pairs %>%  
  group_by(infector.case, cluster.risk) %>%
  summarise(n = n(), delay = mean(delay.infector)) %>%
  filter(!is.na(delay)) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_smooth(method = lm, aes(x=delay, y = n), color = "black", alpha = 0.1, size = 0.7) +
  geom_jitter(aes(x = delay, y = n, colour = cluster.risk), height = 0.3, width = 0.3) +
  scale_y_continuous("Secondary Cases / Infector", breaks = 1:11) +
  scale_x_continuous("Delay from onset-to-confirmation of infector (days)", 
                     expand = c(0,0),                     
                     limits = c(0,27), breaks = seq(0,27, by = 3)) +
  theme_classic() +
  theme(aspect.ratio = 1, legend.position = c(0.85, 0.85), legend.title = element_blank())  #colours are modified custom in post 

#####Calculate delay stats
##median delay across all local clusters
case_data %>%
  filter(cluster.id != 0,
         cluster.category != "Cluster of imported cases") %>%
  mutate(delay = as.numeric(confirm.date - onset.date, na.rm = T)) %>%
  filter(!is.na(delay)) %>%
  pull(delay) %>%
  median()

#Relationship between median delay within clusters and cluster size (excluding two largest)
case_data %>%
  filter(cluster.id != 0, 
         cluster.category != "Cluster of imported cases") %>%
  mutate(delay = as.numeric(confirm.date - onset.date, na.rm = T)) %>%
  group_by(cluster.id) %>%
  mutate(n = n()) %>% 
  dplyr::select(n, delay) %>%
  ungroup() %>%
  filter(!is.na(delay)) %>%
  group_by(n) %>%
  summarise(m.delay = median(delay)) %>%
  filter(n < 22) %>%
  lm(n ~ m.delay, data = .) %>%
  summary()

#median delay among infectors
transmission_pairs %>%
  group_by(infector.case, cluster.risk) %>%
  summarise(n = n(), delay = mean(delay.infector)) %>%
  filter(!is.na(delay)) %>%
  pull(delay) %>%
  median()

#relationship between delay in confirmation of infectors and number of secondary cases
transmission_pairs %>%
  group_by(infector.case, cluster.risk) %>%
  summarise(n = n(), delay = median(delay.infector)) %>%
  filter(delay != "NA") %>% 
  arrange(desc(n)) %>%
  lm(n ~ delay, data = .) %>%
  summary()






