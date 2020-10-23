#1
#1.1 
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)
signif <- read.csv("signif.txt", head = TRUE, sep = '\t')
# sign1 <- read.table("signif.txt",header = T, fill = T)  this can also read file but data structure is strange
# fill = T ?
Sig_Eqs <- as_tibble(signif)
#1.2
top_ten_death <- Sig_Eqs %>%
  select(COUNTRY, YEAR, DEATHS) %>%
    group_by(COUNTRY) %>%
     summarise( death_number = sum(DEATHS)) %>%
      arrange(desc(death_number))
top_ten_death[1:10,]
#1.3
Sig_Eqs %>%
  select(YEAR, EQ_PRIMARY) %>%
  group_by(YEAR) %>%
  filter(EQ_PRIMARY > 6.0) %>%
  summarise(large_EQ_number = n()) %>%
  ggplot(aes(x=YEAR, y=large_EQ_number))+
  geom_line()+
  xlim(-2150,2150)
#1.4
Sig_Eqs_NA <- Sig_Eqs %>%
  filter(EQ_PRIMARY != 'NA')
countEq_LargestEq <- function(country1){
  Sig_Eqs_NA %>%
    filter(COUNTRY == country1) %>%
    mutate(dateEQ = paste(YEAR,MONTH,DAY,sep = '-'))%>%
    select(dateEQ,EQ_PRIMARY) %>%
    summarise(EQ_Num = n(), EQ_Max = dateEQ[which(EQ_PRIMARY == max(EQ_PRIMARY))])
}
i<-1
EQlist<-matrix(ncol = 3,nrow = length(unique(Sig_Eqs_NA$COUNTRY)))
for(CountryName in unique(Sig_Eqs_NA$COUNTRY)){
  EQlist[i,]<-c(as.character(CountryName),
                as.numeric(countEq_LargestEq(CountryName)[1,1]),
                as.character(countEq_LargestEq(CountryName)[1,2]))
  i=i+1
}
#Sort in descending order by earthquake numbers.
EQlist_Order<-EQlist[order(as.numeric(EQlist[,2]),decreasing=T),]
EQlist_Order

#2
library(tidyr)
library(dplyr)
library(ggplot2)
Shenzhen_data <- read.csv("2281305.csv", head = TRUE)
Shenzhen_data %>%
  select(DATE,WND) %>%
  mutate(YM = substr(DATE,1,7), WD = as.numeric(substr(WND,9,12))) %>%
  filter(WD != 9999) %>%
  group_by(YM) %>%
  summarise(date = as.Date(DATE), mon_ave_wd = sum(WD)/n()) %>%
  ggplot(aes(x=date, y=mon_ave_wd))+
  geom_line()

#3
library(tidyr)
library(dplyr)
library(ggplot2)
CO2_data <- read.csv("co2_mm_mlo.csv",head = T)
CO2_tib <- as_tibble(CO2_data)
CO2_tib %>%
  filter(quality != 0) %>%
  ggplot(aes(x=decimal_date,y=co2))+
  geom_line()
CO2_tib %>%
  select(month, co2, quality) %>%
  filter(quality != 0) %>%
  group_by(month) %>%
  summarise(maxCO2 = max(co2), minCO2 = min(co2) ) %>%
  arrange(desc(maxCO2))
CO2_tib %>%
  filter(quality != 0) %>%
  group_by(year) %>%
  summarise(maxCO2 = max(co2), minCO2 = min(co2)) %>%
  arrange(desc(maxCO2))
  
  