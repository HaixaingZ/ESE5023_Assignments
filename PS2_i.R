#1
#1.1 
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)
signif <- read.csv("signif.txt", head = TRUE, sep = '\t')
# @MingYANG recommended：
# "read.delim" is recommended here, but read.table/read.csv/read.delim don`t have significant differences
# they are different in the default format and the speed of reading
# see also:  https://blog.csdn.net/wmm131333/article/details/100691190?utm_medium=distribute.pc_relevant.none-task-blog-title-2&spm=1001.2101.3001.4242
# the end

# sign1 <- read.table("signif.txt",header = T, fill = T)  this can also read file but data structure is strange
# fill = T ?
Sig_Eqs <- as_tibble(signif)
#1.2
top_ten_death <- Sig_Eqs %>%
  select(COUNTRY, YEAR, DEATHS) %>%
#@MingYANG recommended:
# the colum of "YEAR" was not used
# the end 
    group_by(COUNTRY) %>%
     summarise( death_number = sum(DEATHS)) %>%
#@MingYANG noticed:
# the data with the value of "NA" should be noticed and removed
# replacing line 20 with "summarise( death_number = sum(DEATHS,na.rm=T)) %>%" so as to avoid "NA" value
# the end
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
# @MingYANG recommended:
# R save data as "data.frame" format for reading data from csv file directly
# using as_tible to convert the data format into "tbl_df"
# "tbl_df" is based on "data.frame" so it has downward compatibility
# but "tbl_df" is much quick than "data.frame"
# see also: http://blog.fens.me/r-tibble/
# the end
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
  
  
