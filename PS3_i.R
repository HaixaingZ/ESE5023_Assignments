library(tidyr)
library(dplyr)
library(ggplot2)

#1
#1.1
#set the array
Unseeded_Day <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
Seeded_Day <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
Rainfall_day <- cbind(Unseeded_Day,Seeded_Day)
boxplot(Rainfall_day)
#1.2
Rainfall_day_tbl <- as_tibble(Rainfall_day)
Rainfall_day_tbl %>%
  summarise(
    mean_Seeded = mean(Rainfall_day_tbl$Seeded_Day),
    mean_Unseeded = mean(Rainfall_day_tbl$Unseeded_Day),
    increse_rate = mean(((Rainfall_day_tbl$Seeded_Day)-mean(Rainfall_day_tbl$Unseeded_Day))/mean(Rainfall_day_tbl$Unseeded_Day))
  )
t.test(Unseeded_Day, Seeded_Day)
# MingYANG noticed: 
# you don`t need to obtain the average value of rainfall
# do "t.test(Unseeded_Day,Seeded_Day)" directly after plot
# the end

#2
TR_bone <- read.csv("PS3_data.csv",header = T)
TR_bone_tbl <- as_tibble(TR_bone)
TR_bone_tbl %>%
  group_by(Bone_type) %>%
  summarise(
    count = n(),
    mean_OIC = mean(OIC),
    sd_OIC = sd(OIC)
  )
#use boxplot to check if there is a different between the distribution
ggplot(TR_bone_tbl, aes(x=Bone_type, y=OIC, fill=Bone_type))+
  geom_boxplot()+
  theme_classic()
#anova one way test
AOW_TR <- aov(OIC~Bone_type, data = TR_bone_tbl)
summary(AOW_TR)
# MingYANG noticed：
# right analysis wrong conclusion
# statistically the temperature are different, but they are in totally Within 4 degrees centigrade 
# so this Tyrannosaurus  should be warm-blooded
# the end

#3
V_Z <- read.csv("PS3_data.csv",header = T)
V_Z_tbl <- as_tibble(V_Z)
names(V_Z_tbl)
V_Z_tbl %>%
  filter(people_type != "") %>%
  select(people_type,zine_level) %>%
  group_by(people_type) %>%
  summarise(
    count = n(),
    mean_Zine = mean(zine_level),
    sd_Zine = sd(zine_level)
  )
AOW_Zine <- aov(zine_level~people_type, data = V_Z_tbl)
summary(AOW_Zine)

#4
Elevation <- c(180,305,381,488,549,640,762,883)
Temperature <- c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)
matrix1 <- data.frame(Elevation,Temperature)
plot(Temperature~Elevation, data = matrix1)
regression_line <- lm(Temperature~Elevation, data = matrix1)
abline(regression_line, col='red')
summary(regression_line)$coefficients[2,1]
#summary(regression_line)$coefficients[,1] or coef(regression_line) 第一个是截距，第二个是斜率
#learn from https://blog.csdn.net/dingchenxixi/article/details/50543822
lapse_rate <- summary(regression_line)$coefficients[2,1]*1000
lapse_rate

#5
#5.1 
install.packages("ggpmisc")
library(ggpmisc)
BBT <- read.csv("PS3_data.csv",header = T)
BBT_tbl <- as_tibble(BBT)
names(BBT_tbl)
BBT_tbl %>%
  filter(Nebula != "") %>%
  select(Nebula,Velocity,Distance) %>%
  ggplot(aes(y = Distance,x = Velocity))+
  geom_point()+
  #5.2
  geom_smooth(method="lm", formula = y ~ x)+ # learn from https://blog.csdn.net/weixin_42933967/article/details/96200165 下一行也是
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~~')), formula = y ~ x, parse = T) +
  theme_classic()
#5.3
#the first assumption sounds reasonable because the universe
#was come from a single point( big bang theory)
#5.4

#6
install.packages('leaps')
library(tidyr)
library(dplyr)
library(ggplot2)
library(leaps)
library(MASS)
data(cpus)
head(cpus)
nrow(cpus)
#make the order random
random_selcet <- runif(209,0,100)
cpu1 <- cbind(cpus,random_selcet)
cpu_random <- cpu1 %>%
  arrange(desc(random_selcet))
#seperate it into two group
cpu_train_set <- cpu_random[1:167,]
cpu_test_set <- cpu_random[168:209,]
#6.1
train_subset_result <- regsubsets(perf ~ syct+mmin+mmax+cach+chmin+chmax, data=cpu_train_set, nbest=2, nvmax = 6)
plot(train_subset_result)

#6.2
test_subset_result <- regsubsets(perf ~ syct+mmin+mmax+cach+chmin+chmax, data=cpu_test_set, nbest=2, nvmax = 6)
plot(test_subset_result)
# 6.2
model_1 <- lm(perf~syct+mmin+mmax+cach+chmin+chmax, data = train)
summary(model_1)

# MingYANG noticed：
# try using the following code:
# real values
plot(test$perf,type = "o",col = 'red')
# predicted values
lines(predict(model_1,test),type = "o", col = 'blue')
# quantify the mean bias between actual pref values and predicted pref values
mean(test$perf - predict(model_1,test))
# the end

#7
data_Lab <- read.csv("CMMNDHgfinal.csv",header = T)
data_tbl <- as_tibble(data_Lab)
names(data_tbl)
#which kind of bird have the higest Hg concentration
data_tbl %>%
  filter(Species != "") %>%
  group_by(Species) %>%
  #summarise(Hg_mean = mean(Hgppm)) %>%
  #arrange(desc(Hg_mean)) %>%
  ggplot(aes(x=Species,y=Hgppm),head = T, fill=Species)+
  theme_classic()+
  geom_boxplot()

#t test
MYWA <- data_tbl %>%
  filter(Species == "MYWA")%>%
  select(Hgppm)
NOWA <- data_tbl %>%
  filter(Species == "NOWA")%>%
  select(Hgppm)
t.test(MYWA,NOWA)

#ANOVA test
anova_bird <- aov(Hgppm~Species,data=data_tbl)
summary(anova_bird)

#linear regression model
NOWA_LG <- data_tbl %>%
  filter(Species == "NOWA")
plot(Deterium~Hgppm, data = NOWA_LG)
regression_line <- lm(Deterium~Hgppm, data = NOWA_LG)
abline(regression_line, col='red')
