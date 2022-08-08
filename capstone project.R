library(stringr)
library(tidyr)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(viridis)
library(mapview)
library(corrplot)
library(readxl)  
library(writexl)
options(tigris_use_cache = TRUE)
setwd("C:\\Users\\karl\\Documents\\Capstone Project")

acsdata = get_acs(geography = "county",
                   variables = c(sparent_m = "DP02_0007PE", sparent_f = "DP02_0011PE", hhage65a = "DP02_0013PE", hhsize = "DP02_0016E", 
                                 college_higher = "DP02_0068PE", ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE",
                                 disable = "DP02_0072PE", disable65a = "DP02_0078PE", 
                                 fborn = "DP02_0093PE",fb_nonc = "DP02_0096PE", computer = "DP02_0152PE", broadband = "DP02_0153PE", 
                                 lcp = "DP03_0003PE", ur = "DP03_0009PE",
                                 commute_p = "DP03_0021PE", wfh = "DP03_0024PE", commute_ms = "DP03_0025E",
                                 eea_hhs = "DP03_0042PE", eea_lh = "DP03_0043PE", mincome = "DP03_0062E", 
                                 fstamp = "DP03_0074PE", 
                                 hi_pub = "DP03_0098PE", hi_no = "DP03_0099PE", hi_no_neea ="DP03_0113PE", hi_no_nlc ="DP03_0118PE",
                                 poverty ="DP03_0119PE",
                                 rentalu = "DP04_0047PE", rental_size = "DP04_0049E", mrent = "DP04_0134E", mhomeprice = "DP04_0089E", 
                                 pop = "DP05_0001E", mage ="DP05_0018E", a85a ="DP05_0017PE", a7584="DP05_0016PE",
                                 a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE",a4554 ="DP05_0012PE",
                                 a3544 ="DP05_0011PE",a2534 ="DP05_0010PE",a2024 ="DP05_0009PE",a1519 ="DP05_0008PE",
                                 black ="DP05_0038PE", asian = "DP05_0044PE", aindian = "DP05_0039PE", latino = "DP05_0071PE"
                   ),                                        
                   survey = "acs5",
                   output = "wide",
                   year = 2019)  

acsdata1 = acsdata %>% select(GEOID, NAME, sparent_m, sparent_f, hhage65a, hhsize, college_higher, disable, disable65a, 
                            fborn,fb_nonc, computer, broadband, lcp, ur,commute_p, commute_ms, wfh, eea_hhs, eea_lh, mincome, 
                            fstamp, hi_pub, hi_no, hi_no_neea, hi_no_nlc, poverty, rentalu, rental_size, mrent, mhomeprice, 
                            pop, mage, a85a, a7584, a6574, a6064, a5559, a4554,a3544,a2534, a2024,a1519,black, asian, aindian, latino) 

chci = acsdata %>% select(GEOID, NAME, ed_below9, ed_g912,ed_hs, ed_scollege ,ed_associate,ed_bachelor,ed_higher)
chci = chci %>% mutate(chci=(1/100)*(50*ed_below9+100*ed_g912+120*ed_hs+130*ed_scollege+140*ed_associate+190*ed_bachelor+230*ed_higher))
acs19 = left_join(acsdata1, chci[,c("GEOID","chci")], by="GEOID")

acs19 = acs19 %>% mutate(sparent = sparent_m + sparent_f, a75a=a7584+a85a, a65a=a6574+a75a, a60a=a65a+a6064, a5564=a5559+a6064,
                         a3554=a3544+a4554, a2034=a2024+a2534, afford = mincome/mhomeprice) %>% rename(county=GEOID)

diabetesdata = read_excel("DiabetesAtlasData.XLSX") 
diabetesdata = subset(diabetesdata, select=c(4,5,7))
diabetesdata = subset(diabetesdata, State != "District of Columbia")
names(diabetesdata)[names(diabetesdata) == "Diagnosed Diabetes Percentage"] = "Diabetes_Incidence"

diabetesmortality = read_excel("diabetesmortality.XLSX")
diabetesmortality = diabetesmortality %>% mutate(mortalityrate = (deaths/population)*100)
diabetesmortality = subset(diabetesmortality, select=c(6,7))
#got diabetes diagnosed % and mortality % by county 
new = left_join(acs19, diabetesdata, by="county") 
new2 = left_join(new, diabetesmortality, by="county") 

health = read_excel("healthrankings.XLSX") #from county health rankings

new3 = left_join(new2, health, by="county") 

new4 = subset(new3, select=c(1,8,12,27,34:36,44:71))
new4 = subset(new4, select=-c(1,14:16,18,20,23,24))#for diabetes diagnosed rate as independent variable

new5 = subset(new3, select=c(1,8,12,27,34:36,44:71))
new5 = subset(new5, select=-c(1,14:16,18,20,22,24))#for diabetes mortality rate as independent variable
(colMeans(is.na(new4)))*100
is.na(new4)
new4[new4==""] = NA
new4[new4=="0"] = NA
new4 = na.omit(new4)
is.na(new4)#all missing values removed still have 2410 obs


is.na(new5)
new5[new5==""] = NA
new5[new5=="0"] = NA
new5 = na.omit(new5)
is.na(new5)#all missing values removed still have 1433 obs, mortality rate had way more NA'S than incidence rate did so there are a lot less obs
new4= new4[,c(1:14,16:27,15)]
new5= new5[,c(1:14,16:27,15)]

new4minusstate = subset(new4, select= -c(27))
new5minuesstate = subset(new5, select= -c(27))
cor(new4minusstate)
corrplot(cor(new4minusstate))#no variable correlation of .95 or higher

cor(new5minuesstate)
corrplot(cor(new5minuesstate))#no variable correlation of .95 or higher


write.csv(new4,"new4.csv")
write.csv(new5,"new5.csv")


set.seed(101)
sample = sample.int(n = nrow(new4), size = floor(.75*nrow(new4)), replace = F)
train = new4[sample, ]
test = new4[-sample, ]


eq01 = lm(Diabetes_Incidence ~., data = train)
summary(eq01) 
#adj rsq = .5477
#74 coefficients
pred01 = predict(eq01,test)
RMSEeq01= sqrt(mean((test$Diabetes_Incidence - pred01)^2))
RMSEeq01
#RMSE=1.209591
plot(pred01,test$Diabetes_Incidence,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq01)


eq02 = lm(Diabetes_Incidence ~ disable + computer + black + asian + aindian + latino + chci + sparent + a85a + a7584 + a6574 + a5564 + a2034 + 
     Adult_obesity + Physical_inactivity + Uninsured + Adult_smoking + Low_birthweight + per_Female + Excessive_drinking + Access_to_exercise_opportunities + State, data = train)
summary(eq02) 
#adj rsq = .5477
#70 coefficients
pred02 = predict(eq02,test)
RMSEeq02= sqrt(mean((test$Diabetes_Incidence - pred02)^2))
RMSEeq02
#RMSE=1.210594
plot(pred02,test$Diabetes_Incidence,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq02)


eq03 = lm(Diabetes_Incidence ~ disable + computer + black + asian + aindian + latino + chci + sparent + a85a + a7584 + a6574 + a5564 + a2034 + 
            Adult_obesity + Physical_inactivity + Uninsured + Adult_smoking + Low_birthweight + per_Female + Excessive_drinking + Access_to_exercise_opportunities, data = train)
#no state fixed effect
summary(eq03) 
#adj rsq = .4669
#21 coefficients
pred03 = predict(eq03,test)
RMSEeq03= sqrt(mean((test$Diabetes_Incidence - pred03)^2))
RMSEeq03
#RMSE=1.325801
plot(pred03,test$Diabetes_Incidence,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq03)
#excessive drinking, uninsured are both negative meaning they lower the change of diabetes incidence rate
#im guessing this means uninsured, excessive drinkers are irresponsible and therefore are more unlikely to 
#see a doctor and therefore wont be diagnosed
#percent female is positive and significant, since men are supposed to be more likely to get diabetes 
#compared to women im guessing on average women are more likely to see a doctor and get diagnosed


library(MASS)


aic01 = stepAIC(eq01, direction = 'backward')
aic01
aic01best = lm(Diabetes_Incidence ~ disable + computer + a85a + a7584 + black + 
                 aindian + latino + chci + sparent + a5564 + a2034 + 
                 Poor_health + Adult_obesity + Physical_inactivity + Uninsured + 
                 per_Female + Excessive_drinking + Access_to_exercise_opportunities + State ,
               data = train)
summary(aic01best) 
#adj, rsq = .5394
#67 coefficients 
pred06 = predict(aic01best,test)
RMSEeq06= sqrt(mean((test$Diabetes_Incidence - pred06)^2))
RMSEeq06
#RMSE=1.181307
plot(pred06,test$Diabetes_Incidence,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(aic01best)





sample2 = sample.int(n = nrow(new5), size = floor(.75*nrow(new5)), replace = F)
train2 = new5[sample2, ]
test2 = new5[-sample2, ]


eq04 = lm(mortalityrate ~., data = train2)
summary(eq04) 
#adj rsq = 0.499
#74 coefficients 
pred04 = predict(eq04,test2)
RMSEeq04= sqrt(mean((test2$mortalityrate - pred04)^2))
RMSEeq04
#RMSE=0.01372755
plot(pred04,test2$mortalityrate,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq04)
#now that the dependent variable is mortality rate percent female is negative as thought
#uninsured and excessive drinking still negative but not as significant so potentially not that important,maybe if your uninsured and or excessive 
#drinker you're more likely to die from something other than diabetes since diabetes usually takes a long time to kill



eq05 = lm(mortalityrate ~ computer + poverty + black + asian + aindian + latino + chci + sparent + a85a +
            Uninsured + per_Female + Excessive_drinking + State, data = train2)
summary(eq05)
#adj r sqr= .4866 
#61 coefficients
pred05 = predict(eq05,test2)
RMSEeq05= sqrt(mean((test2$mortalityrate - pred05)^2))
RMSEeq05
#RMSE=0.01364472
plot(pred05,test2$mortalityrate,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq05)

eq08 = lm(mortalityrate ~ computer + poverty + black + asian + aindian + latino + chci + sparent + a85a + 
            Uninsured + per_Female + Excessive_drinking, data = train2)
summary(eq08)#no state fixed effect
#adj r sqr= .4543  
#12 coefficients
pred08 = predict(eq08,test2)
RMSEeq08= sqrt(mean((test2$mortalityrate - pred08)^2))
RMSEeq08
#RMSE=0.01388422
plot(pred08,test2$mortalityrate,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(eq08)

aic02 = stepAIC(eq04, direction = 'backward')
aic02
aic02best = lm(mortalityrate ~ computer + poverty + a85a +
                 black + aindian + latino +chci + sparent + 
                 Uninsured + per_Female + Excessive_drinking + State, data = train2)
summary(aic02best) 
#adj, rsq = 0.4871  
#60 coefficients 
pred07 = predict(aic02best,test2)
RMSEeq07= sqrt(mean((test2$mortalityrate - pred07)^2))
RMSEeq07
#RMSE=0.013642
plot(pred07,test2$mortalityrate,
     xlab="predicted",ylab="actual", main="Predicted vs Actual")
abline(a=0,b=1, col = 'red')
par(mfrow = c(2, 2))
plot(aic02best)

library(ggplot2)
library(usmap)
library(RColorBrewer)

qt1=quantile(diabetesdata$Diabetes_Incidence, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
diabetesdata$Diabetes_Incidence =cut(diabetesdata$Diabetes_Incidence, breaks=qt1, labels=paste(qt1[-1]))
diabetesdata = diabetesdata %>% rename(fips=county) 

plot_usmap(regions = "county", data = diabetesdata, values = "Diabetes_Incidence", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 



qt2=quantile(diabetesmortality$mortalityrate, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
diabetesmortality$mortalityrate =cut(diabetesmortality$mortalityrate, breaks=qt2, labels=paste(qt2[-1]))
diabetesmortality = diabetesmortality %>% rename(fips=county) 

plot_usmap(regions = "county", data = diabetesmortality, values = "mortalityrate", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 


statediabetes = read_excel("statediabetesrate.XLSX")
qt3=quantile(statediabetes$DiabetesRate, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
statediabetes$DiabetesRate =cut(statediabetes$DiabetesRate, breaks=qt3, labels=paste(qt3[-1]))

plot_usmap(regions = "state", data = statediabetes, values = "DiabetesRate", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 


qt4=quantile(statediabetes$deaths_per_100000, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
statediabetes$deaths_per_100000 =cut(statediabetes$deaths_per_100000, breaks=qt4, labels=paste(qt4[-1]))

plot_usmap(regions = "state", data = statediabetes, values = "deaths_per_100000", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 


qt5=quantile(statediabetes$obesity_rate, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
statediabetes$obesity_rate =cut(statediabetes$obesity_rate, breaks=qt5, labels=paste(qt5[-1]))

plot_usmap(regions = "state", data = statediabetes, values = "obesity_rate", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 


cor_coefs = cor.test(new4$Diabetes_Incidence,new4$Adult_obesity )
ggplot(data = new4, aes(x = Diabetes_Incidence, y = Adult_obesity)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  annotate("text", x = 20, y = .5, label = paste0("R: ", round(cor_coefs$estimate, 2))) 
  
