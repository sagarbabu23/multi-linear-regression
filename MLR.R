install.packages("data.table")
install.packages("xlsx")
install.packages("car")
library(data.table)
library(xlsx)
library(car)
startup_50 <- read.csv(choose.files())
View(startup_50)
summary(startup_50)
var(startup_50$`R.D.Spend`)
var(startup_50$Administration)
var(startup_50$`Marketing.Spend`)
var(startup_50$Profit)
sd(startup_50$`R.D.Spend`)
sd(startup_50$Administration)
sd(startup_50$`Marketing.Spend`)
sd(startup_50$Profit)

pairs(Corolla)
cor(Corolla)

Newyork <- ifelse(startup_50$State=="New York",1,0)
California <- ifelse(startup_50$State=="California",1,0)
Florida <- ifelse(startup_50$State=="Florida",1,0)
startup_50 <- cbind(startup_50,Newyork,California,Florida)
startup_50 <- startup_50[,-4]
pairs(startup_50)
cor(startup_50)
colnames(startup_50)

Profit_Model <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = startup_50)

summary(Profit_Model)
library(car)   
influenceIndexPlot(Profit_Model)
influencePlot(Profit_Model,id.n=3)

Profit_Model_Inf <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = startup_50[-c(50,49)])

summary(Profit_Model_Inf)

######### variance check for collinearity b/w varaibles###
Profit_Model <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = startup_50)
class(startup_50$Marketing.Spend)
vif(Profit_Model)
summary(Profit_Model)
avPlots(Profit_Model)

####### removed adminstration#######
Profit_Model_Revised <- lm(Profit~ R.D.Spend+Administration+Marketing.Spend+Newyork+California+Florida, data = startup_50)

library(MASS)

stepAIC(Profit_Model_Revised)
Profit_Model_Final <- lm(Profit~ R.D.Spend+Marketing.Spend, data = startup_50)

summary(Profit_Model_Final)

plot(Profit_Model_Final)

qqPlot(Profit_Model_Final, id.n=5) ###94.3

###### removing market spend########

Profit_Model_Final1<- lm(Profit~ R.D.Spend++Administration, data = startup_50)

summary(Profit_Model_Final1)

plot(Profit_Model_Final1)

qqPlot(Profit_Model_Final, id.n=5) ###94.5





################## computer sales data set############

Computer_Data <- read.csv(choose.files())
colnames(Computer_Data)
attach(Computer_Data)
str(Computer_Data)
var(Computer_Data$X)
var(Computer_Data$price)
var(Computer_Data$speed)
var(Computer_Data$hd)
var(Computer_Data$ram)
var(Computer_Data$screen)
var(Computer_Data$ads)
var(Computer_Data$trend)
View(Computer_Data)
sd(Computer_Data$X)
sd(Computer_Data$price)
sd(Computer_Data$speed)
sd(Computer_Data$hd)
sd(Computer_Data$ram)
sd(Computer_Data$screen)
sd(Computer_Data$ads)
sd(Computer_Data$trend)
summary(Computer_Data)



Computer_Data$cd_dummy1 <- ifelse(Computer_Data$cd=="yes",1,0)
Computer_Data$multi_dummy1 <- ifelse(Computer_Data$multi=='yes',1,0)
Computer_Data$premium_dummy1 <- ifelse(Computer_Data$premium=='yes',1,0)

Computer_Data$cd_dummy2 <- ifelse(Computer_Data$cd=='no',1,0)
Computer_Data$multi_dummy2 <- ifelse(Computer_Data$multi=='no',1,0)
Computer_Data$premium_dummy2 <- ifelse(Computer_Data$premium=='no',1,0)


comp_data <- Computer_Data[,-(7:9)]
comp_data <- comp_data[,-1]
View(comp_data)

str(comp_data)
attach(comp_data)

pairs(comp_data)
cor(comp_data)
comp_model <- lm(price ~ speed+hd+ram+screen+ads+trend+cd_dummy1+multi_dummy1+premium_dummy1, data = comp_data)

summary(comp_model)

influenceIndexPlot(comp_model, id.n=3)
avPlots(comp_model)

library(MASS)
vif(comp_model)
stepAIC(comp_model)


comp_model1 <- lm(price ~ speed+hd+ram+screen+ads+trend+multi_dummy1+premium_dummy1, data = comp_data[-c(1441,1701),])
summary(comp_model1)


library(MASS)
vif(comp_model1)
stepAIC(comp_model1)
influenceIndexPlot(comp_model1, id.n=3)
avPlots(comp_model1)



comp_model_final <- lm(price ~ speed+hd+ram+screen+ads+trend+cd_dummy1+multi_dummy1+premium_dummy1, data = comp_data[-c(1441,1701)])


summary(comp_model_final)


############# tayota carolla ############

ToyotaCorolla <- read.csv(choose.files())
Corolla<- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(Corolla)
View(Corolla)
var(Corolla$Price)
var(Corolla$Age_08_04)
var(Corolla$HP)
var(Corolla$KM)
var(Corolla$cc)
var(Corolla$Doors)
var(Corolla$Gears)
var(Corolla$Quarterly_Tax)
var(Corolla$Weight)
sd(Corolla$Price)
sd(Corolla$Age_08_04)
sd(Corolla$HP)
sd(Corolla$KM)
sd(Corolla$cc)
sd(Corolla$Doors)
sd(Corolla$Gears)
sd(Corolla$Quarterly_Tax)
sd(Corolla$Weight)


Corolla_Model <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Corolla)

summary(Corolla_Model)
influenceIndexPlot(Corolla_Model)
influencePlot(Corolla_Model,id.n=3)


vif(Corolla_Model)
stepAIC(Corolla_Model)


Corolla_Model_final <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Corolla[-c(81,222,961),])

summary(Corolla_Model_final)

plot(Corolla_Model_final)

qqPlot(Corolla_Model_final, id.n=5)
