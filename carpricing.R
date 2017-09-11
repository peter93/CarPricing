#load library
library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(stringr)
library(ggplot2)

#Setting Working Directory 
#change working directory according to your project
setwd("/home/asher/Desktop/Assignment- Linear Regression/")


#====================
#Data Sourcing
#====================
#1) read car data from .csv
#The unique value in this dataframe is car_ID
cars <- read.csv(file="CarPrice_Assignment.csv", header=TRUE, stringsAsFactors=FALSE)


#Examine the data
View(cars)

# Check the structure of "cars"
str(cars)


#====================
#Data Cleaning Finding & Derived Metrics
#====================

#Checking for duplicate values
unique(cars)

#Checking for null/missing values
sum(is.na(cars))

# Spliting the CarName into manufacture name and model name

cars$CarName <- tolower(cars$CarName)
cars$CarName <- as.character(cars$CarName)
cars <- cbind(cars,str_split_fixed(cars$CarName, " ", 2))
names(cars)[length(names(cars))-1]<-"ManufactureName" 
cars$`2` <-NULL
# Finding the names all name of car manufacture and cleaning the wrong data 
unique(cars$ManufactureName)

cars$ManufactureName <- as.factor(cars$ManufactureName)
cars$ManufactureName[cars$ManufactureName == "maxda"] <- "mazda"
cars$ManufactureName[cars$ManufactureName == "vokswagen" | cars$ManufactureName == "vw" ] <- "volkswagen"
cars$ManufactureName[cars$ManufactureName == "toyouta"] <- "toyota"
cars$ManufactureName[cars$ManufactureName == "porcshce"] <- "porsche"
cars$ManufactureName <- factor(cars$ManufactureName)

#Rounding Price of cars to 2 decimal value.
cars$price <- round(cars$price,digits=2)

#Adding new value average of city and highway mileage  
cars$avgmpg <- (cars$citympg + cars$highwaympg)/2

#Adding new value power to weight ratio of hourse power to curb weight  
cars$ptw <- cars$horsepower/cars$curbweight

#removing value that are not usefull in analysis
#removing car_ID its an unique value to idetify each variable but not a feature.
cars$car_ID <- NULL

#removing CarName as we have created to new independent variable from this variable 
cars$CarName <- NULL

#Checking for outliers in wheelbase
quantile(cars$wheelbase,seq(0,1,0.01))

#We can see a jump in value in 99% to 100% 115.544 to 120.900
cars$wheelbase[which(cars$wheelbase > 115.544)]<-115.544

#Checking for outliers in carlength
quantile(cars$carlength,seq(0,1,0.01))

#We can see a jump in value in 99% to 100% 202.480 to 208.100 
cars$carlength[which(cars$carlength > 202.480)]<-202.480

#We can see a jump in value in 2% to 3% 150.000 to 155.900
cars$carlength[which(cars$carlength < 155.900)]<-155.900

#Checking for outliers in carwidth
quantile(cars$carwidth,seq(0,1,0.01))

#We can see a jump in value in 0% to 1% 60.300 to 62.536
cars$carlength[which(cars$carwidth < 62.536)]<-62.536

#Checking for outliers in carheight
quantile(cars$carheight,seq(0,1,0.01))

#We don't see any jump in value

#Checking for outliers in curbweight
quantile(cars$curbweight,seq(0,1,0.01))

#We can see a jump in value in 0% to 1% 1488.00 to 1819.72 
cars$curbweight[which(cars$carwidth < 1819.72 )]<-1819.72 

#We can see a jump in value in 93% to 94% 3376.08 to 3471.80 
cars$curbweight[which(cars$carwidth > 3376.08 )]<-3376.08 


#Checking for outliers in enginesize
quantile(cars$enginesize,seq(0,1,0.01))

#We can see a jump in value in  2% to 3% 79.08 to  90.00
cars$enginesize[which(cars$enginesize < 90.00 )]<-90.00 

#We can see a jump in value in 99%  to 100% 302.16 to 326.00
cars$enginesize[which(cars$enginesize > 302.16 )]<-302.16 

#Checking for outliers in boreratio
quantile(cars$boreratio,seq(0,1,0.01))

#We can see a jump in value in 0%  to  1% 2.5400 to 2.9100
cars$boreratio[which(cars$boreratio < 2.9100 )]<-2.9100

#Checking for outliers in stroke
quantile(cars$stroke,seq(0,1,0.01))

#We can see a jump in value in 1% to 2% 2.1968 to 2.6400
cars$stroke[which(cars$stroke < 2.6400 )]<-2.6400

#We can see a jump in value in 95% to 96% 3.6400 to 3.8248
cars$stroke[which(cars$stroke > 3.6400 )]<-3.6400

#Checking for outliers in compressionratio
quantile(cars$compressionratio,seq(0,1,0.01))

#We can see a jump in value in 90% to 91% 10.9400 to 21.0000
cars$compressionratio[which(cars$compressionratio > 10.9400 )]<-10.9400

#Checking for outliers in horsepower
quantile(cars$horsepower,seq(0,1,0.01))

#We can see a jump in value in 99% to 100% 207.00 to 288.00 
cars$horsepower[which(cars$horsepower > 207.00 )]<-207.00

#Checking for outliers in peakrpm
quantile(cars$peakrpm,seq(0,1,0.01))

#We can see a jump in value in 99% to 100% 6000 to 6600 
cars$peakrpm[which(cars$peakrpm > 6000 )]<-6000

#Checking for outliers in citympg
quantile(cars$citympg,seq(0,1,0.01))

#We can see a jump in value in 98% to 99% 38.00 to 44.72  
cars$citympg[which(cars$citympg > 38.00 )]<-38.00

#Checking for outliers in highwaympg
quantile(cars$highwaympg,seq(0,1,0.01))

#We can see a jump in value in 99% to 100%  49.88 to 54.00  
cars$highwaympg[which(cars$highwaympg > 49.88 )]<-49.88

#Checking for outliers in price
quantile(cars$price,seq(0,1,0.01))

#We can see a jump in value in 4% to 5% 5655.68 to 6197.00 
cars$price[which(cars$price < 6197.00)]<- 6197.00

#We can see a jump in value in 98% to 99% 36809.60 to 40802.72 
cars$price[which(cars$price > 36809.60)]<- 36809.60


#Checking for outliers in avgmpg
quantile(cars$avgmpg,seq(0,1,0.01))

#We can see a jump in value in  98% to 99%   42.42 to 47.30  
cars$avgmpg[which(cars$avgmpg > 49.88 )]<-49.88

#Checking for outliers in ptw
quantile(cars$ptw,seq(0,1,0.01))

#We can see a jump in value in  98% to 99%   0.06611995 to  0.07506164 
cars$ptw[which(cars$ptw > 0.06611995 )]<-0.06611995

#====================
#Viewing the trend of data 
#====================
#Plot of the price distribution by car manufacturers
ggplot(cars,aes(x=ManufactureName ,y=price)) + geom_point() 

#Plot of the price distribution by fuel type
ggplot(cars,aes(x=fueltype,y=price)) + geom_point()

#Plot of the price distribution by aspiration
ggplot(cars,aes(x=aspiration,y=price)) + geom_point()

#Plot of the price distribution by doornumber
ggplot(cars,aes(x=doornumber,y=price)) + geom_point()

#Plot of the price distribution by carbody
ggplot(cars,aes(x=carbody,y=price)) + geom_point()

#Plot of the price distribution by drivewheel
ggplot(cars,aes(x=drivewheel,y=price)) + geom_point()

#Plot of the price distribution by enginelocation
ggplot(cars,aes(x=enginelocation,y=price)) + geom_point()

#Plot of the price distribution by enginetype
ggplot(cars,aes(x=enginetype,y=price)) + geom_point()

#Plot of the price distribution by cylindernumber
ggplot(cars,aes(x=cylindernumber,y=price)) + geom_point()

#Plot of the price distribution by fuelsystem
ggplot(cars,aes(x=fuelsystem,y=price)) + geom_point()

#====================
#Converting Categorical variables into dummy variables 
#====================

#convert fueltype variable to numeric is to replace the levels- Gas and diesel with 1 and 0 is:
cars$fueltype <- as.factor(cars$fueltype)
levels(cars$fueltype)<-c(1,0)
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]


#convert aspiration variable to numeric is to replace the levels- std and turbo with 1 and 0 is:
cars$aspiration <- as.factor(cars$aspiration)
levels(cars$aspiration)<-c(1,0)
cars$fueltype <- as.numeric(levels(cars$aspiration))[cars$aspiration]

#convert doornumber variable to numeric is to replace the levels- four and two with 1 and 0 is:
cars$doornumber <- as.factor(cars$doornumber)
levels(cars$doornumber)<-c(1,0)
cars$fueltype <- as.numeric(levels(cars$doornumber))[cars$doornumber]

#convert enginelocation variable to numeric is to replace the levels- front and rear with 1 and 0 is:
cars$enginelocation <- as.factor(cars$enginelocation)
levels(cars$enginelocation)<-c(1,0)
cars$fueltype <- as.numeric(levels(cars$enginelocation))[cars$enginelocation]

#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = cars))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_1)
cars$carbody <- NULL

#Converting "drivewheel" into dummies . 
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_2 <- dummy_2[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_2)
cars$drivewheel <- NULL

#Converting "enginetype" into dummies . 
dummy_3 <- data.frame(model.matrix( ~enginetype, data = cars))
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_3)
cars$enginetype <- NULL

#Converting "cylindernumber" into dummies . 
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_4)
cars$cylindernumber <- NULL

#Converting "fuelsystem" into dummies . 
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_5)
cars$fuelsystem <- NULL

#Converting "ManufactureName" into dummies . 
dummy_6 <- data.frame(model.matrix( ~ManufactureName, data = cars))
dummy_6 <- dummy_6[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
cars <- cbind(cars, dummy_6)
cars$ManufactureName <- NULL

#====================
#Divide into training and test data set 
#====================

#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(cars), 0.7*nrow(cars))
# generate the train data set
train = cars[trainindices,]

#store the rest of the observations into an object "test".
test = cars[-trainindices,]

#====================
#Creating Multilinear Model from 
#====================

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)

summary(model_1)
# Adjusted R-squared:  0.9746

# Creating Model using stepAIC
step <- stepAIC(model_1, direction="both")

#Creating second model removing variable given in step AIC
model_2 <- lm(formula = price ~ cylindernumbersix + cylindernumberfive 
              + carlength + drivewheelfwd + ManufactureNamejaguar      
              + fuelsystemspdi + cylindernumberfour + cylindernumberthree 
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_2)
#Adjusted R-squared:  0.9781 

##check for multicollinearity 
vif(model_2)

##Taking VIF threshold as  2

# Creating model by remvoing cylindernumbersix which VIF > 2 and p value > 0.05

model_3 <- lm(formula = price ~ cylindernumberfive 
              + carlength + drivewheelfwd + ManufactureNamejaguar      
              + fuelsystemspdi + cylindernumberfour + cylindernumberthree 
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_3)
#Adjusted R-squared:  0.9779

##check for multicollinearity 
vif(model_3)

# Creating model by remvoing cylindernumberfive which VIF > 2 and p value > 0.05

model_4 <- lm(formula = price ~  carlength + drivewheelfwd + ManufactureNamejaguar      
              + fuelsystemspdi + cylindernumberfour + cylindernumberthree 
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_4)
#Adjusted R-squared:  0.978 

##check for multicollinearity 
vif(model_4)


# Creating model by remvoing cylindernumberfour which VIF > 2 and p value > 0.05

model_5 <- lm(formula = price ~  carlength + drivewheelfwd + ManufactureNamejaguar      
              + fuelsystemspdi  + cylindernumberthree 
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_5)
#Adjusted R-squared:  0.9781

##check for multicollinearity 
vif(model_5)


# Creating model by remvoing cylindernumberthree which VIF > 2 and p value > 0.05

model_6 <- lm(formula = price ~  carlength + drivewheelfwd + ManufactureNamejaguar      
              + fuelsystemspdi + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_6)
#Adjusted R-squared:  0.9782 

##check for multicollinearity 
vif(model_6)


# Creating model by remvoing fuelsystemspdi which VIF > 2 and p value > 0.05

model_7 <- lm(formula = price ~  carlength + drivewheelfwd + ManufactureNamejaguar      
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_7)
#Adjusted R-squared:  0.9779 

##check for multicollinearity 
vif(model_7)


# Creating model by remvoing carlength which VIF > 2 and p value > 0.05

model_8 <- lm(formula = price ~  drivewheelfwd + ManufactureNamejaguar      
              + ManufactureNamehonda + ManufactureNameisuzu + enginetypeohc
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_8)
#Adjusted R-squared:  0.9774 

##check for multicollinearity 
vif(model_8)

# Creating model by remvoing enginetypeohc which VIF > 2 and p value > 0.05

model_9 <- lm(formula = price ~  drivewheelfwd + ManufactureNamejaguar      
              + ManufactureNamehonda + ManufactureNameisuzu 
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_9)
#Adjusted R-squared:  0.9769 

##check for multicollinearity 
vif(model_9)

# Creating model by remvoing drivewheelfwd which VIF > 2 and p value > 0.05

model_10 <- lm(formula = price ~  ManufactureNamejaguar      
              + ManufactureNamehonda + ManufactureNameisuzu 
              + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
              + fuelsystemmpfi + fuelsystem2bbl + horsepower + enginesize 
              + enginetypel + ManufactureNamenissan + enginetypedohcv 
              + carbodyhardtop + carwidth + carbodysedan 
              + ManufactureNamedodge + carbodyhatchback 
              + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
              + ManufactureNameplymouth + ManufactureNamerenault 
              + ManufactureNametoyota + aspiration + ptw
              + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
              + ManufactureNamebmw + fueltype, data = train)

summary(model_10)
#Adjusted R-squared:  0.9762  

##check for multicollinearity 
vif(model_10)

# Creating model by remvoing fuelsystemmpfi which VIF > 2 and p value > 0.05

model_11 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
               + fuelsystem2bbl + horsepower + enginesize 
               + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + carbodysedan 
               + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration + ptw
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_11)
#Adjusted R-squared:  0.9753  

##check for multicollinearity 
vif(model_11)

# Creating model by remvoing fuelsystem2bbl which VIF > 2 and p value > 0.05

model_12 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm + enginetypeohcf 
               + horsepower + enginesize 
               + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + carbodysedan 
               + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration + ptw
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_12)
#Adjusted R-squared:  0.9747 

##check for multicollinearity 
vif(model_12)

# Creating model by remvoing enginetypeohcf which VIF > 2 and p value > 0.05

model_13 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm 
               + horsepower + enginesize 
               + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + carbodysedan 
               + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration + ptw
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_13)
#Adjusted R-squared:  0.9737  

##check for multicollinearity 
vif(model_13)

#since horsepower and ptw has continuously high VIF value we will correlation
cor(cars$horsepower,cars$ptw)

#since it has high correlation value of 0.8658817 we check which value greater p value remove it
#As horsepower has p value 2.34e-06 and  ptw  has p value 7.27e-07 we remove hourse power build our model

model_14 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm 
               + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + carbodysedan 
               + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration + ptw
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_14)
#Adjusted R-squared:  0.9683
#We keep this model as the Adjusted R-squared value is still good

vif(model_14)


# Creating model by remvoing ptw which VIF > 2 and p value > 0.05

model_15 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm 
               + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + carbodysedan 
               + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_15)
#Adjusted R-squared:  0.9678   

##check for multicollinearity 
vif(model_15)

#since carbodysedan and carbodyhatchback has continuously high VIF value we will correlation
cor(cars$carbodysedan,cars$carbodyhatchback)

#since it has high negative correlation value of -0.6757787 we check which value greater p value remove it
#As carbodysedan has p value 1.87e-05 and  carbodyhatchback  has p value 2.31e-06 we remove carbodysedan build our model

model_16 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm 
               + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + carwidth + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_16)
#Adjusted R-squared:  0.9626

#We keep this model as the Adjusted R-squared value is still good

##check for multicollinearity 
vif(model_16)

#since enginesize and carwidth has continuously high VIF value we will correlation
cor(cars$enginesize,cars$carwidth)

#since it has high correlation value of 0.7393806 we check which value greater p value remove it
#As enginesize has p value 2e-16 and  carwidth  has p value 1.95e-08 we remove carwidth build our model

model_17 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + boreratio + enginetyperotor + peakrpm 
               + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_17)
#Adjusted R-squared:  0.9514

#We keep this model as the Adjusted R-squared value is still good

##check for multicollinearity 
vif(model_17)

# Creating model by remvoing boreratio which VIF > 2 and p value > 0.05

model_18 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + peakrpm 
               + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_18)
#Adjusted R-squared:  0.9481 

##check for multicollinearity 
vif(model_18)

#since enginesize and peakrpm has continuously high VIF value we will correlation
cor(cars$enginesize,cars$peakrpm)

#since it has moderate negative correlation value of  -0.2324405 we check which value greater p value remove it
#As enginesize has p value < 2e-16 and  peakrpm  has p value 1.09e-06 we remove peakrpm build our model

model_19 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNamebuick + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_19)

#Adjusted R-squared:  0.9371

#We keep this model as the Adjusted R-squared value is still good

##check for multicollinearity 
vif(model_19)

#since enginesize and ManufactureNamebuick has continuously high VIF value we will correlation
cor(cars$enginesize,cars$ManufactureNamebuick)

#since it has good correlation value of 0.4922118 we check which value greater p value remove it
#As enginesize has p value < 2e-16 and  ManufactureNamebuick  has p value 2.96e-06 we remove ManufactureNamebuick build our model

model_20 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda  + carbodywagon
               + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_20)

#Adjusted R-squared:  0.9251

#We keep this model as the Adjusted R-squared value is still good

##check for multicollinearity 
vif(model_20)
#All variables VIF value is higher than 2 so no multicollinearity

#We will remove carbodywagon as it has high p value

model_21 <- lm(formula = price ~  ManufactureNamejaguar      
               + ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_21)
#Adjusted R-squared:  0.9257 


#We will remove ManufactureNamejaguar as it has high p value
model_22 <- lm(formula = price ~ ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge + carbodyhatchback 
               + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_22)
#Adjusted R-squared:  0.926

#We will remove carbodyhatchback as it has high p value
model_23 <- lm(formula = price ~ ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + carbodyhardtop + ManufactureNamedodge 
               + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_23)
#Adjusted R-squared:  0.9242

#We will remove carbodyhardtop as it has high p value
model_24 <- lm(formula = price ~ ManufactureNamehonda + ManufactureNameisuzu 
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_24)
#Adjusted R-squared:  0.9224


#We will remove ManufactureNameisuzu as it has high p value
model_25 <- lm(formula = price ~ ManufactureNamehonda  
               + enginetyperotor + enginesize + enginetypel + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_25)
#Adjusted R-squared:  0.9187


#We will remove enginetypel as it has high p value
model_26 <- lm(formula = price ~ ManufactureNamehonda  
               + enginetyperotor + enginesize + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamevolkswagen  + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_26)
#Adjusted R-squared:  0.9151

#We will remove ManufactureNamevolkswagen as it has high p value
model_27 <- lm(formula = price ~ ManufactureNamehonda  
               + enginetyperotor + enginesize + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_27)
#Adjusted R-squared:  0.9125

#We will remove ManufactureNamehonda as it has high p value
model_28 <- lm(formula = price ~ + enginetyperotor + enginesize + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNamemazda + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_28)
#Adjusted R-squared:  0.9106

#We will remove ManufactureNamemazda as it has high p value
model_29 <- lm(formula = price ~ + enginetyperotor + enginesize + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNameplymouth + ManufactureNamerenault 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_29)
#Adjusted R-squared:  0.9075

#We will remove ManufactureNamerenault as it has high p value
model_30 <- lm(formula = price ~ + enginetyperotor + enginesize + ManufactureNamenissan + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNameplymouth  
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_30)
#Adjusted R-squared:  0.9038

#We will remove ManufactureNamenissan as it has high p value
model_31 <- lm(formula = price ~ + enginetyperotor + enginesize + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNameplymouth  
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_31)
#Adjusted R-squared:  0.8989


#We will remove ManufactureNameplymouth as it has high p value
model_32 <- lm(formula = price ~ + enginetyperotor + enginesize + enginetypedohcv 
               + ManufactureNamedodge + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_32)
#Adjusted R-squared:  0.8943 

#We will remove ManufactureNamedodge as it has high p value
model_33 <- lm(formula = price ~ + enginetyperotor + enginesize + enginetypedohcv 
               + ManufactureNametoyota + aspiration 
               + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_33)
#Adjusted R-squared:  0.8892

#We will remove ManufactureNametoyota as it has high p value
model_34 <- lm(formula = price ~ + enginetyperotor + enginesize + enginetypedohcv 
               + aspiration + ManufactureNamemitsubishi  
               + ManufactureNamebmw + fueltype, data = train)

summary(model_34)
#Adjusted R-squared:  0.8849


#We will remove ManufactureNamemitsubishi as it has high p value
model_35 <- lm(formula = price ~ + enginetyperotor + enginesize + enginetypedohcv 
               + aspiration + ManufactureNamebmw + fueltype, data = train)

summary(model_35)
#Adjusted R-squared:  0.8787

#We will remove enginetypedohcv as it has high p value
model_36 <- lm(formula = price ~ + enginetyperotor + enginesize  
               + aspiration + ManufactureNamebmw + fueltype, data = train)

summary(model_36)
#Adjusted R-squared:  0.8724

#We have all variable p value less than 0.05

# predicting the results in test dataset
Predict_1 <- predict(model_36,test[,-1])
test$test_price <- Predict_1

#we test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#0.8700487

#We have almost same rsquare value for test data and train data, that mean this is good model to predict the price of cost of vehicle
#price = 4244.510 + 5947.433 * enginetyperotor + 155.725 * enginesize + 3132.669*aspiration0 + 7796.294 * ManufactureNamebmw + -12062.212 * fueltype
#hence we can conclude that we need to cosnider enginetype as rotor and engine size and aspiration as 0 and manufacture segment which come under bmw 
#and fuel type when we put price for the vechicle
