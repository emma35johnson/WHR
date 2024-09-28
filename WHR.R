## World Happiness Report 2023

browseURL("https://happiness-report.s3.amazonaws.com/2023/WHR+23_Statistical_Appendix.pdf") ## Appendix
browseURL("https://worldhappiness.report/data/") ## All resources
browseURL("https://drive.google.com/drive/u/2/folders/1cg0GfV7MZQGsECTMZbWWA94wFK90IGNt")
  ## download WHR.clean.csv ^ 

all.data <- as.matrix(read.csv(file.choose()), header = TRUE)   ## choose WHR.clean.csv from Google Drive
country <- all.data[-99,1]
happiness <- as.numeric(all.data[-99,2])
GDP <- as.numeric(all.data[-99,3])
soc.supp <- as.numeric(all.data[-99,4])
life.exp <- as.numeric(all.data[-99,5])
freedom <- as.numeric(all.data[-99,6])
generosity <- as.numeric(all.data[-99,7])
corruption <- as.numeric(all.data[-99,8])

###################IGNORE
## Indicate which countries we want to compare -- THESE CAN CHANGE IF WE WANT
US <- ifelse(country == "United States", 1, 0)  ## ranked 15
FIN <- ifelse(country == "Finland", 1, 0)  ## ranked 1
AFG <- ifelse(country == "Afghanistan", 1, 0)  ## ranked 137 (last)


## run when we decide on categorical vars for countries:
WHR <- matrix(c(happiness, GDP, soc.supp, life.exp, 
         freedom, generosity, corruption), ncol = 7) ## ncol = 8+ depending on how many countries we compare 
WHR


## Models
hap.model <- lm(happiness ~ US + FIN + AFG + GDP + soc.supp + life.exp + freedom + generosity + corruption)
summary(hap.model)


if (!require(leaps)) install.packages("leaps")
library(leaps)
collection <- regsubsets(happiness ~ GDP + soc.supp + life.exp + freedom + generosity + corruption, data = data.frame(data.frame(GDP, soc.supp, life.exp, freedom, generosity, corruption)), nbest = 10)
plot(collection, scale="adjr2")
plot(collection, scale="bic")
plot(collection, scale="Cp")


cor(data.frame(GDP, soc.supp, life.exp, freedom, generosity, corruption))
## country 99 (State of Palestine) missing life.exp value


###########IGNORE
life.exp[is.na(life.exp)] <- 74.2798
life.exp


## Testing variables to drop
anova(hap.model)

## no need for categorical variables -- already accounted for in ranking
  ## could classify countries by region; would take some work
    ## continent?

hap.model2 <- lm(happiness ~ GDP + soc.supp + life.exp + freedom + generosity + corruption)
summary(hap.model2)


## Testing whether life expectancy and generosity can be dropped from the model
anova(lm(happiness ~ GDP + soc.supp + freedom + corruption + generosity + life.exp))
(0.038 + 0.318/2)/(30.619/130) ## life.exp and generosity
1-pf(0.8364088, 2, 130)
## Drop both


hap.model3 <- lm(happiness ~ GDP + soc.supp + freedom + corruption)
summary(hap.model3)

qqplot(hap.model3)

hap.fitted <- -1.41635 + 0.25374*GDP + 4.19937*soc.supp + 2.33212*freedom - 0.86952*corruption

plot(hap.fitted, hap.model3$residuals)
pairs(~ happiness + GDP + soc.supp + freedom + corruption)
## correlation among var.s == BAD

cor(data.frame(GDP, soc.supp, freedom, corruption))
## social support and GDP correlated
## look at graphs of model residuals vs interactions


if (!require(car)) install.packages("car")
library(car)
vif(hap.model2)
vif(hap.model3)
## below 10 is okay


## Outliers (Y)
## Bonferroni adj
hatvalues(hap.model3)
qt(1-0.05/(2*136), 130) #t
sort(abs(rstudent((hap.model3)))) #ti
which(abs(rstudent((hap.model3))) > qt(1-0.05/(2*136), 130))
## Outlier at 131
country[131]
## Botswana

## Outliers (X)
sum(hatvalues(hap.model3))
5/136   #leverage avg
2*5/136   #outlier indication
which(hatvalues(hap.model3) > 2*5/136)
country[c(1,2,6,25,88,105,115,126,135,136)]
## Finland, Denmark, Sweden, Singapore, Venezuela, Turkey, Benin, Madagascar, Lebanon, Afghanistan
hatvalues(hap.model3)[c(1,2,6,25,88,105,115,126,135,136)]

## Influential Observations
## Small data set
which(abs(dffits(hap.model3)) > 1)
country[88]
## Venezuela
dffits(hap.model3)[88]

## Large data set
which(abs(dffits(hap.model3)) > 2*sqrt(5/136))
country[c(82,88,114,115,125,128,131,133,135)]
## Hong Kong, Venezuela, Cambodia, Benin, India, Tanzania, Botswana, Zimbabwe, Lebanon
dffits(hap.model3)[c(82,88,114,115,125,128,131,133,135)]


## boxcox?
boxcox(hap.model3) ## no need for transformation of y


## Splitting data in 2
ind <- sample(1:136, 68)


c2 <- corruption^2
c3 <- corruption^3

########### do not use
fit2 <- lm(happiness ~ GDP + soc.supp + freedom + corruption + c2)
summary(fit2)
fitv2 <- fit2$coef[1] + fit2$coef[2]*GDP + fit2$coef[3]*soc.supp + fit2$coef[4]*freedom + fit2$coef[5]*corruption + fit2$coef[6]*c2
fit3 <- lm(happiness ~ GDP + soc.supp + freedom + corruption + c2 + c3)
summary(fit3)
fitv3 <- fit3$coef[1] + fit3$coef[2]*GDP + fit3$coef[3]*soc.supp + fit3$coef[4]*freedom + fit3$coef[5]*corruption + fit3$coef[6]*c2 + fit3$coef[7]*c3

plot(fitv2, fit2$residuals)
plot(fitv3, fit3$residuals)
#################
    
summary(hap.model3)
plot(hap.fitted, hap.model3$residuals)
abline(hap.model3)

hap.fitted[c(82,88,114,115,125,128,131,133,135)]
happiness[c(82,88,114,115,125,128,131,133,135)]

stem(GDP)
stem(soc.supp)
stem(freedom)
stem(corruption)
which(corruption < .3)
country[c(1,2,6,7,8,10,25)]

plot(hap.fitted, hap.model3$residuals) +
  points(hap.fitted[c(1,2,6,7,8,10,25)], hap.model3$residuals[c(1,2,6,7,8,10,25)], col = "blue") +
  points(hap.fitted[c(82,88,114,115,125,128,131,133,135)], hap.model3$residuals[c(82,88,114,115,125,128,131,133,135)], col = "red")

plot(hap.fitted, hap.model3$residuals) +
  points(hap.fitted[c(82,88,114,115,125,128,131,133,135)], hap.model3$residuals[c(82,88,114,115,125,128,131,133,135)], pch = 8, col = "red")
  
plot(hap.fitted, hap.model3$residuals) +
  points(hap.fitted[c(1,2,6,25,88,105,115,126,135,136)], hap.model3$residuals[c(1,2,6,25,88,105,115,126,135,136)], pch = 8, col = "red")


WHR[c(1,2,6,25,88,105,115,126,135,136)]

stem(hap.fitted)
which(hap.fitted < 3)
country[136]
plot()


#######
if (!require(caret)) install.packages("caret")
library(caret)

setIndex <- createDataPartition(WHR$happiness, p = 0.5, list = FALSE)
train_data <- data[setIndex, ]
test_data <- data[-setIndex, ]

# Training vs. testing sets
ind <- sample(1:136, 68)

happiness.train <- happiness[ind]
GDP.train <- GDP[ind]
soc.supp.train <- soc.supp[ind]
freedom.train <- freedom[ind]
corruption.train <- corruption[ind]

WHR.train <- data.frame(matrix(c(happiness.train, GDP.train, soc.supp.train,
                      freedom.train, corruption.train), ncol = 5))


happiness.test <- happiness[-ind]
GDP.test <- GDP[-ind]
soc.supp.test <- soc.supp[-ind]
freedom.test <- freedom[-ind]
corruption.test <- corruption[-ind]

WHR.test <- matrix(c(happiness.test, GDP.test, soc.supp.test,
                      freedom.test, corruption.test), ncol = 5)
data.frame(WHR.test)

## OR
WHR.data <- data.frame(matrix(c(happiness, GDP, soc.supp, freedom, corruption), ncol = 5))
colnames(WHR.data) <- c("happiness", "GDP", "soc.supp", "freedom", "corruption")

train <- WHR.data[ind,]
test <- WHR.data[-ind,]

train.m <- lm(happiness ~ GDP + soc.supp + freedom + corruption, data = train)
summary(train.m)

test.p <- predict(train.m, newdata = test)
t.test(test.p, test$happiness)


## Training model
hap.m.train <- lm(happiness.train ~ GDP.train + soc.supp.train + freedom.train + corruption.train)
predictions <- predict(hap.m.train, newdata = data.frame(WHR.test)) ##I don't think this element is correct
mse <- mean((happiness.train - predictions)^2)