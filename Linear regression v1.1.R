wc.at <- read.csv("~/Desktop/Datasets_BA/wc-at.csv",header=T)

library("lattice")
?lattice

# Exploratory data analysis
summary(wc.at)

# Graphical exploration
dotplot(wc.at$Waist, main="Dot Plot of Waist Circumferences")
dotplot(wc.at$AT, main="Dot Plot of Adipose Tissue Areas")
boxplot(wc.at$Waist,col="dodgerblue4")
boxplot(wc.at$AT,col="red", horizontal = T)

#Scatter plot
plot(wc.at$Waist,wc.at$AT,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Waist Ciscumference", 
     ylab="Adipose Tissue area", pch=20)  # plot(x,y)

?plot

attach(wc.at)

cor(Waist, AT)

reg <- lm(AT~Waist, data=wc.at) # Y ~ X
summary(reg)
confint(reg,level=0.95)
predict(reg,interval="predict")

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(AT~sqrt(Waist), data=wc.at)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

reg_log<-lm(AT~log(Waist), data=wc.at)
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")



reg1<-lm(log(AT)~Waist + I(Waist*Waist), data=wc.at)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")


reg_sqrt1<-lm(sqrt(AT)~Waist, data=wc.at)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")

reg_log1<-lm(log(AT)~Waist, data=wc.at)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")

newspaper_data<-read.csv("~/Desktop/NewspaperData.csv", header=TRUE)
attach(newspaper_data)
dotplot(sunday, main="Dot Plot of Sunday Circulations",col="dodgerblue4")
dotplot(daily, main="Dot Plot of Daily Circulations", col="dodgerblue4")
boxplot(sunday,col="dodgerblue4", horizontal = T)
boxplot(daily,col="dodgerblue4", horizontal= T)
plot(daily,sunday,main="Scatter Plot ", col="Dodgerblue4", col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Daily Circulations", ylab="Sunday Circulations", pch=20)
reg.model <- lm(sunday~daily)
summary(reg.model)
anova(reg.model)
plot(reg.model)

?lm


layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)
confint(reg.model,level=0.95)
predict(reg.model,interval="predict")

plot(daily,sunday,main="Scatter Plot ", col="Dodgerblue4", col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Daily Circulations", ylab="Sunday Circulations", pch=20)
segments(daily, sunday, daily, 100+daily*1.3)
abline(coef=c(100,1.3), col="red")

plot(daily,sunday,main="Line of Best Fit", col="Dodgerblue4", col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Daily Circulations", ylab="Sunday Circulations", pch=20)
abline(reg.model, col="red")
segments(daily, sunday, daily,predict(reg.model))

predict(reg.model,newdata= data.frame(daily=c(500000,2000000)),interval="predict")


library("car")
?car
cigarette_consumption <- read.csv("~/Desktop/Datasets_BA/CigaretteConsumption.csv",header=T)
attach(cigarette_consumption)

cg <- cigarette_consumption[,-1]

summary(cigarette_consumption[,2:8]) #[rows, columns]

pairs(cigarette_consumption[,2:8],col="dodgerblue4",pch=20)

cor(cigarette_consumption[,2:8])  # correlation matrix

reg.model <- lm(Sales~Age+HS+Income+Black+Female+Price)
summary(reg.model)
layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)
confint(reg.model,level=0.95)

reg.reduced<-lm(Sales~Age+Income+Black+Price)
summary(reg.model)
summary(reg.reduced)
anova(reg.model)
anova(reg.reduced)

f=((34960-34926)/(6-4))/(34926/(51-6-1))
pf(f,2,(51-6-1))


Cars <- read.csv("~/Desktop/Datasets_BA/Cars.csv")
attach(Cars)

summary(Cars)

pairs(Cars)

cor(Cars)

# The Linear Model of interest
model.car <- lm(MPG~VOL+HP+SP+WT)
summary(model.car)

model.carV<-lm(MPG~VOL)
summary(model.carV)

model.carW <- lm(MPG~WT)
summary(model.carW)

model.carVW <- lm(MPG~VOL+WT)
summary(model.carVW)

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Cars, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

### Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Cars))
cor(Cars)

# Diagnostic Plots
library(car)
plot(model.car)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
plot(model.car) # Residuals vs Regressors
qqPlot(model.car, id.n=5) # QQ plots of studentized residuals, helps identify outliers
avPlots(model.car, id.n=2, id.cex=0.7) # Added Variable Plots

# Deletion Diagnostics for identifying influential variable
influence.measures(model.car)
influenceIndexPlot(model.car, id.n=3) # Index Plots of the influence measures
influencePlot(model.car, id.n=3) # A user friendly representation of the above

## Regression after deleting the 77th observation
model.car1<-lm(MPG~VOL+HP+SP+WT, data=Cars[-77,])
summary(model.car1)

## Regression after deleting the 77thand 79th observations

model.car<-lm(MPG~VOL+HP+SP+WT, data=Cars[-c(71,77),])
summary(model.car)


### Variance Inflation Factors
vif(model.car)  # VIF is > 10 => collinearity
plot(VOL,WT, col="dodgerblue4",pch=20)
plot(HP,SP, col="dodgerblue4",pch=20)
layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)

influence.measures(reg.model)
hat.values<-hatvalues(reg.model)
plot(reg.model)
#### Added Variable Plots ######
avPlots(model.car, id.n=2, id.cex=0.8, col="red")

## Diagnostic Plots ###
influenceIndexPlot(reg.model, id.n=3)
influencePlot(reg.model, id.n=3)

library("MASS")
stepAIC(model.car) # backward

plot(model.car)

# Lower the AIC value better is the model. AIC is used only if you build
# multiple models.