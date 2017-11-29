Cars <- read.csv("~/Desktop/Datasets_BA/Cars.csv")
attach(Cars)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Cars)

# 7. Find the correlation between Output (MPG) & inputs (HP, VOL, SP, WT) - SCATTER DIAGRAM
pairs(Cars)

# 8. Correlation coefficient - Strength & Direction of correlation
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

### Partial Correlation matrix - Pure correlation between the variables
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Cars))

# It is better to delete a single observation rather than entire variable to get rid of collinearity problem
# Deletion Diagnostics for identifying influential variable
influence.measures(model.car)
influenceIndexPlot(model.car, id.n=3) # Index Plots of the influence measures
influencePlot(model.car, id.n=3) # A user friendly representation of the above

## Regression after deleting the 77th observation, which is influential observation
model.car1<-lm(MPG~VOL+HP+SP+WT, data=Cars[-77,])
summary(model.car1)

## Regression after deleting the 77thand 71st observations
model.car2<-lm(MPG~VOL+HP+SP+WT, data=Cars[-c(71,77),])
summary(model.car2)

### Variance Inflation Factors is a formal way to check for collinearity
vif(model.car)  # VIF is > 10 => collinearity
plot(VOL,WT, col="dodgerblue4",pch=20)
plot(HP,SP, col="dodgerblue4",pch=20)
layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)

avPlots(model.car, id.n=2, id.cex=0.7) # Added Variable Plots

# VIF & avPlots has given us an indication to delete 'WT' variable
finalmodel <- lm(MPG~VOL+HP+SP)
summary(finalmodel)

# Evaluate model LINE assumptions
plot(model.car)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.car, id.n=5) # QQ plots of studentized residuals, helps identify outliers


library("MASS")
stepAIC(model.car) # backward

# Lower the AIC value better is the model. AIC is used only if you build multiple models.
