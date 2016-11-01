#LINEAR REGRESSION MODEL ON BOSTON-HOUSING DATA SET.
setwd("C:/Users/prakhar/Desktop/boston")

#Data preparation.
install.packages("data.table")
library(data.table)
boston<-fread("C:/Users/prakhar/Desktop/boston/Boston_Housing.csv")
head(boston)
summary(boston)

#Plotting Histogram.
data_frame<-as.data.frame(boston)
data_frame
drpd<-data_frame[,1:14]
hist(data_frame$MEDV)

par(mfrow=c(2,2))
plot(data_frame$MEDV,data_frame$CRIM)
plot(data_frame$MEDV,data_frame$ZN)
plot(data_frame$MEDV,data_frame$INDUS)
plot(data_frame$MEDV,data_frame$CHAS)
plot(data_frame$MEDV,data_frame$NOX)
plot(data_frame$MEDV,data_frame$RM)
plot(data_frame$MEDV,data_frame$AGE)
plot(data_frame$MEDV,data_frame$DIS)
plot(data_frame$MEDV,data_frame$RAD)
plot(data_frame$MEDV,data_frame$TAX)
plot(data_frame$MEDV,data_frame$PTRATIO)
plot(data_frame$MEDV,data_frame$B)
plot(data_frame$MEDV,data_frame$LSTAT)

#CHECKING CORRELATIONS.
install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
descrCor<-cor(data_frame)
install.packages("caret")
library(caret)
data_frame
highlyCorrelated<-findCorrelation(descrCor,cutoff = 0.6)
#IDENTIFYING HIGHLY CORRELATED VARIABLES.
highlyCorCol<-colnames(data_frame)[highlyCorrelated]
highlyCorCol
descrCor

#OVERALL MODEL 
fit<-lm((MEDV)~.,data=drpd)

#overall summary of model
summary_fit<-summary(fit)

paste(paste("Multiple R-squared: ",round(summary_fit$r.squared,digits=6)), paste("Adjusted R-squared: ",round(summary_fit$adj.r.squared,digits=6)))

#plotting the whole model

plot(fit)

#EXTRACTING R SQUARED.
summary(fit)$r.squared

#EXTRACTING ADJ.R.SQUARED
summary(fit)$adj.r.squared

#STEPWISE SELECTION BASED ON AIC
install.packages("MASS")
library(MASS)
step<-stepAIC(fit,direction = "both")
summary(step)

#BACKWARD SELECTION BASED ON AIC
step<-stepAIC(fit,direction = "backward")
summary(step)

#FORWARD SELECTION BASED ON AIC
step<-stepAIC(fit,direction = "forward")
summary(step)

#STEPWISE SELECTION WITH BIC
n=dim(drpd)[1]
stepbic<-stepAIC(fit,k=log(n))
summary(stepbic)

#R FUNCTION: STANDARDISED COEFFICIENTS
std.coff<-function(regmodel)
  {
 
   b<-summary(regmodel)$coef[-1,1]
  sx<-sapply(regmodel$model[-1],sd)
  sy<-sapply(regmodel$model[-1],sd)
  beta<-b*sx/sy
  return(beta)
  
}

std.coff<-data.frame(Standardised.Coeff=stdz.coff(stepbic))
std.coff<-cbind(Variable=row.names(std.coff),std.coff)
row.names(std.coff)=NULL

#TEST FOR MULTICOLLEARNITY.
#CHECKING VIF OF ALL VARIABLES.
install.packages("car")
library(car)
vif(stepbic)

#AUTOCORRELATION TEST.
durbinWatsonTest(stepbic)

#NORMALITY OF RESIDUALS (SHOULD BE > 0.05)
res=residuals(stepbic,type="pearson")
shapiro.test(res)

#TEST FOR HETEROSCADESTICITY (SHOULD BE >0.05)
ncvTest(stepbic)

#OUTLIERS TEST.
outlierTest(stepbic)

#SEE RESIDUALS
resid<-residuals(stepbic)

#RELATIVE IMPORTANCE
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepbic)

#SEE PREDICTED VALUE
pred=predict(stepbic,drpd)

#SEE ACTUAL VS PREDICTED.
final=cbind(boston,pred)
print(head(subset(final,select=c(MEDV,pred))))

#CALCULATING RMSE.
rmse <- sqrt(mean((drpd$MEDV - pred)^2))
print(rmse)

#CALCULATING R SQUARED MANUALLY.
y=drpd[,c("MEDV")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#CALCULATING ADJ.R.SQUARED MANUALLY.
y=dim(drpd)[1]
p=dim(summary(stepbic)$coeff)[1]-1
adj.r.squared=1-(1-r.squared)*((n-1)/(p-1))
print(adj.r.squared)

#BOX-COX TRANSFORMATION.
install.packages("lmSupport")
library(lmSupport)
modelboxcox(stepbic)




























