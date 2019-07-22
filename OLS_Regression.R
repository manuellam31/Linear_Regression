#OLS Regression

##########################
#Import Data#
library(readxl)
CBZ <- read_excel("your_path.xlsx")
View(CBZ)
CBZ <- as.data.frame(CBZ)
###########################
str(CBZ) #Look at the data
colnames(CBZ) <- c('ngmL','AreaRatio')
str(CBZ) #ok
########################

#Check the correlation coefficient
cor(CBZ$ngmL,CBZ$AreaRatio)
# 0.9971903 # OK

########################

LinearModel <- lm(CBZ$AreaRatio~ CBZ$ngmL)

#0.46374+0.07562x

library(ggplot2)

#Model Accuracy
summary(LinearModel)
plot(LinearModel) #Indication if there are non-linear pattern(Residuals vs Fitted plot),Residuals should be normally distributed(Normal Q-Q),Assumption of equal variance(homoscedasticity(Scale-Location)),without influential cases (inside a red line-Cook's distance)

ggplot(CBZ, aes(ngmL,AreaRatio))+
  geom_point()+
  stat_smooth(method=lm,color="green", se = TRUE,level = 0.95)+
  ggtitle("Carbamazepine", subtitle = '068370+0.07233x')+
  theme_bw()

#Check lack of fit
library(rsm)
LinearModel2 <- rsm(AreaRatio~FO(ngmL), data = CBZ) 
summary(LinearModel2)  
