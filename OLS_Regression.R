#OLS Regression

##########################
#Import Data#
library(readxl)
CBZ <- read_excel("your_path.xlsx")
View(CBZ)
CBZ <- as.data.frame(CBZ)
##############################################
CBZ
               Concentration(ng/mL) AreaRatio(CBZ/CBZD2)
1                   5.0                0.829
2                   5.0                0.785
3                   5.0                0.801
4                  10.0                1.770
5                  10.0                1.700
6                  10.0                1.680
7                  20.0                2.490
8                  20.0                2.450
9                  20.0                2.460
10                 30.0                2.860
11                 30.0                2.840
12                 30.0                2.840
13                 40.0                3.670
14                 40.0                3.850
15                 40.0                3.630
16                 50.0                4.230
17                 50.0                4.380
18                 50.0                4.270
19                  2.5                0.557
20                  2.5                0.622
21                  2.5                0.706
22                  5.0                0.947
23                  5.0                0.987
24                  5.0                0.949
25                 60.0                4.740
26                 60.0                4.850
27                 60.0                4.850
##############################################
str(CBZ) #Look at the data
colnames(CBZ) <- c('Concentration(ng/mL)','AreaRatio(CBZ/CBZD2)')
CBZ$'Concentration(ng/mL)' <- as.numeric(CBZ$'Concentration(ng/mL)')#numeric variable
str(CBZ) #ok
########################

#Check the correlation coefficient
cor(CBZ$`Concentration(ng/mL)`,CBZ$`AreaRatio(CBZ/CBZD2)`)
# 0.9893089 # OK

########################

LinearModel <- lm(CBZ$`AreaRatio(CBZ/CBZD2)`~ CBZ$`Concentration(ng/mL)`)
LinearModel

#068370+0.07233x

library(ggplot2)

#Model Accuracy
summary(LinearModel)
#Call: lm(formula = CBZ$`AreaRatio(CBZ/CBZD2)` ~ CBZ$`Concentration(ng/mL)`)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.30753 -0.17376 -0.03042  0.18293  0.36296 

#Coefficients:
#  Estimate Std. Error
#(Intercept)                0.683696   0.068021
#CBZ$`Concentration(ng/mL)` 0.072334   0.002133
#                              t value   Pr(>|t|)    
#(Intercept)                  10.05     2.89e-10 ***
#CBZ$`Concentration(ng/mL)`   33.92  <   2e-16 ***

#  Signif. codes:  
#  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#RSE represents the average difference between the observed outcome values and the predicted values by the model. The lower the RSE the best the model fits to our data.
#Residual standard error: 0.2233 on 25 degrees of freedom

#Multiple R-squared:  0.9787
#Adjusted R-squared:  0.9779 #variability in the outcome has been explained by the regression model.

#F-statistic:  1150 on 1 and 25 DF,  p-value: < 2.2e-16 #High F-> high significance of the model.

anova(LinearModel)
#Analysis of Variance Table

#Response: CBZ$`AreaRatio(CBZ/CBZD2)`
#                            Df   Sum Sq   Mean Sq  F value    Pr(>F)    
#CBZ$`Concentration(ng/mL)`  1    57.381  57.381    1150.5   < 2.2e-16 ***
#Residuals                   25   1.247   0.050                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ggplot(CBZ, aes(`Concentration(ng/mL)`,`AreaRatio(CBZ/CBZD2)`))+
  geom_point()+
  stat_smooth(method=lm,color="green", se = TRUE,level = 0.95)+
  ggtitle("Carbamazepine", subtitle = '068370+0.07233x')+
  theme_bw()

plot(LinearModel) #Indication if there are non-linear pattern(Residuals vs Fitted plot),Residuals should be normally distributed(Normal Q-Q),Assumption of equal variance(homoscedasticity(Scale-Location)),without influential cases (inside a red line-Cook's distance)

