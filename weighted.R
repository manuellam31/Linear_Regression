#Regression

#Check the correlation coefficient
cor(CBZ$ngmL,CBZ$AreaRatio)
# 0.9971903 # OK

Ftabreg <- qf(.95, df1=1, df2=26)
Ftabreg
Ftablack <- qf(.95, df1=7, df2=19)
Ftablack

########################
library(rsm)
LinearModel <- rsm(AreaRatio~FO(ngmL), data=CBZ)
#Model Accuracy
summary(LinearModel)
lin=function(x){LinearModel[["coefficients"]][["(Intercept)"]]+LinearModel[["coefficients"]][["FO(ngmL)"]]*x}

library(ggplot2)
ggplot(CBZ, aes(ngmL,AreaRatio))+
  geom_point()+
  stat_smooth(method=lm,color="green", se = TRUE,level = 0.95)+
  ggtitle("Carbamazepine", subtitle = '0.464(0.035)+0.076(0.011)*[CBZ]')+
  theme_bw()+
  xlab("Concentration(ng/mL")+
  ylab("Area Ratio (CBZ/CBZD2)")+
  labs(caption = "Linear Regression: Factor 1")

plot(LinearModel) #Indication if there are non-linear pattern(Residuals vs Fitted plot),Residuals should be normally distributed(Normal Q-Q),Assumption of equal variance(homoscedasticity(Scale-Location)),without influential cases (inside a red line-Cook's distance)

#Checking if any weighting is needed
CBZ_2 
#   x    y1    y2    y3
#  2.5 0.557 0.676 0.761
#  5.0 0.853 0.843 0.801
# 10.0 1.250 1.210 1.260
# 15.0 1.530 1.600 1.570
# 20.0 1.850 1.840 1.810
# 30.0 2.860 2.840 2.840
# 40.0 3.660 3.700 3.630
# 50.0 4.230 4.380 4.270
# 60.0 4.830 4.840 4.870

i=0
for(i in 1:9){
  CBZ_2$Ym[i] <- (CBZ_2$y1[i]+CBZ_2$y2[i]+CBZ_2$y3[i])/3
  i=i+1
} #create Y mean

i=0
for(i in 1:9){
  CBZ_2$var[i] <-((CBZ_2$y1[i]-CBZ_2$Ym[i])**2+(CBZ_2$y2[i]-CBZ_2$Ym[i])**2+(CBZ_2$y3[i]-CBZ_2$Ym[i])**2)/2
  i=i+1
} #create y sd^2

i=0
for(i in 1:9){
  CBZ_2$sd[i] <- sqrt(CBZ_2$var[i])
  i=i+1
} #create y sd


CBZ_2
# x   y1    y2    y3        Ym           sd        var
#  2.5 0.557 0.676 0.761 0.6646667 0.0105003333 0.10247113
#  5.0 0.853 0.843 0.801 0.8323333 0.0007613333 0.02759227
# 10.0 1.250 1.210 1.260 1.2400000 0.0007000000 0.02645751
# 15.0 1.530 1.600 1.570 1.5666667 0.0012333333 0.03511885
# 20.0 1.850 1.840 1.810 1.8333333 0.0004333333 0.02081666
# 30.0 2.860 2.840 2.840 2.8466667 0.0001333333 0.01154701
# 40.0 3.660 3.700 3.630 3.6633333 0.0012333333 0.03511885
# 50.0 4.230 4.380 4.270 4.2933333 0.0060333333 0.07767453
# 60.0 4.830 4.840 4.870 4.8466667 0.0004333333 0.02081666

ggplot(CBZ_2) + geom_line(aes(x=x, y=sd, color="sd")) + geom_line(aes(x=x, y=var, col="var")) + scale_color_discrete(name="Legend") + labs(title="Weight Analysis")+xlab("[CBZ] ng/mL")+ylab("value")+theme_bw()

WeightedModel <- rsm(AreaRatio~FO(ngmL), data=CBZ,weights = (1/(CBZ$ngmL^2)))
#Model Accuracy
summary(WeightedModel)
plot(WeightedModel)
wr=function(x){WeightedModel[["coefficients"]][["(Intercept)"]]+WeightedModel[["coefficients"]][["FO(ngmL)"]]*x}

ggplot(CBZ, aes(ngmL,AreaRatio))+
  geom_point()+
  stat_smooth(method=lm,color="green", se = FALSE,level = 0.95)+
  stat_function(fun=wr,color="blue", geom = 'line')+
  ggtitle("Carbamazepine", subtitle = '0.474(0.020)+0.074(0.003)*[CBZ]')+
  theme_bw()+
  xlab("Concentration(ng/mL")+
  ylab("Area Ratio (CBZ/CBZD2)")+
  labs(caption = "Weighted Regression: Factor 1/x²")

#both together
ggplot(CBZ, aes(ngmL,AreaRatio))+
  geom_point()+
  stat_function(fun=lin,aes(colour="Factor: 1"))+
  stat_function(fun=wr,aes(colour="Factor: 1/x²"))+
  scale_colour_manual("Regression Weight", values = c("green", "blue"))+
  ggtitle("Carbamazepine")+
  theme_bw()+
  xlab("Concentration(ng/mL")+
  ylab("Area Ratio (CBZ/CBZD2)")

