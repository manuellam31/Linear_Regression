# Evaluate if two linear regressions are equal

#Example
x1=c(1:10)
y1=sort(rnorm(n = 10))
reg1 <- lm(y1~x1)
reg1
plot(x1,y1)
abline(reg1)
summary(reg1)

x2=c(0.47:10.47)
y2=sort(rnorm(n = 11))
reg2 <- lm(y2~x2)
reg2
plot(x2,y2)
abline(reg2)
summary(reg2)
#################

#Dummy method

x=c(x1,x2) #combine x1 and x2
y=c(y1,y2) #combine y1 and y2
plot(x,y)
abline(reg1)
abline(reg2)

w=c(rep(x = 0,times=10),rep(x = 1,times=11)) #dummy variable
xw=x*w

reg3 <- lm(y ~ x+w+xw) #y=b0+b1x+b2w+b3xw
reg3
summary(reg3)
anova(reg3)

# if xw is significant b3 is different from 0 then the regressions differ from each other by the slope
# if w is significant b2 is different from 0 then the regressions differ from each other by the intercept
# if w and xw are not significant then both regions have the same regression

#Analysis of Variance Table

#Response: y
#         Df  Sum Sq Mean Sq  F value    Pr(>F)    
#x          1 30.0049 30.0049 427.4979 1.737e-13 ***
#w          1  3.8106  3.8106  54.2919 1.098e-06 ***
#xw         1  0.4218  0.4218   6.0095   0.02534 *  
#Residuals 17  1.1932  0.0702 

#For the example the regressions are different 
