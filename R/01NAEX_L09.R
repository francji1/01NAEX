################################################################
# 01NAEX -  Lecture 09
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code are from R Companion to Montgomery's DAoE 
################################################################

######################
# get requirements 
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","metR",
                      "MASS","agricolae","nlme","lme4","agricolae",
                      "scatterplot3d","FrF2","rsm","DoE.base","geoR",
                      "qualityTools","alr4")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

#library(FrF2)          #for 2^k  factorial design
######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path.
#setwd("H:/01NAEX/")

##############################################################################
# Follow Chapter 9 and 11 - Montgomery DAOE
##############################################################################

data_55   <- read.table("data/Ex55.csv",header=T,sep=";")
# factorize dataset
data_55f  <- data_55 %>% 
  mutate(Angle = as.factor(Angle),
         Speed = as.factor(Speed))
# centering dataset
data_55c  <- data_55 %>% 
  mutate(Angle = Angle - 20,
         Speed = Speed - 150)

model55_1  = lm(Life ~ Angle * Speed,data = data_55)
model55_1f = lm(Life ~ Angle * Speed,data = data_55f)

summary(model55_1f)

lm(Life~Angle*Speed,data=data_55f) %>% aov()  %>% summary()

lm(Life~Angle+Speed,data=data_55) %>% summary()

lm(Life ~ Angle + Speed + Angle:Speed + I(Angle^2)+I(Speed^2) 
         + I(Angle^2):Speed+I(Speed^2):Angle + I(Angle^2):I(Speed^2),data = data_55) %>%
  summary

lm(Life ~ Angle + Speed + Angle:Speed + I(Angle^2)+I(Speed^2) 
           + I(Angle^2):Speed+I(Speed^2):Angle + I(Angle^2):I(Speed^2),data = data_55) %>%
  aov() %>%
  summary()

######################
# EXAMPLE 11_1

time  = c(30, 40, 30, 40)
temp  = c(150, 150, 160,160)
yield = c(39.3, 40.9, 40.0, 41.5)
x1    = (time - 35)/5
x2    = (temp - 155)/5

data11_1 = data.frame(time,temp,x1,x2,yield)
model1   = lm( yield ~ x1 + x2,data = data11_1)
summary(model1)


#mod1=lm(yield~x1+x2+I(x1^2)+I(x2^2)+x1*x2,data=data11_1)
mod1=lm(yield~x1+x2,data=data11_1)
summary(mod1)
x=seq(-1,1,0.1); 
y=seq(-1,1,0.1)
f = function(x,y){
   new.x=data.frame(x1=x,x2=y)
   predict(mod1,new.x)
  }

z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(35,45,by=0.1),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(55,60,65))
axis(2,at=seq(-1,1,by=1),labels=c(160,165,170))
box()

pf(0.063,1,4, lower.tail=F)

######################
# EXAMPLE 11_2

time  = c(30, 40, 30, 40, 35,35,35,35,35)
temp  = c(150, 150, 160,160, 155,155,155,155,155)
yield = c(39.3, 40.9, 40.0, 41.5, 40.5, 40.3, 40.7, 40.2,40.6)
x1    = (time - 35)/5
x2    = (temp - 155)/5

data11_2 = data.frame(time,temp,x1,x2,yield)
model2  = lm( yield ~ x1*x2,data = data11_2)
summary(model2)
summary(lm( yield ~ x1+x2+I(x1^2)+I(x2^2)+x1*x2,data = data11_2))
summary(lm( yield ~ time+temp+I(time^2)+I(temp^2)+time*temp,data = data11_2))



################################################
# dataset from Montgomery DAOE - (old version)
time  = c(80,90,80,90,78,92,85,85,85,85,85,85,85)
temp  = c(170,170,180,180, 175,175,168,182,175,175,175,175,175)
yield = c(76.3,79.2 ,77.0 ,80.8 ,75.5 ,80.0 ,75.0 ,77.5 ,80.9 ,79.7 ,80.5 ,80.4 ,80.0 )


#Time Temperature x1 x2 Yield
#80 170 -1    -1     76.3
#90 170  1    -1     79.2
#80 180 -1     1     77.0
#90 180  1     1     80.8
#78 175 -1.414 0     75.5
#92 175  1.414 0     80.0
#85 168  0    -1.414 75.0
#85 182  0     1.414 77.5
#85 175  0     0     80.9
#85 175  0     0     79.7
#85 175  0     0     80.5
#85 175  0     0     80.4
#85 175  0     0     80.0
x1 = (time - 85)/5
x2 = (temp - 175)/5
data11_3 = data.frame(time,temp,x1,x2,yield)

model3=lm(yield~x1+x2+I(x1^2)+I(x2^2)+x1*x2,data=data11_3)
summary(model3)
summary(aov(model3))


model4=lm(yield~x1+x2+I(x1^2)+I(x2^2),data=data11_3)
summary(model4)

x=seq(-1.5,1.5,0.2); 
y=seq(-1.5,1.5,0.2)
f = function(x,y){
  new.x=data.frame(x1=x,x2=y)
  predict(model4,new.x)
}
z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()
################################################



################################################
# dataset from Montgomery DAOE - eight edition

#Time Temperature x1      x2    y1   y2 y3 
#80      170     -1      -1     76.5 62 2940
#80      180     -1       1     77.0 60 3470
#90      170      1      -1     78.0 66 3680
#90      180      1       1     79.5 59 3890
#85      175      0       0     79.9 72 3480
#85      175      0       0     80.3 69 3200
#85      175      0       0     80.0 68 3410
#85      175      0       0     79.7 70 3290
#85      175      0       0     79.8 71 3500
#92.07   175      1.414   0     78.4 68 3360
#77.93   175     -1.414   0     75.6 71 3020
#85      182.07   0       1.414 78.5 58 3630
#85      167.93   0      -1.414 77.0 57 3150


time  = c(80,80,90,90,85,85,85,85,85,92.07,77.93,85,85)
temp  = c(170,180,170,180, 175,175,175,175,175,175,175,182.07,167.93)
y1    = c(76.5,77.0,78.0,79.5,79.9,80.3,80.0,79.7,79.8,78.4,75.6,78.5,77.0)
y2    = c(62,60,66,59,72,69,68,70,71,68,71,58,57)
y3    = c(2940,3470,3680,3890,3480,3200,3410,3290,3500,3360,3020,3630,3150)

x1 = (time - 85)/5
x2 = (temp - 175)/5
data11_3b = data.frame(time,temp,x1,x2,y1,y2,y3)

model3b=lm(y1~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model3b)

summary(aov(model3b))

model3bb=lm(y1~x1+x2+I(x1^2)+I(x2^2),data=data11_3b)
summary(model3bb)
summary(aov(model3bb))

model3b$coeff

b = matrix(c(model3b$coeff[2],model3b$coeff[3]),2,1)
B = matrix(c(model3b$coeff[4], model3b$coeff[6]/2,
             model3b$coeff[6]/2,model3b$coeff[5]),2,2) 
cbind(b,B)
x_stat       = -1/2 * solve(B) %*% b
x_stat_natur = c((5*x_stat[1]+85), (5*x_stat[2] +175))
y_stat_natur = predict(model3b,data.frame(x1 = x_stat[1], x2 = x_stat[2]))
x_stat
x_stat_natur
y_stat_natur

x=seq(-1.5,1.5,0.01); 
y=seq(-1.5,1.5,0.01)
f = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model3b,new.x)
}
z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
points(x_stat[1],x_stat[2],pch=19)
text(x_stat[1]+0.05,x_stat[2]+0.05,"Stationary point",pos=4,cex=.6)
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()





model_y1=lm(y1~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model_y1)

model_y2=lm(y2~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model_y2)

model_y3=lm(y3~x1+x2,data=data11_3b)
summary(model_y3)


f1 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y1,new.x)
}
f2 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y2,new.x)
}
f3 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y3,new.x)
}
z1 = outer(x,y,f1)
z2 = outer(x,y,f2)
z3 = outer(x,y,f3)

op <- par(mfrow=c(2,3))

persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
persp(x,y,z2,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
persp(x,y,z3,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")

image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

image(x,y,z2,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z2,levels=seq(50,100,by=1),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

image(x,y,z3,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z3,levels=seq(3000,4000,by=50),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

par(op)


###### task: find solution with following parameters ####
#     y1 (yield)               > 78.5,
#62 < y2 (viscosity)           < 68
#     y3 (molecular weight Mn) < 3400

z4 = z1
z4[z1<78.5]         = 0
z4[(z2<62)|(z2>68)] = 0
z4[z3>3400]         = 0
z4[z4>0]            = 1 
table(z4)

opar <- par(mfrow=c(1,1))
contour(x,y,z1,axes=F,levels=seq(78.5,90,by=50),col="peru")
contour(x,y,z2,levels=seq(62,68,by=6),add=T,col="red")
contour(x,y,z3,levels=seq(3400,4000,by=1000),add=T,col="blue")
.filled.contour(x,y,z4,levels=c(0.1,1.1),col="red")
axis(1,at=seq(-1.4,1.4,by=0.2),labels=seq(78,92,by=1))
axis(2,at=seq(-1.4,1.4,by=0.2),labels=seq(168,182,by=1))
par(opar)



############################################################
# Exercise 09
###############################
# Solve problems 11._08 and 11_12 from D. C. Montogomery DAOE.
############################################################

# 11_08
# The data were collected in an experiment to optimize crystal growth as a function of three
# variables x1, x2, and x3. Large values of y (yield in grams) are desirable. Fit a second-order
# model and analyze the fitted surface. Under what set of conditions is maximum growth
# achieved?

data_11_08   <- read.table("data/Ex118.csv",header=T,sep=";")
head(data_11_08)


# 11_12
# Consider the three-variable central composite design. Analyze the data and draw
# conclusions, assuming that we wish to maximize conversion (y1) with activity (y2) between
# 55 and 60 achieved?
data_11_12   <- read.table("data/Ex1112.csv",header=T,sep=";")
head(data_11_12)



###################
# Example of plot with two overlying contourplots (data from Exercise 06)
############
data <- read.table("data/Problem_6_31.txt", header=TRUE, sep=";")
data <- data[-c(17,18,19,20),]

p.lm            <- lm(Weight ~ Temperature + Concentration + Time + Temperature:Concentration, data=data)
p.lm2           <- lm(Viscosity ~ Temperature + Concentration, data=data)
p.tmp           <- list(Temperature=seq(100,120,by=1),Concentration=seq(4,8,by=.1))
p.new.data      <- expand.grid(p.tmp)
p.new.data$Time <- 24
p.new.data$fit  <- predict(p.lm ,p.new.data)

p.new.data2  <- p.new.data
p.new.data2$fit  <- predict(p.lm2 ,p.new.data)
p.new.data$response = "Weight"
p.new.data2$response= "Viscosity"
p.data = rbind(p.new.data,p.new.data2)


p.new.data3 <- p.new.data2 %>% 
  mutate(fit2  = predict(p.lm ,p.new.data)) %>% 
  rename(Viscosity = fit, 
         Weight = fit2) %>% 
  dplyr::select(-response)
head(p.new.data3)
v <- reshape2::melt(volcano)
v$value2 <- v$value+rnorm(5307,1,3)

library(metR)
g <- ggplot(p.new.data3, aes(Temperature, Concentration)) +
  geom_contour(aes(z = Viscosity),col= "red",binwidth = 10) +
  geom_contour(aes(z = Weight), col = "blue",binwidth = 10) +
  geom_text_contour(aes(z = Viscosity), col="red") +
  geom_text_contour(aes(z = Weight),col="blue")
g



ggplot(p.new.data, aes(Temperature,Concentration)) +
  geom_contour_filled(aes(z = fit)) +
  labs(title = "Contour plot of p",
       x="A:Temperature",y="C:Concentration",
       level = "Weight") 


p2 = ggplot(p.data, aes(x=Temperature , y=Concentration , z=fit, colour=response )) +
  stat_contour(aes(alpha=..level..), binwidth=10) +
  theme(panel.background=element_rect(fill="white")) +
  theme(panel.grid=element_blank()) +
  labs(title="Plot 2")
p2

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

p.data2 <- p.data %>% 
  mutate_cond(response == "Weight", fit = fit - 2400) %>% 
  mutate_cond(response == "Viscosity", fit = fit - 1500)

p3 = ggplot(p.data2, aes(x=Temperature , y=Concentration , z=fit, colour=response )) +
  stat_contour(aes(alpha=..level..), binwidth=10) +
  theme(panel.background=element_rect(fill="gray")) +
  theme(panel.grid=element_blank()) +
  labs(title="Plot 2")
p3

