##f statistic R alpha 0.05 and the relevant degrees of freedom

qf(0.95,3,24)

## slide analysis 1

install.packages("HSAUR2")
library(HSAUR2)
install.packages("cowplot")
library(cowplot)

data("weightgain",package="HSAUR2")

summary(weightgain)
##foster feeding in weighgain dataset
tapply(weightgain$weightgain,list(weightgain$source,weightgain$type),mean) #calculate

plot.design(weightgain) ##Plot Univariate Effects of a Design or Model

## for these two I will use the general linear model

wg.aovsource <- anova(lm(weightgain~C(source),data=weightgain)) 
summary(wg.aovsource)

##

wg.aovtype <- anova(lm(weightgain~C(type),data=weightgain))
summary(wg.aovtype)

##slide analysis 2

anova(lm(weightgain~C(type)*C(source),data=weightgain))

##slide MANOVA

data("water",package="HSAUR2")
summary(water)

##one way MANOVA

summary(manova(cbind(hardness,mortality)~C(location),data=water),test="Wilks")

##summary(manova(cbind(hardness,mortality)~C(location),data=water),test="Roy")

##summary(manova(cbind(hardness,mortality)~C(location),data=water),test="Hotelling-Lawley")

##summary(manova(cbind(hardness,mortality)~C(location),data=water),test="Pillai")

summary(water)
dev.off()


##
library(ggplot2)

ap<-ggplot(water, aes(x=as.factor(location), y=hardness)) + geom_boxplot() + ggtitle("Boxplot of water hardness by location")
bp<-ggplot(water, aes(x=as.factor(location), y=mortality)) + geom_boxplot() + ggtitle("Boxplot of mortality by location")

plot_grid(ap, bp, ncol = 2, nrow = 1)


##

data("mtcars",package="datasets")
##Build aModel with interaction between categorical variable and predictor variable
##result <- aov(mpg~hp*am,data = mtcars)
##print(summary(result)) ##hp:am interaction does not seem  important
##Model without interaction between categorical variable and predictor variable
##result <- aov(mpg~hp+am,data = mtcars)
##print(summary(result))
##comparing the two models
result1 <- aov(mpg~hp*am,data = mtcars)
result2 <- aov(mpg~hp+am,data = mtcars)
# Compare the two models.
print(anova(result1,result2))##mileage will depend on horsepower in a similar manner whether or not the car is auto of manual transmission

##
mtcarsubset=subset(mtcars,,select=c('mpg','hp','am'))
ggplot(mtcarsubset, aes(x=hp, y=mpg,shape=as.factor(am),color=as.factor(am))) +
  geom_point() +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  ggtitle("MPG -versus hp for different transmission types") +
  xlab("horsepower")+
  ylab("mpg")+
  theme_bw()
##tidy it up a bit
mtcarsubset=subset(mtcars,,select=c('mpg','hp','am'))

##meets  spec homogenity of regession lines
ggplot(mtcarsubset, aes(x=hp, y=mpg,color=as.factor(am))) +
  geom_point() +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  ggtitle("MPG -versus hp for different transmission types") +
  xlab("horsepower")+
  ylab("mpg")+
  scale_colour_manual(name="transmission",values=c("red","blue"),labels=c("manual","automatic"))+
  theme_bw()

##meets  independence of covariates metrics, hmm not sure
## see below
library(ggplot2)

ap<-ggplot(mtcarsubset, aes(x=as.factor(am), y=mpg)) + geom_boxplot() + ggtitle("Boxplot of mpg by transmission type")+xlab("transmission type")
bp<-ggplot(mtcarsubset, aes(x=as.factor(am), y=hp)) + geom_boxplot() + ggtitle("Boxplot of horsepower by  transmission type")+xlab("transmission type")

plot_grid(ap, bp, ncol = 2, nrow = 1)
##need to run a test this looks like it could breach this assumption



anova(lm(mpg~(as.factor(am)),data=mtcarsubset))
##does differ
anova(lm(hp~(as.factor(am)),data=mtcarsubset))
##does not differ okay 