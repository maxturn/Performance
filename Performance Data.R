setwd("~/desktop/R Projects/Performance/Test Data ")

library(tidyverse) # import the Tidyverse
library(RCurl)

rpedata<- read.csv("~/Desktop/R Projects/Performance/Test Data/RPE-Table 1.csv", stringsAsFactors=FALSE)

Week34 = filter(rpedata, Week == 34)
Week35 = filter(rpedata, Week == 35)
Week36 = filter(rpedata, Week == 36)
Week37 = filter(rpedata, Week == 37)

boxplot(Week34$RPE.Load, Week35$RPE.Load, Week36$RPE.Load, Week37$RPE.Load,
        names=c("34", "35", "36", "37"),
        col = blues9,
        xlab = "Week",
        ylab = "RPE",
        main = "Boxplot of RPE Load by Week")
points(1:4, c(mean(Week34$RPE.Load), mean(Week35$RPE.Load), mean(Week36$RPE.Load), mean(Week37$RPE.Load)),
       pch=18, cex=1.5)

out = lm(RPE.Load~Week, data=rpedata)
anova(out)

MSE = anova(out)$"Mean Sq"[2]

# 95% CI
round(mean(Week34$RPE.Load)+c(-1,1)*sqrt(MSE/94)*qt(1-0.5/2, 380),2)
mean(Week34$RPE.Load)
round(sqrt(MSE/94),2)
max(Week34$RPE.Load)
min(Week34$RPE.Load)

round(mean(Week35$RPE.Load)+c(-1,1)*sqrt(MSE/96)*qt(1-0.5/2, 380),2)
round(mean(Week35$RPE.Load),2)
round(sqrt(MSE/96),2)
max(Week35$RPE.Load)
min(Week35$RPE.Load)

round(mean(Week36$RPE.Load)+c(-1,1)*sqrt(MSE/94)*qt(1-0.5/2, 380),2)
round(mean(Week36$RPE.Load),2)
round(sqrt(MSE/94),2)
max(Week36$RPE.Load)
min(Week36$RPE.Load)

round(mean(Week37$RPE.Load)+c(-1,1)*sqrt(MSE/98)*qt(1-0.5/2, 380),2)
round(mean(Week37$RPE.Load),2)
round(sqrt(MSE/98),2)
max(Week37$RPE.Load)
min(Week37$RPE.Load)


library(agricolae)
#ANOVA for the data -> Construction the multiple comparisons LSD, SNK, Duncan Test
testdata.aov = anova(lm(RPE.Load~Week, data=rpedata))

# Student Newman Keuls Test
out.snk = SNK.test(rpedata$RPE.Load, rpedata$Week,
                   tail(testdata.aov$"Df",1), tail(testdata.aov$"Mean Sq",1),console=TRUE)



####################


# RPE= RPE[1:18,c(1,2,8,10, 11, 12)]
head(rpedata)
names(rpedata)

ggplot(rpedata, aes(x=Position, y=RPE)) +
  geom_boxplot()

out=lm(RPE~Position, data=rpedata)
anova(out)



# Looking at Week vs RPE
rpedata$Week = as.factor(rpedata$Week)
ggplot(rpedata, aes(x=Week, y=RPE)) +
  geom_boxplot() 

rpedata %>%
  group_by(Week) %>%
  summarise(meanRPE = mean(RPE), sdRPE = sd(RPE))

out=lm(RPE~Week, data=rpedata)
anova(out)



boxplot(RPE~Week, data=rpedata, col=blues9,
        xlab="Week", ylab="RPE")

rpedata %>%
  group_by(Athlete="99E") %>%
  summarise(meanData = mean(RPE), sdData = sd(RPE))

data99E = rpedata[1:11,]
head(data99E)

out99E = lm(RPE~Week, data=data99E)
summary(out99E)
names(data99E)

plot(Week, RPE, data=data99E)

class(data99E)
class(data99E$Week)

rpedata$Week = as.numeric(rpedata$Week)
rpedata$RPE = as.numeric(rpedata$RPE)
barchart(rpedata$Week, rpedata$RPE)

class(rpedata$Week)
class(rpedata$RPE)







#############################################################


gpsData<- read.csv("~/Desktop/R Projects/Performance/Test Data/GPS-Table 1.csv", stringsAsFactors=FALSE)
newgpsData = filter(gpsData, period == "Session")
gpsData = newgpsData[,c(1,9, 21)]
head(gpsData)
names(gpsData)

gpsData[gpsData==0] <- NA
# Delete the rows associated with NA.
gpsData<-gpsData[complete.cases(gpsData),]

out=lm(total.player.load~total.distance, data=gpsData)
summary(out)
anova(out)

ggplot(gpsData, aes(x=total.distance, y=total.player.load)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') + 
  ggtitle("Total Player Load vs. Distance") + 
  xlab("Total Distance") + 
  ylab("Total Player Load") + 
  theme_bw()

cor(gpsData$total.distance, gpsData$total.player.load)^2  # calculates the correlation 
# value can be interpreted as 79.19% of the total variation in y can be explained by x.












########################################################################
forceData<- read.csv("~/Desktop/R Projects/Performance/Test Data/Forceplate-Table 1.csv", stringsAsFactors=FALSE)
names(forceData)
newforceData = forceData[,c(1, 4, 5, 7, 8, 9, 10)]

library(dplyr)
forceData = rename(newforceData, Height = Jump.Height..Flight.Time...cm.,
       Mean.Power = Concentric.Mean.Power..W., 
       Time = Flight.Time.Contraction.Time,
       Peak.Power = Peak.Power..W.,
       Weight = Body.Weight..kg.)
forceData = forceData[-c(601),]

head(forceData)       
class(forceData)


out = lm(Height~Time*Mean.Power, data=forceData)
anova(out)

#plot(Height~Peak.Power, data=forceData)

#ggplot(forceData, aes(x=Time, y=Height)) + 
  #geom_point(color='#2980B9', size = 1) + 
  #geom_smooth(method=lm, color='#2C3E50')


newforceData = aggregate(forceData[,c(2,4:7)], list(forceData$Athlete), mean)

# Good Predictors

p1 = ggplot(newforceData, aes(x=Weight, y=Peak.Power))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Peak Power vs. Weight ") + 
  xlab("Weight (kg)") + 
  ylab("Peak Power (Watts)") + 
  theme_bw()

p2 = ggplot(newforceData, aes(x=Weight, y=Mean.Power))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Mean Power vs. Weight") + 
  xlab("Weight (kg)") + 
  ylab("Mean Power (Watts)") + 
  theme_bw()

p3 = ggplot(newforceData, aes(x=Peak.Power, y=Mean.Power))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Mean Power vs. Peak Power") + 
  xlab("Peak Power (Watts)") + 
  ylab("Mean Power (Watts)") + 
  theme_bw()

p4 = ggplot(newforceData, aes(x=Weight, y=Height))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Jump Height vs. Weight") + 
  xlab("Weight (kg)") + 
  ylab("Height (cm)") + 
  theme_bw()

multiplot(p1, p4, p2, p3, cols=2)



# 4 interesting graphs

p5 = ggplot(newforceData, aes(x=Time, y=Peak.Power))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Peak Power vs. Contraction Time") + 
  ylab("Peak Power (Watts)") + 
  xlab("Time") + 
  theme_bw()

p6 = ggplot(newforceData, aes(x=Time, y=Mean.Power))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Mean Power vs. Contraction Time") + 
  ylab("Mean Power (Watts)") + 
  xlab("Time") + 
  theme_bw()

outMPT=lm(Mean.Power~Time, data=newforceData)
summary(outMPT)
anova(outMPT)

outPPT=lm(Peak.Power~Time, data=newforceData)
anova(outPPT)


p7 = ggplot(newforceData, aes(x=Mean.Power, y=Height))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Jump Height vs. Mean Power") + 
  xlab("Mean Power (Watts)") + 
  ylab("Height (cm)") + 
  theme_bw()

p8 = ggplot(newforceData, aes(x=Peak.Power, y=Height))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Jump Height vs. Peak Power") + 
  xlab("Peak Power (Watts)") + 
  ylab("Jump Height") + 
  theme_bw()

outH=lm(Height~Mean.Power*Peak.Power, data=newforceData)
anova(outH)

multiplot(p5, p6, p7, p8, cols=2)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}








#  I don't think I want this one.

ggplot(newforceData, aes(x=Time, y=Height))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  ggtitle("Contraction Time vs. Jump Height") + 
  xlab("Time") + 
  ylab("Height (cm)") + 
  theme_bw()


ggplot(newforceData, aes(x=Peak.Power, y=Time))+ 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50') +
  ggtitle("Contraction Time vs. Peak Power ") + 
  xlab("Peak Power (Watts)") + 
  ylab("Time") + 
  theme_bw()
