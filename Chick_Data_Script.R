###Plotting and statistics exercise, EEB R bootcamp###
ChickWeight=ChickWeight
data<-ChickWeight
data<-within(data,{
  Chick<-factor(Chick)
  Diet<-factor(Diet)
  Time<-factor(Time)
})

### Question 1

data.t0<-data[data$Time==0,]#this is filtering out all chicks at T=0

boxplot(weight~Diet, data=data.t0,xlab="Diet Type", ylab="Weight", main="Weights of chicks at t=0")
anova1=aov(weight~Diet, data=data.t0) # set up the statistical test
summary(anova1) # look at the results of the statistical test
TukeyHSD(anova1) # examine each pair using a Tukey test
#summary(anova1) # look at the results of the statistical test
#Df          Sum Sq Mean Sq F    value  Pr(>F)
#Diet         3   4.32   1.440   1.132  0.346
#Residuals   46  58.50   1.272   

#There is no significant difference in weights between diet types for chicks at T=0

### Question 2

data.t21<-data[data$Time==21,]#this is filtering out all chicks at T=0

boxplot(weight~Diet, data=data.t21,xlab="Diet Type", ylab="Weight", main="Weights of chicks at T=21")
anova1=aov(weight~Diet, data=data.t21) # set up the statistical test
summary(anova1) # look at the results of the statistical test
summary(anova1) # look at the results of the statistical test
#Df Sum Sq Mean Sq F value  Pr(>F)   
#Diet         3  57164   19055   4.655 0.00686 **
  #Residuals   41 167839    4094                   
TukeyHSD(anova1) # examine
#$Diet
#diff        lwr       upr     p adj
#2-1  36.95000  -32.11064 106.01064 0.4868095
#3-1  92.55000   23.48936 161.61064 0.0046959
#4-1  60.80556  -10.57710 132.18821 0.1192661
#3-2  55.60000  -21.01591 132.21591 0.2263918
#4-2  23.85556  -54.85981 102.57092 0.8486781
#4-3 -31.74444 -110.45981  46.97092 0.7036249

#There is a significant effect of diet type on weight of chicks at T=21, specifically the difference lies between diet types 1 and 3


###Question 3

summary(aov(weight~Diet*Time+Error(Chick),data=data))
#Error: Chick
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Diet       3 155863   51954   6.795 0.000799 ***
#  Time       5  60748   12150   1.589 0.184743    
#Residuals 41 313495    7646 
#yes there is an effect of diet on chick p=0.000799

###Question 4

plot(as.numeric(data$Time),data$weight, pch=19)
cols=sub('1',"red",data$Diet)
cols=sub('2',"blue",cols)
cols=sub('3',"green",cols)
cols=sub('4',"yellow",cols)


plot(as.numeric(data$Time),data$weight, xlab='Time',ylab='Weight',main="Chick Weights over Time", # same as above...
     col=cols, # add color
     pch=16,las=1,cex.axis=1.5,cex.lab=1.5) # same as above
# add a legend:
legend("topleft",title="Diet", legend=unique(data$Diet), text.col=c("red", "blue", "green","yellow"),pch=16, col=c("red", "blue", "green","yellow"))


###Question 5

plot(NA,ylim=c(0,375),xlim=c(0,21),xlab="Time", ylab="Weight", main="Individual Chick Growth Over Time")
colors=rainbow(50)
for(ID in 1:50){
  use.rows=which(data$Chick==ID)
  lines(x=data$Time[use.rows],
        y=data$weight[use.rows], col=colors[ID])
}

