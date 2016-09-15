x<-1:9
for(ii in x){if(ii<9){cat("\n")}else{cat("*\n")}}  
#this is the answer for exercise 1

for(ii in 1:19){if(ii%%2){cat("*")}else{cat("&")}}
#this is the answer for exercise 2

dogs <- 10;
for (i in 1:5){
  dogs <- dogs + 1; 
###
#Initial value of dogs=10
#fist=11
#second=12
#third=13
#fourth=14
#End=15
###
meatloaf <- 0; 
for (i in 5:9){
  meatloaf <- meatloaf - i + 1;
  cat(meatloaf) 
###
#Initial value of meatloaf=0
#first=0-5+1+ -4
#second=-4-6+1=-9
#third=-9-7+1=-15
#fourth=-15-8+1=-22
#End=-22-9+1=-30
###
bubbles <- 12;
for (i in -1:-4){
  bubbles <- i;
###
#Initial value of bubbles=12
#first=-1
#second=-2
#third=-3
#fourth=-4
###
#Answer for exercise 3

years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  }
  if(years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)
  }
}
#Answer for exercise 4

bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5)
compounded<-rep(0,6)
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }
#Answer for exercise 5

bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

for (j in 1:5) {
  for (i in 1:length(bankAccounts)){
    bankAccounts[i]<-(bankAccounts[i]-house[i]-food[i]-fun[i]+income[i])*(1+interestRate)
    }
}
#Answer for exercise 6

Years<-c(2015,2016,2017,2018,2019,2020)
for (j in 1:length(Years)) {
  if(Years[j]%%2==1){
    bankAccounts[1]=bankAccounts[1]+5000
    bankAccounts[3]=bankAccounts[3]+5000
  }
  
  for (i in 1:length(bankAccounts)){
    bankAccounts[i]<-(bankAccounts[i]-house[i]-food[i]-fun[i]+income[i])*(1+interestRate)
  }
}
#Answer for exercise 7

xsum<-0
x<-1
while(x<=17){
  xsum=xsum+x
  x=x+1
  }
  
#Answer for exercise 8


