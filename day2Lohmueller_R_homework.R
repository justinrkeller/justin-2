hap<-read.table('hapmaps.txt',header=TRUE)#establishes our data set in golbal environment 
compute_chisquare=function(x){ #creating a function to be computed by "compute_chisquare"
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))#minor allele freq
  cnt0=sum(x==0,na.rm=TRUE)#counts of the types of phenotypes 
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n #hardyWeinberg stuff 
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}
snps=as.matrix(hap) #makes data file a matrix
chisqs=apply(snps,1,compute_chisquare)#compute the snps
pvals=pchisq(chisqs,1,lower.tail=FALSE)
### Above is answer for part a

signifthres<-0.05
sum(pvals<signifthres)
length(pvals)
sum(pvals<signifthres)/(length(pvals))
#proportion of significant pvalues is 0.04509218

signifthres<-0.01
sum(pvals<signifthres)
length(pvals)
sum(pvals<signifthres)/(length(pvals))
#proportion of significant pvalues is 0.0121425

signifthres<-0.001
sum(pvals<signifthres)
length(pvals)
sum(pvals<signifthres)/(length(pvals))
#proportion of significant pvalues is 0.00124564

### Above is answer for part b

num_pval<-length(pvals)
#4014 SNPs were tested for HardyWeinberg eq

### Above is answer for part c

exp_pvals<-seq(1,num_pval, by=1)/num_pval#expected pvalues under equlibrium 

### Above is answer for part d

sort_pvals<-sort(pvals,decreasing=FALSE)

### Above is answer for part e

log_sort_pvals<-(-log10(sort_pvals))
log_exp_pvals<-(-log10(exp_pvals))

### Above is answer for part f

plot(log_exp_pvals,log_sort_pvals, xlab="-log10(Expected P-value)",ylab="-log10(Sorted P-value)",pch=19)
### Above is the answer for g

abline(0,1,col=2, lty=2, lwd=2)

### Above is the asnwer for h

#################################
#QUESTION 2

zz<-read.table("phenotype.txt",header=TRUE)
### Above is the answer for a

quantile(zz$glucose_mmolperL,0.25)#use $ to extract out the column you want
# Just to check
sort(zz$glucose_mmolperL)
#the phenotype 4.768756 has 25% of the individuals below this value 

### Above is answer for b

quantile(zz$glucose_mmolperL,0.75)

#the phenotype 7.354975 has 75% of the individuals above this value 

### Above is the answer for c

hist(zz$glucose_mmolperL,main="Histogram",xlab="Glucose(mmol/L)")

lowtail=quantile(zz$glucose_mmolperL,0.25)

hightail=quantile(zz$glucose_mmolperL,0.75)
abline(v=lowtail,col=3,lwd=2)
abline(v=hightail,col=2,lwd=2)

### Above is the answer for d