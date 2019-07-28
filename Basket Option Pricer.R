#The copyright of this dissertation rests with the author and no quotation from it or 
#information derived from it may be published without prior written consent of the author.
#Dissertation in FM4T8E
#London School of Economics and Political Sciences
#Candidate Number: 14346"
################################################################

# Functions for Basket options for two assets using moment matching technique

################################################################
BasketOptionVol<-function(pc,ATM,T,NCCY1,NCCY2,F1,F2,S1,S2,K,sigma1,sigma2,rdom,rCCY1,rCCY2,corr){
  #pc: call=1 put=-1
  #ATM=1 for at the money option
  #NCC1 notional for first pair NCC2 notional for second pair
  #K=strike
  #S1,S2 spot rates today
  #sigma is local volatility
  #rdom, rCCY1,rCCY2 are the interest rates for domestic and foreign currencies
  #corr is the correlation of the currencies
  w1<-NCCY1/(NCCY1+NCCY2)
  w2<-NCCY2/(NCCY1+NCCY2)
  sigma<-sqrt((1/T) * log((S1^2*exp((sigma1^2)*T)+S2^2*exp((sigma2^2)*T)+2*S1*S2*
                             exp(corr*sigma1*sigma2*T))/(S1+S2)^2))
  return(round(sigma,5))
}


BasketOptionPricer<-function(pc,ATM,T,NCCY1,NCCY2,F1,F2,S1,S2,K,sigma1,sigma2,rdom,rCCY1,rCCY2,corr){
  #pc: call=1 put=-1
  #ATM=1 for at the money option
  #NCC1 notional for first pair NCC2 notional for second pair
  #K=strike
  #S1,S2 spot rates today
  #sigma is local volatility
  #rdom, rCCY1,rCCY2 are the interest rates for domestic and foreign currencies
  #corr is the correlation of the currencies
  w1<-NCCY1/(NCCY1+NCCY2)
  w2<-NCCY2/(NCCY1+NCCY2)
  mu<- (1/T) * log((w1*S1*exp((rdom-rCCY1)*T)+w2*S2*exp((rdom-rCCY2)*T))/(w1*S1+w2*S2))
  sigma<-BasketOptionVol(pc,ATM,T,NCCY1,NCCY2,F1,F2,S1,S2,K,sigma1,sigma2,rdom,rCCY1,rCCY2,corr)
  SBasket<-(w1*S1+w2*S2)
  FBasket<-SBasket*exp(mu*T)
  if (ATM==1){
    K<-w1*F1+w2*F2
    d1<-(log(FBasket/K)+0.5*sigma^2*T)/(sigma*sqrt(T))
    d2<-d1-sigma*sqrt(T)
  }else{
    d1<-(log(FBasket/K)+0.5*sigma^2*T)/(sigma*sqrt(T))
    d2<-d1-sigma*sqrt(T)
  }
  V<-exp(-rdom*T)*(pc*FBasket*pnorm(pc*d1)-pc*K*pnorm(pc*d2))
  return(round(V,5))
}


################################################################

# Pricing for different dates

################################################################

# Importing data entries required for pricing from excel in table FX
library(readxl)
install.packages("xlsx")
library("xlsx")
filepath<-"~/Google Drive/FINANCE LSE/Dissertation/DATA/4.Case study data.xlsm"
FX <- read_xlsx(filepath, sheet = "Basket Pricer Data ",col_names=T)


# Running the loop to price Basket call options for 12 consecutive months
BasketOptionPrice<-c()
BasketVol<-c()
for (i in 1:12){
  BasketVol[i]<-BasketOptionVol(as.numeric(FX[i,2]),as.numeric(FX[i,3]),as.numeric(FX[i,4]),
                                           as.numeric(FX[i,5]),as.numeric(FX[i,6]),as.numeric(FX[i,7]),
                                           as.numeric(FX[i,8]),as.numeric(FX[i,9]),as.numeric(FX[i,10]),
                                           as.numeric(FX[i,11]),as.numeric(FX[i,12]),as.numeric(FX[i,13]),
                                           as.numeric(FX[i,14]),as.numeric(FX[i,15]),
                                           as.numeric(FX[i,16]),as.numeric(FX[i,17]))
  BasketOptionPrice[i]<-BasketOptionPricer(as.numeric(FX[i,2]),as.numeric(FX[i,3]),as.numeric(FX[i,4]),
                                           as.numeric(FX[i,5]),as.numeric(FX[i,6]),as.numeric(FX[i,7]),
                                           as.numeric(FX[i,8]),as.numeric(FX[i,9]),as.numeric(FX[i,10]),
                                           as.numeric(FX[i,11]),as.numeric(FX[i,12]),as.numeric(FX[i,13]),
                                           as.numeric(FX[i,14]),as.numeric(FX[i,15]),
                                           as.numeric(FX[i,16]),as.numeric(FX[i,17]))
}


#Put everything into a data table and export to excel
BasketOptions<-data.frame(FX[,1],BasketVol,BasketOptionPrice)
BasketOptions
write.xlsx(BasketOptions, "~/Google Drive/FINANCE LSE/Dissertation/DATA/4.Case study data.xlsm", 
           sheetName="Basket Option Prices", append=T)

