##
## This program analyzes the data from the Moderna Phase 3 Clinincal Trial of mRNA-1273, the COVE trial.
##
## The numbers of volunteers who were PCR+ on day 29 are compared.
##
##

Y<-c(rep(0,14134-14),rep(1,14),rep(0,14073-38),rep(1,38))
Z<-c(rep(1,14134),rep(0,14073))
VEPI<-1-(sum(Y*Z)/sum(Z))/(sum(Y*(1-Z))/sum(1-Z))
I<-seq(1:length(Y))
VEPIB<-NULL
for(i in 1:10000){
  Ib<-sample(I,replace=TRUE)
  Yb<-Y[Ib]
  Zb<-Z[Ib]
  VEPIB[i]<-1-(sum(Yb*Zb)/sum(Zb))/(sum(Yb*(1-Zb))/sum(1-Zb))
}
VEPIBS<-sort(VEPIB)
CI<-c(VEPIBS[.025*10000],VEPIBS[.975*10000])
CI
