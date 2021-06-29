#-*- mode: Fundamental; -*-

#read in data
xm<-matrix(scan("freqhist",0),ncol=2,byrow=T)
xr<-xm[,1]
xnr<-xm[,2]
xn<-sum(xr*xnr)

# make averaging transform
xnrz<-nrzest(xr, xnr)

# get Linear Good-Turing estimate
xf<-lsfit(log(xr), log(xnrz))
xcoef<-xf$coef
xrst<-rstest(xr,xcoef)
xrstrel<-xrst/xr

# get Turing estimate
xrtry<-xr == c(xr[-1] - 1, 0)
xrstarel<-rep(0, length(xr))
xrstarel[xrtry]<-(xr[xrtry] + 1) / xr[xrtry] * c(xnr[-1], 0) [xrtry] / xnr[xrtry]

# make switch from Turing to LGT estimates
tursd<-rep(1, length(xr))
for (i in 1:length(xr)) if (xrtry[i])
    tursd[i]<-(i+1) / xnr[i] * sqrt(xnr[i+1] * (1 + xnr[i+1] / xnr[i]))
