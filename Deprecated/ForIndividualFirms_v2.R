asdf = fread("C:/Users/Nathan/Downloads/PerturbationMethods/FiddlingwithLambdaZ.csv")
asdf[,rownum:=.I]
wut = lm(log(V1)~rownum,data=asdf[1:50])
summary(wut)
predict(wut)
asdf[,mordecaistheory := 0.96^rownum]
# summary(lm(log(mordecaistheory)~rownum,data=asdf[1:50]))
set.seed(9)
N=40000
testdata = rnorm(N)
asdf = rnorm(N)
asdf[1]=1
x = foreach(i=2:N)%do%{
  asdf[i]=asdf[i-1]*0.5+testdata[i]
  NULL
}
sums = 1:(N/2)
x = foreach(i=1:(N/2))%do%{
  sums[i]=asdf[2*i-1]+asdf[2*i]
  NULL
}
summary(lm(asdf[1:(N-1)]~asdf[2:N]))
summary(lm(asdf[seq(1,N,2)]~asdf[seq(2,N,2)]))
summary(lm(sums[1:(N/2-1)]~sums[2:(N/2)]))