Q1=function(y)
{
  set.seed(20190322)
  x=rnorm(y,0,1)
  hist(x)
  plot(density(x))
  par(mfrow=c(2,1))
  L1=qnorm(0.025,mean(x),sd(x))
  L2=qnorm(0.975,mean(x),sd(x))
  NUM=sum(L1<x & x<L2)
  p=sum(L1<x & x<L2)/y
  OUT=list("信賴下界"=L1,"信賴上界"=L2, "個數"=NUM,"CP"=p)
  return(OUT)
}
Q1(100)
Q1(1000)
Q1(10000)
Q1(100000000)
