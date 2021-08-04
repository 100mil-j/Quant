
m <- read.csv("https://courses.edx.org/assets/courseware/v1/615d261221493ee432c46110dacf842a/asset-v1:MITx+15.455x+3T2020+type@asset+block/PS3-data.csv")
a <- mean(m[,5])
a <- a*252
a

b <- sd(m[,5])
b <- b*sqrt(252)
b

plot(m[,5],type = "l")
acf(m[,5])

n <- runif(252*10000, min=-1, max=1)
n <- matrix(n,ncol=10000,nrow=252)

n[n<0] <- -1
n[n>0] <- 1
n[n==0] <- -1

z <- n

rt = 0.06/252+0.4/sqrt(252)*z

exprt = exp(rt)

prices <- matrix(,nrow=252,ncol=10000)
prices<- rbind(100,prices)

for (t in 2:253){
  for (j in 1:10000){
  prices[t,j]=prices[t-1,j]*exp(rt[t-1,j])
  }
}

prices <- format(prices, scientific=FALSE)

meanterminal = mean(as.numeric(as.character(prices[253,])))

sdterminal = sd(prices[253,])
sdterminal = format(sdterminal, scientific = FALSE)
sdterminal

originreversalcounts <- matrix(,nrow=10000)
for (k in 1:10000){
  originreversalcounts[k,] <- sum(prices[2:253,k] <= 100)
}

origincountsupper <- 0

for (k in 2:252){
  for (j in 1:10000){
    if (prices[k,j]>100){
      if (prices[k+1,j]<=100){
        origincountsupper <- origincountsupper + 1
      }
    }
  }
}

origincountslower <- 0

for (k in 2:252){
  for (j in 1:10000){
    if (prices[k,j]<100){
      if (prices[k+1,j]>=100){
        origincountslower <- origincountslower + 1
      }
    }
  }
}

origincounts = origincountsupper + origincountslower
origincounts

k = 100
callvalue <- matrix(,nrow =253,ncol=10000)
 
for (z in 1:253){
  for (y in 1:10000){
    if (prices[z,y] > k){
      callvalue[z,y] <- as.numeric(prices[z,y]) - 100
    }else callvalue[z,y] <- 0
  }
}

empericalcallpayoff <- mean(callvalue[253,])

putvalue <- matrix(,nrow =253,ncol=10000)

for (z in 1:253){
  for (y in 1:10000){
    if (prices[z,y] < k){
      putvalue[z,y] <- k - as.numeric(prices[z,y]) 
    }else putvalue[z,y] <- 0
  }
}

empericalputpayoff <- mean(putvalue[253,])












