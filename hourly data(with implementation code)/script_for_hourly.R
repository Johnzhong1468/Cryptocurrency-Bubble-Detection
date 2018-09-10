require(yuima)
setwd('/Users/0429z/OneDrive - Cornell University/class/ORIE5640_Stats for FE/project/hourly/hourly')

filenames <- c()

# for ETH, look at second and third period as before and during, after evaluated in bubble.Rmd
cryptonames <- c('BTC','ETH','BCH','XRP','LTC') #'ETH','XRP',
# for XRP, use the commentted code in line 22-24, kine 27 
# cryptonames <- c('XRP')
for (name in cryptonames){
  filenames<- c(filenames, paste0(name,"_hourly_11012017_02012018.csv"))
}

outputfile <- file('output_hourly10','a')
i = 1
for (filename in filenames){
  X <- read.csv(filename,header=TRUE)
  prices <- X$Close
  l = length(prices)
  l1 = l/3
  l2 = 2*l/3
  # l1 = l/3
  # l2 = (l1+l)/3
  # l3 = (l1+l)/3*2
  price_list <- list(prices[0:l1],prices[l1:l2],prices[l2:l])
  
  # price_list <- list(prices[l1:l2],prices[l2:l3],prices[l3:l])
  write('\n',outputfile)
  write(toString(paste0("Estimates for ",cryptonames[i])), outputfile)

  i <- i+ 1
  j = 1
  for (price in price_list){
    write('\n',outputfile)
    write(toString(paste0("Period ",j)), outputfile)
    
    j <- j+ 1
    
    mod <- setModel(drift="alpha+beta*x", diffusion=matrix("sigma*x^gamma",1,1))
    yuima <- setYuima(data=setData(price), model=mod)
    lambda0 <- list(alpha=10, beta =10, sigma =10, gamma =10)
    start <- list(alpha=1, beta =-.1, sigma =.1, gamma =1)
    low <- list(alpha=-5, beta =-5, sigma =-5, gamma =-5)
    upp <- list(alpha=8, beta =8, sigma =8, gamma =8)

    lasso10 <- lasso(yuima, lambda0, start=start, lower=low, upper=upp, delta=0.5, method="L-BFGS-B")
    # qmle only
    round(lasso10$mle, 3)
    # gamma value and its std
    write(paste(toString(lasso10$mle),toString(lasso10$sd.mle)), outputfile,append=TRUE)
    # magnitude of |alpha|+|beta|+|gamma|+|sigma|
    # u <- abs(lasso10$mle[1])+abs(lasso10$mle[2])+abs(lasso10$mle[3])+abs(lasso10$mle[4])
    # write(u, outputfile,append=TRUE)
    
    # qmle + mild lasso 
    round(lasso10$lasso, 3)
    write(paste(toString(lasso10$lasso),toString(lasso10$sd.lasso)), outputfile,append=TRUE)
    # u <- abs(lasso10$lasso[1])+abs(lasso10$lasso[2])+abs(lasso10$lasso[3])+abs(lasso10$lasso[4])
    # write(u, outputfile,append=TRUE)
    
    # qmle + strong lasso
    lasso20 <- lasso(yuima, lambda0, start=start, lower=low, upper=upp, delta=1, method="L-BFGS-B")
    round(lasso20$lasso, 3)
    write(paste(toString(lasso20$lasso),toString(lasso20$sd.lasso)), outputfile,append=TRUE)
    # u <- abs(lasso20$lasso[1])+abs(lasso20$lasso[2])+abs(lasso20$lasso[3])+abs(lasso20$lasso[4])
    # write(u, outputfile,append=TRUE)
    
    
  }
}

close(outputfile)

outputfile <- file('output_hourly10','a')
i = 1


filename = filenames[2]
X <- read.csv(filename,header=TRUE)
prices <- X$Close
l = length(prices)
l1 = l/3
l2 = 2*l/3
# l1 = l/3
# l2 = (l1+l)/3
# l3 = (l1+l)/3*2
price_list <- list(prices[0:l1],prices[l1:l2],prices[l2:l])

# price_list <- list(prices[l1:l2],prices[l2:l3],prices[l3:l])


i <- i+ 1
j = 1

for (price in price_list){
  
  j <- j+ 1
  
  mod <- setModel(drift="alpha+beta*x", diffusion=matrix("sigma*x^gamma",1,1))
  yuima <- setYuima(data=setData(price), model=mod)
  lambda0 <- list(alpha=10, beta =10, sigma =10, gamma =10)
  start <- list(alpha=1, beta =-.1, sigma =.1, gamma =1)
  low <- list(alpha=-5, beta =-5, sigma =-5, gamma =-5)
  upp <- list(alpha=8, beta =8, sigma =8, gamma =8)
  
  lasso10 <- lasso(yuima, lambda0, start=start, lower=low, upper=upp, delta=0.5, method="L-BFGS-B")
  # qmle only
  round(lasso10$mle, 3)
  residual = na.omit(diff(price))-(lasso10$mle[3]+lasso10$mle[4]*price[1:l1-1])/(365*24*60)/(lasso10$mle[1]*price[1:l1-1])^(lasso10$mle[2])
  #acf(residual)
  qqnorm(residual)
  qqline(residual)
  # gamma value and its std

  # magnitude of |alpha|+|beta|+|gamma|+|sigma|
  # u <- abs(lasso10$mle[1])+abs(lasso10$mle[2])+abs(lasso10$mle[3])+abs(lasso10$mle[4])
  # write(u, outputfile,append=TRUE)
  
  # qmle + mild lasso 

  # u <- abs(lasso10$lasso[1])+abs(lasso10$lasso[2])+abs(lasso10$lasso[3])+abs(lasso10$lasso[4])
  # write(u, outputfile,append=TRUE)
  #residual1 = (na.omit(diff(price))-(lasso10$lasso[3]+lasso10$lasso[4]*price[1:l1-1])/(365*24*60))/(lasso10$lasso[1]*price[1:l1-1])^(lasso10$lasso[2])
  #acf(residual1)
  # qmle + strong lasso
  lasso20 <- lasso(yuima, lambda0, start=start, lower=low, upper=upp, delta=1, method="L-BFGS-B")
  #round(lasso20$lasso, 3)
  residual = (na.omit(diff(price))-(lasso20$mle[3]+lasso20$mle[4]*price[1:l1-1])/(365*24*60))/(lasso20$mle[1]*price[1:l1-1])^(lasso20$mle[2])
  #acf(residual2)
  #qqnorm(residual)
  #qqline(residual)
  # u <- abs(lasso20$lasso[1])+abs(lasso20$lasso[2])+abs(lasso20$lasso[3])+abs(lasso20$lasso[4])
  # write(u, outputfile,append=TRUE)
}
