#Lab 7
#Task one: Describe the Population Distribution
library(tidyverse)

#function plots the beta distribution for alpha, beta
beta.plot = function(alpha, beta){
  beta.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
    # compute the beta PDF
    mutate(beta.pdf = dbeta(x, alpha, beta))         
  distribution = ggplot(data= beta.dat)+                                              # specify data
    geom_line(aes(x=x, y=beta.pdf, color=paste("Beta(", alpha, ", ", beta, ")", sep = ""))) +                 # plot beta dist
    geom_hline(yintercept=0)+                                            # plot x axis
    theme_bw()+                                                          # change theme
    xlab("x")+                                                           # label x axis
    ylab("Density")+                                                   # label y axis
    labs(color = "")
  return(distribution)
}

beta_2_5 = beta.plot(2,5)
beta_5_5 = beta.plot(5,5)
beta_5_2 = beta.plot(5,2)
beta_0.5_0.5 = beta.plot(0.5,0.5)

summary = function(alpha, beta){ #functions gives a summary for the beta distribution
  mean = alpha/(alpha+beta)
  variance = (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta))
  kurt = (6*((alpha-beta)^2*(alpha+beta+1)-alpha*beta*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  #data frame column for that distributions summary
  dat.summary = data.frame(alpha = alpha, beta = beta, mean = mean, variance = variance, skew = skew, kurt = kurt)
  return(dat.summary)
}
#data frame for all four distributions
beta.data = data.frame()
#binds rows with each distributions summary
beta.data = bind_rows(beta.data, summary(2, 5))
beta.data = bind_rows(beta.data, summary(5, 5))
beta.data = bind_rows(beta.data, summary(5, 2))
beta.data = bind_rows(beta.data, summary(0.5, 0.5))

view(beta.data)

#Task two

#function retuns the kth uncentered or centered moment of beta(alpha, beta)
beta.moment = function(alpha, beta, k, centered) {
  if(centered == F){
    integrand = function(x) {(x^k)*dbeta(x, alpha, beta)}
    result = integrate(integrand, lower = 0, upper = 1)
  }
  else{
    mu = alpha/(alpha+beta) #mean
    integrand = function(x) {((x-mu)^k)*dbeta(x, alpha, beta)}
    result = integrate(integrand, lower = 0, upper = 1)
  }
  return(result)
}

#Compare all values for beta(2, 5)

#mean
mean = beta.moment(2, 5, 1, F)
mean

#variance
variance = beta.moment(2, 5, 2, T)
variance

#skew
x = as.numeric(beta.moment(2, 5, 3, T)[1])
y = as.numeric(beta.moment(2, 5, 2, T)[1])
skew = x/(y^(3/2))
skew

#kurt
a = as.numeric(beta.moment(2, 5, 4, T)[1])
kurt = (a/(y^2)) - 3
kurt

#Task three: Do Data Summaries Help?

