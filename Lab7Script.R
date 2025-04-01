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

#function returns the kth uncentered or centered moment of beta(alpha, beta)
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
library(e1071)
sample = function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  beta.sample <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
  beta.sample = data.frame(x = beta.sample) #turn it into a data frame
  sample.summary = beta.sample |> #numerical summary for each distribution
    summarize(
      alpha = alpha, beta = beta, mean = mean(x), variance = var(x), skew = skewness(x), kurt = kurtosis(x)
    )
  return(sample.summary)
}

plot.funct = function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  beta.sample <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
  beta.sample = data.frame(x = beta.sample) #turn it into a data frame
  sample.plot = ggplot(data= beta.sample, aes(x=x))+                  # specify data
    geom_histogram(aes(y=after_stat(density)), 
                   #binwidth = 0.1
                   #bins=10
                   breaks=seq(0,1,0.10)) +                 
    geom_line(aes(x=x, y=dbeta(x, alpha, beta), color=paste("Beta(", alpha, ", ", beta, ")", sep = "")))+
    geom_hline(yintercept=0)+                                            # plot x axis
    theme_bw()+                                                          # change theme
    geom_density(color = "blue") +
    xlab("x")+                                                           # label x axis
    ylab("Density")+                                                   # label y axis
    labs(color = "")+
    ylim(0,3)
  return(sample.plot)
}
library(patchwork)
plot.funct(2, 5) + plot.funct(5, 5) + plot.funct(5, 2) + plot.funct(0.5, 0.5)

#alpha = 2, beta = 5
#empty data frame and bind the rows for each distribution
beta.sample.summary = data.frame()
beta.sample.summary = bind_rows(beta.sample.summary, sample(2, 5))
beta.sample.summary = bind_rows(beta.sample.summary, sample(5, 5))
beta.sample.summary = bind_rows(beta.sample.summary, sample(5, 2))
beta.sample.summary = bind_rows(beta.sample.summary, sample(0.5, 0.5))
#values are quite similar
view(beta.sample.summary)

#Task 4: Is sample size important?
#install.packages("cumstats")
library(cumstats)
library(patchwork)
data_2_5 = beta.data[1, ] #true population values for beta(2, 5) (first row of task one data frame)
alpha = 2
beta = 5
n = 500
set.seed(7272)
sample.dat = rbeta(n, alpha, beta) #sample data
#view(sample.dat)
#cumulative variables
cum.mean = cummean(sample.dat)
cum.var = cumvar(sample.dat)
cum.skew = cumskew(sample.dat)
cum.kurt = cumkurt(sample.dat)
#data frame for the cumulative numerical summary
cum.dat = tibble(
  data.points = 1:n,
  mean = cum.mean,
  variance = cum.var,
  skewness = cum.skew,
  kurtosis = cum.kurt
)
#view(data_2_5)
#plots for each cumulative variable
mean.plot = ggplot(cum.dat, aes(x=data.points, y = mean)) +
  geom_line() +
  geom_hline(yintercept = data_2_5$mean, linetype = "dashed", color = "blue") +
  labs(title = "Cumulative mean for a Beta(2,5) sample", x = "# of data points", y = "Cumulative Mean")

var.plot = ggplot(cum.dat, aes(x=data.points, y = variance)) +
  geom_line() +
  geom_hline(yintercept = data_2_5$variance, linetype = "dashed", color = "blue") +
  labs(title = "Cumulative variance for a Beta(2,5) sample", x = "# of data points", y = "Cumulative Variance")

skew.plot = ggplot(cum.dat, aes(x=data.points, y = skewness)) +
  geom_line() +
  geom_hline(yintercept = data_2_5$skew, linetype = "dashed", color = "blue") +
  labs(title = "Cumulative skewness for a Beta(2,5) sample", x = "# of data points", y = "Cumulative Skewness")

kurt.plot = ggplot(cum.dat, aes(x=data.points, y = kurtosis)) +
  geom_line() +
  #adjust kurtosis by 3 because cumstats package uses regular kurtosis
  geom_hline(yintercept = data_2_5$kurt + 3, linetype = "dashed", color = "blue") +
  labs(title = "Cumulative kurtosis for a Beta(2,5) sample", x = "# of data points", y = "Cumulative Kurtosis")
mean.plot + var.plot + skew.plot + kurt.plot

for(i in 2:50){
  set.seed(7272+i)
  new.dat = rbeta(n, alpha, beta) #new sample of data
  #new cumulative variables
  new.cum.mean = cummean(new.dat)
  new.cum.var = cumvar(new.dat)
  new.cum.skew = cumskew(new.dat)
  new.cum.kurt = cumkurt(new.dat)
  new.cum.dat = tibble(
    data.points = 1:n,
    mean = new.cum.mean,
    variance = new.cum.var,
    skewness = new.cum.skew,
    kurtosis = new.cum.kurt
  )
  #update plots with a line for the new data
  mean.plot = mean.plot+geom_line(data=new.cum.dat, aes(x=data.points, y = mean), color = i)
  var.plot = var.plot+geom_line(data=new.cum.dat, aes(x=data.points, y = variance), color = i)
  skew.plot = skew.plot+geom_line(data=new.cum.dat, aes(x=data.points, y = skewness), color = i)
  kurt.plot = kurt.plot+geom_line(data=new.cum.dat, aes(x=data.points, y = kurtosis), color = i)
}
mean.plot + var.plot + skew.plot + kurt.plot

#Task 5: How can we model the variation
sample.distribution = data.frame() #data frame that will have all 1000 samples
for(i in 1:1000){
  set.seed(7272+i)
  beta.sample <- rbeta(n,  # sample size
                       alpha,   # alpha parameter
                       beta)    # beta parameter
  beta.sample = data.frame(x = beta.sample) #turn it into a data frame
  new.sample.summary = beta.sample |> #numerical summary for each sample
    summarize(
      alpha = alpha, beta = beta, mean = mean(x), variance = var(x), skew = skewness(x), kurt = kurtosis(x)-3
    )
  #add the data for that sample
  sample.distribution = bind_rows(sample.distribution, new.sample.summary)
}
#view(sample.distribution)
#Distribution plots for each variable
mean.dist.plot = ggplot(data= sample.distribution, aes(x=mean))+                 
  geom_histogram(aes(y=after_stat(density)), 
                 #binwidth = 0.1
                 #bins=10
                 breaks=seq(0.1,0.4,0.005)) +                 
  geom_hline(yintercept=0)+                                            
  theme_bw()+                                                       
  geom_density(color = "blue") +
  xlab("Mean")+                                                        
  ylab("Density")+                                             
  labs(title = "Distribution of Mean")
var.dist.plot = ggplot(data= sample.distribution, aes(x=variance))+                 
  geom_histogram(aes(y=after_stat(density)), 
                 #binwidth = 0.1
                 #bins=10
                 breaks=seq(0,0.05,0.0025)) +                 
  geom_hline(yintercept=0)+                                            
  theme_bw()+                                                       
  geom_density(color = "blue") +
  xlab("Variance")+                                                        
  ylab("Density")+                                             
  labs(title = "Distribution of Variance")
skew.dist.plot = ggplot(data= sample.distribution, aes(x=skew))+                 
  geom_histogram(aes(y=after_stat(density)), 
                 #binwidth = 0.1
                 #bins=10
                 breaks=seq(0,1,0.1)) +                 
  geom_hline(yintercept=0)+                                            
  theme_bw()+                                                       
  geom_density(color = "blue") +
  xlab("Skewness")+                                                        
  ylab("Density")+                                             
  labs(title = "Distribution of Skewness")
kurt.dist.plot = ggplot(data= sample.distribution, aes(x=kurt))+                 
  geom_histogram(aes(y=after_stat(density)), 
                 #binwidth = 0.1
                 #bins=10
                 breaks=seq(-0.5,0.5,0.1)) +                 
  geom_hline(yintercept=0)+                                           
  theme_bw()+                                                       
  geom_density(color = "blue") +
  xlab("Excess Kurtosis")+                                                        
  ylab("Density")+                                             
  labs(title = "Distribution of Excess Kurtosis")
#I notice that these variables follow a relatively normal distribution, especially the 
#skewness and excess kurtosis. Obviously they aren't perfect because we are using samples,
#but you can see that their density lines have the bell curve shape
mean.dist.plot + var.dist.plot + skew.dist.plot + kurt.dist.plot

#Start of Lab 8
#Task six: Collect and Clean Data
dat2022 = read_csv("DeathData/2022Data.csv")
#view(dat2022)
dat2022 = dat2022 |>
  select("Country Name", "2022") |> #only need data for 2022
  mutate(`2022` = `2022` / 1000) |> #convert to rate
  rename(`2022 Death Rate` = "2022") |>#better column name
  filter(!is.na(`2022 Death Rate`)) #remove all empty data points
view(dat2022)
  
#Task seven: What are alpha and beta?
install.packages("nleqslv")
library(nleqslv)
MOM.beta = function(data, par){
  alpha = par[1]
  beta = par[2]
  EX = alpha/(alpha+beta) #first population moment
  m1 = mean(data) #first sample moment
  EX2 = (alpha+1)*alpha/((alpha+beta+1)*(alpha+beta)) #second population moment
  m2 = mean(data^2) #second sample moment
  
  return(c(EX-m1, EX2-m2)) #Vector of two first guesses
}

moms = nleqslv(x = c(2, 5), # guess
        fn = MOM.beta,
        data=dat2022$`2022 Death Rate`)


MLE.beta = function(par, data, neg=F){
  alpha = par[1]
  beta = par[2]
  loglik = sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)))
  
  return(ifelse(neg, -loglik, loglik))
}

mles = optim(par = c(2,5), #guess
      fn = MLE.beta,
      data=dat2022$`2022 Death Rate`,
      neg = T)

ggdat.beta = tibble(x = seq(0,0.025, length.out = 1000)) |> #data frame for mom and mle
  mutate(mom.pdf = dbeta(x=x, shape1 = moms$x[1], shape2 = moms$x[2]),
         mle.pdf = dbeta(x=x, shape1 = mles$par[1], shape2 = mles$par[2]))



histogram.plot = ggplot()+                  # specify data
  geom_histogram(data = dat2022, aes(x = `2022 Death Rate`, y=after_stat(density)),
                 breaks=seq(0,0.025,0.003)) +
  geom_line(data = ggdat.beta, aes(x=x, y=mom.pdf, color="MOM"))+ #MOM line
  geom_line(data = ggdat.beta, aes(x=x, y=mle.pdf, color="MLE"))+ #MLE line
  geom_hline(yintercept=0)+                                          # plot x axis
  theme_bw()+                                                        # change theme
  #geom_density(color = "blue") +
  xlab("2022 Death Rate")+                                           # label x axis
  ylab("Density")+                                                   # label y axis
  labs(color = "")
histogram.plot

#Task eight: Which estimators should we use?

alpha = 8
beta = 950
n = 266
estimates.data = data.frame()
for(i in 1:1000){
  set.seed(7272+i)
  new.data = rbeta(n, alpha, beta) #new sample
  mom = nleqslv(x=c(2,5), fn = MOM.beta, data = new.data) #new method of moments estimate
  mle = optim(par=c(2,5), fn = MLE.beta, data = new.data, neg = T) #new mle
  estimates.data = bind_rows(estimates.data, data.frame(mom$x[1], mom$x[2], mle$par[1], mle$par[2])) #add new values to data frame
}
estimates.data = estimates.data |>
  rename(mom.alpha = "mom.x.1.",
         mom.beta = "mom.x.2.",
         mle.alpha = "mle.par.1.",
         mle.beta = "mle.par.2.")

mom.alpha.plot = ggplot(data = estimates.data, aes(x = mom.alpha)) +
  geom_density(fill = "red") +
  ggtitle("MOM Alpha")

mom.beta.plot = ggplot(data = estimates.data, aes(x = mom.beta)) +
  geom_density(fill = "blue") +
  ggtitle("MOM Beta")

mle.alpha.plot = ggplot(data = estimates.data, aes(x = mle.alpha)) +
  geom_density(fill = "green") +
  ggtitle("MLE Alpha")

mle.beta.plot = ggplot(data = estimates.data, aes(x = mle.beta)) +
  geom_density(fill = "black") +
  ggtitle("MLE Beta")

(mom.alpha.plot | mom.beta.plot) / ((mle.alpha.plot | mle.beta.plot))

summary = function(estimate, exact) {
  bias = mean(estimate) - exact
  precision = 1/var(estimate)
  MSE = var(estimate) + bias^2
  return(c(bias, precision, MSE))
}

alpha.mom = summary(estimates.data$mom.alpha, alpha)
beta.mom = summary(estimates.data$mom.beta, beta)
alpha.mle = summary(estimates.data$mle.alpha, alpha)
beta.mle = summary(estimates.data$mle.beta, beta)

table = data.frame()
table = rbind( #table containing the 4 estimates for each metric
  alpha.mom, beta.mom, alpha.mle, beta.mle
) |> as.data.frame()
summary.table = table |>
  rename(Bias = "V1",
         Precision = "V2",
         MSE = "V3")

view(summary.table)

