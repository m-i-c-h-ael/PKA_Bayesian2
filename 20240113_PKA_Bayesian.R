library('rjags')

set.seed(10)

# Estimate mean and sd of normal distribution
obs_data = rnorm(n = 20, mean = 5, sd = 3)

modStr = 'model{
  for(i in 1:length(Y)){
    Y[i] ~ dnorm(mu, 1/sd^2)
  }
  
  #priors
  mu ~ dnorm(0, 1/10^2)
  sd ~ dexp(4)
}'

jagMod = jags.model(file = textConnection(modStr), data = list(Y = obs_data))
cS = coda.samples(model = jagMod, variable.names = c('mu', 'sd'), n.iter = 10000)

DF = data.frame(cS[[1]])
hist(DF$mu)
hist(DF$sd)

par(mfrow = c(2, 1))
# prior predictive
hist( rnorm(n = 1000, mean = rnorm(n = 1000, mean = 0, sd = 10), sd = rexp(n = 1000, rate = 4)), xlim = c(-20, 20), col= 'skyblue')
# posterior predictive
postPred = rnorm(n = length(DF$mu), mean = DF$mu, sd = DF$sd)
hist(postPred, xlim = c(-20, 20), col= 'skyblue')
points(obs_data, rep(0, length(obs_data)), col = 'red')

# ----------------------------------------------------------------------------
# Data are time-dependent; model parameters for line, but no ODE
a_sim = 0.5
data2 = as.list(tibble(times = 0:10, Y = a_sim * times + rnorm(length(times), mean = 0, sd = 0.1)))

modStr2 = 'model{
  for(i in 1:length(Y)){
    Y[i] ~ dnorm(a * times[i], 1/sd^2)
  }

  #priors
  a ~ dnorm(0, 1/1^2)
  sd ~ dexp(1)
}'

jagMod2 = jags.model(file = textConnection(modStr2), data = data2)
cS2 = coda.samples(model = jagMod2, variable.names = c('a', 'sd'), n.iter = 10000)
DF2 = data.frame(cS2[[1]])
hist(DF2$a)


# ----------------------------------------------------------------------------
# It seems like rjags cannot handle ODEs, but rstan can - bad luck for me: better get used to STAN
 # https://mc-stan.org/docs/stan-users-guide/ode-solver.html

library('rstan')
library('tidyverse')

 # STAN for getting posterior on mu and sigma of normal distribution around
  # linear model mu = a * t; Y[i] ~ normal(mu[i], sigma)
modSTAN2 = "
  data{
    int<lower=1> N;  // data points
    real t[N];
    real Y[N];
  }
  parameters{
    real a;
    real<lower=0> sigma;
  }
  transformed parameters{
    real mu[N];
    for(i in 1:N){
      mu[i] = a * t[i];
    }
  }
  model{
    // Priors
    a ~ normal(0, 1);
    sigma ~ exponential(1);  // Prior
    
    // Likelihoods
    for(i in 1:N){
      
      Y[i] ~ normal(mu[i], sigma);
    }
  }
"

# modSTAN2s = stan_model(model_code = modSTAN2) not used

data2 = tibble(times = 0:10, Y = 0.5 * times + rnorm(length(times), mean = 0, sd = 0.1))
data_list = list(N = dim(data2)[1], t = data2$times, Y = data2$Y)

stanOUT2 = stan(model_code = modSTAN2, data = data_list)
stanOUT2_arr = as.array(stanOUT2)

plot(density(stanOUT2_arr[,1,'a']))
abline(v = a_sim, col = 'blue', lty = 2)


# ----------------------------------------------------------------------------
# Example: Harmonic oscillator with STAN
 # https://mc-stan.org/docs/stan-users-guide/example-simple-harmonic-oscillator.html
# d_y1/dt = y2
# d_y2/dt = -y1 - theta*y2

 # model treats theta and y0 as parameters
 # assuming normal error distribution
 # priors: sigma ~ normal()
functions {
  vector myODE(real t,
               vector y,
               real theta){
    vector[2] dy_dt;
    dy_dt[1] = y[2];
    dy_dt[2] = -y[1] - theta * y[2];
    return dy_dt;
  }
}
data {
  int<lower=1> T; // number of TPs
  real t0;
  vector[T] real ts;
  array[T] vector[2] y;
}
parameters{
  real theta;
  vector[2] real y0;
  vector<lower=0>[2] sigma;
}
model{
  array[T] vector[2] mu = ode_rk45(myODE, y0, t0, ts, theta);
  sigma ~ normal(0, 2.5); //prior
  theta ~ std_normal(); //prior
  y0 ~ std_normal(); //prior
  
  for(t in 1:T){
    y[t] ~ normal(mu[t], sigma)
  }
}