## ---
## Regressao Logistica sob Inferencia Bayesiana
## ---

## -----
## Carregamento de pacotes
## -----

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(carData, rstanarm, brms)

data(Wells)

#(reg_formula <- formula(paste("outcome ~", paste(names(diabetes)[1:(p-1)], collapse = " + "))))

modelo1 = stan_glm(
  
  switch ~ (arsenic + distance + association + education),
  data = Wells,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5),
  seed = 123, refresh = 0
  
)

## Intervalo de credibilidade para cada parametro e variavel preditora

post.plot = plot(modelo1, "areas", prob = 0.95, prob_outer = 1)
post.plot+ geom_vline(xintercept = 0)

## sumario do modelo
summary(modelo1,
        probs = c(0.025, 0.5, 0.975))



exp(modelo1$coefficients)

odds2prob = function(odds){
  odds/(1 + odds)  
}

odds2prob(exp(modelo1$coefficients))

## Pela ideia basal, temos 46% de chance de trocar.
## A cada um aumento de arsenio, temos a probabildade de 11% de trocar (comparado com 50%).
## A cada um ano de educação, diminui-se em 1% a chance do morador trocar (comparado com 50%).
## 



# We can extract corresponding posterior median estimates using ‘coef’ 
# function and to get a sense for the uncertainty in our estimates we can
# use the posterior_interval function to get Bayesian uncertainty intervals.
# The uncertainty intervals are computed by finding the relevant quantiles of
# the draws from the posterior distribution. For example, to compute median and 
# 90% intervals we use:

round(coef(modelo1), 2)

## intervalo de credibilidade de 95%

round(posterior_interval(modelo1, prob = 0.9),4)


## Existe uma probabilidade de 95% dointercept estar entre  -0.3228  e 0.0028.
## Existe uma probabilidade de 95% dointercept estar entre  -0.3228  e 0.0028.

## Coeficientes na forma log da Chance

exp(modelo1$coefficients) ## transforma

## Espera-se  uma chance de 15% dos habitantes na mudem  
## A cada 

#rstanarm supports loo package which implements fast Pareto smoothed leave-one-out cross-validation (PSIS-LOO) 
#(Vehtari, Gelman and Gabry, 2017b) to compute expected log predictive density (elpd):

(loo1 <- loo(modelo1, save_psis = TRUE))
loo1



m1brms = brm(
  ## se for binomial
  # sucesses | trials(n_trials) ~ x1 + x2
  switch ~ (arsenic + distance + association + education),
  data = Wells,
  family = bernoulli,
  prior = c(
    set_prior("normal(0, 2.5)", class = "b"),
    set_prior("normal(0, 2.5)", class = "Intercept")
  ),
  seed = 123
)






















