
if(!require(pacman)) install.packages("pacman"); library(pacman)

p_load(bayesrules, rstanarm, bayesplot,
       tidyverse, tidybayes, broom.mixed)



hist(rnorm(1000,mean=-1.4,sd = 0.7), breaks = 12)

hist(rnorm(1000,mean=0.07,sd = 0.035),breaks = 12)


## Load and process the data

data("weather_perth")

weather = weather_perth %>% 
  select(day_of_year, raintomorrow, humidity9am,
         humidity3pm, raintoday)

head(weather)

## Execucao de uma simulacao de nossa priori

modelo.priori <- stan_glm(raintomorrow~humidity9am,
                          data = weather, family = binomial,
                          prior_intercept = normal(-1.4, 0.7),
                          prior = normal(0.07,0.035),
                          chains = 4, iter=500*2,
                          seed=10, prior_PD = TRUE)

summary(modelo.priori)

set.seed(10)

## graficos de entendimento da priori


## Relacao entre a probabilidade de chover e  a nivel de 
## humidade no dia anterior

## grafico de 100 modelos a priori com humidade

weather %>% 
  add_fitted_draws(modelo.priori, n=100) %>% 
  ggplot(aes(x = humidity9am, y = raintomorrow)) + 
  geom_line(aes(y = .value, group = .draw), linewidth = 0.1)


## Relacao entre a probabilidade de chover e  a nivel de 
## proporcao observada de dias nos quais choveu

## grafico das proporcoes observadas de chuva em 100 datasets prioris

weather %>% add_predicted_draws(modelo.priori, n = 100) %>% 
  group_by(.draw) %>% 
  summarise(proportion_rain = mean(.prediction == 1)) %>% 
  ggplot(aes(x = proportion_rain)) +
  geom_histogram(color = "white")


# Plot the observed proportion of rain in 100 prior datasets

# --
# Simulacao da posteriori
# --

## estatistica descritiva

ggplot(weather, aes(x = humidity9am, y = raintomorrow)) + 
  geom_jitter(size = 0.5) + coord_flip()


##  calculo e grafico da taxa de chuva por intervalo de humidade

weather %>% 
  mutate(humity_bracket = 
           cut(humidity9am, breaks = seq(10,100, by = 10))) %>% 
  group_by(humity_bracket) %>% 
  summarise(rain_rate = mean(raintomorrow == "Yes")) %>% 
  ggplot(aes(x = humity_bracket, y = rain_rate)) +
  geom_point(size = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))









