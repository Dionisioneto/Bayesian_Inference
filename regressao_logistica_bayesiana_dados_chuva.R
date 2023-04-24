
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

## O grafico jitter revela que os dias chuvoso sao bem menos comuns que
## dias nao chuvosos.


##  calculo e grafico da taxa de chuva por intervalo de humidade

## Embora nao tenhamos dados o suficiente para termos uma ideia estavel
## dobre a probabilidade de chover para cada nivel de humidade, nos podemos
## exemaminar a porbabilidade de chover em dias chuvosos em similar niveis
## de humidade.


weather %>% 
  mutate(humity_bracket = 
           cut(humidity9am, breaks = seq(10,100, by = 10))) %>% 
  group_by(humity_bracket) %>% 
  summarise(rain_rate = mean(raintomorrow == "Yes")) %>% 
  ggplot(aes(x = humity_bracket, y = rain_rate)) +
  geom_point(size = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

## No geral, note que a chance de chover parece flutuar em torno de 10%
## quando  os niveis de humidade sao abaixo de 60%  e entao a, partir
## deste ponto, comecam a crescer.

## Esta tendencia sicroniza com o nosso entendimento a priori de que chover
## e a humidade estao positivamente associadas.

## No entanto, enquanto nosso entendimento anterior deixava em aberto a possibilidade
##de que a probabilidade de chuva se aproximasse de 100% à medida que os níveis de umidade
##chegassem ao máximo (Figura 13.2), vemos nos dados que a chance de chuva nos níveis mais
## altos de umidade mal chegava a 50%.


## processo de simulacao da nossa posteriori


chuva.modelo1 = update(modelo.priori, prior_PD = FALSE)

## graficos de diagnostico para verificar a estatbilidade dos resultados

mcmc_trace(chuva.modelo1)
mcmc_dens_overlay(chuva.modelo1)
mcmc_acf(chuva.modelo1)

## Ajuste de 100 palusiveis posterioris

## Naturalamente, apos incorporarmos os dados da verossimilhanca, os 100 modelos
## ajustados sao bem menos caoticos em relacao a observacao vista na priori, isto e,
## agora, estamos bem mais certos sobre a relacao da chuva com a umidade.

## Agora verificamos que a probabilidade de chuver fortemente aumenta com a humidade,
## ainda não é até uma umidade de aproximadamente 95% que atingimos o ponto de 
## inflexao quando a chuva se torna mais provável do que a sua nao ocorrencia.
## Em contraste, quando e 9 horas da manha, a umidade e menor que 25% e temos que
## bem pouco provavel chover.


weather %>% add_fitted_draws(chuva.modelo1, n = 100) %>% 
  ggplot(aes(x = humidity9am, y = raintomorrow)) + 
  geom_line(aes(y = .value, group = .draw), alpha = 0.15) +
  labs(y = "Probabilidade de Chover")


## Informacoes mais precisas sobre a relacao da umidade as 9 da manha
## e a cguva esta contida no parametro estimado beta1, por isso, e preciso 
## summarizar os resultados da nossa posteriori.

## escala do log da odds
posterior_interval(chuva.modelo1, prob=0.9)


## escala da odds

exp(posterior_interval(chuva.modelo1, prob=0.9))

## interpretacoes

## Para cada incremento de uma procentagem na umidade observada de manha, as 9 horas,
## existe 90% de chance a posteriori de que o log(chance de chover) aumente entre 
## 0.03961991 e 0.05698684. Esta taxa e bem menor do que a nossa crenca a priori
## de 0.07 como media da priori de beta1. A chance de chover significativamente aumenta
## om a umidade, mas nao no grau que nos antecipamos.

exp(posterior_interval(chuva.modelo1, prob=0.9)*15)[2,]

## Mais substancialmente, para cada incremento na porcentagem da umidade vista hoje de 
## manha, as 9 horas, a chance de chover aumenta em 4,04% e 5,86%. Equivalentemente, para 
## cada 15 unidades de incremento na porcentagem da umidade vista hoje de 
## manha, as 9 horas, a chance de chovepraticamente dobra, pois temos 90%
## de credibildade de que com esse incremento a chance de chober varia de 81% a 135% a mais. 


## Predicao e classificacao

## Agora queremos predizer se ira ou nao chover na cidade dado
## que foi observada um nivel de humidade de 99%.

## simulacao de 20.000 resultados de chuva da variavel aleatoria Y, um par por cada 
## 20.000 ajustes de parametros da nossa cadeia de Markov. 


set.seed(10)

predicao.binaria = posterior_predict(
  chuva.modelo1, newdata = data.frame(humidity9am = 99)
)


## Para realmente conectarmos com o conceito de predicao, podemos simular 
## da nossa preditiva a posteirori do principio. 

## Para cada 2.000 plausiveis pares da nossa posteriori, contendo (beta0, beta1)
## da nossa simulacao da cadeia de Markov, calculamos o log(chance de chover). E
## então, transformamos o log(chance) para a chance de chover  e a probabilidade
## de chover.

## Finalmente, para cada uma das 2.000 probabilidades, nos simulamos a variavel 
## aleatoria Bernoulli Y (Chover ou Nao chover).


# Posterior predictions of binary outcome - from scratch

set.seed(10)


modelo.chuva1 = as.data.frame(chuva.modelo1) %>% 
                mutate(log_odds = `(Intercept)` + humidity9am*99,
                       odds = exp(log_odds),
                       prob = odds/(1+odds),
                       Y = rbinom(n = 2000, size = 1, prob = prob))

## verificacao

head(modelo.chuva1,2)


## histograma
mcmc_hist(predicao.binaria) + labs(x = "Y")

## juntando todas as 2.000 predicoes juntas, examinamos que a preditiva posteriori 
## indica que, com 99% de umidade, a regressao loistica bayesiana sugere que
## chover e mais provavel do que nao chover.

ggplot(modelo.chuva1, aes(x = Y)) + 
  stat_count()

## devemos ou nao ter um guarda chuva?
## Sim, quando construimos a nossa posteriori de Y do principio
## 1078 amostras (54%) indicaram chuva. Então, dado que a chover e mais
## provavel do que não chover na preditiva posteriori de modelo de Y
## e razoavel classificar Y como chuva!


# Sumario das descobertas a posteriori de Y

table(predicao.binaria); prop.table(table(predicao.binaria))


## regra de Classificao 

## Se tiver mais da metade de nossas classificacoes predizendo Y = 1, entao Y = 1.
## Caso contrario, Y sera 0 (nao chuva).

## ---
## Avaliacao do modelo
## ---

proporcao.chuva = function(p){mean(p == 1)}

pp_check(chuva.modelo1, nreps = 200,
         plotfun= "stat", stat = "proporcao.chuva") +
  xlab("Probabilidade de chover")
  








