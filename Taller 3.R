# ConfiguraciÃ³n de opciones generales en R ----
options(digits = 3,
        scipen = 100)
rm(list=ls())

# Carga de librerias y paquetes ----
library(tidyverse)
library(lmtest)
library(DescTools)
library(car)
library(caret)
library(plotROC)
library(MASS)
library(ROCR)
library(haven)
library(margins)
library(psych)
library(stats)
library(moments)
library(pROC)


#II. PARTE PRACTICA
##2. 
###a.

data <- read_dta("infieles.dta")

# DeclaraciÃ³n de variables ----
data <- data %>% 
  mutate(rating_matrimonio = as.factor(rating_matrimonio),
         religiosidad = as.factor(religiosidad)) %>% 
  as.data.frame()



data <- data %>% 
  mutate(infieles = ifelse(extramatrimonial>0, 1, 0))



#b.
#i. 
model1 <- lm(infieles ~ rating_matrimonio + edad + años_casados + hijos + religiosidad + educ, data)
summary(model1)

###ii.

data <- data %>% 
  mutate(prediction_lineal = predict(model1),
         residual_lineal = resid(model1))
data %>% 
  ggplot() +
  geom_point(aes(x=prediction_lineal, y = infieles)) +
  geom_smooth(aes(x=prediction_lineal, y = infieles), method = "lm", se = F)

#Algunos valores de probabilidad no se encuentran entre 0 y 1
#El problema se soluciona usando un modelo logit


###iii


bptest(model1)

data %>% 
  ggplot(aes(x = prediction_lineal,
             y = residual_lineal)) +
  geom_point()

#Existen problemas de heterocedasticidad, se debe usar un modelo de probabilidad logistico

###iv

# Matriz de confusion
#se deben convertir las probabilidades predichas a factores.

data <- data %>% 
  mutate(infieles = as.factor(infieles))


#threshold 1:0.4

data <- data %>% 
  mutate(prediction_lineal_factor1 = as.factor(as.numeric(prediction_lineal>=0.4)))


confusionMatrix(data = data %>% 
                pull(prediction_lineal_factor1), reference = data %>% 
                pull(infieles),positive = "1")

#threshold 2:0.5

data <- data %>% 
  mutate(prediction_lineal_factor2 = as.factor(as.numeric(prediction_lineal>=0.5)))


confusionMatrix(data = data %>% 
                  pull(prediction_lineal_factor2), reference = data %>% 
                  pull(infieles),positive = "1")


#threshold 3:0.9

data <- data %>% 
  mutate(prediction_lineal_factor3 = as.factor(as.numeric(prediction_lineal>=0.9)))


confusionMatrix(data = data %>% 
                  pull(prediction_lineal_factor3), reference = data %>% 
                  pull(infieles),positive = "1")


#v
# Curva ROC
#La curva debe estar por encima de la linea creciente, entre mas arriba esté, mejor
roc <- data %>% 
  mutate(y = as.numeric(levels(infieles)[as.numeric(infieles)])) %>% 
  ggplot(aes(m = prediction_lineal, d = y)) + 
  geom_roc(n.cuts=30,
           labels=FALSE) + 
  style_roc(guide = F) + 
  geom_line(aes(x=seq(0,1,1/(6366-1)), 
                y=seq(0,1,1/(6366-1)))) +
  geom_roc(labelround = 2) +
  theme_bw()

roc
#El punto mas arriba es el thresfold optimo. Este se encuentra entre 0.27 y 0.34
roc_model1 <- roc(response = model1$model$infieles,
                  predictor = predict(model1, type = "response"))

data.frame(threshold = roc_model1$thresholds,
           sentivity = roc_model1$sensitivities,
           specificity = roc_model1$specificities) %>% 
  mutate(f1_score = 2*sentivity*specificity/(sentivity + specificity)) %>% 
  arrange(desc(f1_score)) %>% 
  filter(row_number()==1)
#el f1 score arroja 0.30 como el mejor threshold

data <- data %>% 
  mutate(prediction_lineal_factor4 = as.factor(as.numeric(prediction_lineal>=0.3050512)))


confusionMatrix(data = data %>% 
                  pull(prediction_lineal_factor4), reference = data %>% 
                  pull(infieles),positive = "1")



# Area bajo la curva
calc_auc(roc)
#AUC es 0.744 lo que indica que es un modelo bueno



###c.
###i.

trace(glm.fit, quote(print(coefold)), at = list(c(22, 4, 8, 4, 19, 3)))
model2 <- glm(infieles ~ rating_matrimonio+ edad+ años_casados+ hijos+ religiosidad+ educ, 
             data = data, 
             family = binomial(link = "logit"),
             control = glm.control(trace = TRUE))
summary(model2)
untrace(glm.fit)

##Verosimilitud
logLik(model2)

## Prueba de significancia global
lrtest(model2)

###ii.

exp(cbind(coef(model2), confint(model2, trace = T, level = 0.95)))  
exp(coef(model2))-1

#odds ratio

###iii
# Efectos marginales: efectos de cambios en un x sobre la probabilidad ----
margins(model2)

# Funciones
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Efecto en el promedio: (para categoricas la categoria mÃ¡s comun)
x_prom <- data %>% 
  summarise(rating_matrimonio = getmode(rating_matrimonio),
            edad = getmode(edad),
            años_casados = getmode(años_casados),
            hijos = getmode(hijos),
            religiosidad = getmode(religiosidad),
            educ = getmode(educ))

prob <- predict(model2, x_prom, type = "response")
prob*(1-prob)*coef(model2)

# En la moda el efecto marginal de un aumento en el raiting del matrimonio en una unidad frente al raiting 1
#aumenta la probabilidad de ser infiel en 34.44 puntos basicos


#iv
data <- data %>% 
  mutate(prediction_logit = predict(model2),
         residual_logit = resid(model2))


#threshold 1:0.4

data <- data %>% 
  mutate(prediction_logit_factor1 = as.factor(as.numeric(prediction_logit>=0.4)))


confusionMatrix(data = data %>% 
                  pull(prediction_logit_factor1), reference = data %>% 
                  pull(infieles),positive = "1")

#threshold 2:0.5

data <- data %>% 
  mutate(prediction_logit_factor2 = as.factor(as.numeric(prediction_logit>=0.5)))


confusionMatrix(data = data %>% 
                  pull(prediction_logit_factor2), reference = data %>% 
                  pull(infieles),positive = "1")


#threshold 3:0.9

data <- data %>% 
  mutate(prediction_logit_factor3 = as.factor(as.numeric(prediction_logit>=0.9)))


confusionMatrix(data = data %>% 
                  pull(prediction_logit_factor3), reference = data %>% 
                  pull(infieles),positive = "1")

#v
# Curva ROC
#La curva debe estar por encima de la linea creciente, entre mas arriba esté, mejor
roc <- data %>% 
  mutate(y = as.numeric(levels(infieles)[as.numeric(infieles)])) %>% 
  ggplot(aes(m = prediction_logit, d = y)) + 
  geom_roc(n.cuts=30,
           labels=FALSE) + 
  style_roc(guide = F) + 
  geom_line(aes(x=seq(0,1,1/(6366-1)), 
                y=seq(0,1,1/(6366-1)))) +
  geom_roc(labelround = 2) +
  theme_bw()

roc
####################
#El punto mas arriba es el thresfold optimo. Este seria -0.8
roc_model2 <- roc(response = model2$model$infieles,
                  predictor = predict(model2, type = "response"))

data.frame(threshold = roc_model2$thresholds,
           sentivity = roc_model2$sensitivities,
           specificity = roc_model2$specificities) %>% 
  mutate(f1_score = 2*sentivity*specificity/(sentivity + specificity)) %>% 
  arrange(desc(f1_score)) %>% 
  filter(row_number()==1)
#el f1 score arroja 0.3026 como el mejor threshold


data <- data %>% 
  mutate(prediction_logit_factor4 = as.factor(as.numeric(prediction_logit>=0.3026989)))


confusionMatrix(data = data %>% 
                  pull(prediction_logit_factor4), reference = data %>% 
                  pull(infieles),positive = "1")




# Area bajo la curva
calc_auc(roc)
#AUC es 0.743 lo que indica que es un modelo bueno

#d
#i
# Criterio de informacion de Akaike

AIC(model1)
AIC(model2)
#Se debe elegir el modelo con menor AIC, es decir el modelo 2 (6971)
