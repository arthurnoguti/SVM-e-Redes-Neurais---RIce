#### importando arquivo ####
library(readxl)
library(tidyverse)
library(e1071)
rice <- read_xlsx(file.choose(), sheet = 1)

#### analise descritiva ####
rice %>%
  group_by(Class) %>%
  summarise(
    quantidade = n()
  )
#####Dividindo em grupos de 20%, Cross validation k = 5 ####
#Método de Validacao Cruzada k-fold k = 5

set.seed(2)

y <- c(1:3810)
sorteio1 <- sample(y, 762)
sorteio2 <- sample(y[-sorteio1], 762)
sorteio3 <- sample(y[c(-sorteio1, - sorteio2)], 762)
sorteio4 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3)], 762)
sorteio5 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3, - sorteio4)], 762)

rice_treinamento1 <- rice[-sorteio1, ]
rice_teste1 <- rice[sorteio1, ]

rice_treinamento2 <- rice[-sorteio2, ]
rice_teste2 <- rice[sorteio2, ]

rice_treinamento3 <- rice[-sorteio3, ]
rice_teste3 <- rice[sorteio3, ]

rice_treinamento4 <- rice[-sorteio4, ]
rice_teste4 <- rice[sorteio4, ]

rice_treinamento5 <- rice[-sorteio5, ]
rice_teste5 <- rice[sorteio5, ] 

#### RODADA SVM 1 ####
rice_treinamento1$Class <- as.factor(rice_treinamento1$Class)

modelo_svm_linear1 <- svm(Class ~ ., data = rice_treinamento1, kernel = "linear")
modelo_svm_polynomial1 <- svm(Class ~ ., data = rice_treinamento1, kernel = "polynomial")
modelo_svm_radial1 <- svm(Class ~ ., data = rice_treinamento1, kernel = "radial")

summary(modelo_svm_linear1) 
summary(modelo_svm_polynomial1)  
summary(modelo_svm_radial1)  

pred_linear1 <- predict(modelo_svm_linear1, rice_teste1)
pred_polynomial1 <- predict(modelo_svm_polynomial1, rice_teste1)
pred_radial1 <- predict(modelo_svm_radial1, rice_teste1)

tab_linear1 <- table(Predict = pred_linear1, Actual = rice_teste1$Class)
tab_polynomial_1 <- table(Predict = pred_polynomial1, Actual = rice_teste1$Class)
tab_radial1 <- table(Predict = pred_radial1, Actual = rice_teste1$Class)

tab_linear1
tab_polynomial_1
tab_radial1

#### RODADA SVM 2 ####
rice_treinamento2$Class <- as.factor(rice_treinamento2$Class)

modelo_svm_linear2 <- svm(Class ~ ., data = rice_treinamento2, kernel = "linear")
modelo_svm_polynomial2 <- svm(Class ~ ., data = rice_treinamento2, kernel = "polynomial")
modelo_svm_radial2 <- svm(Class ~ ., data = rice_treinamento2, kernel = "radial")

summary(modelo_svm_linear2) 
summary(modelo_svm_polynomial2)  
summary(modelo_svm_radial2)  

pred_linear2 <- predict(modelo_svm_linear2, rice_teste2)
pred_polynomial2 <- predict(modelo_svm_polynomial2, rice_teste2)
pred_radial2 <- predict(modelo_svm_radial2, rice_teste2)

tab_linear2 <- table(Predict = pred_linear2, Actual = rice_teste2$Class)
tab_polynomial_2 <- table(Predict = pred_polynomial2, Actual = rice_teste2$Class)
tab_radial2 <- table(Predict = pred_radial2, Actual = rice_teste2$Class)

tab_linear2
tab_polynomial_2
tab_radial2

#### RODADA SVM 3 ####
rice_treinamento3$Class <- as.factor(rice_treinamento3$Class)

modelo_svm_linear3 <- svm(Class ~ ., data = rice_treinamento3, kernel = "linear")
modelo_svm_polynomial3 <- svm(Class ~ ., data = rice_treinamento3, kernel = "polynomial")
modelo_svm_radial3 <- svm(Class ~ ., data = rice_treinamento3, kernel = "radial")

summary(modelo_svm_linear3) 
summary(modelo_svm_polynomial3)  
summary(modelo_svm_radial3)  

pred_linear3 <- predict(modelo_svm_linear3, rice_teste3)
pred_polynomial3 <- predict(modelo_svm_polynomial3, rice_teste3)
pred_radial3 <- predict(modelo_svm_radial3, rice_teste3)

tab_linear3 <- table(Predict = pred_linear3, Actual = rice_teste3$Class)
tab_polynomial_3 <- table(Predict = pred_polynomial3, Actual = rice_teste3$Class)
tab_radial3 <- table(Predict = pred_radial3, Actual = rice_teste3$Class)

tab_linear3
tab_polynomial_3
tab_radial3

#### RODADA SVM 4 ####
rice_treinamento4$Class <- as.factor(rice_treinamento4$Class)

modelo_svm_linear4 <- svm(Class ~ ., data = rice_treinamento4, kernel = "linear")
modelo_svm_polynomial4 <- svm(Class ~ ., data = rice_treinamento4, kernel = "polynomial")
modelo_svm_radial4 <- svm(Class ~ ., data = rice_treinamento4, kernel = "radial")

summary(modelo_svm_linear4) 
summary(modelo_svm_polynomial4)  
summary(modelo_svm_radial4)  

pred_linear4 <- predict(modelo_svm_linear4, rice_teste4)
pred_polynomial4 <- predict(modelo_svm_polynomial4, rice_teste4)
pred_radial4 <- predict(modelo_svm_radial4, rice_teste4)

tab_linear4 <- table(Predict = pred_linear4, Actual = rice_teste4$Class)
tab_polynomial_4 <- table(Predict = pred_polynomial4, Actual = rice_teste4$Class)
tab_radial4 <- table(Predict = pred_radial4, Actual = rice_teste4$Class)

tab_linear4
tab_polynomial_4
tab_radial4

#### RODADA SVM 5 ####
rice_treinamento5$Class <- as.factor(rice_treinamento5$Class)

modelo_svm_linear5 <- svm(Class ~ ., data = rice_treinamento5, kernel = "linear")
modelo_svm_polynomial5 <- svm(Class ~ ., data = rice_treinamento5, kernel = "polynomial")
modelo_svm_radial5 <- svm(Class ~ ., data = rice_treinamento5, kernel = "radial")

summary(modelo_svm_linear5) 
summary(modelo_svm_polynomial5)  
summary(modelo_svm_radial5)  

pred_linear5 <- predict(modelo_svm_linear5, rice_teste5)
pred_polynomial5 <- predict(modelo_svm_polynomial5, rice_teste5)
pred_radial5 <- predict(modelo_svm_radial5, rice_teste5)

tab_linear5 <- table(Predict = pred_linear5, Actual = rice_teste5$Class)
tab_polynomial_5 <- table(Predict = pred_polynomial5, Actual = rice_teste5$Class)
tab_radial5 <- table(Predict = pred_radial5, Actual = rice_teste5$Class)

tab_linear5
tab_polynomial_5
tab_radial5

#### MEDIDAS DE DESEMPENHO SVM ####
#MACRO 
list_matriz_linear <- list(tab_linear1,tab_linear2,tab_linear3,tab_linear4,tab_linear5)
list_matriz_polynomial <- list(tab_polynomial_1,tab_polynomial_2,tab_polynomial_3,tab_polynomial_4,tab_polynomial_5)
list_matriz_radial <- list(tab_radial1,tab_radial2,tab_radial3,tab_radial4,tab_radial5)

#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}

acc_macro_linear <- sum(sapply(list_matriz_linear, acc_macro))/5
acc_macro_polynomiaL <- sum(sapply(list_matriz_polynomial, acc_macro))/5
acc_macro_radial <- sum(sapply(list_matriz_radial, acc_macro))/5

acc_macro_linear
acc_macro_polynomiaL
acc_macro_radial

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}

sen_macro_linear <- sum(sapply(list_matriz_linear, sen_macro))/5
sen_macro_polynomial <- sum(sapply(list_matriz_polynomial, sen_macro))/5
sen_macro_radial <- sum(sapply(list_matriz_radial, sen_macro))/5

sen_macro_linear
sen_macro_polynomial
sen_macro_radial

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}

esp_macro_linear <- sum(sapply(list_matriz_linear, esp_macro))/5
esp_macro_polynomial <- sum(sapply(list_matriz_polynomial, esp_macro))/5
esp_macro_radial <- sum(sapply(list_matriz_radial, esp_macro))/5

esp_macro_linear
esp_macro_polynomial
esp_macro_radial

#acuracia balanceada

BA_macro_linear <- (sen_macro_linear+esp_macro_linear)/2
BA_macro_polynomial <- (sen_macro_polynomial+esp_macro_polynomial)/2
BA_macro_radial <- (sen_macro_radial+esp_macro_radial)/2

BA_macro_linear
BA_macro_polynomial
BA_macro_radial

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}

p_macro_linear <- sum(sapply(list_matriz_linear, p_macro))/5
p_macro_polynomial <- sum(sapply(list_matriz_polynomial, p_macro))/5
p_macro_radial <- sum(sapply(list_matriz_radial, p_macro))/5

p_macro_linear
p_macro_polynomial
p_macro_radial

#f1-score
f1_macro_linear <- (2*p_macro_linear*sen_macro_linear)/(p_macro_linear+sen_macro_linear)
f1_macro_polynomial <- (2*p_macro_polynomial*sen_macro_polynomial)/(p_macro_polynomial+sen_macro_polynomial)
f1_macro_radial <- (2*p_macro_radial*sen_macro_radial)/(p_macro_radial+sen_macro_radial)

f1_macro_linear
f1_macro_polynomial
f1_macro_radial

metricas <- data.frame(
  Kernel_type = c("linear","polynomial","radial"),
  Acurácia = c(acc_macro_linear,acc_macro_polynomiaL,acc_macro_radial),
  Sensibilidade = c(sen_macro_linear,sen_macro_polynomial,sen_macro_radial),
  Especificidade = c(esp_macro_linear,esp_macro_polynomial,esp_macro_radial),
  Acurácia_balanceada = c(BA_macro_linear,BA_macro_polynomial,BA_macro_radial),
  Precisão = c(p_macro_linear,p_macro_polynomial,p_macro_radial),
  F1_score = c(f1_macro_linear,f1_macro_polynomial,f1_macro_radial)
)

#### Redes Neurais ####
library('neuralnet')
library('caret')
library('nnet')


classificador1 <- nnet(Class ~.,data = rice_treinamento1, size = 3)
previsao <- predict(classificador1, newdata = rice_teste1)



classificador1 <- neuralnet(Class ~.,data = rice_treinamento1, hidden = c(3,3), threshold = 0.01 )

plot(classificador1)

previsao1 <- compute(classificador1, rice_teste1)
previsao1.1 <- data.frame()
for(i in 1:762){
  previsao1.1 <- rbind(previsao1.1, which.max(previsao1$net.result[i,]))
}
previsao1.1$X2L <- gsub(1,"Cammeo",previsao1.1$X2L)
previsao1.1$X2L <- gsub(2,"Osmancik",previsao1.1$X2L)
predicit <- as.factor(previsao1.1$X2L)

confusionMatrix(predicit,rice_teste1$Class)
