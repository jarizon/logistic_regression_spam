# Projeto Regressão Logística Udemy - Spam Score #
# Jari Nogueira - jari.nogueira@gmail.com #


# Necessário gerar Score com a probabilidade de SPAM da base.
## Considerações:
# Utilizar base "spam" da biblioteca "kernlab";
# Primeiras 48 variáveis, contém a frequência de palavras;
# 49 à 54  apontam a frequência de caract. especiais;
# 55 à 57 são referentes aos caracteres (média, maior e comprimento total de cx alta)

## ---------------------------------------------- ##


library(kernlab)
library(arules)
library(tibble)

data("spam")
View(spam)

bd_spam <-  spam

str(bd_spam)
summary(bd_spam)

# Categorizando a frequência de ocorrência das variáveis.
## Caso não teve ocorrência, 0, se não, agrupamento por cluster em 3 grupos (1, 2 e 3).

for (i in 1:57)
{
  bd_spam[,i] <- ifelse(bd_spam[,i]==0, 0, discretize(bd_spam[,i], "cluster", breaks = 3))
  
}

bd_spam <- as.tibble(bd_spam)
bd_spam$spam_ok <- as.factor(ifelse(bd_spam$type=="spam",1,0))

str(bd_spam)


# Separando amostras de treinamento e teste.

spam_sample <- round(nrow(bd_spam)*0.8)
train_index <- sort(sample(1:nrow(bd_spam), spam_sample))

spam_training <- bd_spam[train_index,]
spam_test <- bd_spam[-train_index,]

nrow(bd_spam)==(nrow(spam_training)+nrow(spam_test)) # Validação simples das amostras.

# Primeiro modelo - Todas as variáveis.

spam_model1 <- glm(spam_ok~
                     make+address+all+num3d+
                     our+over+remove+internet+
                     order+mail+receive+will+
                     people+report+addresses+free+
                     business+email+you+credit+
                     your+font+num000+money+
                     hp+hpl+george+num650+
                     lab+labs+telnet+num857+
                     data+num415+num85+technology+
                     num1999+parts+pm+direct+
                     cs+meeting+original+project+
                     re+edu+table+conference+
                     charSemicolon+charRoundbracket+charSquarebracket+charExclamation+
                     charDollar+charHash+capitalAve+capitalLong+
                     capitalTotal
                     , data = spam_training, family = binomial())

summary(spam_model1)

# Novo modelo retirando variáveis que não fizeram sentido ao modelo 1, para avaliar novo AIC.

spam_model2 <- glm(spam_ok~
                     original+credit+all+make+charDollar+charExclamation+
                     edu+re+meeting+num650+george+hp+money+your+business+
                     free+internet+remove+our+capitalTotal+project+technology+
                     num85+num000+font+will+over+charSemicolon+conference+report+people+mail
                   , data = spam_training, family = binomial())

summary(spam_model2)

# Modelo 3, retirando novas variáveis do modelo 2, com p-valor mais alto.

spam_model3 <- glm(spam_ok~
                     conference+num85+project+our+remove+internet+free+business+
                     your+money+hp+george+num650+meeting+re+edu+charExclamation+
                     charDollar+report+over+will+font+num000+technology+
                     capitalTotal
                   , data = spam_training, family = binomial())

summary(spam_model3)

# Model 4 - Modelo feito com o método Stepwise forward/backward, manualmente.

spam_model4 <- glm(spam_ok~
                     make+all+num3d+
                     our+over+remove+internet+
                     mail+will+people+report+free+ 
                     business+credit+your+font+num000+money+
                     hp+george+num650+data+num85+technology+
                     cs+meeting+original+project+re+edu+conference+
                     charSemicolon+charExclamation+charDollar+capitalLong+capitalTotal
                   , data = spam_training, family = binomial())


summary(spam_model4)

# Comparando AIC dos modelos:

AIC_Modelos <- c(AIC(spam_model1), AIC(spam_model2), AIC(spam_model3), AIC(spam_model4))
Modelo <- c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4")

print(cbind(Modelo, AIC_Modelos))


# Intervalo de confiança do modelo 4.

coef(spam_model4)
odds4 <- exp(coef(spam_model4))

IC_odds4 <- exp(confint(spam_model4)) 

# OBS: identificado casos com intersecção com #1, porém retirar, não melhorou o modelo.

round(cbind(odds4, IC_odds4), 3)

# Aplicando o modelo da base treino, na base teste.

spam_predict <- predict(spam_model4, spam_test, type = "response")
spam_test$predict <- spam_predict

# Matriz de confusão:

spam_test$fx_prob <- discretize(spam_test$predict*100, "frequency", breaks = 10)

table(spam_test$fx_prob, spam_test$spam_ok)




