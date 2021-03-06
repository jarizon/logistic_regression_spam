# Projeto Regress�o Log�stica Udemy - Spam Score #
# Jari Nogueira - jari.nogueira@gmail.com #


# Necess�rio gerar Score com a probabilidade de SPAM da base.
## Considera��es:
# Utilizar base "spam" da biblioteca "kernlab";
# Primeiras 48 vari�veis, cont�m a frequ�ncia de palavras;
# 49 � 54  apontam a frequ�ncia de caract. especiais;
# 55 � 57 s�o referentes aos caracteres (m�dia, maior e comprimento total de cx alta)

## ---------------------------------------------- ##


library(kernlab)
library(arules)
library(tibble)

data("spam")
View(spam)

bd_spam <-  spam

str(bd_spam)
summary(bd_spam)

# Categorizando a frequ�ncia de ocorr�ncia das vari�veis.
## Caso n�o teve ocorr�ncia, 0, se n�o, agrupamento por cluster em 3 grupos (1, 2 e 3).

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

nrow(bd_spam)==(nrow(spam_training)+nrow(spam_test)) # Valida��o simples das amostras.

# Primeiro modelo - Todas as vari�veis.

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

# Novo modelo retirando vari�veis que n�o fizeram sentido ao modelo 1, para avaliar novo AIC.

spam_model2 <- glm(spam_ok~
                     original+credit+all+make+charDollar+charExclamation+
                     edu+re+meeting+num650+george+hp+money+your+business+
                     free+internet+remove+our+capitalTotal+project+technology+
                     num85+num000+font+will+over+charSemicolon+conference+report+people+mail
                   , data = spam_training, family = binomial())

summary(spam_model2)

# Modelo 3, retirando novas vari�veis do modelo 2, com p-valor mais alto.

spam_model3 <- glm(spam_ok~
                     conference+num85+project+our+remove+internet+free+business+
                     your+money+hp+george+num650+meeting+re+edu+charExclamation+
                     charDollar+report+over+will+font+num000+technology+
                     capitalTotal
                   , data = spam_training, family = binomial())

summary(spam_model3)

# Model 4 - Modelo feito com o m�todo Stepwise forward/backward, manualmente.

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


# Intervalo de confian�a do modelo 4.

coef(spam_model4)
odds4 <- exp(coef(spam_model4))

IC_odds4 <- exp(confint(spam_model4)) 

# OBS: identificado casos com intersec��o com #1, por�m retirar, n�o melhorou o modelo.

round(cbind(odds4, IC_odds4), 3)

# Aplicando o modelo da base treino, na base teste.

spam_predict <- predict(spam_model4, spam_test, type = "response")
spam_test$predict <- spam_predict

# Matriz de confus�o:

spam_test$fx_prob <- discretize(spam_test$predict*100, "frequency", breaks = 10)

table(spam_test$fx_prob, spam_test$spam_ok)




