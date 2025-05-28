setwd("C:/Users/afons/Documents/MECD/TAE/projeto")
dmat<-read.table("student-mat.csv",sep=";",header=TRUE)
View(dmat)

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggcorrplot)
library(nortest)
library(mvnormtest)
library(psych)

#Criar variavel resposta binária

PF <- ifelse(dmat$G3 >= 10, 1, 0)
head(PF)
head(dmat$G3)


#Análise Exploratoria de Dados

dim(dmat)
names(dmat)

summary(dmat)
str(dmat) #tipo das variaveis
View(dmat)
names(dmat)

#Vars que nao sao numericas
names(dmat[,setdiff(names(dmat), names(dmat_num))])
# [1] "school"     "sex"        "address"    "famsize"    "Pstatus"    "Mjob"      
# [7] "Fjob"       "reason"     "guardian"   "schoolsup"  "famsup"     "paid"      
# [13] "activities" "nursery"    "higher"     "internet"   "romantic"

#Matriz com variaveis apenas numericas
#
dmat_num <- dmat[sapply(dmat, is.numeric)]
length(dmat_num) #16 vars numericas (17 do tipo char)

#DS nao possuim NAs
colSums(is.na(dmat))

library(ggplot2)
library(reshape2)

# Primeiro, selecionamos apenas as variáveis quantitativas que interessam
# Subset dos dados
dados_boxplot <- dmat_rf[, c("PF", c("school", "sex", "Mjob", "Fjob",
                                     "guardian","schoolsup",
                                     "higher", "internet", "romantic"))]


names(dmat_rf)

# Converter para formato long (melhor para facet_wrap)
dados_long <- melt(dados_boxplot, id.vars = "PF")

# Fazer os hist
ggplot(dados_long, aes(x = value, fill = as.factor(PF))) +
  geom_bar(position = "dodge") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(x = "Category", y = "Count", fill = "Final Status") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme(strip.text = element_text(size = 12))



#Escola

table(dmat$school)
slice <- table(dmat$school)
lbls <- c("Gabriel Pereira (GP)", "Mousinho da Silveira (MS)")
pct <- round(100*slice/sum(slice))
lbls <- paste(lbls, pct)
lbls <- paste (lbls, "%")
pie(slice, col = c("blue", "coral"), labels = lbls)

#Sexo
table(dmat$sex)
slice2 <- table(dmat$sex)
lbls2 <- c("Sexo Feminino", "Sexo Masculino")
pct2 <- round(100*slice2/sum(slice2))
lbls2 <- paste(lbls2, pct2)
lbls2 <- paste (lbls2, "%")
pie(slice2, col = c("pink", "blue"), labels = lbls2)

#Sexo por Escola

ggplot(dmat, aes(x = "", fill = factor(interaction(school, sex),
                                       levels = c("GP.F", "GP.M", "MS.F", "MS.M")))) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Distribuição por Escola e Sexo", fill = "Grupo")+
  scale_fill_manual(values = c("GP.F" = "violet","GP.M" = "lightblue",
                               "MS.F" = "#FF1493","MS.M" = "darkblue"))

#Trabalho da Mãe
table(dmat$Mjob)
ggplot(dmat, aes(x=Mjob)) +
  geom_bar(fill="blue")+
  labs(title = "Histograma do Tipo de Trabalho da Mãe",
       x="Trabalho da Mãe",
       y = "Frequência")+
  theme_minimal()


#Trabalho do Pai
table(dmat$Fjob)
ggplot(dmat, aes(x=Fjob)) +
  geom_bar(fill="blue")+
  labs(title = "Histograma do Tipo de Trabalho do Pai",
       x="Trabalho do Pai",
       y = "Frequência")+
  theme_minimal()

#Trabalho Pai vs Mae


job_df <- data.frame(
  Parent = rep(c("Fjob","Mjob"), each = nrow(dmat)),
  Job = c(dmat$Fjob, dmat$Mjob))

ggplot(job_df, aes(x=factor(Job), fill = Parent))+
  geom_bar(position = "dodge")+
  labs(title = "Setor Profissional: Mãe vs Pai",
       x = "Setor Profissional",
       y = "Frequência")+
  theme_minimal()



#Status
ggplot(dmat, aes(x=Pstatus)) +
  geom_bar(fill="coral")+
  geom_text(stat="count", aes(label = ..count..), vjust = -0.3)+
  labs(title = "Status de Habitação dos Pais",
       y = "Frequência")+
  theme_minimal()
  
#Educação dos Pais
  edu_df <- data.frame(
    Parent = rep(c("Fedu", "Medu"), each = nrow(dmat)),
    Education = c(dmat$Fedu, dmat$Medu)
  )
  edu_df
  
  ggplot(edu_df, aes(x = factor(Education), fill = Parent)) +
    geom_bar(position = "dodge") +
    labs(title = "Nível de Escolaridade: Mãe vs Pai",
         x = "Nível de Educação (0 = nenhum, 4 = superior)",
         y = "Frequência", fill = "Parente") +
    theme_minimal()
  
#Internet
  ggplot(dmat, aes(x=internet)) +
    geom_bar(fill="coral")+
    geom_text(stat="count", aes(label = ..count..), vjust = -0.3)+
  labs(title = "Acesso à Internet", x = "Acesso à Internet",
       y = "Frequência")+
    theme_minimal()
  
#Tempo de estudo
  
  ggplot(dmat, aes(x = factor(studytime))) +
    geom_bar(fill = "coral") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
    labs(title = "Tempo de Estudo",
         x = "Tempo de Estudo",
         y = "Frequência") +
    scale_x_discrete(labels = c("1" = "<2 h", "2" = "2–5 h", "3" = "5–10 h", "4" = ">10 h")) +
    theme_minimal()

  

#Meio onde vivem
table(dmat$address)
slice3 <- table(dmat$address)
lbls3 <- c("Rural", "Urbano")
pct3 <- round(100*slice3/sum(slice3))
lbls3 <- paste(lbls3, pct3)
lbls3 <- paste (lbls3, "%")
pie(slice3, col = c("green", "grey"), labels = lbls3)

table(dmat$famsize)
table(dmat$Fedu)

#histogramas para ter mais informações sobre as distribuicoes dos preditores


hists <- lapply(c("traveltime", "studytime", "health", "absences"), function(col){
    ggplot(dmat, aes_string(x = col)) +
      geom_histogram(bins = 10, fill = "steelblue", color = "black") +
    if (col != "abscences"){ geom_text(stat="count", aes(label = ..count..), vjust = -0.3)}+
      theme_minimal() +
      labs(title = paste("Histograma de", col), x = col, y = "Frequência")
})

hists2 <- lapply(c("traveltime", "studytime", "health", "absences"), function(col) {
  p <- ggplot(dmat, aes_string(x = col)) +
    geom_histogram(bins = 10, fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = paste("Histograma de", col), x = col, y = "Frequência")
  
  # Adiciona geom_text() apenas se a variável não for 'absences'
  if (col != "absences") {
    p <- p + geom_text(stat = "count", aes_string(label = "..count.."), vjust = -0.3)
  }
  
  return(p)
})

windows()

do.call(grid.arrange, c(hists2, nrow=1))



lillie.test(dmat$age)
lillie.test(dmat$Medu)
lillie.test(dmat$studytime)
lillie.test(dmat$freetime)
lillie.test(dmat$goout)
lillie.test(dmat$G1)
lillie.test(dmat$G2)
lillie.test(dmat$G3)

mshapiro.test(t(dmat_num))

#nao podemos assumir normalidade em nenhuma variavel

#Matriz de Correlação
?ggcorrplot
?corTest
ggcorrplot(cor(dmat_num, method = "spearman"), type = "upper", title = "Spearman Correlation Matrix", lab = T, lab_size = 3)

plot(dmat$goout, dmat$G3)

#Análise Exploratória com G3 (Variável Resposta)

#Histograma das Notas
?hist
hist(dmat$G3, breaks = c(0:20), col = "lightblue", main = "G3", xlab = "Grades (0-20)", ylab = "Frequency")

ggplot(dmat, aes(x = Mjob, y=G3))+
  geom_boxplot()+
  theme_minimal()

ggplot(dmat, aes(x = (PF)))+geom_histogram(stat = "count")+theme_minimal()

ggplot(dmat, aes(x = PF)) +
  geom_bar(fill = "lightblue", color = "black", width = 0.6) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  labs(x = "Final Status (0 = Fail, 1 = Pass)", y = "Number of Students") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

hist(dmat$G3[dmat$G3!=0])
boxplot(dmat$G3)
boxplot(dmat$G3[dmat$G3!=0])
median(dmat$G3)
median(dmat$G3[dmat$G3!=0])
table(dmat$G3)


#G3 por variaveis Qualitativas (Boxplot e Violin Plots)
cat_vars <- names(dmat)[sapply(dmat, is.character)]

for (var in cat_vars) {
  print(
    ggplot(dmat, aes_string(x = var, y = "G3", fill = var)) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      geom_boxplot(width = 0.1, fill = "grey", outlier.size = 1, outlier.shape = 16) +
      labs(title = paste("Distribuição de G3 por", var),
           x = var, y = "G3") +
      theme_minimal() +
      theme(legend.position = "none")
  )
}

for (var in cat_vars) {
  sapply((cat_vars), function(var){
    ggplot(dmat, aes_string(x = var, y = "G3", fill = var)) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      geom_boxplot(width = 0.1, fill = "grey", outlier.size = 1, outlier.shape = 16) +
      labs(title = paste("Distribuição de G3 por", var),
           x = var, y = "G3") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

for (var in cat_vars){
  print(
    ggplot(dmat, aes(x = .data[[var]], fill = factor(PF))) +
      geom_bar(position = "fill") +
      labs(title = paste("Proporção de Passou/Reprovou por",var), x = var, y = "Proporção", fill = "Passou") +
      scale_fill_manual(values = c("Fail" = "tomato", "Pass" = "seagreen")) +
      theme_minimal()
  )
}

#G3 por variaveis Numericas
?plot
plot(G3~G2, data = dmat_num, col = "blue", type="p", pch=16)
plot(G3~G1, data = dmat_num, col = "blue", type="p", pch=16)
plot(G3~age, data = dmat_num, col = "blue", type="p", pch=16)
plot(G3~absences, data = dmat_num, col="blue", pch=16)


#Boxplots das variaveis numericas (mas categoricas)
num_vars <- names(dmat)[sapply(dmat, is.numeric)]
num_vars <- setdiff(num_vars, "G3")  # remover G3 explicitamente
num_vars2 <- setdiff(num_vars, "G2")
num_vars3 <- setdiff(num_vars2, "G1")
num_vars4 <- setdiff(num_vars3, "absences")
# Selecionar apenas as variáveis numéricas com poucos valores únicos (≤6)
num_disc_vars <- num_vars[sapply(dmat[num_vars], function(col) {length(unique(col)) <= 6})]

# Gerar boxplots todos
for (var in num_disc_vars) {
  print(
    ggplot(dmat, aes(x = factor(.data[[var]]), y = G3, fill = factor(.data[[var]]))) +
      geom_boxplot() +
      labs(title = paste("Boxplot de G3 por níveis de", var),
           x = var, y = "G3", fill = var) +
      theme_minimal() +
      theme(legend.position = "none")
  )
}
#criar boxplots com cor negativas
plots <- lapply(c("age", "traveltime", "failures", "goout"), function(var) {
  ggplot(dmat, aes(x = factor(.data[[var]]), y = G3, fill = factor(.data[[var]]))) +
    geom_boxplot() +
    labs(title = paste("Boxplot de G3 por níveis de", var),
         x = var, y = "G3", fill = var) +
    theme_minimal() +
    theme(legend.position = "none")
})

table(dmat$age)
# Mostrar os 4 gráficos lado a lado
grid.arrange(grobs = plots, ncol = 4)

#BOXPLOT DE G3 por MEDU e FEDU

# Reorganizar os dados: passar Medu e Fedu para formato longo
edu_long <- dmat %>%
  select(G3, Medu, Fedu) %>%
  pivot_longer(cols = c(Medu, Fedu), names_to = "Parente", values_to = "Nível")

# Criar boxplot
ggplot(edu_long, aes(x = factor(Nível), y = G3, fill = Parente)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Notas Finais (G3) por Nível de Escolaridade dos Pais",
       x = "Nível de Escolaridade (0 = nenhum, 4 = superior)",
       y = "Nota Final (G3)",
       fill = "Parente") +
  theme_minimal()



#alguns tables de algumas variaveis que pode ajudar na interpretacao
table(dmat$Medu) #apenas 3 alunos cuja mãe nao tem quaisquer estudos, mas a mediana é 15
table(dmat$Fedu)#apenas 3 alunos cujo pai nao tem quaisquer estudos
table(dmat$health) #alunos com nivel de saude muito mau parecem ter mediana superior

# Gerar boxplots + violin das variaveis por grupo de alunos (Pass or Fail) => cortar, violin plots nao sao aconselhados para vars quantitativas
#

plots <- lapply((num_vars4), function(var){
  ggplot(dmat, aes(x = .data[[var]], fill = factor(PF))) +
    geom_bar(position = "fill") +
    labs(title = paste("Proporção de Pass/Fail por", var)) +
    scale_fill_manual(values = c("Fail" = "tomato", "Pass" = "seagreen")) +
    theme_minimal()
})
grid.arrange(grobs=plots, ncol=4)

table(dmat$Dalc)

#------------------------------------------------------------------------------------------------------------------------
#PARTE 2

#dataset sem g1 e g2
?select
dmat_s12 <- dmat %>% select(-G1, -G2, -G3)
dim(dmat_s12)
head(dmat_s12)

#------------------------------------------------------------------------------------------------------------------------

#Selecao de Preditores (RegSubsets e G3)

#Construir os melhores 20 modelos com 1,2,...,ate 20 preditores
library(leaps)
?regsubsets
regfit.full <- regsubsets(dmat$G3 ~ ., dmat, nvmax = 20)#todos os modelos com todos os preditores
summary(regfit.full)

regfit.sg1g2 <- regsubsets(G3 ~ . - G1 - G2, data = dmat, nvmax = 20)
#todos os modelos com todos os preditores
reg_summary <- summary(regfit.sg1g2)
reg_summary$adjr2
regfit.sg1g2

#vars nao presentes nos melhores 20 modelos:
#School, Pstatus, Fedu, guardian, traveltime, paid, activities, nursey, internet, famrel, Dalc, Walc

#grafico:

# Extrair a matriz de seleção de variáveis
which_matrix <- as.data.frame(reg_summary$which)

# Tirar a variável de resposta (primeira coluna é (Intercept))
which_matrix <- which_matrix[, -1]

# Calcular a frequência de cada variável
var_frequency <- colSums(which_matrix)


# Transformar em data frame
var_freq_df <- data.frame(
  Variable = names(var_frequency[var_frequency != 0]),
  Frequency = as.numeric(var_frequency[var_frequency != 0])
)

# Plot
ggplot(var_freq_df, aes(x = reorder(Variable, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frequência de seleção dos preditores (Regsubsets)",
       x = "Variável",
       y = "Frequência de seleção") +
  theme_minimal()

#------------------------------------------------------------------------------------------------------------------------

#Regressao Lasso

#############
# The Lasso #
#############
# Vimos que a regressão ridge, com uma escolha sábia de λ, pode superar os mínimos quadrados, bem como o modelo nulo, no conjunto de dados dos Hitters.
# Agora perguntamos se o lasso pode produzir um modelo mais preciso ou mais interpretável do que a regressão ridge.

library(glmnet)

# creates a design (or model) matrix
x <- model.matrix(PF~., data = dmat_s12) #matriz de desenho excluind coluna 1 (salary)
head(x) # dimensão 395x40
dim(x)
head(PF)
length(PF) # 395 obs
y <- PF

# alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
grid <- 10^seq(10, -2, length=100)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid, family = "binomial")
plot(lasso.mod)


 #------------------------------------------------------------------------------------------------------------------------
#Grafico dos coeficientes
library(glmnet)
library(ggplot2)
library(reshape2)


# Extrai os coeficientes (sem intercepto)
coefs <- as.matrix(lasso.mod$beta)
lambda_vals <- lasso.mod$lambda

# Converte para data frame no formato long
df <- as.data.frame(coefs)
df$Predictor <- rownames(df)
df_long <- melt(df, id.vars = "Predictor", variable.name = "LambdaIndex", value.name = "Coefficient")

# Adiciona coluna com valores de lambda
df_long$Lambda <- rep(lambda_vals, each = nrow(coefs))
df_long <- df_long[df_long$Coefficient != 0, ]  # remove coef zero p/ clareza
df_long0 <- df_long[df_long$Coefficient == 0, ] #df so com 0

# Gráfico com ggplot
ggplot(df_long, aes(x = log(Lambda), y = Coefficient, color = Predictor)) +
  geom_line(aes(group = Predictor), linewidth = 1) +
  geom_text(data = subset(df_long, Lambda == min(Lambda)), 
            aes(label = Predictor), hjust = 0, size = 3, show.legend = FALSE) +
  labs(title = "Trajetória dos coeficientes Lasso",
       x = "log(Lambda)", y = "Coeficiente") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(log(df_long$Lambda)), max(log(df_long$Lambda)))  # espaço p/ texto


#------------------------------------------------------------------------------------------------------------------------



# We can see from the coefficient plot that depending on the choice of tuning parameter, some of the coefficients will be exactly equal to zero. 

# We now perform cross-validation and compute the associated test error.
set.seed(3030)
cv.out <- cv.glmnet(x,y,alpha=1, lambda = grid, family = "binomial", nfolds = 10)
plot(cv.out) #gráfico da validação cruzada para escolher o melhor valor de lambda na regressão Lasso
bestlam <- cv.out$lambda.min
bestlam #0.0231013
log(bestlam)# -3.767867
simplam <- cv.out$lambda.1se
log(simplam) #-2.37236

lasso.mod.best <- glmnet(x, y, alpha=1, lambda=bestlam, family = "binomial")
lasso.coef <- predict(lasso.mod.best, type="coefficients", s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

#Variaveis Anuladas: School, address, famsize, Pstatus, Medu, Fedu, Fjob, reason, guardian, traveltime, studytime, paid, activities, nursery, higher, internet, famrel, freetime, Dalc, Walc 

lasso.mod.simp <- glmnet(x, y, alpha=1, lambda=simplam, family = "binomial")

lasso.coef.simp <- predict(lasso.mod.simp, type="coefficients", s=simplam)
lasso.coef.simp
lasso.coef.simp[lasso.coef.simp!=0]

#Variaveis Anuladas: Todas exceto failures

#------------------------------------------------------------------------------------------------------------------------

#Previsão com Regressão Lasso e K-fold 10

library(glmnet)
library(caret)  # para confusionMatrix

set.seed(3030)
folds <- sample(rep(1:10, length.out = nrow(x)))  # 10 folds

y_true <- c()
y_pred <- c()

for (k in 1:10) {
  train <- which(folds != k)
  test <- which(folds == k)
  
  model <- glmnet(x[train, ], y[train], alpha = 1, lambda = bestlam, family = "binomial")
  prob <- predict(model, newx = x[test, ], s = bestlam, type = "response")
  pred <- ifelse(prob > 0.5, 1, 0)
  
  y_true <- c(y_true, y[test])
  y_pred <- c(y_pred, pred) #acrescenta as previsões
} 

# Accuracy
acc <- mean(y_true == y_pred)
cat("Accuracy média (10-fold):", round(acc, 4), "\n") #acc = 70.63% - modelo com 12 variaveis 

# Matriz de confusão
conf_mat <- confusionMatrix(factor(y_pred), factor(y_true))
print(conf_mat)

#Previsão de Lasso com modelo mais simples e K-fold 10

set.seed(3030)
folds <- sample(rep(1:10, length.out = nrow(x)))  # 10 folds

y_true_simp <- c()
y_pred_simp <- c()

for (k in 1:10) {
  train <- which(folds != k)
  test <- which(folds == k)
  
  model2 <- glmnet(x[train, ], y[train], alpha = 1, lambda = simplam, family = "binomial")
  prob2 <- predict(model2, newx = x[test, ], s = simplam, type = "response")
  pred2 <- ifelse(prob2 > 0.5, 1, 0)
  
  y_true_simp <- c(y_true_simp, y[test])
  y_pred_simp <- c(y_pred_simp, pred2)
}

# Accuracy
acc <- mean(y_true_simp == y_pred_simp)
cat("Accuracy média (10-fold) do Modelo Simples:", round(acc, 4), "\n") #acc = 69.11% - modelo com 1 variavel (failures) 

# Matriz de confusão
conf_mat <- confusionMatrix(factor(y_pred_simp), factor(y_true_simp))
print(conf_mat)

#------------------------------------------------------------------------------------------------------------------------

#testar o modelo com melhor lambda numa divisão de dataset 70/30

set.seed(3030)

n <- nrow(x) #395

# Índices para treino (70%)
train <- sample(1:n, size = round(0.7 * n))


# Treina o modelo com os dados de treino
lasso.mod.best.train <- glmnet(x[train, ], y[train], alpha = 1, lambda = bestlam, family = "binomial")

# Previsões no conjunto de teste
prob.test <- predict(lasso.mod.best.train, newx = x[-train, ], s = bestlam, type = "response")
pred.test <- ifelse(prob.test > 0.5, 1, 0)

# Accuracy
accuracy <- mean(pred.test == y[-train])
cat("Accuracy no conjunto de teste:", round(accuracy, 4), "\n") #acc = 63.87%

# Matriz de confusão
conf_mat <- confusionMatrix(factor(pred.test), factor(y[-train]))
print(conf_mat)

#testar o modelo com lambda do modelo mais simples numa divisão de dataset 70/30

set.seed(3030)


# Treina o modelo com os dados de treino
lasso.mod.simp.train <- glmnet(x[train, ], y[train], alpha = 1, lambda = simplam, family = "binomial")

# Previsões no conjunto de teste
prob.test.simp <- predict(lasso.mod.simp.train, newx = x[-train, ], s = simplam, type = "response")
pred.test.simp <- ifelse(prob.test.simp > 0.5, 1, 0)

# Accuracy
accuracy <- mean(pred.test.simp == y[-train])
cat("Accuracy no conjunto de teste:", round(accuracy, 4), "\n") #acc = 62.18%

# Matriz de confusão
conf_mat <- confusionMatrix(factor(pred.test.simp), factor(y[-train]))
print(conf_mat)

#------------------------------------------------------------------------------------------------------------------------

#Arvores de Decisão
library(tree)
?tree

dmat_s123 <- dmat_s12
dmat_s123[] <- lapply(dmat_s12, function(x) {
  if (is.character(x)) as.factor(x) else x
})
summary(dmat_s123)

#arvore treinada com todas as observações
tree.dmat = tree(as.factor(PF)~., data = dmat_s123)
summary(tree.dmat) #acc = 77.22%

plot(tree.dmat)
?text
text(tree.dmat,pretty=1, cex=0.7)

#arvore com treino/teste de 70/30
set.seed(3030)
n <- nrow(dmat_s123)
train <- sample(1:n, size = round(0.7 * n))

dmat.test = dmat_s123[-train,]
PF.test = PF[-train]

tree.dmat.train = tree(as.factor(PF)~., dmat_s123, subset=train)
plot(tree.dmat.train)
text(tree.dmat.train, pretty = 1, cex = 0.7)
tree.pred = predict(tree.dmat.train, dmat.test, type="class")
table(tree.pred, PF.test)
(21+63)/(21+63+35) #acc = 70.58%

# Matriz de confusão
conf_mat <- confusionMatrix(factor(tree.pred), factor(PF[-train]))
print(conf_mat)

#arvore treinada todas as observações e avaliada com cv K-fold = 10
set.seed(3030)
?cv.tree
cv.dmat = cv.tree(tree.dmat, FUN=prune.misclass, K=10)
names(cv.dmat)
cv.dmat

windows()
par(mfrow=c(1,2))
plot(cv.dmat$size, cv.dmat$dev, type="b")
plot(cv.dmat$size, cv.dmat$dev, type = "b",
     xlab = "Tree Size (Number of Terminal Nodes)",
     ylab = "Cross-Validation Classification Error",
     main = "Cross-Validation Error vs. Tree Size")
plot(cv.dmat$k, cv.dmat$dev, type="b")

#-------------------------------------------------------------------------------------------------
#2 folhas
?prune.misclass
prune.dmat = prune.misclass(tree.dmat, best=2) # apenas vai ter failures (2 folhas)
plot(prune.dmat)
text(prune.dmat, pretty=0)
tree.pred = predict(prune.dmat,type="class")
table(tree.pred, PF)
(234+52)/(286+31+78) #72.40%
# Matriz de confusão
conf_mat <- confusionMatrix(factor(tree.pred), factor(PF))
print(conf_mat)

#Em dataset 70/30:
tree.pred = predict(prune.dmat, dmat.test, type="class")
table(tree.pred, PF.test)
(17+68)/(17+68+34) #acc = 71.43%
# Matriz de confusão 70/30
conf_mat <- confusionMatrix(factor(tree.pred), factor(PF.test))
print(conf_mat)
#-------------------------------------------------------------------------------------------------

#4 folhas
prune.dmat2 = prune.misclass(tree.dmat, best=4) #4 folhas
plot(prune.dmat2)
text(prune.dmat2, pretty=0)
tree.pred2 = predict(prune.dmat2,type="class")
table(tree.pred2, PF)
(245+49)/(245+49+101) #74.43%
# Matriz de confusão 70/30
conf_mat <- confusionMatrix(factor(tree.pred2), factor(PF))
print(conf_mat)

#Em dataset 70/30:
tree.pred2 = predict(prune.dmat2, dmat.test, type="class")
table(tree.pred2, PF.test)
(17+69)/(17+69+33) #acc = 72.27%
# Matriz de confusão 70/30
conf_mat <- confusionMatrix(factor(tree.pred2), factor(PF.test))
print(conf_mat)
#-------------------------------------------------------------------------------------------------

#10 folhas

prune.dmat3 = prune.misclass(tree.dmat, best=10) #10 folhas
plot(prune.dmat3)
text(prune.dmat3, pretty=0)
tree.pred3 = predict(prune.dmat3,type="class")
table(tree.pred3, PF)
(237+68)/(237+68+62+28) #acc = 77.22%
# Matriz de confusão
conf_mat <- confusionMatrix(factor(tree.pred3), factor(PF))
print(conf_mat)

#Em dataset 70/30:
tree.pred3 = predict(prune.dmat3, dmat.test, type="class")
table(tree.pred3, PF.test)
(17+69)/(17+69+33) #acc = 75.63%
# Matriz de confusão 70/30
conf_mat <- confusionMatrix(factor(tree.pred3), factor(PF.test))
print(conf_mat)

#-------------------------------------------------------------------------------------------------

#RF

library(randomForest)
library(caret)

dmat_rf <- data.frame(dmat_s123, PF = factor(PF, levels = c(0, 1)))

set.seed(3030)
?train
model <- train(PF ~., data = dmat_rf, method = "rf",
               trControl = trainControl("cv", number = 10),
               tuneLength = 38,
               importance =TRUE)
#Best tuning parameter

model$finalModel #OOB Confusion Matrix	Erro interno do Random Forest no próprio treino, usando Out-Of-Bag (OOB) samples



#Best parameter mtry
model$bestTune

model$results

#melhores com mtry = 18 (0.7472), 30 (0.7496) e 36 (0.75481) 

#grafico de Accuracy em função do mtry (numeron de variaveis por split)
library(ggplot2)

# Dados dos resultados de tuning
results <- model$results

# Gráfico
ggplot(results, aes(x = mtry, y = Accuracy)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), 
                width = 1, color = "gray40") +
  labs(title = "Accuracy vs mtry in Random Forest",
       x = "Number of variables considered (mtry)",
       y = "Mean Accuracy (10-fold CV)") +
  theme_minimal()


confusionMatrix(model) # média dos valores de todas as 10 matrizes de confusão, Erro externo calculado por validação cruzada 10-fold feita pelo caret => praticamente igual à acc do melhor modelo (36)



# The importance of each variable can be printed using function importance()
library(randomForest)
?importance
importance(model$finalModel)




# Plots for MeanDecreaseAccuracy and MeanDecreaseGini
?varImpPlot
varImpPlot(model$finalModel, type = 1) #MeanDecrease acc - Impacto médio global na accuracy quando a variável é permutada
varImpPlot(model$finalModel, type = 2) #MeanDecrease gini - Importância da variável medida pela redução média do índice de Gini nas divisões,	Quão útil foi a variável para melhorar a pureza dos nós da árvore
# Function varImp() display the importance variables in percentage
varImp(model) #em 100% das arvores manteve-se glu

#---------------------------------------------------------------------------------------------------
#VARimpplot acc mais bonito

library(ggplot2)

# Extrair a importância
importance_df <- as.data.frame(importance(model$finalModel))
rr
# Adicionar coluna com nome das variáveis
importance_df$Variable <- rownames(importance_df)

# Focar apenas no MeanDecreaseAccuracy
importance_plot <- importance_df[, c("Variable", "MeanDecreaseAccuracy")]

# Ordenar pelas mais importantes
importance_plot <- importance_plot[order(-importance_plot$MeanDecreaseAccuracy), ]

# Escolher as top 20 variáveis
importance_top20 <- head(importance_plot, 20)

# Plot
ggplot(importance_top20, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                             y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # gira o gráfico para ficar horizontal
  labs(title = "Top 20 Variáveis mais Importantes na Random Forest",
       x = "Variável",
       y = "Decréscimo Médio de Accuracy") +
  theme_minimal()
#---------------------------------------------------------------------------------------------------

