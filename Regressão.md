######Abrir a pasta onde encontram-se os dados#########
setwd("C:/Users/leoan/Downloads/Dados_TCC") 

#####Importar os dados via arquivo txt (menos dor de cabeça)####
Data <- read.table("Dados_regressao.txt", header = TRUE, dec = ".")
View(Data)

#carregando pacaotes#
library(ExpDes.pt)
library(lme4)
library(car)  # Para VIF e diagnóstico
library(lmtest)  # Para Breusch-Pagan
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)

###colocando o trtamento como nível###
levels(factor(Data$TRAT))

# Converter TRAT em fator
Data$TRAT <- factor(Data$TRAT)

# Ajuste da regressão linear simples para a variável AP (comportamento linear)
linear_AP <- lm(AP ~ TRAT, data = Data)

# Ajuste da regressão quadrática para a variável DC (comportamento quadrático)
quadratic_DC <- lm(DC ~ TRAT + I(TRAT^2), data = Data)

# Ajuste da regressão quadrática para a variável AIE (comportamento quadrático)
quadratic_AIE <- lm(AIE ~ TRAT + I(TRAT^2), data = Data)

# Ajuste da regressão quadrática para a variável NFV (comportamento quadrático)
quadratic_NFV <- lm(NFV ~ TRAT + I(TRAT^2), data = Data)

# Exibir os resultados da ANOVA
anova(linear_AP)
anova(quadratic_DC)
anova(quadratic_AIE)
anova(quadratic_NFV)

# Resumo dos modelos
summary(linear_AP)
summary(quadratic_DC)
summary(quadratic_AIE)
summary(quadratic_NFV)

# Verificação dos pressupostos dos modelos

# 1. Resíduos vs Ajustados (Homocedasticidade)
par(mfrow = c(2, 2))  # Para visualizar múltiplos gráficos
plot(linear_AP)
plot(quadratic_DC)
plot(quadratic_AIE)
plot(quadratic_NFV)
par(mfrow = c(1, 1))

# 2. Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(residuals(linear_AP))
shapiro.test(residuals(quadratic_DC))
shapiro.test(residuals(quadratic_AIE))
shapiro.test(residuals(quadratic_NFV))

# 3. Teste de homocedasticidade (Teste de Breusch-Pagan)
bptest(linear_AP)
bptest(quadratic_DC)
bptest(quadratic_AIE)
bptest(quadratic_NFV)


# 5. Análise de outliers e pontos influentes (Distância de Cook)
cooksd_linear_AP <- cooks.distance(linear_AP)
cooksd_quadratic_DC <- cooks.distance(quadratic_DC)
cooksd_quadratic_AIE <- cooks.distance(quadratic_AIE)
cooksd_quadratic_NFV <- cooks.distance(quadratic_NFV)

plot(cooksd_linear_AP, pch = "*", cex = 2, main = "Distância de Cook - Modelo Linear")
abline(h = 4/(nrow(Data)-length(coef(linear_AP))), col = "red")  # Limite de influência

plot(cooksd_quadratic_DC, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_DC))), col = "red")  # Limite de influência

plot(cooksd_quadratic_AIE, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_AIE))), col = "red")  # Limite de influência

plot(cooksd_quadratic_NFV, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_NFV))), col = "red")  # Limite de influência

##############################################################################################
# Calcular média e erro padrão para cada nível de TRAT
summary_data <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_DC = mean(DC, na.rm = TRUE),
    erro_padrao = sd(DC, na.rm = TRUE) / sqrt(n())
  )

# Ajustar o modelo de regressão quadrática
quadratic_model <- lm(DC ~ TRAT + I(TRAT^2), data = Data)

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_model, newdata = summary_data, interval = "confidence")
summary_data$fit <- predicoes[, "fit"]
summary_data$lwr <- predicoes[, "lwr"]
summary_data$upr <- predicoes[, "upr"]

############################AP###############################
# Ajuste da regressão linear simples para a variável DC (comportamento linear)
linear_model <- lm(AP ~ TRAT, data = Data)

# Calcular média e erro padrão para cada nível de TRAT
summary_data_AP <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_AP = mean(AP, na.rm = TRUE),
    erro_padrao = sd(AP, na.rm = TRUE) / sqrt(n())
  )

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_model, newdata = summary_data_AP, interval = "confidence")
summary_data_AP$fit <- predicoes[, "fit"]
summary_data_AP$lwr <- predicoes[, "lwr"]
summary_data_AP$upr <- predicoes[, "upr"]

##############################AIE#################################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_AIE <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_AIE = mean(AIE, na.rm = TRUE),
    erro_padrao = sd(AIE, na.rm = TRUE) / sqrt(n())
  )

# Ajuste da regressão quadrática para a variável AIE (comportamento quadrático)
quadratic_AIE <- lm(AIE ~ TRAT + I(TRAT^2), data = Data)

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_AIE, newdata = summary_data_AIE, interval = "confidence")
summary_data_AIE$fit <- predicoes[, "fit"]
summary_data_AIE$lwr <- predicoes[, "lwr"]
summary_data_AIE$upr <- predicoes[, "upr"]


##############################NFV#################################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_NFV <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_NFV = mean(NFV, na.rm = TRUE),
    erro_padrao = sd(NFV, na.rm = TRUE) / sqrt(n())
  )

# Ajuste da regressão quadrática para a variável NFV (comportamento quadrático)
quadratic_NFV <- lm(NFV ~ TRAT + I(TRAT^2), data = Data)

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_NFV, newdata = summary_data_NFV, interval = "confidence")
summary_data_NFV$fit <- predicoes[, "fit"]
summary_data_NFV$lwr <- predicoes[, "lwr"]
summary_data_NFV$upr <- predicoes[, "upr"]

###########################Apresentação gráfica#################################

# Definir os valores específicos de TRAT
summary_data$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_AP$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_AIE$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_NFV$TRAT <- c(25156, 33007, 40625, 48242)

# Recriar o gráfico G1 com os valores do eixo X ajustados
G1 <- ggplot(summary_data, aes(x = TRAT, y = media_DC)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_DC - erro_padrao, ymax = media_DC + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Diâmetro do Colmo (mm)", 
       title = "A") +
  annotate("text", x = 35000, y = 21, label = "y = 1.065 + 0.191x - 0.01x²", size = 5, family = "serif") +  # Equação de regressão
  annotate("text", x = 35000, y = 20.5, label = "R² = 0.715", size = 5, family = "serif") +  # Ajuste do R²
  scale_x_continuous(breaks = c(25156, 33007, 40625, 48242)) + # Ajuste do eixo X
  theme(
    text = element_text(size = 16, family = "serif", color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )

# Recriar o gráfico G2 com os valores do eixo X ajustados
G2 <- ggplot(summary_data_AP, aes(x = TRAT, y = media_AP)) + 
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_AP - erro_padrao, ymax = media_AP + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x,  
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Altura de Plantas (m)", 
       title = "B") +
  annotate("text", x = 35000, y = 2.4, label = "y = a + bx", size = 5, family = "serif") +  # Equação de regressão genérica
  annotate("text", x = 35000, y = 2.35, label = "R² = valor", size = 5, family = "serif") +  # Ajuste do R² (substitua 'valor' pelo R² real)
  scale_x_continuous(breaks = c(25156, 33007, 40625, 48242)) + # Ajuste do eixo X
  theme(
    text = element_text(size = 16, family = "serif", color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )

# Recriar o gráfico G3 com os valores do eixo X ajustados
G3 <- ggplot(summary_data_AIE, aes(x = TRAT, y = media_AIE)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_AIE - erro_padrao, ymax = media_AIE + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Altura de Inserção de Espigas (m)", 
       title = "C") +
  annotate("text", x = 35000, y = 1.3, label = "y = 1.065 + 0.191x - 0.01x²", size = 5, family = "serif") +  
  annotate("text", x = 35000, y = 1.28, label = "R² = 0.715", size = 5, family = "serif") + 
  scale_x_continuous(breaks = c(25156, 33007, 40625, 48242)) + # Ajuste do eixo X
  theme(
    text = element_text(size = 16, family = "serif", color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )

# Recriar o gráfico G4 com os valores do eixo X ajustados
G4 <- ggplot(summary_data_NFV, aes(x = TRAT, y = media_NFV)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_NFV - erro_padrao, ymax = media_NFV + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Número de Folhas Verdes", 
       title = "D") +
  annotate("text", x = 35000, y = 8.6, label = "y = 1.065 + 0.191x - 0.01x²", size = 5, family = "serif") +  
  annotate("text", x = 35000, y = 8.4, label = "R² = 0.715", size = 5, family = "serif") +
  scale_x_continuous(breaks = c(25156, 33007, 40625, 48242)) + # Ajuste do eixo X
  theme(
    text = element_text(size = 16, family = "serif", color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold", color = "black")
  )

# Exibir os gráficos
print(G1)
print(G2)
print(G3)
print(G4)

# Combine os gráficos em uma disposição de 2x2
combined_plot <- (G1 | G2) / (G3 | G4)  # 2 gráficos lado a lado por linha

# Salvar a figura como um arquivo PNG
ggsave("graficos_combinados_2x2.png", combined_plot, width = 12, height = 10)

# Também é possível salvar como PDF, se preferir:
ggsave("graficos_combinados_2x2.pdf", combined_plot, width = 12, height = 10)
