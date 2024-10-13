######Abrir a pasta onde encontram-se os dados#########
setwd("C:/Users/leoan/Downloads/Dados_TCC") 

#####Importar os dados via arquivo txt (menos dor de cabeça)####
Data <- read.table("Dados_regressao.txt", header = TRUE, dec = ".")
View(Data)

#carregando pacaotes#
library(ExpDes.pt)
library(lme4)
library(car)  # Para VIF e diagnóstico
library(lmtest) # Para Breusch-Pagan
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)

###colocando o trtamento como nível###
levels(factor(Data$TRAT))

# Converter TRAT em fator
Data$TRAT <- factor(Data$TRAT)

# Ajuste da regressão linear simples para a variável CE (comportamento linear)
quadratic_CE <- lm(CE ~ TRAT + I(TRAT^2), data = Data)

# Ajuste da regressão quadrática para a variável DE (comportamento quadrático)
quadratic_DE <- lm(DE ~ TRAT + I(TRAT^2), data = Data)

# Ajuste da regressão quadrática para a variável PMG (comportamento quadrático)
quadratic_PMG <- lm(PMG ~ TRAT + I(TRAT^2), data = Data)

# Ajuste da regressão quadrática para a variável NFG (comportamento linear)
linear_NFG <- lm(NFG ~ TRAT, data = Data)

# Exibir os resultados da ANOVA
anova(quadratic_CE)
anova(quadratic_DE)
anova(quadratic_PMG)
anova(linear_NFG)

# Resumo dos modelos
summary(quadratic_CE)
summary(quadratic_DE)
summary(quadratic_PMG)
summary(linear_NFG)

# Verificação dos pressupostos dos modelos

# 1. Resíduos vs Ajustados (Homocedasticidade)
par(mfrow = c(2, 2))  # Para visualizar múltiplos gráficos
plot(quadratic_CE)
plot(quadratic_DE)
plot(quadratic_PMG)
plot(linear_NFG)
par(mfrow = c(1, 1))

# 2. Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(residuals(quadratic_CE))
shapiro.test(residuals(quadratic_DE))
shapiro.test(residuals(quadratic_PMG))
shapiro.test(residuals(linear_NFG))

# 3. Teste de homocedasticidade (Teste de Breusch-Pagan)
bptest(quadratic_CE)
bptest(quadratic_DE)
bptest(quadratic_PMG)
bptest(linear_NFG)

# 5. Análise de outliers e pontos influentes (Distância de Cook)
cooksd_quadratic_CE <- cooks.distance(quadratic_CE)
cooksd_quadratic_DE <- cooks.distance(quadratic_DE)
cooksd_quadratic_PMG <- cooks.distance(quadratic_PMG)
cooksd_linear_NFG <- cooks.distance(linear_NFG)


plot(cooksd_quadratic_CE, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_CE))), col = "red")  # Limite de influência

plot(cooksd_quadratic_DE, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_DE))), col = "red")  # Limite de influência

plot(cooksd_quadratic_PMG, pch = "*", cex = 2, main = "Distância de Cook - Modelo Quadrático")
abline(h = 4/(nrow(Data)-length(coef(quadratic_PMG))), col = "red")  # Limite de influência

plot(cooksd_linear_NFG, pch = "*", cex = 2, main = "Distância de Cook - Modelo Linear")
abline(h = 4/(nrow(Data)-length(coef(linear_NFG))), col = "red")  # Limite de influência

##############################################################################################
############################CE###############################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_CE <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_CE = mean(CE, na.rm = TRUE),
    erro_padrao = sd(CE, na.rm = TRUE) / sqrt(n())
  )

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_CE, newdata = summary_data_CE, interval = "confidence")
summary_data_CE$fit <- predicoes[, "fit"]
summary_data_CE$lwr <- predicoes[, "lwr"]
summary_data_CE$upr <- predicoes[, "upr"]

############################DE###############################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_DE <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_DE = mean(DE, na.rm = TRUE),
    erro_padrao = sd(DE, na.rm = TRUE) / sqrt(n())
  )

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_DE, newdata = summary_data_DE, interval = "confidence")
summary_data_DE$fit <- predicoes[, "fit"]
summary_data_DE$lwr <- predicoes[, "lwr"]
summary_data_DE$upr <- predicoes[, "upr"]

##############################PMG#################################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_PMG <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_PMG = mean(PMG, na.rm = TRUE),
    erro_padrao = sd(PMG, na.rm = TRUE) / sqrt(n())
  )

# Prever valores ajustados e intervalos de confiança
predicoes <- predict(quadratic_PMG, newdata = summary_data_PMG, interval = "confidence")
summary_data_PMG$fit <- predicoes[, "fit"]
summary_data_PMG$lwr <- predicoes[, "lwr"]
summary_data_PMG$upr <- predicoes[, "upr"]


##############################NFG#################################
# Calcular média e erro padrão para cada nível de TRAT
summary_data_NFG <- Data %>%
  group_by(TRAT) %>%
  summarise(
    media_NFG = mean(NFG, na.rm = TRUE),
    erro_padrao = sd(NFG, na.rm = TRUE) / sqrt(n())
  )


# Prever valores ajustados e intervalos de confiança
predicoes <- predict(linear_NFG, newdata = summary_data_NFG, interval = "confidence")
summary_data_NFG$fit <- predicoes[, "fit"]
summary_data_NFG$lwr <- predicoes[, "lwr"]
summary_data_NFG$upr <- predicoes[, "upr"]

###########################Apresentação gráfica#################################

# Definir os valores específicos de TRAT
summary_data_CE$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_DE$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_PMG$TRAT <- c(25156, 33007, 40625, 48242)
summary_data_NFG$TRAT <- c(25156, 33007, 40625, 48242)


# Recriar o gráfico G1 com os valores do eixo X ajustados
G1 <- ggplot(summary_data_CE, aes(x = TRAT, y = media_CE)) + 
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_CE - erro_padrao, ymax = media_CE + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),  
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Comprimento de Espigas (cm)", 
       title = "A") +
  annotate("text", x = 32000, y = 13.45, label = expression("y = 7,8742"^{ns}*" + 0,000448x"^{ns}*" - 7e-9x"^{2}*"*"), size = 5, family = "serif") +  # Equação de regressão genérica
  annotate("text", x = 32000, y = 13.30, label = expression("R"^{2}*" = 0,9095"), size = 5, family = "serif") +  # Ajuste do R² (substitua 'valor' pelo R² real)
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
G2 <- ggplot(summary_data_DE, aes(x = TRAT, y = media_DE)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_DE - erro_padrao, ymax = media_DE + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Diâmetro de Espigas (mm)", 
       title = "B") +
  annotate("text", x = 32000, y = 42.8, label = expression("y = 32,343** + 0,000787x"^{ns}*" - 1e-8x"^{2}*""^{ns}), size = 5, family = "serif") +  
  annotate("text", x = 32000, y = 42.5, label = expression("R"^{2}*" = 0,4113"), size = 5, family = "serif") + 
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
G3 <- ggplot(summary_data_PMG, aes(x = TRAT, y = media_PMG)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_PMG - erro_padrao, ymax = media_PMG + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = "Massa de 1000 Grãos (g)", 
       title = "C") +
  annotate("text", x = 32000, y = 330, label = expression("y = 258,316* + 0,0086x"^{ns}*" - 2e-7x"^{2}*""^{ns}), size = 5, family = "serif") +  
  annotate("text", x = 32000, y = 324, label = expression("R"^{2}*" = 0,6553"), size = 5, family = "serif") +
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
G4 <- ggplot(summary_data_NFG, aes(x = TRAT, y = media_NFG)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = media_NFG - erro_padrao, ymax = media_NFG + erro_padrao), 
                width = 1000, color = "black", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "black", se = FALSE, size = 0.7) +
  theme_classic() +
  labs(x = expression("População de plantas ha"^{-1}), 
       y = expression("Número de Fileiras de Grãos"), 
       title = "D") +
  annotate("text", x = 32000, y = 13.16, label = "y = 15,4549** - 0,000044x*", size = 5, family = "serif") +  
  annotate("text", x = 32000, y = 13.05, label = expression("R"^{2}*" = 0,8112"), size = 5, family = "serif") +  
  scale_x_continuous(breaks = c(25156, 33007, 40625, 48242)) + 
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
ggsave("graficos_combinados_2x2_P.png", combined_plot, width = 12, height = 10)

# Também é possível salvar como PDF, se preferir:
ggsave("graficos_combinados_2x2_P.pdf", combined_plot, width = 12, height = 10)
