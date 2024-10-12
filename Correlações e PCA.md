######Abrir a pasta onde encontram-se os dados#########
setwd("C:/Users/leoan/Downloads/Dados_TCC") 

#####Importar os dados via arquivo txt (menos dor de cabeça)####
Data <- read.table("Dados_TCC_PCA.txt", header = TRUE, dec = ".")

###Para plotar um agrfico com distribuição dos tratamentos, 
#é necessário inportar outro conjunto de dados, esses podem conter o nome real dos tratamentos##
Data1 <- read.table ("Suporte_ACP.txt", header = TRUE, dec = ".")
View(Data)
View(Data1)

rownames(Data)=Data$TRAT ##cuidado! esse comando não aceita informações repetidas
                         #referente aos tratamentos, use as indentificações das retições ou blocos#
Data1 <- Data[,-1]

######Análise de correlação########
library(biotools)
library(dplyr)
library(metan)
library(ggplot2)
library(factoextra)
library(FactoMineR)

######Análise de correlação########
df_selecionado <- Data1 %>%
  select(DC, AP, AIE, NFV, NFS, NPT, NTP, PPT, ESP, PSR, CE, DE, NFG, PMG, PROD, PRODP)

Cor <- cor(df_selecionado)

round(Cor, digits = 2)

multcor.test(cor(df_selecionado), Df = nrow(df_selecionado) - 2)

##Gráfico de correlação componetes morfométricos, seus efeitos e redimento####

df_selecionado1 <- Data1 %>%
  select(DC, AP, AIE, NFV, NPT, NTP, PPT, PROD)

CCM <- corr_plot(df_selecionado1, #este é possui uma estetica mais minimalista##
                 alpha.point = 0.6,
                 maxsize = 3.5,
                 minsize = 1.5)

corr_plot(df_selecionado1, #este gráfico possui um aspecto visualmente poluido!##
            shape.point = 21,
            col.point = "black",
            fill.point = "orange",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 3.5,
            minsize = 1.5,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "cyan",
            upper = "scatter",
            lower = "corr",
            diag.type = "density",
            col.diag = "cyan",
            pan.spacing = 0,
            lab.position = "bl")

# Salvar o gráfico em alta resolução
ggsave("grafico_exemplo.png", plot = CCM, width = 10, height = 7, dpi = 300)

##Gráfico de correlação componetes produtivos####
df_selecionado2 <- Data1 %>%
  select(NTP, ESP, CE, DE, NFG, PMG, PROD, PRODP)

par(family = "Times New Roman")

CCP <- corr_plot(df_selecionado2,
                 alpha.point = 0.6,
                 maxsize = 3.5,
                 minsize = 1.5)

# Salvar o gráfico em alta resolução
ggsave("grafico_exemplo.png", plot = CCP, width = 10, height = 7, dpi = 300)


##############################PCA##################################
library(MultivariateAnalysis)

# Selecionar apenas as colunas desejadas
df_selecionado3 <- Data1 %>%
  select(DC, AP, NFV, NFS, NPT, ESP, CE, DE, PMG, PROD, PRODP)

#Uso deste pacote possui um viés exploratório###
ComponentesPrincipais(df_selecionado3,
                      padronizar=TRUE,
                      xlab="PCA 1",
                      ylab="PCA 2")
############
# Executar a Análise de Componentes Principais (PCA) com dados padronizados
pca_result <- prcomp(df_selecionado3,  center = TRUE, scale = TRUE)

# Visualizar o resumo da PCA
summary(pca_result)

# Autovalores
pca_result

# Obter as cargas (loadings)
loadings <- pca_result$rotation
print("Cargas (Loadings):")
print(loadings)

# Obter contribuições das variáveis para os componentes
contrib <- get_pca_var(pca_result)$contrib
print("Contribuições (%):")
print(contrib)

# Obter os scores (coordenadas das observações nos componentes principais)
scores <- pca_result$x
print("Scores:")
print(scores)

# Obter os autovalores (eigenvalues) e variância explicada por cada componente
eigenvalues <- pca_result$sdev^2
variancia_explicada <- eigenvalues / sum(eigenvalues) * 100

# Exibir autovalores e a variância explicada
print("Autovalores (Eigenvalues):")
print(eigenvalues)

print("Variância explicada (%):")
print(variancia_explicada)

# Visualizar as contribuições de cada variável para os componentes principais
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Visualizar autovalores e variância explicada graficamente
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

########################3Uso do pacote factor FactoMineR####################################
# Executar a Análise de Componentes Principais (PCA) com o FactoMineR
pca_result <- PCA(df_selecionado3, scale.unit = TRUE, graph = FALSE)

# 1. Obter as cargas (loadings)
print("Cargas (Loadings):")
loadings <- print(pca_result$var$coord)
round(loadings, digits = 3)

# 2. Obter os scores (coordenadas das observações nos componentes principais)
print("Scores:")
print(pca_result$ind$coord)

# 3. Obter os autovalores (eigenvalues)
print("Autovalores (Eigenvalues):")
eigenvalues <- print(pca_result$eig[, 1])
round(eigenvalues, digits = 3)

# 4. Obter a variância explicada por cada componente
print("Variância explicada (%):")
variância_explicada <- print(pca_result$eig[, 2])
round(variância_explicada, digits = 3)

# 5. Obter as contribuições de cada variável para os componentes principais
print("Contribuições das variáveis (%):")
print(pca_result$var$contrib)

# 6. Visualizar os autovalores (Scree plot)
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# 7. Visualizar a contribuição das variáveis para os componentes principais
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# 8. Visualizar os scores das observações
fviz_pca_ind(pca_result, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#####Apresentação gráfica final#########
fviz_pca_biplot(pca_result, 
                # Individuos
                geom.ind = "point",
                fill.ind = Data1$TRAT, col.ind = "black",
                pointshape = 21, pointsize = 3,
                palette = "jco",
                addEllipses = TRUE, label = "var",
                # Variaveis
                col.var = "contrib", ellipse.level=0.95,
                legend.title = list(fill = "Tratamentos", color = "Contribuição")
)+labs(title =" ", x = "CP1 (33,415%)", y = "CP2 (31,219%)")

