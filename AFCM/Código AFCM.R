install.packages(c("dplyr", "FactoMineR", "factoextra", "ggplot2", "gridExtra", "GDAtools"))
install.packages("plotly")
library(plotly)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(GDAtools)

#Categorizando os dados

Dados1_cat <- Dados1_[, !(colnames(Dados1_) %in% c("Col1"))]


Dados1_cat <- Dados1_cat %>%
  mutate(across(1:10, ~ case_when(
    . <= 4 ~ "baixo",
    . > 4 & . < 8 ~ "médio",
    . >= 8 ~ "alto"
  )))

View(Dados1_cat)


dados=Dados1_cat[,1:10]
dados

#Tabela Lógica

dados.logica=dichotom(dados, out = "numeric")
dados.logica

#Tabela Burt

dados.burt = burt(dados)
View(dados.burt)

#Tabela de burt percentual

diagonal_princ = diag(dados.burt)
diagonal_princ

tabela_percentual <- matrix(0, nrow = nrow(dados.burt), ncol = ncol(dados.burt))

total_individuos <- 32

for (i in 1:nrow(dados.burt)) {
  for (j in 1:ncol(dados.burt)) {
    if (i == j) {
      # Diagonal principal: divide pelo total (32)
      tabela_percentual[i, j] <- (dados.burt[i, j] / total_individuos) * 100
    } else {
      # Fora da diagonal: divide pelo valor da diagonal da linha (i)
      tabela_percentual[i, j] <- (dados.burt[i, j] / diagonal_princ[i]) * 100
    }
  }
}

rownames(tabela_percentual) <- rownames(dados.burt)
colnames(tabela_percentual) <- colnames(dados.burt)



tabela_percentual <- round(tabela_percentual, 1)
View(tabela_percentual)
write.csv2(tabela_percentual, "tabela_percentual")


#AFC

res.mca <- MCA(Dados1_cat, graph = F)


#Autovalores, inercia e inercia total

eig.val <- get_eigenvalue(res.mca); 
round(eig.val,3)

#Scree Plot

fviz_eig(res.mca, xlab = "Dimensão", main= "Scree Plot", linecolor="red",
         ylab = "Variância explicada (%)")


#Resultados para as variáveis
var <- get_mca_var(res.mca)   

#Coordenadas
var$coord[,1:2]

#Contribuições
var$contrib[,1:2]

#Cosseno 2  (Correlação: Categoria x Dimensão)

round(var$cos2,1)
var$cos2

#Contribuicao das variáveis para o dimensao 1 - 15 primeiras

fviz_contrib(res.mca, choice = "var", axes = 1, top = 15) +
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = "Contribuição - 1° Dimensão ", y = "Contribuição (%)")

#Dim 2

fviz_contrib(res.mca, choice = "var", axes = 2, top = 15) +
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = "Contribuição - 2° Dimensão ", y = "Contribuição (%)")

#1o Plano

fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15) +
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = "Contribuição -1° Plano Fatorial ", y = "Contribuição (%)")

#Contribuicao das variáveis

fviz_contrib(res.mca, choice = "var", axes = 1:2) +
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = "Contribuição -1° Plano Fatorial ", y = "Contribuição (%)")

#Categorias no 1 Plano Fatorial

fviz_mca_var(res.mca, repel = F, labelsize = 3, geom = c("point", "text"), pointsize = 2,
             col.var = "darkslategray4")+ theme_light()+
  labs(title = "Categorias - 1o Plano")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits = c(-2,2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  theme(plot.title = element_text(hjust = 0.5))


#Grafico interativo para ajudar na visualizacao das categorias

gr = fviz_mca_var(res.mca, repel = F, labelsize = 3, geom = c("point", "text"), pointsize = 2,
                  col.var = "darkslategray4")+ theme_light()+
  labs(title = "Categorias - 1o Plano")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits = c(-2,2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  theme(plot.title = element_text(hjust = 0.5))
grafico_interativo <- ggplotly(gr)
grafico_interativo
