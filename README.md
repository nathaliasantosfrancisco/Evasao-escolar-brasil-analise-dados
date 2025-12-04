# Análise do Abandono Escolar por Renda (2013–2023)

Este repositório reúne o estudo desenvolvido por Graziela Leite Casoti e Nathalia Santos Francisco sobre a relação entre a renda média e a taxa de abandono escolar no Brasil, no período de 2013 a 2023. O trabalho tem como objetivo avaliar se a renda média domiciliar exerce impacto significativo sobre o abandono escolar, por meio de modelos de regressão linear, testes estatísticos e visualizações gráficas.
## Introdução

O abandono escolar é um dos principais desafios da educação brasileira. Neste estudo, analisamos como a renda média domiciliar influencia a taxa de abandono escolar ao longo dos últimos anos.  
A análise considera dados públicos e aplica técnicas estatísticas para identificar padrões e relações.

## Dataset

- Arquivo utilizado: **AbandonoEscolar_RendaMedia_2013_2023.csv**
  
Fonte: https://www.kaggle.com/datasets/joaoassaoka/taxa-de-abandono-escolar-por-renda-mdia-brasil

## Metodologia

A análise inclui:
- Exploração inicial dos dados;
- Transformação e padronização das variáveis;
- Ajuste de modelos de regressão linear;
- Seleção de variáveis via Stepwise (Forward, Backward, Both);
- Visualização gráfica da relação entre renda e abandono.

## Código utilizado na análise
```r
#Biblioteca
library(readr)
library(dplyr)
library(ggplot2)
library(writexl)
library(broom)
library(writexl)
library(sjPlot)
library(gridExtra)

PATH<-"C:/Users/Nathalia/OneDrive/Documentos/FACULDADE 4° SEMESTRE/Projeto Integrador/Abandono escolar por renda"
setwd(PATH)

arquivo_csv <- "AbandonoEscolar_RendaMedia_2013_2023.csv"
abandono_renda <- read_csv(arquivo_csv)
abandono_renda

dim(abandono_renda) #Dimensão
str(abandono_renda) #Estrutura dados 
summary(abandono_renda) #Estatisticas
names(abandono_renda) #Colunas

print("Primeiras linhas do dataset:")
print(head(abandono_renda))
head(abandono_renda)

#Descobrir qual é a categoria de referencia
str(abandono_renda$Unidade_Geografica)#Tipo
abandono_renda$Unidade_Geografica <- as.factor(abandono_renda$Unidade_Geografica) #Convertendo para factor
levels(abandono_renda$Unidade_Geografica) #A primeira que aparecer é a categoria de ref
abandono_renda$Unidade_Geografica <- relevel(abandono_renda$Unidade_Geografica, ref = "Brasil") #Trocando a ref pra BRASIL

#Modelo completo
modelo_renda <- lm(Taxa_Abandono~., data= abandono_renda)
modelo_renda

str(modelo_renda)#Estrutura dados 
formula(modelo_renda)
summary(modelo_renda)#Estatisticas

#Seleção das das melhores variaveis
modelo_vazio<- lm(Taxa_Abandono~1,
                  abandono_renda)

step_backward <- step(modelo_renda, 
                                    direction = "backward",
                                    trace = 0)
formula(step_backward)

step_forward <- step(modelo_vazio,
                                   direction = "forward",
                                   scope = formula(modelo_renda),
                                   trace = 0)
formula(step_forward)

step_both <- step(modelo_renda,
                         direction = "both",
                         trace = 0)
formula(step_both)

#Vizualização
resultados_limpos <- tidy(step_both)
View(resultados_limpos)

#Gráficos
grafico_renda <- ggplot(abandono_renda, aes(x = Renda_Media, 
                                            y = Taxa_Abandono)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relação entre Renda Média e Taxa de Abandono Escolar",
       x = "Renda Média (R$)",
       y = "Taxa de Abandono (%)") +
  theme_minimal()
grafico_renda

#Exportar arquivos
write_xlsx(resultados_limpos, "Coeficientes_AbandonoEscolar.xlsx")
