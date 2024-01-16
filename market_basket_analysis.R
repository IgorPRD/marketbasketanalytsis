
# Algoritmo de Market Basket Analytsis (MBA)

# Carrega os Pacotes

library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(ggplot2)



# Carrega o Arquivo Exportado da Bagy 

csv <- read.csv("exportar_pedidos_2024-01-16T12_06_07.164598Z.csv")
df <- as.data.frame(csv)

# Prepara os Dados e Agrupa os Produtos Vendidos por ID

produtos_por_id <- df %>% 
  group_by(id) %>% 
  summarise(Order.Items__name)

dados <- produtos_por_id %>%
  group_by(id) %>%
  mutate(Contagem = row_number())

dados2 <- spread(dados, key = Contagem, value = Order.Items__name)
dados3 <- dados2[!is.na(dados2$`2`), ]

dft <- dadosf[, 2:3]
dft2 <- as.data.frame(dft)
lista <- as.list(dft)
split <- split(dft$`1`, dft$`2`)


# Execução do Modelo APRIORI 

regras_produto1 <- apriori(split, 
                           parameter = list(conf = 0.5, minlen = 2),
                           appearance = list(rhs = "Lilly", default = "lhs")) 

inspect(head(sort(regras_produto1, by = "confidence"), 5))


# Visualização das Associações

grafico = plot(regras_produto1, method = "graph", control = list(max = 20))

grafico

ggsave("grafo.png", grafico)



