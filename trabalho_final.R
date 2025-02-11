# ---
# title: "Trabalho Final - AED - FGV"
# author: "Johnny Correia Kung"
# date: "2024-07-23"
# Código em R
# ---

# Questões
  
# 1) Escolha 5 variáveis e apresente as seguintes medidas resumos: Q1, Q2, Q3, média, mínimo e máximo. Dica: função summary()
# 2) Mostre como as variáveis que você escolheu em (1) estão distribuídas, utilizando um boxplot. É possível identificar alguma que tenha comportamento aproximadamente Normal utilizando os boxplots. Caso positivo, quais são estas variáveis e por quê foi possível identificar o comportamento aproximadamente normal?
# 3) Apresente uma Matriz de Correlação com o respectivo heatmap entre todas as variáveis quantitativas.
# 4) É possível inferir que alguma dessas variáveis seja a causa da taxa de crimes nas cidades analisadas? Por quê?
# 5) Repita a análise realizada em (3), separando os dados por região. Houve alguma mudança, em termos de força e sentido da correlação entre as variáveis nesses grupos, em relação aos resultados de (3)?
# 6) Ordene o data set e apresente as 50 cidades mais violentas. Salve este resultado em um novo objeto
# 7) Apresente as 50 cidades MENOS violentas. Salve este resultado em um novo objeto 
# 8) Qual região possui mais cidades entre as 50 mais violentas? E entre as menos violentas? Apresente gráficos de barras para corroborar suas análises.
# 9) Repita o estudo de correlação realizado em (3) em cada um dos grupos acima (50 mais violentas e 50 menos violentas). Houve alguma mudança, em termos de força e sentido da correlação entre as variáveis nesses grupos, em relação aos resultados de (3)?
# 10) Crie uma variável “Total de crimes por 100 mil habitantes” em seu data set. Análise a correlação entre esta variável e as demais, bem como sua distribuição. Houve alguma diferença com relação a variável “Total de Crimes”?
# 11) Analise a distribuição de cada uma das variáveis por meio de um histograma ou gráfico de densidade. Quais são as variáveis que mais se aproximam da normalidade?
# 12) Faça transformações para aproximar a renda per capita de uma distribuição Normal. Mostre o novo gráfico de densidades desta variável transformada. Você consideraria que a nova variável é aproximadamente Normal? Por quê?
# 13) Utilizando a nova variável criada em (12), realize uma padronização dos dados e apresente quais são as cidades com renda percapita acima de 2σ e abaixo de −2σ.


## Inicio do Trabalho  
    
# Instalar pacotes necessarios
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")

# Carregar pacotes
library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)

# Carregar os dados a partir de um arquivo Excel
dados_violencia_eua <- read_excel("C:/Users/johnn/Desktop/Johnny/MBA - FGV/Analise Exploratoria de Dados/dadostrabalho.xls")


  
### Questão 1 ### 

# **Escolha 5 variáveis e apresente as seguintes medidas resumo: Q1, Q2, Q3, média, mínimo e máximo.**


# Mudar o nome das colunas para ficar mais legivel
names(dados_violencia_eua)[10] = "total_crimes"
names(dados_violencia_eua)[11] = "conclusao_ensino_medio"
names(dados_violencia_eua)[13] = "nivel_pobreza"
names(dados_violencia_eua)[14] = "desempregados"
names(dados_violencia_eua)[15] = "renda_per_capita"

nomes_colunas = c(names(dados_violencia_eua)[10], names(dados_violencia_eua)[11], names(dados_violencia_eua)[13], names(dados_violencia_eua)[14], names(dados_violencia_eua)[15])
print(nomes_colunas)
# Selecionar 5 variáveis de interesse (substitua pelos nomes reais das variáveis)
variaveis_selecionadas <- dados_violencia_eua %>%
  select(total_crimes, conclusao_ensino_medio, nivel_pobreza, desempregados, renda_per_capita)

# Obter medidas resumo usando a função summary()
summary_stats <- summary(variaveis_selecionadas)
print(summary_stats)


### Questão 2 ### 

# Criar boxplot para cada variavel selecionada usando um loop
i = 1

# Criar boxplot para a variável total crimes
for (variavel in variaveis_selecionadas){
  # print(variavel)
  plot <- ggplot(variaveis_selecionadas, aes(y = variavel)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = paste("Boxplot da Variável", nomes_colunas[i]), y = "Valores") +
  theme_minimal()

  print(plot)
  i = i + 1
}

### Questão 3 ### 


# Calcular a matriz de correlação
matriz_correlacao <- cor(variaveis_selecionadas, use = "complete.obs")

# Criar o heatmap usando o pacote corrplot
corrplot(matriz_correlacao, method = "color", col = colorRampPalette(c("red", "white", "blue"))(200),
         type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.7, 
         diag = FALSE)

# Renomear tabela regiao -> x16
names(dados_violencia_eua)[17] = "região"


### Questão 5 ### 

# Adicionar dados de região aos dados existentes

variaveis_selecionadas_regiao <- dados_violencia_eua %>%
  select(total_crimes, conclusao_ensino_medio, nivel_pobreza, desempregados, renda_per_capita, região)

# Calcular a matriz de correlação
matriz_correlacao_mais_regiao <- cor(variaveis_selecionadas_regiao, use = "complete.obs")

# Criar o heatmap usando o pacote corrplot
corrplot(matriz_correlacao_mais_regiao, method = "color", col = colorRampPalette(c("red", "white", "blue"))(200),
         type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.8,
         diag = FALSE)

### Questão 6 ### 

# Ordenar dataset por número de crimes
cidades_mais_violentas <- dados_violencia_eua %>%
  arrange(desc(total_crimes)) %>%
  head(50)

### Questão 7 ### 

# Visualizar as 50 cidades mais violentas
cidades_mais_violentas

# Ordenar dataset por número de crimes de forma crescente
cidades_menos_violentas <- dados_violencia_eua %>%
  arrange(total_crimes) %>%
  head(50)

# Visualizar as 50 cidades menos violentas
cidades_menos_violentas

### Questão 8 ### 

# Criar nomes das regiões
nomes_regioes <- c('NE', 'NC', 'S', 'W')

# Função para substituir números por nomes de regiões
substituir_nomes <- function(df, nomes_regioes) {
  df %>%
    mutate(região = factor(região, levels = 1:length(nomes_regioes), labels = nomes_regioes))
}

# Contando regiões entre as cidades mais violentas
regiao_mais_violentas <- cidades_mais_violentas %>%
  count(região) %>%
  substituir_nomes(nomes_regioes) %>%
  arrange(desc(n))

# Contando regiões entre as cidades menos violentas
regiao_menos_violentas <- cidades_menos_violentas %>%
  count(região) %>%
  substituir_nomes(nomes_regioes) %>%
  arrange(desc(n))

# Plotando gráficos de barras
ggplot(regiao_mais_violentas, aes(x = reorder(região, n), y = n, fill = região)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribuição Regional das 50 Cidades Mais Violentas",
       x = "Região",
       y = "Número de Cidades")+
  coord_flip()

ggplot(regiao_menos_violentas, aes(x = reorder(região, n), y = n, fill = região)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribuição Regional das 50 Cidades Menos Violentas",
       x = "Região",
       y = "Número de Cidades") +
  coord_flip()


### Questão 9 ### 


# Extrair nomes das variáveis como vetor de caracteres
nomes_variaveis <- variaveis_selecionadas_regiao$região

# Calcular a matriz de correlação para as 50 cidades mais violentas
matriz_correlacao_mais_violentas <- cidades_mais_violentas %>%
  select(all_of(nomes_variaveis)) %>%
  select(where(is.numeric)) %>%  # Seleciona apenas colunas numéricas
  cor(use = "complete.obs")

# Calcular a matriz de correlação para as 50 cidades menos violentas
matriz_correlacao_menos_violentas <- cidades_menos_violentas %>%
  select(all_of(nomes_variaveis)) %>%
  select(where(is.numeric)) %>%  # Seleciona apenas colunas numéricas
  cor(use = "complete.obs")

# Comparação das correlações
lista_matrizes <- list(
  MaisViolentas = matriz_correlacao_mais_violentas,
  MenosViolentas = matriz_correlacao_menos_violentas
)

# Exibir a lista de matrizes
print(lista_matrizes)


### Questão 10 ###

# Atribuir x4 como população

names(dados_violencia_eua)[5] = "população"

# Criar variável "Total de crimes por 100 mil habitantes"
dados_violencia_eua <- dados_violencia_eua %>%
  mutate(crimes_por_100mil = (total_crimes / população) * 100000)

# Matriz de correlação incluindo a nova variável
cor_matrix_completa <- cor(dados_violencia_eua %>% select_if(is.numeric), use = "complete.obs")

# Plotar heatmap
corrplot(cor_matrix_completa, method = "color", tl.cex = 0.8)

# Analisar distribuição da nova variável usando uma linha de densidade
ggplot(dados_violencia_eua, aes(x = crimes_por_100mil)) +
  geom_density(fill = "lightblue", alpha = 0.3, color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Distribuição de Crimes por 100 mil Habitantes",
    x = "Crimes por 100 mil Habitantes",
    y = "Densidade"
  )