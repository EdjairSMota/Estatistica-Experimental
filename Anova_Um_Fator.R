metodo_1 <- c(25, 28, 26, 29, 27, 24, 30, 25, 28, 27)
metodo_2 <- c(30, 32, 29, 31, 33, 30, 32, 31, 30, 29)
metodo_3 <- c(20, 22, 19, 21, 23, 18, 20, 22, 21, 19)

# Combinando os dados em um único vetor para a variável dependente
produtividade <- c(metodo_1, metodo_2, metodo_3)

# Criando um fator para a variável independente (Método de Treinamento)
metodo <- factor(c(rep("Metodo_1", 10), rep("Metodo_2", 10), rep("Metodo_3", 10)))

# Criando um data frame (tabela) combinando as duas variáveis
dados_treinamento <- data.frame(produtividade, metodo)

# Opcional: Visualizando os dados com boxplots
boxplot(produtividade ~ metodo, data = dados_treinamento, main = "Produtividade por Método de Treinamento", 
        xlab = "Método de Treinamento", 
        ylab = "Linhas de Código/Dia",  
        col = c("lightblue", "lightgreen", "lightcoral"))

# Teste ANOVA de um fator
# A função aov() é usada para ajustar modelos de Análise de Variância
# O modelo 'produtividade ~ metodo' significa: produtividade explicada pelo método
modelo_anova_produtividade <- aov(produtividade ~ metodo, data = dados_treinamento)

# Exibindo o sumário da ANOVA (estatística F e o valor p para a comparação das médias)
cat("### Sumário da ANOVA ###\n")
summary(modelo_anova_produtividade)
