# Dados brutos das pontuações de percepção eco-friendly
design_x <- c(7, 8, 7, 9, 8, 7, 6, 8, 7, 9)
design_y <- c(5, 6, 5, 7, 6, 6, 5, 7, 6, 5)
design_z <- c(9, 10, 9, 8, 10, 9, 8, 9, 10, 9)

# Combinar todas as pontuações em um único vetor (variável dependente)
pontuacao_eco <- c(design_x, design_y, design_z)

# Criar um vetor para o fator (design da embalagem)
# O R vai tratar "Design_X", "Design_Y", "Design_Z" como os níveis do fator
design_embalagem <- factor(c(rep("Design_X", 10), rep("Design_Y", 10), rep("Design_Z", 10)))

# Criar um data frame combinando as duas variáveis
dados_embalagem <- data.frame(pontuacao_eco, design_embalagem)

# Definir o nível de significância
alpha <- 0.05 # 5% de significância

# Visualizar os dados com boxplots (muito útil para ter uma ideia das médias e variabilidades)
boxplot(pontuacao_eco ~ design_embalagem, data = dados_embalagem,
        main = "Percepção de Eco-amigabilidade por Design de Embalagem",
        xlab = "Design de Embalagem", ylab = "Pontuação de Eco-amigabilidade (1-10)",
        col = c("lightblue", "lightgreen", "lightcoral"))

# Realizar a Análise de Variância (ANOVA)
# O modelo 'pontuacao_eco ~ design_embalagem' significa que
# queremos explicar a pontuação deco-friendly pelas diferenças no design da embalagem.
modelo_anova_eco <- aov(pontuacao_eco ~ design_embalagem, data = dados_embalagem)

# Exibir o sumário da ANOVA
cat("\n### Sumário da ANOVA de Um Fator (Delineamento Inteiramente Casualizado) ###\n")
summary(modelo_anova_eco)

