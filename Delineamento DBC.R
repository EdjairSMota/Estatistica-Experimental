# Dados brutos em vetores
revestimento_X <- c(120, 130, 115, 125)
revestimento_Y <- c(110, 125, 105, 118)
revestimento_Z <- c(100, 115, 95, 108)

# Combinar os tempos de durabilidade em um único vetor
durabilidade <- c(revestimento_X, revestimento_Y, revestimento_Z)

# Criar o vetor para o fator 'Revestimento' (tratamento)
# Repete cada revestimento 4 vezes (uma para cada máquina/bloco)
revestimento <- factor(c(rep("Revestimento_X", 4), rep("Revestimento_Y", 4), rep("Revestimento_Z", 4)))

# Criar o vetor para o fator 'Maquina' (bloco)
# Cada número de máquina aparece 3 vezes (uma para cada revestimento)
maquina <- factor(rep(1:4, 3)) # Repete de 1 a 4, 3 vezes

# Criar o data frame combinado
dados_revestimentos <- data.frame(durabilidade, revestimento, maquina)

# Definir o nível de significância
alpha <- 0.05 # 5% de significância

# Opcional: Visualizar os dados (boxplots podem ser úteis para ter uma ideia, mas para blocos, um gráfico de linha pode ser mais informativo)
# Boxplot da durabilidade por revestimento
boxplot(durabilidade ~ revestimento, data = dados_revestimentos,
        main = "Durabilidade por Tipo de Revestimento",
        xlab = "Revestimento", ylab = "Tempo até a Falha (horas)",
        col = c("lightblue", "lightgreen", "lightcoral"))

### Realização do Teste ANOVA (Análise do DBC) ###

# Realizar a Análise de Variância (ANOVA) para o DBC
# O modelo agora inclui o fator de bloqueio 'maquina'
# 'durabilidade ~ revestimento + maquina' significa: durabilidade explicada pelo revestimento e pela máquina
modelo_anova_dbc <- aov(durabilidade ~ revestimento + maquina, data = dados_revestimentos)

# Exibir o sumário da ANOVA
cat("\n### Sumário da ANOVA para Delineamento em Blocos Casualizados ###\n")
summary(modelo_anova_dbc)



