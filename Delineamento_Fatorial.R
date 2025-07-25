# Dados brutos das pontuações de conforto
conforto_10graus_espuma <- c(7, 8, 7, 9, 8, 7, 6, 8, 7, 9)
conforto_10graus_silicone <- c(5, 6, 5, 7, 6, 6, 5, 7, 6, 5)
conforto_20graus_espuma <- c(6, 7, 6, 8, 7, 7, 6, 8, 7, 6)
conforto_20graus_silicone <- c(9, 10, 9, 8, 10, 9, 8, 9, 10, 9)

# Combinar todas as pontuações em um único vetor (variável dependente)
pontuacao_conforto <- c(conforto_10graus_espuma, conforto_10graus_silicone,
                        conforto_20graus_espuma, conforto_20graus_silicone)

# Criar o vetor para o fator 'Angulo de inclinação'
angulo_inclinacao <- factor(c(rep("10_Graus", 20), rep("20_Graus", 20)))

# Criar o vetor para o fator 'Material do apoio para o pulso'
material_apoio <- factor(rep(c(rep("Espuma_Gel", 10), rep("Silicone", 10)), 2))

# Criar o data frame combinado
dados_teclados <- data.frame(pontuacao_conforto, angulo_inclinacao, material_apoio)

# Definir o nível de significância
alpha <- 0.05

# Opcional: Visualizar os dados com um gráfico de interação
# É crucial para entender a interação em delineamentos fatoriais
# install.packages("ggplot2") # Descomente se não tiver
library(ggplot2)
ggplot(dados_teclados, aes(x = angulo_inclinacao, y = pontuacao_conforto, color = material_apoio, group = material_apoio)) +
  stat_summary(fun = mean, geom = "line", aes(group = material_apoio), size = 1) + # Linhas para as médias
  stat_summary(fun = mean, geom = "point", size = 3) + # Pontos para as médias
  labs(title = "Gráfico de Interação: Conforto por Ângulo e Material",
       x = "Ângulo de Inclinação", y = "Pontuação de Conforto",
       color = "Material do Apoio") +
  theme_minimal() +
  scale_color_manual(values = c("Espuma_Gel" = "darkblue", "Silicone" = "darkred")) # Cores personalizadas

### Realização da ANOVA de Dois Fatores ###
# Realizar a Análise de Variância (ANOVA) para o Delineamento Fatorial
# O modelo inclui ambos os fatores (angulo_inclinacao, material_apoio) e a interação entre eles.
# O '*' entre os fatores no modelo cria automaticamente os efeitos principais e o termo de interação.
modelo_anova_fatorial <- aov(pontuacao_conforto ~ angulo_inclinacao * material_apoio, data = dados_teclados)

# Exibir o sumário da ANOVA
cat("\n### Sumário da ANOVA de Dois Fatores (Delineamento Fatorial) ###\n")
summary(modelo_anova_fatorial)

