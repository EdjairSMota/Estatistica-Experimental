# Dados brutos da durabilidade (número de ciclos de flexão)
material_A <- c(520, 550, 530, 540, 510, 560, 535, 525, 545, 530)
material_B <- c(580, 600, 590, 575, 610, 585, 595, 605, 588, 592)
material_C <- c(500, 515, 490, 505, 510, 495, 508, 500, 512, 498)
material_D <- c(630, 620, 640, 615, 635, 628, 645, 622, 638, 625)

# Combinar todas as durabilidades em um único vetor (variável dependente)
durabilidade <- c(material_A, material_B, material_C, material_D)

# Criar um vetor para o fator (tipo de material)
# É CRUCIAL que o grupo de controle seja o primeiro nível para o Teste de Dunnett funcionar corretamente por padrão.
# Podemos garantir isso com levels = c("Material_A", ...).
tipo_material <- factor(c(rep("Material_A", 10), rep("Material_B", 10), 
                          rep("Material_C", 10), rep("Material_D", 10)),
                        levels = c("Material_A", "Material_B", "Material_C", "Material_D")) # Garante a ordem do controle

# Criar um data frame combinando as duas variáveis
dados_armacoes <- data.frame(durabilidade, tipo_material)

# Definir o nível de significância
alpha <- 0.05 # 5% de significância

# Opcional: Visualizar os dados com boxplots
boxplot(durabilidade ~ tipo_material, data = dados_armacoes,
        main = "Durabilidade por Tipo de Material",
        xlab = "Tipo de Material", ylab = "Ciclos de Flexão",
        col = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrod"))

# Realizar a Análise de Variância (ANOVA)
modelo_anova_durabilidade <- aov(durabilidade ~ tipo_material, data = dados_armacoes)

# Exibir o sumário da ANOVA
cat("### Sumário da ANOVA de Um Fator ###\n")
summary(modelo_anova_durabilidade)

# Certificar-se de ter o pacote 'multcomp' instalado e carregado
# install.packages("multcomp") # Descomente e execute se não tiver
library(multcomp)

# Aplicar o Teste de Dunnett
cat("\n### Resultados do Teste de Dunnett (Comparando com o Material A - Controle) ###\n")
# Use a função 'contrMat()' para criar a matriz de contraste de Dunnett de forma explícita
# Isso garante que o R entenda exatamente as comparações e qual é a base.
dunnett_contrasts <- contrMat(table(dados_armacoes$tipo_material), type = "Dunnett")

# Agora, passe essa matriz de contraste para 'linfct'
teste_dunnett_durabilidade <- glht(modelo_anova_durabilidade,
                                   linfct = dunnett_contrasts)

# Sumário para ver os resultados ajustados
summary(teste_dunnett_durabilidade, test = adjusted("single-step"))

