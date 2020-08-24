df_rn <- df_covid %>% 
    select(date, state, confirmed, deaths, death_rate)

# Semente aleatória para geração dos mesmos resultados
set.seed(101)

# Síntese estatística (primeiro/terceiro quartis, mediana, mínimo/máximo) dos dados
summary(df_rn)

# Verificando se existe algum 'na' na importação
any(is.na(df_rn))

lm1 <- train(confirmed ~ deaths, data = df_rn, method = "lm")
rf1 <- train(confirmed ~ deaths, data = df_rn, method = "rf")

# Treinamento com NeuralNet - repare estamos 
modelo_RNA = neuralnet(equation_model, data = dados_norm_treinamento, hidden = c(5,3), linear.output = TRUE)
modelo_RNA