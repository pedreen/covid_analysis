# Tratamento de dados para criar o indicador Brasil

## Dados do IBGE
ibge <- read.csv("www/mapas/estadosibge.csv", header = T) %>% 
    select(codigo.uf = Código.UF, UF) 
## Centroides
centroids <- readRDS("www/mapas/centroids") %>% 
    rename(UF = uf)

## Shape Ufs Brasil
shp <- shape_uf_br <- st_read('www/mapas/shape_ufs.geojson')

# Carregando base de dados do Covid 19
# df_covid <- get_corona_minsaude()
# saveRDS(df_covid, "www/base_covid")
df_covid <- readRDS("www/base_covid") %>% 
    mutate(state = state %>% as.character())

# Variável de datas da base

date_covid_list <- df_covid %>% pull(date) %>% unique() 

# Lista de estados 

state_list_br <- df_covid %>% pull(state) %>% unique() %>% as.character() %>%  unlist()

# Criando DF do Brasil
date_cov <- df_covid$date[1] 

uf_cov <- "BR"

filter_cov <- df_covid %>% 
    filter(date == df_covid$date[1]) 

confirmed_cov <- sum(filter_cov$confirmed) %>% as.numeric()

death_cov <- sum(filter_cov$deaths) %>% as.numeric()

death_rate_cov <- sum(filter_cov$death_rate) %>% as.numeric()

estimated_population_2019_br <- sum(filter_cov$estimated_population_2019) %>% as.numeric()

confirmed_per_100k_inhabitants_cov <- sum(filter_cov$confirmed_per_100k_inhabitants) %>% as.numeric() %>% round()

df_brasil <- data.frame(list(date_cov, uf_cov, "", "", confirmed_cov, death_cov, "", estimated_population_2019_br, "",
                          confirmed_per_100k_inhabitants_cov, death_rate_cov))

colnames(df_brasil) <- c("date" ,  "state",  "city" , "place_type" , "confirmed", "deaths",
                         "is_last", "estimated_population_2019", "city_ibge_code",
                         "confirmed_per_100k_inhabitants", "death_rate")    

df_junto <- rbind(df_brasil, df_covid)
