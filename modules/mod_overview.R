
# Module UI
overview_ui <- function(id){
    ns <- NS(id)
    

    
    tagList(
        
        panel(
            
            fluidRow(
                box(title = glue("Dados sobre Covid-19 no Brasil"),
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    
                    # ValueBox dos dados de evoluçaõ do Covid 
                    column(12,
                           valueBoxOutput(ns("info6"), width = 4),
                           valueBoxOutput(ns("info5"), width = 4),
                           valueBoxOutput(ns("info1"), width = 4) # %>%  withSpinner(type = 4, color = '#405f83'),
                           
                    ), column(12,
                          valueBoxOutput(ns("info2"), width = 4),
                          valueBoxOutput(ns("info3"), width = 4),
                          valueBoxOutput(ns("info4"), width = 4) 
                        )
                    
                )

            ),
            
            fluidRow(
                
                box(title  = "Mapa do Brasil",
                    width  = 12,
                    status = "warning",
                    solidHeader = TRUE,
                     leafletOutput(ns("mapa_covid"))  %>%  withSpinner(type = 4, color = '#0e51a1')
                    
                 )
            ),
            fluidRow(
                box(width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                sliderTextInput(
                    inputId = ns("data_br"),
                    label   = "Data (filtro para gráficos e tabela)",
                    choices = date_covid_list %>% rev(),
                    grid = TRUE,
                    selected = c(last(date_covid_list), first(date_covid_list))
                ),
                br()
                )
            ),
            
            fluidRow(
                # Tabela 
                box(width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    title = "Tabela",
                    DTOutput(ns("tabela_cov")) %>%  withSpinner(type = 4, color = '#0e51a1')
                )
            ),
            
            # Gráficos
            fluidRow(
                tabBox(width = 12,
                    tabPanel(title = "Casos confirmados",
   
                        plotlyOutput(ns("estados_covid")) %>%  withSpinner(type = 4, color = '#0e51a1')
                    ), 
                    tabPanel(title = "Mortes confirmadas",
                             plotlyOutput(ns("mortes_covid")) %>%  withSpinner(type = 4, color = '#0e51a1')
                             
                    ), 
                    tabPanel(title = "Casos confirmados x Mortes confirmadas",
                             selectInput(ns("uf_comparacao"),
                                         "Selecione a UF:",
                                         choices =  state_list_br,
                                         selected = "",
                                         width = '30%'),
                             plotlyOutput(ns("confirmadosxmortes")) %>%  withSpinner(type = 4, color = '#0e51a1')
                    )
                )
                ),
            
            fluidRow(
                tabBox(
                    width = 12,
                tabPanel(
                    title = "Correlação dos dados",
                    plotOutput(ns("correlacao")) %>%  withSpinner(type = 4, color = '#0e51a1')
                ),
                tabPanel(
                    title = "Frequência de distribuição das categorias",
                    plotOutput(ns("histo")) %>%  withSpinner(type = 4, color = '#0e51a1')
                ) 
                # tabPanel(
                #     title = "Previsão",
                #     plotOutput(ns("predict")) %>%  withSpinner(type = 4, color = '#0e51a1')
                # )
            )
            ), 
            fluidRow(
                box(
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    title = "Regressão linear",
                    verbatimTextOutput(ns("regressao")) %>%  withSpinner(type = 4, color = '#0e51a1')
                ),
                
                box(
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    title = "Residuos",
                    selectInput(ns("predict_uf"),
                                "Selecione a UF:",
                                choices =  state_list_br,
                                selected = "",
                                width = '30%'),
                    plotlyOutput(ns("plot_residuos")) %>%  withSpinner(type = 4, color = '#0e51a1')
                    
                )
            )
            

    )
    )
}

# Module Server
overview_server <- function(input, output, session, uf_selected){
    
    uf_selected
    
    output$info1 <- renderValueBox({
        value1 <- df_brasil$confirmed %>% format(big.mark = ".")
        valueBox(
            "Casos confirmados",
            value1,
            icon = icon("syringe") 
        )
        
    })
    
    output$info2 <- renderValueBox({
        value2 <- df_brasil$deaths %>% format(big.mark = ".")
        valueBox(
            "Mortes confirmadas",
            value2,
            icon = icon("skull")
        )
        
    })
    
    output$info3 <- renderValueBox({
        value3 <- df_brasil$death_rate # %>% format(big.mark = ".")
        valueBox(
            "Taxa de Mortalidade",
            paste(value3 %>% round(2), " %"),
            icon = icon("fas fa-chart-line")
        )
        
    })
    
    
    output$info4 <- renderValueBox({
        value4 <- df_brasil$confirmed_per_100k_inhabitants %>% format(big.mark = ".")
        valueBox(
            "Confirmados por 100k",
            value4,
            icon = icon("syringe")
        )
        
    })
    
    output$info5 <- renderValueBox({
        value5 <- df_brasil$estimated_population_2019 %>% format(big.mark = ".")
        valueBox(
            "População estimada",
            value5,
            icon = icon("users")
        )
        
    })
    
    output$info6 <- renderValueBox({
        value6 <- date_cov 
        valueBox(
            "Última coleta de dados",
            value6,
            icon = icon("calendar-alt")
        )
        
    })
    
    mapa_reactive <- reactive({
        uf_input <- uf_selected()

        df <- df_covid %>% mutate(state = as.character(state))
        
        if(uf_input == state_list_br){
            df <- df %>% 
                dplyr::filter(date == df$date %>% first())
        }  else {
            df <- df %>% 
                filter(state %in% uf_input) %>% 
                dplyr::filter(date == df$date %>% first())
        }
        
        
        df
    
    })
    
    # Mapa do Brasil
    
    observeEvent(uf_selected(),{
        uf <- uf_selected()
        if(is.null(df)){
            df <- data.frame()
            validate(need(nrow(df) != 0, 'Sem Dados.'))
        }
    })
    
    output$mapa_covid <- renderLeaflet({
        
        df_mapa <- mapa_reactive()
        
        
        
        colnames(df_mapa) <- c("date" ,  "UF",  "city" , "place_type" , "confirmed", "deaths",
                               "is_last", "estimated_population_2019", "city_ibge_code",
                               "confirmed_per_100k_inhabitants", "death_rate")  
        
        df1 <- ibge %>% left_join(df_mapa, by = "UF")
        df2 <- df1 %>% left_join(centroids, by = "UF")
        map <- merge(shp, df2, by.x = "CD_GEOCUF", by.y = "codigo.uf") %>%
            clean_names()
        
        
        # Paleta de cores
        pal <- colorNumeric(palette = "Blues", domain = map$confirmed)
        
        #map$urfs <- lapply(map$urfs, paste, collapse = "<br>")
        
        state_popup <- paste0("<strong>Estado: </strong>",
                              map$nm_estado,
                              "<br><strong>Período: </strong>",
                              map$date,
                              "<br><strong>Casos confirmados: </strong>",
                              prettyNum(map$confirmed, big.mark = ".", decimal.mark = ","),
                              "<br><strong>Mortes confirmadas: </strong>",
                              prettyNum(map$deaths, big.mark = ".", decimal.mark = ","),
                              "<br><strong>Casos por 100 mil habitantes: </strong>",
                              prettyNum(map$confirmed_per_100k_inhabitants %>% round(), big.mark = ".", decimal.mark = ",")
                              # "<br><strong>Portos: </strong><br>",
                              # map$urfs
        )
        
        # Label
        labels <- sprintf("<strong>%s</strong><br/>%s",
                          map$nm_estado,
                          paste0(prettyNum(round(map$confirmed,2),
                                           big.mark = ".",
                                           decimal.mark = ","), "")) %>%
            
            lapply(htmltools::HTML)
        
        #Mapa
        leaflet(data = map) %>%
            addTiles(options = tileOptions(minZoom = 3.5, maxZoom = 6)) %>%
            setMaxBounds(lng1 = -90, lat1 = 10, lng2 = 8, lat2 = -35) %>%
            
            addPolygons(label = labels,
                        fillColor = ~pal(confirmed), fillOpacity = 1,
                        color = "#BDBDC3", weight = 1,
                        popup = state_popup,
                        popupOptions = popupOptions(maxWidth = 350),
                        highlightOptions = highlightOptions(color = "black",
                                                            weight = 2,
                                                            bringToFront = TRUE)) %>%
            
            addLegend(position = "bottomright", title = "N° de casos confirmados", na.label = "Sem dados",
                      pal = pal, values = ~confirmed, opacity = 1.0) %>%
            
            addLabelOnlyMarkers(lng = ~lon, lat = ~lat, label = ~uf,
                                labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
        
        
    })
    
    
    estados_input <- reactive({
        uf_input <- uf_selected()
        data_input <- input$data_br

        df <- df_covid
        
            df <- df %>% 
                filter(state %in% uf_input) %>% 
                filter(date >= data_input[1]) %>%
                filter(date <= data_input[2])
    
        validate(need(nrow(df) != 0, 'Sem Dados.'))
            
        df
    })

    # Plot do gráfico do Brasil
    output$estados_covid <- renderPlotly({
        
        df <- estados_input()
        
        p <- plot_ly(df, x = ~date , y = ~confirmed, type = 'scatter',
                     mode = 'lines+markers',
                     color = as.character(df$state),
                     colors = colorRampPalette(brewer.pal(12, "Paired"))(27)[-c(1,2)],
                     hoverinfo = 'text',
                     text = ~paste("<b>Categoria:</b>", "Evolução do contágio no Brasil",
                                   "\n<b>Período:</b>",  df$date,
                                   "\n<b>Origem:</b>", df$state,
                                   "\n<b>Casos confirmados:</b>", df$confirmed %>% format(big.mark = "."),
                                   "\n<b>Mortes confirmadas:</b>", df$deaths %>% format(big.mark = "."),
                                   "\n<b>Taxa de mortalidade:</b>", paste(round(df$death_rate, 2)*100, "%")
                                   
                     ))
        
        p %>%
            layout(title = "",
                   annotations = list(x = 0.45 , y = 1, text = "Casos de COVID 19 no Brasil", showarrow = F,
                                      xref='paper', yref='paper'),
                   #legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                   #margin = list(b = 60, t = 25, pad = 0, l = 20),
                   xaxis = list(title = "",  showgrid = FALSE ),
                   yaxis = list(title = "N° de pessoas infectadas")
            )
        
    })
    

    output$tabela_cov <- renderDT({
        
        uf_input <- uf_selected()
        
        data_input <- input$data_br
        
        df <- df_junto %>% 
            filter(state %in% uf_input) %>% 
            filter(date >= data_input[1]) %>%
            filter(date <= data_input[2]) %>% 
            select(-place_type, - city_ibge_code, - city, -is_last)

        
        colnames(df) <- c("Data", "Origem", "População", "Confirmados", "Mortes", "Taxa de Mortalidade", "Confirmados por 100k")
        

        validate(need(nrow(df) != 0, 'Sem Dados.'))
        
        datatable(df, options = list(scrollX = TRUE))
        
    })
    
    output$mortes_covid <- renderPlotly({
        
        uf_input <- uf_selected()
        
        data_input <- input$data_br
        
        df <- df_covid
        
        df <- df %>% 
            filter(state %in% uf_input) %>% 
            filter(date >= data_input[1]) %>%
            filter(date <= data_input[2])
        
        validate(need(nrow(df) != 0, 'Sem Dados.'))
        
        p <- plot_ly(df, x = ~date , y = ~deaths, type = 'scatter',
                     mode = 'lines+markers',
                     color = as.character(df$state),
                     colors = colorRampPalette(brewer.pal(12, "Paired"))(27)[-c(1,2)],
                     hoverinfo = 'text',
                     text = ~paste("<b>Categoria:</b>", "Mortes confirmadas",
                                   "\n<b>Período:</b>",  df$date,
                                   "\n<b>Origem:</b>", df$state,
                                   "\n<b>Mortes confirmadas:</b>", df$deaths %>% format(big.mark = ".")
                                   
                     ))
        
        p %>%
            layout(title = "",
                   annotations = list(x = 0.45 , y = 1, text = " Mortes confirmadas", showarrow = F,
                                      xref='paper', yref='paper'),
                   # legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                   # margin = list(b = 60, t = 25, pad = 0, l = 20),
                   xaxis = list(title = "",  showgrid = FALSE ),
                   yaxis = list(title = "N° de mortes")
            )
        
        
    })
    
    
    output$confirmadosxmortes <- renderPlotly({
        
        uf_input <- input$uf_comparacao
        
        data_input <- input$data_br
        
        df <- df_covid
        
        df <- df %>% 
            filter(state == uf_input) %>% 
            filter(date >= data_input[1]) %>%
            filter(date <= data_input[2])
        
        validate(need(nrow(df) != 0, 'Sem Dados.'))
        
        p <- plot_ly(df, x = ~date , y = ~confirmed, type = 'scatter',
                     mode = 'lines+markers',
                     name = "Casos confirmados",
                     #color = as.character(df$state),
                     #colors = colorRampPalette(brewer.pal(12, "Paired"))(27)[-c(1,2)],
                     hoverinfo = 'text',
                     text = ~paste("<b>Categoria:</b>", glue("Casos confirmados em {uf_input}"),
                                   "\n<b>Período:</b>",  df$date,
                                   "\n<b>Origem:</b>", df$state,
                                   "\n<b>Casos confirmados:</b>", df$confirmed %>% format(big.mark = ".")
                                  # "\n<b>Mortes confirmadas:</b>", df$deaths %>% format(big.mark = ".")
                                   
                     )) %>% 
            add_trace(y = ~deaths, type = 'scatter',
                      mode = 'lines+markers',
                      name = "Mortes confirmadas",
                      #color = as.character(df$state),
                      #colors = colorRampPalette(brewer.pal(12, "Paired"))(27)[-c(1,2)],
                      hoverinfo = 'text',
                      text = ~paste("<b>Categoria:</b>", glue("Mortes confirmadas em {uf_input}"),
                                    "\n<b>Período:</b>",  df$date,
                                    "\n<b>Origem:</b>", df$state,
                                     "\n<b>Mortes confirmadas:</b>", df$deaths %>% format(big.mark = "."))
            )
        
        p %>%
            layout(title = "",
                   annotations = list(x = 0.45 , y = 1, text = "Casos confirmados X Mortes confirmadas", showarrow = F,
                                      xref='paper', yref='paper'),
                   legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                   margin = list(b = 60, t = 25, pad = 0, l = 20),
                   xaxis = list(title = "",  showgrid = FALSE ),
                   yaxis = list(title = "N° de pessoas infectadas X N° de mortes")
            )
        
        
    })
    
    output$regressao <- renderPrint({

        df <- df_covid

        df_predict <- lm(confirmed ~ deaths, df)
        resultado <- summary(df_predict)
        
        resultado

    })
    
    output$correlacao <- renderPlot({
        
        df <- df_covid %>% 
        select(-place_type, - city_ibge_code, - city, -is_last)
        
        plot_correlation(na.omit(df), maxcat = 5L)
        
    })
    
    output$plot_residuos <- renderPlotly({
        
        uf_input <- input$predict_uf
        
        data_input <- input$data_br
        
        df <- df_covid %>% 
            filter(state == uf_input) %>% 
            select(-place_type, - city_ibge_code, - city, -is_last) 
        
        df_predict <- lm(confirmed ~ deaths, df)
        resultado <- summary(df_predict)
        
        resultado
        
        residuals <- stats::residuals(resultado)
        res <- base::as.data.frame(residuals)
        
        data_realizada <- df %>% dplyr::select(date)
        df <- base::cbind(data_realizada, res) 
        
        plotly::plot_ly(df, x = ~date, y = ~residuals, 
                        type = 'scatter', mode = 'lines+markers', 
                        #color = ~residuals,
                        line = base::list(width = 1.5), marker = base::list(size = 4)) 
            
    })
    
    output$histo <- renderPlot({
        
        df <- df_covid %>% 
            select(-place_type, - city_ibge_code, - city, -is_last)
        
        plot_histogram(df)
    })
    
   
    
}

