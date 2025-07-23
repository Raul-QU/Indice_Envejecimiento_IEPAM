library(shiny)
library(dplyr)
library(tidyr)
library(fmsb)
library(ggplot2)
library(ggtext)
library(leaflet)
library(sf)

shinyServer(function(input, output, session) {
  
  observeEvent(input$estado, {
    updateSelectInput(session, "estado_mapa", selected = input$estado)
  })
  
  datos_estado <- reactive({
    datos %>% filter(NOMGEO == input$estado)
  })
  
  output$titulo_estado <- renderText({
    paste("Resumen del estado de", input$estado)
  })
  
  output$tabla_resumen <- renderTable({
    df <- datos_estado() %>%
      filter(año %in% c(2019, 2023)) %>%
      select(año, all_of(DOMINIOS)) %>%
      distinct() %>%
      group_by(año) %>%
      summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3))) %>%
      pivot_longer(-año, names_to = "Dominio", values_to = "Valor") %>%
      pivot_wider(names_from = año, values_from = Valor)
    
    nombres_anos <- sort(intersect(c(2019, 2023), as.numeric(colnames(df)[-1])), decreasing = TRUE)
    colnames(df) <- c("Dominio", paste0("Año ", nombres_anos))
    
    df
  }, align = "lcc")
  
  output$grafico_radar <- renderPlot({
    df <- datos_estado() %>%
      filter(año %in% c(2019, 2023)) %>%
      select(año, all_of(DOMINIOS)) %>%
      distinct()
    
    if (nrow(df) < 2) return()
    
    radar_df <- rbind(RADAR_MAX, RADAR_MIN,
                      df %>% filter(año == 2023) %>% select(-año),
                      df %>% filter(año == 2019) %>% select(-año)) %>%
      as.data.frame()
    rownames(radar_df) <- c("Max", "Min", "2023", "2019")
    
    colors_border <- c("blue", "red")
    radarchart(radar_df,
               axistype = 1,
               pcol = colors_border,
               plwd = 3,
               plty = 1,
               cglcol = "grey",
               cglty = 1,
               axislabcol = "grey",
               caxislabels = seq(0,1,0.2),
               cglwd = 0.8,
               vlcex = 0.8,
               title = paste("Gráfico Radar -", input$estado))
    legend("topright", legend = c("2023", "2019"), bty = "n", pch = 20, col = colors_border, text.col = "black")
  })
  
  output$grafico_evolucion <- renderPlot({
    df <- datos_estado() %>%
      filter(año %in% c(2019, 2023)) %>%
      pivot_longer(cols = all_of(DOMINIOS), names_to = "Dominio", values_to = "Valor") %>%
      filter(!is.na(Valor))
    
    if (nrow(df) == 0) return()
    
    df$Dominio <- factor(df$Dominio, levels = c("General", setdiff(unique(df$Dominio), "General")))
    
    ggplot(df, aes(x = Dominio, y = Valor, fill = factor(año))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
      geom_text(aes(label = sprintf("%.2f", Valor)),
                position = position_dodge(width = 0.9),
                vjust = -0.25, size = 3.8) +
      scale_x_discrete(labels = function(x) {
        ifelse(x == "General", paste0("**", x, "**"), x)
      }) +
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
      labs(x = "Dominio", y = "Índice", fill = "Año") +
      ggtitle(paste("Comparación 2019 vs 2023 -", input$estado)) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = ggtext::element_markdown(angle = 270, hjust = 0.5, vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$comparativa_nl <- renderPlot({
    if (input$estado == "Nuevo León") return(NULL)
    
    df <- datos %>%
      filter(NOMGEO %in% c(input$estado, "Nuevo León"), año == 2023) %>%
      select(NOMGEO, General)
    
    colores <- c("Nuevo León" = "#e41a1c")
    colores[input$estado] <- "#377eb8"
    
    ggplot(df, aes(x = NOMGEO, y = General, fill = NOMGEO)) +
      geom_col(width = 0.5, color = "black") +
      geom_text(aes(label = sprintf("%.2f", General)), vjust = -0.5, size = 5) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Comparativa con Nuevo León -", input$estado),
           x = "Estado", y = "Índice General 2023") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white", color = NA))
  })
  
  mapa_datos <- reactive({
    df <- datos %>%
      filter(año == input$año_mapa) %>%
      select(NOMGEO, Valor = !!sym(input$dominio_mapa)) %>%
      distinct()
    
    estados_shp %>% left_join(df, by = "NOMGEO")
  })
  
  output$mapa_dominio <- renderLeaflet({
    df_mapa <- mapa_datos()
    pal <- colorNumeric("YlOrRd", df_mapa$Valor)
    
    leaflet(df_mapa) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Valor),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste(NOMGEO, "<br>", sprintf("%.3f", Valor)),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = ~Valor,
                title = paste("Índice", input$dominio_mapa, "-", input$año_mapa),
                opacity = 1)
  })
  
})