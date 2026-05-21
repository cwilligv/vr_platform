monitor_encuestas_ui <- function(id){
  tagList(
    br(),
    fluidRow(
      column(
        width = 3,
        uiOutput(NS(id, "coaches"))
      ),
      column(
        width = 5,
        align = "left",
        div(
          bslib::layout_columns(
            width = 1/2,
            fillable = T,
            gap = "0px",
            actionButton(NS(id, "filtro_mensual"), label = "MENSUAL", width = "100%", style = "color: #fff; background-color: #006ac2; height: 30px", size = "xs"),
            actionButton(NS(id, "filtro_anual"), label = "ANUAL", width = "100%",  style = "background-color: #f8f9fa; height: 30px", size = "xs"),
            # actionButton(NS(id, "filtro_todos"), label = "TODOS", width = "100%", style = "background-color: #f8f9fa; height: 30px", size = "xs")
          ),
          style = "padding-top: 40px; margin-bottom: -50px"
        )
      ),
      column(
        width = 4,
        uiOutput(NS(id, "empresas"))
      )
    ),
    br(),
    fluidRow(
      column(
        width = 4,
        align = "center",
        style = "z-index: 10",
        div(DT::DTOutput(NS(id, "monitor_avances_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
      ),
      column(
        width = 8,
        align = "center",
        style = "z-index: 10",
        br(),
        br(),
        # radioButtons(NS(id, "monitor_avances_metric_selector"), "", choices = c("N°Capacitados"="Capacitados", "N°Bloques  "="Bloques"), selected = "Capacitados", inline = TRUE),
        div(highchartOutput(NS(id, "monitor_avances_chart")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"))
      )
    )
  )
}

monitor_encuestas_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      filtro_resultados <- reactiveValues(
        mensual = TRUE,
        anual = FALSE
      )
      
      output$coaches <- renderUI({
        ns <- session$ns
        tagList(
          div(selectInput(ns("listado_psicologos"), "Psicologo", choices = get_psicologos(), selected = "Todos"), style = "margin-top:-20px")
        )
      })
      
      output$empresas <- renderUI({
        ns <- session$ns
        tagList(
          tagList(
            div(selectInput(ns("listado_empresas"), "Clientes", choices = get_empresas(session$userData$rol, session$userData$email), selected = as.numeric(session$userData$id_empresa)),style = "margin-top:-20px")
          ))
      })
      
      observeEvent(input$listado_psicologos, {
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      observeEvent(input$filtro_mensual, {
        ns <- session$ns
        filtro_resultados$mensual <- TRUE
        filtro_resultados$anual <- FALSE
        #444
        runjs(paste0('document.getElementById("',ns("filtro_mensual"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_mensual"),'").style.color = "#fff";'))
        runjs(paste0('document.getElementById("',ns("filtro_anual"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_anual"),'").style.color = "#444";'))
      })
      
      observeEvent(input$filtro_anual, {
        ns <- session$ns
        filtro_resultados$mensual <- FALSE
        filtro_resultados$anual <- TRUE
        
        runjs(paste0('document.getElementById("',ns("filtro_mensual"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_mensual"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_anual"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_anual"),'").style.color = "#fff";'))
      })
      # ================= BEGIN: MONITOR =======================
      
      encuestas <- reactive({
        if (as.numeric(input$listado_empresas) == 0) {
          # filtro_s <- c(filter_conditions, paste("1 == 1"))
          filtro_empresa <- expression(1 == 1)
        }else{
          # filtro2 <- c(filter_conditions, paste("id_coach == ", input$listado_psicologos))
          print("filtrando empresa")
          filtro_empresa <- expression(id_empresa == as.numeric(input$listado_empresas))
        }
        
        read.csv("www/resources/encuestas_enriched.csv") %>%
          filter(eval(filtro_empresa)) %>%
          mutate(
            nombres_coach = paste(nombres_coach, apellidos_coach),
            year_month = format(as.Date(fecha_preparacion), "%Y-%m"),
            year = year(fecha_preparacion)
            ) %>% 
          select(id_coach, nombres_coach, id_empresa, fecha_preparacion, year_month, year, score, pregunta_1:pregunta_7) %>% 
          pivot_longer(
            cols = starts_with("pregunta_"),
            names_to = "pregunta",
            values_to = "respuesta"
          ) %>% 
          count(
            id_coach,
            nombres_coach,
            year,
            year_month,
            respuesta
          ) %>% 
          filter(complete.cases(.))
      })
      
      #load responses_df and make reactive to inputs  
      resultados_df <- reactive({
        req(input$listado_empresas)
        print("dentro de resultados reactive")
        dataChangedTrigger()
        input$listado_empresas
        
        encuestas <- read.csv("www/resources/encuestas_enriched.csv") %>%
          mutate(
            nombres_coach = paste(nombres_coach, apellidos_coach),
            year_month = format(as.Date(fecha_preparacion), "%Y-%m"),
            year = year(fecha_preparacion)
          ) %>% 
          select(id_coach, nombres_coach, id_empresa, fecha_preparacion, year_month, year, score)
        
        print(paste0("Coach: ", input$listado_psicologos))
        # Filtro WHERE
        if (input$listado_psicologos == 0) {
          # filtro_s <- c(filter_conditions, paste("1 == 1"))
          filtro_coach <- expression(1 == 1)
        }else{
          # filtro2 <- c(filter_conditions, paste("id_coach == ", input$listado_psicologos))
          
          filtro_coach <- expression(id_coach == as.numeric(input$listado_psicologos))
        }
        
        if (as.numeric(input$listado_empresas) == 0) {
          # filtro_s <- c(filter_conditions, paste("1 == 1"))
          filtro_empresa <- expression(1 == 1)
        }else{
          # filtro2 <- c(filter_conditions, paste("id_coach == ", input$listado_psicologos))
          print("filtrando empresa")
          filtro_empresa <- expression(id_empresa == as.numeric(input$listado_empresas))
        }
        
        if(filtro_resultados$mensual){
          filtro <- expression(eval(filtro_coach) & eval(filtro_empresa))
          # print(paste0("Filtro: ", eval(filtro)))
          
          encuestas %>% 
            filter(eval(filtro)) %>%
            group_by(nombres_coach, year_month) %>% 
            summarise(score = round(mean(score,2))) %>% 
            rename(fecha = year_month) %>% 
            na.omit()
        } else {
          if (filtro_resultados$anual) {
            filtro <- expression(eval(filtro_coach) & eval(filtro_empresa))
            # print(paste0("Filtro: ", eval(filtro)))
            encuestas %>% 
              filter(eval(filtro)) %>%
              group_by(nombres_coach, year) %>% 
              summarise(score = round(mean(score,2))) %>% 
              rename(fecha = year) %>% 
              na.omit()
          }
        }
        
        # dbExecute(pool, 'SET character set "utf8"')
        # tbl <- glue::glue_sql("
        #   with cte_tbl as (
        #   	select x.id_coach, CONCAT(x.nombres_coach, ' ', x.apellidos_coach) as nombres_coach, x.fecha_preparacion, {columna_fecha}, count(distinct x.horario) as cnt_horario, count(x.id_preparacion) as cnt_capacitaciones
        #   	from {`db`}.preparaciones_view x 
        #   	where id_coach is not NULL and id_coach <> 1 and estado in ('capacitado', 'abandona')
        #   	group by 1,2,3,4
        #   )
        #   select id_coach, nombres_coach, fecha, sum(cnt_capacitaciones) as Capacitados, sum(cnt_horario) as Bloques
        #   from cte_tbl
        #   where {filtro2}
        #   group by 1,2,3
        #   order by 3 DESC 
        # ", .con = pool)
        # print(tbl)
        # dbGetQuery(pool, tbl)
      })
      
      output$monitor_avances_table <- DT::renderDataTable({
        print("rendering table resultados")
        
        table <- resultados_df()
        table <- datatable(table,
                           colnames = c("Nombre Coach", "Fecha", "Satisfacción"),
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'none',
                           options = list(searchHighlight = T, searching = F, scrollX = T, autoWidth = F,
                                          language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                          columnDefs = list(
                                            list(
                                              className = 'dt-right',
                                              targets = 1
                                            )
                                          )
                           )
        )
      })
      
      output$monitor_avances_chart <- renderHighchart({
        datos <- encuestas()
        
        # filter by coach
        if (input$listado_psicologos != 0) {
          datos <- datos %>% 
            filter(id_coach == input$listado_psicologos)
          # datos <- encuestas() %>% 
          #   group_by(fecha) %>% 
          #   summarise(
          #     score = mean(score)
          #     # Bloques = sum(Bloques)
          #   ) %>% 
          #   mutate(nombres_coach = 'Todos')
        } #else {
        #   datos <- resultados_df()
        # }
        
        if (filtro_resultados$mensual) {
          # chart <- datos %>% 
          #   arrange(fecha) %>%
          #   hchart(
          #     'line',
          #     hcaes(x = fecha, y = score, group = nombres_coach)
          #   ) %>% 
          #   hc_xAxis(
          #     title = list(text = "Fecha"),
          #     categories = datos$fecha,
          #     tickInterval = 1
          #   ) %>% 
          #   hc_yAxis(
          #     title = list(text = "Satisfacción")
          #   )
          
          # monthly aggregation
          datos <- datos %>% 
            group_by(
              year_month,
              respuesta
            ) %>% 
            summarise(
              number_of_answers = sum(n)
            ) %>% 
            mutate(
              date_order = as.Date(paste0(year_month, "-01")),
              period_label = format_month_spanish(year_month)
            ) %>%
            arrange(date_order, respuesta)
          
          # Get unique periods in chronological order
          period_order <- datos %>%
            select(date_order, period_label) %>%
            distinct() %>%
            arrange(date_order) %>%
            pull(period_label)
          
          x_axis_title <- "Mes"
          
        } else {
          
          # chart <- datos %>% 
          #   mutate(
          #     fecha = as.numeric(fecha)
          #   ) %>% 
          #   arrange(fecha) %>%
          #   hchart(
          #     'line',
          #     hcaes(x = fecha, y = score, group = nombres_coach)
          #   ) %>% 
          #   hc_xAxis(
          #     title = list(text = "Año"),
          #     categories = datos$fecha,
          #     tickInterval = 1
          #   ) %>% 
          #   hc_yAxis(
          #     title = list(text = "Satisfacción")
          #   )
          
          # yearly aggregation
          datos <- datos %>% 
            group_by(
              year,
              respuesta
            ) %>% 
            summarise(
              number_of_answers = sum(n)
            ) %>% 
            mutate(
              period_label = year
            ) %>%
            arrange(year, respuesta)
          
          # Get unique years in chronological order
          period_order <- sort(unique(datos$period_label))
          x_axis_title <- "Año"
        }
        
        # chart
        # Create the stacked bar chart
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = period_order, title = list(text = x_axis_title)) %>%
          hc_yAxis(title = list(text = "Número de Respuestas")) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          hc_colors(c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57"))
        
        # Add series for each category
        answer_categories <- as.character(1:5)
        for (cat in answer_categories) {
          cat_data <- datos %>%
            filter(respuesta == cat) %>%
            select(period_label, number_of_answers)
          
          series_values <- setNames(cat_data$number_of_answers, cat_data$period_label)
          series_ordered <- series_values[as.character(period_order)]
          series_ordered[is.na(series_ordered)] <- 0
          
          hc <- hc %>%
            hc_add_series(
              name = get_category_label(cat),
              data = as.numeric(series_ordered)
            )
        }
        
        hc
      })
    }
  )
}