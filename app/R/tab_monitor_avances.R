monitor_avances_ui <- function(id){
  tagList(
    br(),
    # h1("Monitor de Avances", style = "font-size: 1.8rem;"),
    fluidRow(
      column(
        width = 3
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
        uiOutput(NS(id, "selector"))
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
        radioButtons(NS(id, "monitor_avances_metric_selector"), "", choices = c("N°Capacitados"="Capacitados", "N°Bloques  "="Bloques"), selected = "Capacitados", inline = TRUE),
        div(highchartOutput(NS(id, "monitor_avances_chart")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"))
      )
    )
  )
}

monitor_avances_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      filtro_resultados <- reactiveValues(
        mensual = TRUE,
        anual = FALSE
      )
      
      output$selector <- renderUI({
        ns <- session$ns
        tagList(
          div(selectInput(ns("listado_psicologos"), "Psicologo", choices = get_psicologos(), selected = "Todos"), style = "margin-top:-20px")
        )
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
      
      #load responses_df and make reactive to inputs  
      resultados_df <- reactive({
        req(input$listado_psicologos)
        print("dentro de resultados reactive")
        dataChangedTrigger()
        
        rv$tab_herramientas_internas_clicked
        
        # updateSelectInput(session, "listado_psicologos", selected = as.numeric(session$userData$id_empresa))
        print(paste0("Coach: ", input$listado_psicologos))
        # Filtro WHERE
        if (input$listado_psicologos == 0) {
          filtro2 <- glue::glue_sql("1 = 1", .con = pool)
        }else{
          filtro2 <- glue::glue_sql("id_coach = {input$listado_psicologos}", .con = pool)
        }
        
        if(filtro_resultados$mensual){
          columna_fecha <- glue::glue_sql("date_format(str_to_date(x.fecha_preparacion, '%Y-%m-%d'), '%Y-%m') as fecha", .con = pool)
        } else {
          if (filtro_resultados$anual) {
            columna_fecha <- glue::glue_sql("year(x.fecha_preparacion) as fecha", .con = pool)
          } else {
            columna_fecha <- glue::glue_sql("NULL as fecha", .con = pool)
          }
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("
          with cte_tbl as (
          	select x.id_coach, CONCAT(x.nombres_coach, ' ', x.apellidos_coach) as nombres_coach, x.fecha_preparacion, {columna_fecha}, count(distinct x.horario) as cnt_horario, count(x.id_preparacion) as cnt_capacitaciones
          	from {`db`}.preparaciones_view x 
          	where id_coach is not NULL and id_coach <> 1 and estado in ('capacitado', 'abandona')
          	group by 1,2,3,4
          )
          select id_coach, nombres_coach, fecha, sum(cnt_capacitaciones) as Capacitados, sum(cnt_horario) as Bloques
          from cte_tbl
          where {filtro2}
          group by 1,2,3
          order by 3 DESC 
        ", .con = pool)
        print(tbl)
        dbGetQuery(pool, tbl)
      })
      
      output$monitor_avances_table <- DT::renderDataTable({
        print("rendering table resultados")
        table <- resultados_df() %>% select(-id_coach) %>% 
          mutate(
            
          )
        
        table <- datatable(table,
                           colnames = c("Nombre Coach", "Fecha", "N°Capacitados", "N° Bloques"),
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
        if (input$listado_psicologos == 0) {
          datos <- resultados_df() %>% 
            group_by(fecha) %>% 
            summarise(
              Capacitados = sum(Capacitados),
              Bloques = sum(Bloques)
            ) %>% 
            mutate(nombres_coach = 'Todos')
        } else {
          datos <- resultados_df()
        }
        
        if (filtro_resultados$mensual) {
          chart <- datos %>% 
            mutate(
              fecha = lubridate::ym(fecha)
            ) %>% 
            hchart(
              'line',
              hcaes(x = fecha, y = !!sym(input$monitor_avances_metric_selector), group = nombres_coach)
            )
          
        } else {
          chart <- datos %>% 
            mutate(
              fecha = as.numeric(fecha)
            ) %>% 
            hchart(
              'line',
              hcaes(x = fecha, y = !!sym(input$monitor_avances_metric_selector), group = nombres_coach)
            ) 
        }
        
        chart
      })
    }
  )
}