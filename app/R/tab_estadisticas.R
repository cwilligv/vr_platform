estadisticas_ui <- function(id){
  tagList(
    br(),
    h1("Análisis Resultados 3D", style = "font-size: 1.8rem;"),
    br(),
    fluidRow(
      column(2, div(selectInput(NS(id, "filtro_perfil"), label = "Perfil 3D", choices = c("TODOS", "TÉCNICO", "SUPERVISOR", "STAFF"))),style = "margin-top:-30px"),
      # column(4, align = "center", style = "height: 400px;", highchartOutput(NS(id, "stat_donut_result_final"))),
      column(3, div(textOutput(NS(id, "fecha_ultima_actualizacion")), style = "padding:5px;")),
      column(3, uiOutput(NS(id, "selector_proyecto"))),
      column(4, uiOutput(NS(id, "admin_selector")))
    ),
    # hr(style="border-color: black;"),
    fluidRow(
      # column(4, style = "height: 340px;", highchartOutput(NS(id, "stat_donut_dim_seguridad"))),
      # column(4, style = "height: 340px;", highchartOutput(NS(id, "stat_donut_dim_psicolaboral"))),
      # column(4, style = "height: 340px;", highchartOutput(NS(id, "stat_donut_dim_tecnica")))
      shiny::splitLayout(
        # cellWidths = c("33%", "33%", "33%"),
        highchartOutput(NS(id, "stat_donut_result_final"), height = "400px"),
        highchartOutput(NS(id, "stat_donut_dim_seguridad"), height = "400px"),
        highchartOutput(NS(id, "stat_donut_dim_psicolaboral"), height = "400px"),
        highchartOutput(NS(id, "stat_donut_dim_tecnica"), height = "400px")
      )
    ),
    # hr(style="border-color: black;"),
    markdown("*****") %>% div(class = "sps-dash"),
    fluidRow(
      highchartOutput(NS(id, "stat_bar_subdim_all"))
    ),
    br(),
    fluidRow(
      # column(4, highchartOutput(NS(id, "stat_donut_dim_tecnica"))),
      # column(8, highchartOutput(NS(id, "stat_bar_subdim_tecnica")))
    )
  )
}

estadisticas_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      output$selector_proyecto <- renderUI({
        ns <- session$ns
        print("getting projects")
        tagList(
          div(selectInput(ns("filtro_proyecto"), label = "Contrato/Proyecto", choices = get_proyectos(session$userData$id_empresa), selected = "Todos"), style = "margin-top:-30px")
        )
      })
      
      output$admin_selector <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            div(selectInput(ns("listado_empresas"), "Clientes", choices = get_empresas(session$userData$rol, session$userData$email), selected = as.numeric(session$userData$id_empresa)),style = "margin-top:-30px")
          )
        }
      })
      
      observeEvent(input$listado_empresas, {
        session$userData$id_empresa <- input$listado_empresas
        updateSelectInput(session, "filtro_proyecto", choices = get_proyectos(session$userData$id_empresa))
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      estadisticas_df <- reactive({
        
        #make reactive to
        dataChangedTrigger()
        
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          if (input$filtro_perfil == 'TODOS') {
            filtros <- glue::glue_sql("1 = 1", .con = pool)
          } else {
            filtros <- glue::glue_sql("perfil_3d = {input$filtro_perfil}", .con = pool)
          }
          
        } else {
          if (input$filtro_perfil == 'TODOS') {
            filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
          } else {
            filtros <- glue::glue_sql("perfil_3d = {input$filtro_perfil} AND id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
          }
        }
        
        if (!is.null(input$filtro_proyecto) && input$filtro_proyecto != "Todos") {
          print(paste0("Filtro antes de proyecto: ", filtros))
          if (input$filtro_proyecto == 'NA') {
            filtros <- glue::glue_sql(paste0(filtros, " and proyecto is NULL"), .con = pool)
          }else{
            filtros <- glue::glue_sql(paste0(filtros, " and proyecto = {input$filtro_proyecto}"), .con = pool)
          }
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.estadisticas_view where ({filtros})", .con = pool)
        dbGetQuery(pool, tbl)
        
      }, label = "DB-data")
      
      # output$filtros <- renderUI({
      #   ns <- session$ns
      #   tagList(
      #     selectInput(ns("filtro_perfil"), "Perfil 3D", choices = c("TODOS", "TÉCNICO", "SUPERVISOR", "STAFF"))
      #   )
      # })
      
      output$fecha_ultima_actualizacion <- renderText({
        # proyectos <- c("ALL", unique(estadisticas_df()$proyecto))
        # updateSelectInput(session, "filtro_proyecto", choices = proyectos, selected = "ALL")
        # fecha <- max(unique(estadisticas_df()$fecha_carga_datos))
        input$listado_empresas
        info <- get_info_actualizacion_resultados(session$userData$id_empresa)
        fecha <- info$fecha
        usuario <- info$updated_by
        if (is.null(usuario)) {
          paste0("Última actualización ", format(as.Date(fecha), format = "%d-%m-%y"), " (Sin nombre)")
        }else{
          paste0("Última actualización ", format(as.Date(fecha), format = "%d-%m-%y"), " (", usuario, ")")
        }
      })
      
      resultado_final <- reactive({
        estadisticas_df() %>% 
          group_by(resultado_final_3d) %>% 
          filter(resultado_final_3d != 'SIN RESULTADO') %>% 
          summarise(contador = n()) %>% 
          dplyr::group_by(.) %>% 
          mutate(percentage = round(contador / sum(contador) * 100)) %>% 
          mutate(resultado_final_3d = if_else(resultado_final_3d == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", resultado_final_3d),
                 header = "Resultado Final") %>% 
          relocate(header) 
      })
      
      output$stat_donut_result_final <- renderHighchart({
        validate(need(nrow(resultado_final()) > 0, "No existen datos disponibles"))
        # cols <- c('green', 'yellow', 'red')
        cols <- resultado_final() %>% group_by(resultado_final_3d) %>% distinct(resultado_final_3d, .keep_all = F) %>% 
          mutate(color = case_when(
            resultado_final_3d == 'COMPETENTE' ~ '#77C151', # green
            resultado_final_3d == 'COMPETENTE C/O' ~ '#F1C429', # yellow
            resultado_final_3d == 'NO COMPETENTE' ~ '#E4465C',
            resultado_final_3d == 'RESULTADO PENDIENTE' ~ '#B9CFDE',
            .default = '#E4465C' # red
          )) %>% 
          select(color) %>% 
          pull()
        
        highchart() %>% 
          hc_add_series(type = "pie", data = resultado_final(), hcaes(resultado_final_3d, percentage), 
                        name = "Categoria (%)", center = c(50, 50), 
                        innerSize="50%",
                        tooltip = list(
                          headerFormat = '{point.header}',
                          pointFormat = 'Resultado Final <br>{point.resultado_final_3d}: <b>{point.percentage:.1f}%</b> <br>PARTICIPANTES: <b>{point.contador}</b>'
                        )
                        # dataLabels = list(distance = -50, 
                        #                   formatter = JS("function () {
                        #                                     return this.y > 5 ? this.point.name : null;
                        #                                   }"))
                        ) %>% 
          hc_colors(cols) %>% 
          hc_title(text = "Resultado Final", style = list(fontSize = '16px')) %>%
          hc_plotOptions(
            innersize="50%", 
            startAngle=90, 
            endAngle=90,
            center=list('50%', '75%'),
            size='110%',
            pie=list(dataLabels=list(enabled = F)))  
          # hc_tooltip(formatter = JS(paste0('function ()
          #                          {return "Resultado Final <br>" +
          #                          (this.points[1].y);}')
          # ), shared = TRUE)
      })
      

      # ====================================================
      # DIMENSION SEGURIDAD
      # ====================================================
      
      dim_seguridad <- reactive({
        estadisticas_df() %>% 
          group_by(dim_seguridad_categoria) %>% 
          drop_na(dim_seguridad_categoria) %>% 
          summarise(contador = n()) %>% 
          dplyr::group_by(.) %>% 
          mutate(percentage = round(contador / sum(contador) * 100)) %>%  
          mutate(dim_seguridad_categoria = if_else(dim_seguridad_categoria == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", dim_seguridad_categoria),
                 header = "Dimensión Seguridad") %>% 
          relocate(header) 
      })
      
      output$stat_donut_dim_seguridad <- renderHighchart({
        validate(need(nrow(dim_seguridad()) > 0, "No existen datos disponibles"))
        # cols <- c('green', 'yellow', 'red')
        cols <- dim_seguridad() %>% group_by(dim_seguridad_categoria) %>% distinct(dim_seguridad_categoria, .keep_all = F) %>% 
          mutate(color = case_when(
            dim_seguridad_categoria == 'COMPETENTE' ~ '#77C151', # green
            dim_seguridad_categoria == 'COMPETENTE C/O' ~ '#F1C429', # yellow
            dim_seguridad_categoria == 'NO COMPETENTE' ~ '#E4465C',
            .default = '#E4465C' # red
          )) %>% 
          select(color) %>% 
          pull()
        
        highchart() %>% 
          hc_add_series(type = "pie", data = dim_seguridad(), hcaes(dim_seguridad_categoria, percentage), 
                        name = "Categoria (%)", center = c(50, 50), 
                        innerSize="50%", 
                        tooltip = list(
                          headerFormat = '{point.header}',
                          pointFormat = 'Dimensión Seguridad <br>{point.dim_seguridad_categoria}: <b>{point.percentage:.1f}%</b> <br>PARTICIPANTES: <b>{point.contador}</b>'
                        )
                        # dataLabels = list(distance = -50, 
                        #                   formatter = JS("function () {
                        #                                     return this.y > 5 ? this.point.name : null;
                        #                                   }"))
          ) %>% 
          hc_colors(cols) %>% 
          hc_title(text = "Dimensión Seguridad", style = list(fontSize = '16px')) %>%
          hc_plotOptions(
            innersize="50%", 
            startAngle=90, 
            endAngle=90,
            center=list('50%', '75%'),
            size='110%',
            pie=list(dataLabels=list(enabled = F)))
      })
      
      # ====================================================
      # DIMENSION PSICOLABORAL
      # ====================================================
      dim_psicolaboral <- reactive({
        estadisticas_df() %>% 
          group_by(dim_psicolaboral_categoria) %>% 
          drop_na(dim_psicolaboral_categoria) %>% 
          summarise(contador = n()) %>% 
          dplyr::group_by(.) %>% 
          mutate(percentage = round(contador / sum(contador) * 100)) %>%  
          mutate(dim_psicolaboral_categoria = if_else(dim_psicolaboral_categoria == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", dim_psicolaboral_categoria),
                 header = "Dimensión Psicolaboral") %>% 
          relocate(header)  
        
      })
      
      output$stat_donut_dim_psicolaboral <- renderHighchart({
        validate(need(nrow(dim_psicolaboral()) > 0, "No existen datos disponibles"))
        # cols <- c('green', 'yellow', 'red')
        cols <- dim_psicolaboral() %>% group_by(dim_psicolaboral_categoria) %>% distinct(dim_psicolaboral_categoria, .keep_all = F) %>% 
          mutate(color = case_when(
            dim_psicolaboral_categoria == 'COMPETENTE' ~ '#77C151', # green
            dim_psicolaboral_categoria == 'COMPETENTE C/O' ~ '#F1C429', # yellow
            dim_psicolaboral_categoria == 'NO COMPETENTE' ~ '#E4465C',
            .default = '#E4465C' # red
          )) %>% 
          select(color) %>% 
          pull()
        
        highchart() %>% 
          hc_add_series(type = "pie", data = dim_psicolaboral(), hcaes(dim_psicolaboral_categoria, percentage), 
                        name = "Categoria (%)", center = c(50, 50), 
                        innerSize="50%",
                        tooltip = list(
                          headerFormat = '{point.header}',
                          pointFormat = 'Dimensión Psicolaboral <br>{point.dim_psicolaboral_categoria}: <b>{point.percentage:.1f}%</b> <br>PARTICIPANTES: <b>{point.contador}</b>'
                        )
                        # dataLabels = list(distance = -50, 
                        #                   formatter = JS("function () {
                        #                                     return this.y > 5 ? this.point.name : null;
                        #                                   }"))
          ) %>% 
          hc_colors(cols) %>% 
          hc_title(text = "Dimensión Psicolaboral", style = list(fontSize = '16px')) %>%
          hc_plotOptions(
            innersize="50%", 
            startAngle=90, 
            endAngle=90,
            center=list('50%', '75%'),
            size='110%',
            pie=list(dataLabels=list(enabled = F)))
      })
      
      # ====================================================
      # DIMENSION TECNICA
      # ====================================================
      dim_tecnica <- reactive({
        estadisticas_df() %>% 
          group_by(dim_tecnica_categoria) %>% 
          drop_na(dim_tecnica_categoria) %>% 
          summarise(contador = n()) %>% 
          dplyr::group_by(.) %>% 
          mutate(percentage = round(contador / sum(contador) * 100)) %>%  
          mutate(dim_tecnica_categoria = if_else(dim_tecnica_categoria == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", dim_tecnica_categoria),
                 header = "Dimensión Técnica") %>% 
          relocate(header)  
        
      })
      
      output$stat_donut_dim_tecnica <- renderHighchart({
        validate(need(nrow(dim_tecnica()) > 0, "No existen datos disponibles"))
        # cols <- c('green', 'yellow', 'red')
        cols <- dim_tecnica() %>% group_by(dim_tecnica_categoria) %>% distinct(dim_tecnica_categoria, .keep_all = F) %>% 
          mutate(color = case_when(
            dim_tecnica_categoria == 'COMPETENTE' ~ '#77C151', # green
            dim_tecnica_categoria == 'COMPETENTE C/O' ~ '#F1C429', # yellow
            dim_tecnica_categoria == 'NO COMPETENTE' ~ '#E4465C',
            .default = '#E4465C' # red
          )) %>% 
          select(color) %>% 
          pull()
        
        highchart() %>% 
          hc_add_series(type = "pie", data = dim_tecnica(), hcaes(dim_tecnica_categoria, percentage), 
                        name = "Categoria (%)", center = c(50, 50), 
                        innerSize="50%",
                        tooltip = list(
                          headerFormat = '{point.header}',
                          pointFormat = 'Dimensión Técnica <br>{point.dim_tecnica_categoria}: <b>{point.percentage:.1f}%</b> <br>PARTICIPANTES: <b>{point.contador}</b>'
                        )
                        # dataLabels = list(distance = -50, 
                        #                   formatter = JS("function () {
                        #                                     return this.y > 5 ? this.point.name : null;
                        #                                   }"))
          ) %>% 
          hc_colors(cols) %>% 
          hc_title(text = "Dimensión Técnica", style = list(fontSize = '16px')) %>%
          hc_plotOptions(
            innersize="50%", 
            startAngle=90, 
            endAngle=90,
            center=list('50%', '75%'),
            size='110%',
            pie=list(dataLabels=list(enabled = F)))
      })
      
      
      # ====================================================
      # SUBDIMENSIONES
      # ====================================================
      subdim_all <- reactive({
        # estadisticas_df() %>% 
        #   dplyr::select(id, psicolaboral_categoria, vr_categoria, conductas_de_riesgo, conocimientos_en_seguridad_categoria, gestion_categoria, tecnica_teorica_categoria, tecnica_practica_categoria) %>% 
        #   tidyr::pivot_longer(cols = c(psicolaboral_categoria, vr_categoria, conductas_de_riesgo, conocimientos_en_seguridad_categoria, gestion_categoria, tecnica_teorica_categoria, tecnica_practica_categoria), names_to = "subdim", values_to = "resultado") %>%
        #   dplyr::group_by(subdim, resultado) %>% 
        #   tidyr::drop_na() %>%
        #   dplyr::summarise(percentage = n()) %>% 
        #   dplyr::group_by(subdim) %>% 
        #   dplyr::mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>% 
        #   dplyr::mutate(resultado = if_else(resultado == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", resultado),
        #          percentage = if_else(is.na(resultado), 0, percentage),
        #          subdim = case_when(
        #            subdim == 'conductas_de_riesgo' ~ 'Conductual',
        #            subdim == 'conocimientos_en_seguridad_categoria' ~ 'Conocimientos Seguridad',
        #            subdim == 'vr_categoria' ~ 'Identificación Riesgos',
        #            subdim == 'gestion_categoria' ~ 'Gestión',
        #            subdim == 'tecnica_teorica_categoria' ~ 'Técnica Teórica',
        #            subdim == 'tecnica_practica_categoria' ~ 'Técnica Práctica',
        #            .default = 'Psicolaboral'
        #          )) %>% 
        #   dplyr::right_join(
        #     data.frame(subdim = c("Psicolaboral", "Conductual", "Conocimientos Seguridad", "Identificación Riesgos", "Técnica Teórica", "Técnica Práctica", "Gestión")),
        #     by = "subdim"
        #   )
        
        # Calculo % actualizado para que considere a todos los participantes como total en cada subdim.
        estadisticas_df() %>% 
          mutate(total = n()) %>% 
          dplyr::select(id, total,psicolaboral_categoria, vr_categoria, conductas_de_riesgo, conocimientos_en_seguridad_categoria, gestion_categoria, tecnica_teorica_categoria, tecnica_practica_categoria) %>% 
          tidyr::pivot_longer(cols = c(psicolaboral_categoria, vr_categoria, conductas_de_riesgo, conocimientos_en_seguridad_categoria, gestion_categoria, tecnica_teorica_categoria, tecnica_practica_categoria), names_to = "subdim", values_to = "resultado") %>%
          dplyr::group_by(subdim, resultado) %>% 
          tidyr::drop_na() %>%
          # dplyr::summarise(percentage = n()) %>%
          dplyr::summarise(contador = n(), percentage = round(n()/max(total) * 100, 1)) %>%
          # dplyr::group_by(subdim) %>% 
          # dplyr::mutate(percentage = round(percentage / sum(percentage) * 100, 1)) %>% 
          dplyr::mutate(resultado = if_else(resultado == "COMPETENTE CON OBSERVACIONES", "COMPETENTE C/O", resultado),
                        percentage = if_else(is.na(resultado), 0, round(contador/sum(contador) * 100, 1)),
                        subdim = case_when(
                          subdim == 'conductas_de_riesgo' ~ 'Conductual',
                          subdim == 'conocimientos_en_seguridad_categoria' ~ 'Conocimientos Seguridad',
                          subdim == 'vr_categoria' ~ 'Identificación Riesgos',
                          subdim == 'gestion_categoria' ~ 'Gestión',
                          subdim == 'tecnica_teorica_categoria' ~ 'Técnica Teórica',
                          subdim == 'tecnica_practica_categoria' ~ 'Técnica Práctica',
                          .default = 'Psicolaboral'
                        )) %>% 
          dplyr::right_join(
            data.frame(subdim = c("Psicolaboral", "Conductual", "Conocimientos Seguridad", "Identificación Riesgos", "Técnica Teórica", "Técnica Práctica", "Gestión")),
            by = "subdim"
          )
      })
      
      output$stat_bar_subdim_all <- renderHighchart({
        validate(need(nrow(subdim_all()) > 0, "No existen datos disponibles"))
        # print(subdim_all())
        colores <- subdim_all() %>% group_by(resultado) %>% distinct(resultado, .keep_all = F) %>% 
          mutate(color = case_when(
            resultado == 'COMPETENTE' ~ '#77C151', # green
            resultado == 'COMPETENTE C/O' ~ '#F1C429', # yellow
            .default = '#E4465C' # red
          )) %>% 
          select(color) %>% 
          tibble::deframe()
        
        subdim_all() %>%
          hchart(type = "column",
                 hcaes(x = subdim,
                       y = contador,
                       group = resultado),
                 tooltip = list(
                   headerFormat = '{point.subdim}',
                   pointFormat = '{point.subdim} <br>{series.name}: <b>{point.percentage:.1f}%</b> <br>PARTICIPANTES: <b>{point.contador}</b>'
                 ),
                 color = colores) %>%
          hc_plotOptions(column = list(stacking = "stack")) %>% 
          hc_title(text = "Sub-Dimensiones", style = list(fontSize = '16px')) %>%
          hc_yAxis(title = list(text = "# Participantes")) %>% 
          hc_xAxis(
            title = list(text = ""),
            categories = list("Psicolaboral", "Conductual", "Conocimientos Seguridad", "Identificación Riesgos", "Técnica Teórica", "Técnica Práctica", "Gestión")
          ) %>%
          hc_add_theme(hc_theme(chart = list(backgroundColor = '#ffffff'))) %>% 
          hc_legend(enabled = F)
      })
    }
  )
}