inicio_ui <- function(id) {
  #ns <- NS(id)
  # Calculate years for choices
  current_year <- lubridate::year(lubridate::today(tzone = "Chile/Continental"))
  years <- current_year:(current_year - 4)
  
  tabItem(
    tabName = "tab1_inicio",
    h1("Resumen", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      collapsible = F,
      headerBorder = F,
      p("Resumen del estado de avance de capacitaciones correspondiente al PRESENTE MES."),
      fluidRow(
        bs4InfoBoxOutput(NS(id, "infobox_capacitaciones_terminadas"), width = 4),
        bs4InfoBoxOutput(NS(id, "infobox_agendamientos"), width = 4),
        bs4InfoBoxOutput(NS(id, "infobox_inasistencias"), width = 4)
      ),
      markdown("*****") %>% div(class = "sps-dash"),
      p("Resumen ANUAL del estado de avance de capacitaciones."),
      fluidRow(
        column(4),
        column(4,
               fluidRow(
                 column(8,align = "left", selectInput(NS(id,"inicio_select_year"), "", choices = years, selected = current_year)) 
                 # column(4, br(), actionButton(NS(id, "inicio_show_button"), "Mostrar"))
               )
        ),
        column(4)
      ),
      fluidRow(
        column(
          width = 12,
          align = "center",
          #plotlyOutput("inicio_bar_chart", width = "100%")
          div(highchartOutput(NS(id, "inicio_bar_chart")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), width = "100%")
        ),
      )
    )
  )
}

inicio_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      # ===============================
      # Dynamic Greetings
      
      # output$texto_saludo_inicio <- renderText({
      #   cat(file=stderr(), session$userData$user()$email, "has logged in \n")
      #   nombre <- session$userData$user()$email %>% stringr::str_split("@") %>% purrr::simplify() %>% first()
      #   paste0("Hola ", nombre,",")
      # })
      
      updateSelectInput(inputId = "inicio_select_year", )
      
      output$inicio_bar_chart <- renderHighchart ({
        validate(need(nrow(chart_data()) > 0, "No existe data disponible"))
        
        cols <- chart_data() %>% distinct(colores) %>% arrange(colores) %>% pull()
        
        chart_data() %>%
          hchart(type = "column",
                 hcaes(x = Meses,
                       y = value,
                       group = estado),
                 color = cols,
                 tooltip = list(
                   headerFormat = '{point.meses_string}',
                   pointFormat = '<br>{point.estado}: <b>{point.value}</b>'
                 )) %>%
          hc_plotOptions(column = list(stacking = "stack")) %>% 
          hc_xAxis(dateTimeLabelFormats = list(month = "%m-%Y"), type = "datetime", title = '') %>%
          hc_yAxis(title = '') %>%
          hc_legend(enabled = F) %>% 
          hc_add_theme(hc_theme(chart = list(backgroundColor = '#ffffff')))
      })
      
      chart_data <- reactive({
        rv$tab_inicio_clicked
        # rv$cambio_empresa
        
        year <- as.numeric(input$inicio_select_year)
        meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("estado in ('inasistencia', 'capacitado') and year(fecha_preparacion) = {year}", .con = pool)
        } else {
          filtros <- glue::glue_sql("estado in ('inasistencia', 'capacitado') and year(fecha_preparacion) = {year} and id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        sql <- glue::glue_sql("select distinct rut, cargo, estado, date_format(fecha_preparacion, '%Y-%m') as Meses
                                from {`db`}.preparaciones_view
                                where ({filtros})", .con = pool)
        
        df <- dbGetQuery(pool, sql)
        year_months <- format(seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-01")), by = 'month'), '%Y-%m-%d') %>% as.Date() %>% as.data.frame()
        names(year_months) <- "Meses"
        
        if (nrow(df)>0) {
          res <- df %>% 
            mutate(Meses = as.Date(paste0(Meses, "-01"))) %>% 
            group_by(Meses, estado) %>% summarise(value = n())
         
          year_months %>% left_join(res, by = "Meses") %>% 
            mutate(value = if_else(is.na(value), 0, value),
                   estado = if_else(is.na(estado), 'no data', 
                                    if_else(estado == 'capacitado', 'Capacitaciones',
                                            ifelse(estado == 'inasistencia', 'Inasistencias', estado))),
                   colores = if_else(estado == 'Capacitaciones', '#006ac2',
                                     if_else(estado == 'Inasistencias', '#becede', 'white')),
                   meses_string = paste0(meses_es[month(ymd(Meses))], "-",year(Meses)))
        }else{
          data.frame()
        }
        
      })
      
      # ===============================
      # ===============================
      # Info boxes
      
      output$infobox_capacitaciones_terminadas <- renderbs4InfoBox({
        bs4InfoBox(title = h4("CAPACITACIONES"),
                   value = h3(cap_terminandas()),
                   subtitle = "Participantes capacitados",
                   icon = icon("users"),
                   # width = 4,
                   color = "success",
                   fill = T)
      })
      
      output$infobox_agendamientos <- renderbs4InfoBox({
        bs4InfoBox(title = h4("EN COORDINACIÓN"),
                   value = h3(agendamientos()),
                   subtitle = "Agendamientos en coordinación",
                   icon = icon("calendar-days"),
                   # width = 4,
                   color = "warning",
                   fill = T)
      })
      
      output$infobox_inasistencias <- renderbs4InfoBox({
        bs4InfoBox(title = h4("INASISTENCIAS"),
                   value = h3(inasistencias()),
                   subtitle = "Inasistencias a capacitación",
                   icon = icon("person-circle-question"),
                   # width = 4,
                   color = "danger",
                   fill = T)
      })
      
      cap_terminandas <- reactive({
        rv$tab_inicio_clicked
        month <- lubridate::month(lubridate::today(tzone = "Chile/Continental"))
        # year <- as.numeric(input$inicio_select_year)
        year <- as.numeric(lubridate::year(lubridate::today(tzone = "Chile/Continental")))
        print(paste("capacitaciones: ",year, month, sep = "-"))
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("estado = 'capacitado' and month(fecha_preparacion) = {month} and year(fecha_preparacion) = {year}", .con = pool)
        } else {
          filtros <- glue::glue_sql("estado = 'capacitado' and month(fecha_preparacion) = {month} and year(fecha_preparacion) = {year} and id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        sql <- glue::glue_sql("select distinct rut, cargo, horario
                                from {`db`}.preparaciones_view
                                where ({filtros})", .con = pool)
        df <- dbGetQuery(pool, sql) %>% nrow()
        df
      })
      
      agendamientos <- reactive({
        rv$tab_inicio_clicked
        month <- lubridate::month(lubridate::today(tzone = "Chile/Continental"))
        # year <- as.numeric(input$inicio_select_year)
        year <- as.numeric(lubridate::year(lubridate::today(tzone = "Chile/Continental")))
        print(paste("agendamientos: ",year, month, sep = "-"))
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("estado in ('en coordinacion','confirmado')", .con = pool)
        } else {
          filtros <- glue::glue_sql("estado in ('en coordinacion','confirmado') and id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        sql <- glue::glue_sql("select distinct rut, cargo, horario
                                from {`db`}.preparaciones_view
                                where ({filtros})", .con = pool)
        df <- dbGetQuery(pool, sql) %>% nrow()
        df
      })
      
      inasistencias <- reactive({
        rv$tab_inicio_clicked
        month <- lubridate::month(lubridate::today(tzone = "Chile/Continental"))
        # year <- as.numeric(input$inicio_select_year)
        year <- as.numeric(lubridate::year(lubridate::today(tzone = "Chile/Continental")))
        print(paste("inasistencias: ", year, month, sep = "-"))
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("estado = 'inasistencia' and month(fecha_preparacion) = {month} and year(fecha_preparacion) = {year}", .con = pool)
        } else {
          filtros <- glue::glue_sql("estado = 'inasistencia' and month(fecha_preparacion) = {month} and year(fecha_preparacion) = {year} and id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        sql <- glue::glue_sql("select distinct rut, cargo, horario
                                from {`db`}.preparaciones_view
                                where ({filtros})", .con = pool)
        df <- dbGetQuery(pool, sql) %>% nrow()
        df
      })
      
      # ===============================
      # ===============================
      # Bar chart
      
    }
  )
}