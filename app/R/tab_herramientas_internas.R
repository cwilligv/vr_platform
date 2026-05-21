herramientas_internas_ui <- function(id){
  tabItem(
    tabName = "tab10_monitor",
    h1("Monitor de Actividad", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      # fluidRow(
      #   column(
      #     width = 8,
      #     p("Grupo de herramientas que proveen monitoreo y analisis de la plataforma.")
      #   )
      # ),
      bs4Dash::tabsetPanel(
        id = "panel_monitor_avances",
        tabPanel(
          title = "Avances",
          monitor_avances_ui(NS(id,"monitor_avances"))
        )
        # tabPanel(
        #   title = "Encuestas",
        #   br(),
        #   "Funcionalidad en desarrollo"
        #   # monitor_encuestas_ui(NS(id, "monitor_encuestas"))
        # )
      )
    )
  )
}

herramientas_internas_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      internal_tabs <- reactiveValues(
        tab_monitor_avances_clicked = FALSE,
        tab_monitor_encuestas_clicked = FALSE
      )
      
      observeEvent(rv$panel_monitor_avances, {
          internal_tabs$tab_monitor_avances_clicked <- input$panel_monitor_avances == "Avances"
          internal_tabs$tab_monitor_encuestas_clicked <- input$panel_monitor_avances == "Encuestas"
          print(paste0("**Herramienta: ",input$panel_monitor_avances, "**"))
        }
      )

      monitor_avances_server("monitor_avances", rv)
      monitor_encuestas_server("monitor_encuestas", internal_tabs)
    } # End of moduleServer function
  )
}