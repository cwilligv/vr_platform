gestion_sistema_ui <- function(id){
  tabItem(
    tabName = "tab_sistema",
    h1("Gestión de Sistema", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 12,
          # tabsetPanel(id = NS(id, "system_tabs"), selected = NULL, 
          #   type = "tabs",
          #   tabPanel(
          #     title = "Usuarios y Roles",
          #     br(),
          #     usuarios_roles_ui(NS(id, "users_and_roles_tabs"))
          #   ),
          #   tabPanel(
          #     title = "Inscripciones",
          #     config_inscripciones_ui(NS(id, "inscripciones_config_tab"))
          #   ),
          #   tabPanel(
          #     title = "Seguimiento",
          #     br(),
          #     config_seguimiento_ui(NS(id, "config_seguimiento_tab"))
          #   )
          # )
          uiOutput(NS(id, "tabset"))
        )
      )
    )
  )
}

gestion_sistema_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      output$tabset <- renderUI({
        if (session$userData$rol %in% c('coordinador')) {
          tabsetPanel(
            id = NS(id, "system_tabs"), 
            selected = NULL, 
            type = "tabs",
            tabPanel(
              title = "Inscripciones",
              config_sistema_ui(NS(id, "config_variables_sistema_tab"))
            )
          )
        } else {
          tabsetPanel(
            id = NS(id, "system_tabs"), 
            selected = NULL, 
            type = "tabs",
            tabPanel(
              title = "Coaches",
              br(),
              usuarios_roles_ui(NS(id, "users_and_roles_tabs"))
            ),
            tabPanel(
              title = "Observaciones",
              config_seguimiento_ui(NS(id, "config_seguimiento_tab"))
            ),
            tabPanel(
              title = "Actividad",
              config_sistema_ui(NS(id, "config_variables_sistema_tab"))
            )
          )
        }
      })
      
      usuarios_roles_server("users_and_roles_tabs")
      
      config_seguimiento_server("config_seguimiento_tab")
      
      config_sistema_server("config_variables_sistema_tab")
    }
  )
  
}