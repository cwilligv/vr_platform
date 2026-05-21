config_sistema_ui <- function(id){
  tagList(
    br(),
    # custom_control_input(
    #   NS(id, "activate_switch"),
    #   type = "switch",
    #   label = "Activar Inscripciones fuera de horario"
    # ),
    uiOutput(NS(id,"switch_ui")),
    hr(),
    uiOutput(NS(id, "inactivity_months_ui"))
  )
}

config_sistema_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      # observe({
      #   update_switch("activate_switch", value = get_system_variable('inscripciones', NULL, 'activar_fuera_horario'))
      # })
      
      output$switch_ui <- renderUI({
        ns <- session$ns
        tagList(
          custom_control_input(
            ns("activate_switch"),
            type = "switch",
            label = "Activar Inscripciones fuera de horario",
            checked = get_system_variable('inscripciones', NULL, 'activar_fuera_horario') %>% as.logical()
          ),
          p(),
          textOutput(ns("status"))
        )
      })
      
      output$status <- renderText({
        print("inside status")
        if(input$activate_switch) {
          "Acceso inscripciones fuera de horario activado"
        } else {
          "Acceso inscripciones fuera de horario desactivado"
        }
      })
      
      observeEvent(input$activate_switch, {
        set_system_variable('inscripciones', NULL, 'activar_fuera_horario', input$activate_switch)
      }, ignoreInit = T, ignoreNULL = T)
      
      # Inactivity months configuration
      output$inactivity_months_ui <- renderUI({
        ns <- session$ns
        current_value <- get_system_variable('sistema', NULL, 'numero_de_meses_de_inactividad')
        if (is.null(current_value) || length(current_value) == 0) {
          current_value <- 6
        }
        
        tagList(
          h5("Configuración de Inactividad de Usuarios"),
          div(
            style = "display: flex; align-items: flex-end; gap: 10px;",
            div(
              style = "width: 350px; margin-bottom: 0;",
              selectInput(
                inputId = ns("inactivity_months"),
                label = "Número de meses para considerar usuario inactivo:",
                choices = 1:12,
                selected = as.numeric(current_value),
                width = "100%"
              )
            ),
            div(
              style = "padding-bottom: 12px;",
              actionButton(
                inputId = ns("save_inactivity_months"),
                label = "Guardar",
                class = "btn-primary"
              )
            )
          ),
          textOutput(ns("inactivity_status"))
        )
      })
      
      observeEvent(input$save_inactivity_months, {
        req(input$inactivity_months)

        if (as.numeric(input$inactivity_months) >= 1 && as.numeric(input$inactivity_months) <= 12) {
          set_system_variable('sistema', NULL, 'numero_de_meses_de_inactividad', as.numeric(input$inactivity_months))
          
          showNotification("Configuración de inactividad guardada.", type = "message")
          
          output$inactivity_status <- renderText({
            paste("Configuración guardada: Los usuarios serán considerados inactivos después de", 
                  input$inactivity_months, "meses.")
          })
        } else {
          output$inactivity_status <- renderText({
            "Error: El número de meses debe estar entre 1 y 12"
          })
        }
      })
      
    }
  )
}

custom_control_input <- function(inputId, type = c("switch", "checkbox", "radio"), label, checked = FALSE, disabled = FALSE) {
  
  div(
    class = paste0("custom-control custom-", type),
    tags$input(
      id = inputId,
      type = ifelse(type == "switch", "checkbox", type),
      disabled = if (disabled) NA,
      checked = if (checked) NA,
      class = "custom-control-input"
    ),
    tags$label(
      label,
      `for` = inputId,
      class = "custom-control-label"
    )
  )
}