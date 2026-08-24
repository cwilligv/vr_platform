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
    uiOutput(NS(id, "inactivity_months_ui")),
    hr(),
    uiOutput(NS(id, "number_of_vr_ui")),
    hr(),
    uiOutput(NS(id, "horario_config_ui"))
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

      # Number of VR headsets configuration
      output$number_of_vr_ui <- renderUI({
        ns <- session$ns
        current_value <- get_system_variable('sistema', NULL, 'numero_de_vr')
        if (is.null(current_value) || length(current_value) == 0) {
          current_value <- 1
        }

        tagList(
          h5("Configuración de Equipos VR"),
          div(
            style = "display: flex; align-items: flex-end; gap: 10px;",
            div(
              style = "width: 350px; margin-bottom: 0;",
              selectInput(
                inputId = ns("number_of_vr"),
                label = "Número de equipos VR disponibles para evaluaciones:",
                choices = 1:10,
                selected = as.numeric(current_value),
                width = "100%"
              )
            ),
            div(
              style = "padding-bottom: 12px;",
              actionButton(
                inputId = ns("save_number_of_vr"),
                label = "Guardar",
                class = "btn-primary"
              )
            )
          ),
          textOutput(ns("vr_status"))
        )
      })

      observeEvent(input$save_number_of_vr, {
        req(input$number_of_vr)

        if (as.numeric(input$number_of_vr) >= 1 && as.numeric(input$number_of_vr) <= 10) {
          set_system_variable('sistema', NULL, 'numero_de_vr', as.numeric(input$number_of_vr))

          showNotification("Configuración de equipos VR guardada.", type = "message")

          output$vr_status <- renderText({
            paste("Configuración guardada:", input$number_of_vr, "equipos VR disponibles para reservas.")
          })
        } else {
          output$vr_status <- renderText({
            "Error: El número de equipos VR debe estar entre 1 y 10"
          })
        }
      })

      # Horario configuration
      output$horario_config_ui <- renderUI({
        ns <- session$ns

        # Get current values or defaults
        horario_inicio <- get_system_variable('sistema', NULL, 'horario_inicio')
        horario_fin <- get_system_variable('sistema', NULL, 'horario_fin')
        intervalo_slots <- get_system_variable('sistema', NULL, 'intervalo_slots')

        if (is.null(horario_inicio) || length(horario_inicio) == 0) {
          horario_inicio <- 9
        }
        if (is.null(horario_fin) || length(horario_fin) == 0) {
          horario_fin <- 19
        }
        if (is.null(intervalo_slots) || length(intervalo_slots) == 0) {
          intervalo_slots <- 30
        }

        # Create hour choices (8 AM to 10 PM)
        hour_choices <- setNames(8:22, paste0(8:22, ":00"))

        # Create interval choices
        interval_choices <- c(
          "15 minutos" = 15,
          "30 minutos" = 30,
          "60 minutos (1 hora)" = 60
        )

        tagList(
          h5("Configuración de Horarios Disponibles"),
          p("Configure el rango de horarios disponibles para agendar evaluaciones."),
          div(
            style = "display: flex; align-items: flex-end; gap: 10px;",
            div(
              style = "width: 200px; margin-bottom: 0;",
              selectInput(
                inputId = ns("horario_inicio"),
                label = "Hora de inicio:",
                choices = hour_choices,
                selected = as.numeric(horario_inicio),
                width = "100%"
              )
            ),
            div(
              style = "width: 200px; margin-bottom: 0;",
              selectInput(
                inputId = ns("horario_fin"),
                label = "Hora de fin:",
                choices = hour_choices,
                selected = as.numeric(horario_fin),
                width = "100%"
              )
            ),
            div(
              style = "width: 200px; margin-bottom: 0;",
              selectInput(
                inputId = ns("intervalo_slots"),
                label = "Intervalo entre slots:",
                choices = interval_choices,
                selected = as.numeric(intervalo_slots),
                width = "100%"
              )
            ),
            div(
              style = "padding-bottom: 12px;",
              actionButton(
                inputId = ns("save_horario"),
                label = "Guardar",
                class = "btn-primary"
              )
            )
          ),
          textOutput(ns("horario_status"))
        )
      })

      observeEvent(input$save_horario, {
        req(input$horario_inicio, input$horario_fin, input$intervalo_slots)

        inicio <- as.numeric(input$horario_inicio)
        fin <- as.numeric(input$horario_fin)
        intervalo <- as.numeric(input$intervalo_slots)

        if (inicio >= fin) {
          output$horario_status <- renderText({
            "Error: La hora de inicio debe ser menor que la hora de fin."
          })
          return()
        }

        if (inicio < 8 || fin > 22) {
          output$horario_status <- renderText({
            "Error: El horario debe estar entre 8:00 y 22:00"
          })
          return()
        }

        if (!intervalo %in% c(15, 30, 60)) {
          output$horario_status <- renderText({
            "Error: El intervalo debe ser 15, 30 o 60 minutos"
          })
          return()
        }

        set_system_variable('sistema', NULL, 'horario_inicio', inicio)
        set_system_variable('sistema', NULL, 'horario_fin', fin)
        set_system_variable('sistema', NULL, 'intervalo_slots', intervalo)

        showNotification("Configuración de horarios guardada.", type = "message")

        output$horario_status <- renderText({
          paste0("Configuración guardada: Horarios disponibles de ", inicio, ":00 a ", fin, ":00 con intervalos de ", intervalo, " minutos")
        })
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