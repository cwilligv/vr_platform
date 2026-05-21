#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

# Sys.setlocale("LC_ALL", "es_ES.UTF-8")

options(
  shiny.reconnect = FALSE,
  shiny.maxRequestSize=40*1024^2
)

server = function(input, output, session) {
  
  # telemetry$start_session(
  #   track_inputs = FALSE,
  #   navigation_input_id = "sidebar_menu",
  #   username = session$userData$auth0_info$name
  # )

  user <- reactive({
    temp <- check_disabled_user(session$userData$auth0_info$name)
    session$userData$id_empresa <- get_empresas(temp$rol, temp$email)[1]
    session$userData$razon_social <- temp$razon_social
    session$userData$email <- temp$email 
    session$userData$telefono <- temp$telefono
    session$userData$nombre <- temp$nombre
    session$userData$apellido <- temp$apellidos
    session$userData$rol <- temp$rol
    session$userData$cargo <- temp$cargo
    session$userData$id_coach <- as.numeric(get_coach_id(session$userData$auth0_info$name))
    
    # NEW: Check user inactivity status
    inactivity_status <- check_user_inactivity(temp$email)
    
    # If user hasn't logged in for 6 months, auto-deactivate
    if (inactivity_status$is_inactive && !inactivity_status$flag_inactivo) {
      deactivate_user_inactivity(temp$email)
      temp$inactivo <- TRUE
    } else {
      temp$inactivo <- inactivity_status$flag_inactivo
    }
    
    # Update last login timestamp (only if not inactive)
    if (!temp$inactivo) {
      update_ultimo_login(temp$email)
    }
    
    temp
  })
  
  prefacturas <- reactive({
    get_prefacturas(user()$id_empresa)
  })
  
  observe({
    # print(paste0("This is email: ", email()))
    # check <- check_disabled_user(email())
    bloqueado <- user()$bloqueado
    user_blocked <- user()$user_blocked
    user_rol <- user()$rol
    id_empresa <- user()$id_empresa
    print(paste0("Empresa: ", session$userData$id_empresa))
    print(paste("block",bloqueado))
    
    # prefacturas <- get_prefacturas(user()$id_empresa)
    
    if (bloqueado) {
      # shinyalert(
      #   title = "<h2>Servicio Suspendido</h2> <h4>Mensaje Importante</h4>",
      #   text = "<hr style='height: 2px; background-color: black; border: none;'>Servicio suspendido temporalmente, debido a facturas impagas y/o no nos han enviado a tiempo las correspondientes ordenes de compra <br><br> Contactar con nosotros a la brevedad, para revisar el estado de cuenta y definir los pasos para la reactivación del servicio.<hr style='height: 1px; background-color: black; border: none;'>",
      #   type = "error",
      #   html = TRUE,
      #   showCancelButton = FALSE,
      #   closeOnEsc = FALSE,
      #   closeOnClickOutside = FALSE,
      #   animation = "slide-from-top",
      #   showConfirmButton = TRUE,
      #   confirmButtonText = "Entiendo",
      #   confirmButtonCol = "#ff5a00"
      # )
      showModal(
        modalDialog(
          div(
            style = "text-align: center;",  # Center all content in the main div
            tags$img(src = "images/exclamation_mark.png", height = "100px", style = "color:blue"),
            h3("Servicio Suspendido", style = "margin-top: 5px;"),  # Added title under the image
            p("Mensaje Importante"),
            hr(),
            paste("Servicio suspendido temporalmente, debido a facturas impagas y/o no nos han enviado a tiempo las correspondientes ordenes de compra." ),
            br(),
            paste("Contactar con nosotros a la brevedad, para revisar el estado de cuenta y definir los pasos para la reactivación del servicio."),
            hr(),
            align = "center"
          ),
          footer = div(
            style = "text-align: center; width: 100%;",
            # actionButton("close_blocked_modal", "Entiendo", style = "background-color: #FF5A00; color: white;")
            actionButton("ir_a_pagos_btn", "Entiendo", style = "background-color: #FF5A00; color: white;")
          ),
          easyClose = FALSE
        )
      )
    } else {
      if (user_blocked) {
        # showModal(
        #   modalDialog(
        #     div(
        #       style = "text-align: center;",  # Center all content in the main div
        #       tags$img(src = "images/exclamation_mark.png", height = "100px", style = "color:blue"),
        #       h3("Servicio Deshabilitado", style = "margin-top: 5px;"),  # Added title under the image
        #       p("Mensaje Importante"),
        #       hr(),
        #       paste("Servicio deshabilitado." ),
        #       br(),
        #       paste("En caso de error por favor contactar con nosotros a la brevedad, para revisar el estado de su cuenta."),
        #       hr(),
        #       align = "center"
        #     ),
        #     footer = div(
        #       # modalButton("Cerrar")
        #     ),
        #     easyClose = FALSE
        #   )
        # )
        auth0::logout()
      }
    }
    
    observeEvent(input$close_blocked_modal, {
      removeModal()
    })
    
    # NEW: Check if user is deactivated due to inactivity
    num_meses_inactividad <- as.numeric(get_system_variable('sistema', NULL, 'numero_de_meses_de_inactividad'))
    if (isTRUE(user()$inactivo)) {
      showModal(
        modalDialog(
          div(
            style = "text-align: center;",
            tags$img(src = "images/exclamation_mark.png", height = "100px", style = "color:blue"),
            h3("Cuenta Desactivada", style = "margin-top: 5px;"),
            p("Mensaje Importante"),
            hr(),
            paste("Tu cuenta ha sido desactivada por inactividad (Más de ",num_meses_inactividad ,"meses sin iniciar sesión)."),
            br(), br(),
            tags$p(
              "Para reactivarla, contacte a soporte al",
              tags$br(),
              icon("phone", style = "color: red;"), " +56 22 756 7186."
            ),
            hr(),
            align = "center"
          ),
          footer = div(
            style = "text-align: center; width: 100%;",
            actionButton("logout_inactivo_btn", "Entendido", style = "background-color: #FF5A00; color: white;")
          ),
          easyClose = FALSE
        )
      )
    }
    # END NEW

    if (nrow(prefacturas()) > 0 & !user()$bloqueado) {
      showModal(
        modalDialog(
          # title = "Mensaje Importante",
          div(
            # tags$img(src = "images/exclamation_mark.png", height = "100px", style = "color:blue"), 
            style = "text-align: center;",  # Center all content in the main div
            tags$img(src = "images/exclamation_mark.png", height = "100px", style = "color:blue"),
            h3("OC PENDIENTES", style = "margin-top: 5px;"),  # Added title under the image
            p("Mensaje Importante"),
            paste("Estimado cliente, favor enviar OC por los siguientes servicios:" ),
            br(),
            align = "center"
          ),
          div(DT::DTOutput('oc_pendientes_tbl') %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px")),
          footer = div(
            style = "text-align: center; width: 100%;",
            # align = "center",
            shinyjs::disabled(actionButton("ir_a_pagos_btn", "Revisar", style = "background-color: #FF5A00; color: white;"))
          ),
          easyClose = FALSE
        )
      )
    }
  })
  
  # NEW: Observer for logout button when user is inactive
  observeEvent(input$logout_inactivo_btn, {
    auth0::logout()
  })
  # END NEW
  
  observeEvent(input$oc_pendientes_tbl_rows_all, { # Observe when the table is rendered
    if (!is.null(input$oc_pendientes_tbl_rows_all)) { # Check if rows are not NULL (table loaded)
      # updateActionButton(session, "ir_a_pagos_btn", disabled = FALSE) # Enable the button
      shinyjs::enable("ir_a_pagos_btn")
    }
  })
  
  observeEvent(input$ir_a_pagos_btn, {
    updatebs4TabItems(session, "sidebar_menu", selected = "tab9_pagos")
    removeModal()
  })
  
  output$oc_pendientes_tbl = DT::renderDataTable({
    table <- prefacturas() %>% select(fecha_servicio, total, retraso)
    datatable(
      table, 
      class = "compact", 
      escape = FALSE,
      colnames = c('Fecha de servicio', 'Total', 'Atraso (días)'),
      selection = 'none',
      options = list(
        lengthChange = FALSE, 
        searching = FALSE,
        ordering = FALSE,
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = c(1, 3))),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      )
    ) %>% 
      formatCurrency(
        columns = 'total',
        currency = "$",
        interval = 3,
        mark = ".",
        digits = 0
      )
  })
  
  output$texto_saludo_inicio <- renderUI({
    cat(file=stderr(), user()$email, "has logged in \n")
    nombre <- user <- user()$nombre
    ahora <- lubridate::now(tzone = "Chile/Continental")
    hora <- hour(ahora)
    minuto <- minute(ahora)
    time_of_day <- "Hola"
    
    if (hora >= 6 & (hora < 12 | (hora == 11 & minuto <= 59))) {
      time_of_day <- "¡VR Buenos días!"
    } else if (hora >= 12 & (hora < 21 | (hora == 20 & minuto <= 59))) {
      time_of_day <- "¡VR Buenas tardes!"
    } else {
      time_of_day <- "¡VR Buenas noches!"
    }
    
    tagList(tags$img(src=paste0("images/waving_hand_", env$ENV, ".png"), height=30,width=30), paste0(" ",time_of_day, " ", nombre, ","))
  })
  
  # Reactive values object
  rv_tabs <- reactiveValues(
    tab_monitor_clicked = FALSE,
    tab_inscripciones_clicked = FALSE,
    tab_herramientas_clicked = FALSE,
    tab_inicio_clicked = FALSE,
    cambio_empresa = FALSE,
    tab_pagos = FALSE
  )
  
  # Update rv_tabs$modify_port_clicked when tabs are clicked
  observeEvent(input$sidebar_menu, {
    rv_tabs$tab_inscripciones_clicked <- input$sidebar_menu == "tab2_inscripciones"
    rv_tabs$tab_monitor_clicked <- input$sidebar_menu == "tab3_seguimiento"
    rv_tabs$tab_encuesta_clicked <- input$sidebar_menu == "tab_encuestas"
    rv_tabs$tab_herramientas_clicked <- input$sidebar_menu == "tab_herramientas"
    rv_tabs$tab_inicio_clicked <- input$sidebar_menu == "tab1_inicio"
    rv_tabs$tab_pagos <- input$sidebar_menu == "tab9_pagos"
    print(paste("Inscripcion tab clicked", rv_tabs$tab_inscripciones_clicked))
    print(paste("Monitor tab clicked", rv_tabs$tab_monitor_clicked))
    print(paste("Herramientas tab clicked", rv_tabs$tab_herramientas_clicked))
    print(paste("Inicio tab clicked", rv_tabs$tab_inicio_clicked))
  })
  
  output$user <- renderUser({
    if (user()$rol == 'cliente') {
      empresa <- session$userData$razon_social
    } else {
      empresa <- 'TODAS'
    }
    dashboardUser(
      name = icon("caret-down"),
      image = "images/user-solid.svg",
      title = paste(user()$nombre, user()$apellidos),
      subtitle = toupper(user()$rol),
      footer = actionLink("sign_out", label = "Cerrar sesión", icon = icon("sign-out")),
      fluidRow(
        dashboardUserItem(
          width = 5,
          "Empresa: "
        ),
        dashboardUserItem(
          width = 7,
          empresa
        )
      )
      # fluidRow(
      #   dashboardUserItem(
      #     width = 6,
      #     div(selectInput("listado_empresas", "", choices = get_empresas(user()$rol, user()$email)), style = 'font-size:10px')
      #   ),
      #   dashboardUserItem(
      #     width = 6,
      #     actionButton("seleccionar_empresa", "Seleccionar", style = 'margin-top:13px; margin-left:-40px;')
      #   )
      # )
    )
  })
  
  observeEvent(input$seleccionar_empresa, priority = 20,{
    print(paste0("Empresa seleccionada: ", input$listado_empresas))
    old <- session$userData$id_empresa
    session$userData$id_empresa <- input$listado_empresas
    rv_tabs$cambio_empresa <- old == input$listado_empresas
    rv_tabs$tab_inscripciones_clicked <- input$sidebar_menu == "tab2_inscripciones"
    rv_tabs$tab_monitor_clicked <- input$sidebar_menu == "tab3_seguimiento"
    rv_tabs$tab_encuesta_clicked <- input$sidebar_menu == "tab_encuestas"
    rv_tabs$tab_certificados_clicked <- input$sidebar_menu == "tab_certificados"
    rv_tabs$tab_herramientas_clicked <- input$sidebar_menu == "tab_herramientas"
    rv_tabs$tab_inicio_clicked <- input$sidebar_menu == "tab1_inicio"
    rv_tabs$tab_pagos <- input$sidebar_menu == "tab9_pagos"
    rv_tabs$tab_herramientas_internas_clicked <- input$sidebar_menu == "tab10_monitor"
  })
  
  output$menu_inscripciones <- renderMenu({
    if (user()$bloqueado) {
      return(NULL)
    } else {
      if (!(user()$rol %in% c('administrativo', 'cliente_jefatura'))) {
        print("Desplegando menu inscripciones")
        menuItem(
          text = "Inscripciones",
          tabName = "tab2_inscripciones",
          icon = icon("user-plus", lib = "font-awesome")
        )
      }
    }
  })
  
  output$menu_monitor <- renderMenu({
    if (user()$bloqueado) {
      return(NULL)
    } else {
      if (!(user()$rol %in% c('administrativo'))) {
        print("Desplegando menu monitor")
        menuItem(
          text = "Seguimiento",
          tabName = "tab3_seguimiento",
          icon = icon("binoculars", lib = "font-awesome")
        )
      }
    }
  })
  
  output$menu_satisfaccion <- renderMenu({
    if (user()$bloqueado) {
      return(NULL)
    } else {
      if (user()$rol %in% c('admin', 'cliente', 'coordinador')) {
        print("Desplegando menu satisfaccion")
        menuItem(
          text = "Satisfaccion",
          tabName = "tab_encuestas",
          icon = icon("heart", lib = "font-awesome")
        )
      }
    }
  })
  
  output$menu_certificados <- renderMenu({
    if (user()$bloqueado) {
      return(NULL)
    } else {
      #if (user()$rol %in% c('admin', 'cliente_jefatura', 'cliente', 'coordinador')) {
      if (user()$rol %in% c('admin', 'coordinador')) {
        print("Desplegando menu certificados")
        menuItem(
          text = "Certificados",
          tabName = "tab_certificados",
          icon = icon("certificate", lib = "font-awesome")
        )
      }
    }
  })
  
  output$menu_herramientas <- renderMenu({
    if (user()$bloqueado) {
      return(NULL)
    } else {
      if (user()$rol %in% c('admin', 'cliente_jefatura')) {
        print("Desplegando menu herramientas")
        menuItem(
          text = "Herramientas",
          tabName = "tab_herramientas",
          icon = icon("tools", lib = "font-awesome")
        )
      }
    }
  })
  
  # output$menu_resultados <- renderMenu({
  #   if (!(user()$rol %in% c('asistente', 'coach', 'administrativo', 'cliente_jefatura'))) {
  #     print("Desplegando menu resultados")
  #     menuItem(
  #       text = "Resultados",
  #       tabName = "tab4",
  #       icon = icon("traffic-light", lib = "font-awesome")
  #     )
  #   }
  # })
  
  # output$menu_alertas <- renderMenu({
  #   if (user()$rol %in% c('admin','cliente')) {
  #     print("Desplegando menu alertas")
  #     menuItem(
  #       text = "Alertas",
  #       tabName = "tab10",
  #       icon = icon("triangle-exclamation", lib = "font-awesome")
  #     )
  #   }
  # })
  
  # output$menu_analisis <- renderMenu({
  #   if (!(user()$rol %in% c('asistente', 'coach', 'administrativo'))) {
  #     print("Desplegando menu alertas")
  #     menuItem(
  #       text = "Análisis",
  #       tabName = "tab5",
  #       icon = icon("chart-simple", lib = "font-awesome")
  #     )
  #   }
  # })
  
  output$menu_pagos <- renderMenu({
    if (user()$rol %in% c('admin','cliente','administrativo', 'cliente_jefatura')) {
      print("Desplegando menu pre-facturas")
      menuItem(
        text = "Pagos",
        tabName = "tab9_pagos",
        icon = icon("dollar", lib = "font-awesome")
      )
    }
  })
  
  output$menu_monitor_interno <- renderMenu({
    if (user()$rol %in% c('admin', 'administrativo')) {
      print("Desplegando menu monitor interno")
      menuItem(
        text = "Monitor",
        tabName = "tab10_monitor",
        icon = icon("magnifying-glass", lib = "font-awesome")
      )
    }
  })
  
  output$menu_administracion <- renderMenu({
    if (user()$rol %in% c('admin', 'coordinador')) {
      print("Desplegando menu admin")
      menuItem(
        text = "Gestión Clientes",
        tabName = "tab8",
        icon = icon("users", lib = "font-awesome")
      )
    }
  })
  
  output$menu_sistema <- renderMenu({
    if (user()$rol %in% c('admin','coordinador')) {
      print("Desplegando menu admin sistema")
      menuItem(
        text = "Gestión Sistema",
        tabName = "tab_sistema",
        icon = icon("cog", lib = "font-awesome")
      )
    }
  })
  
  observeEvent(input$sign_out, {
    auth0::logout()
  })
  
  inicio_server("inicio", rv_tabs)

  inscripcion_participantes_server("inscripcion_participantes", session$userData$rol, rv_tabs)
  
  monitor_preparaciones_server("monitor_preparaciones", rv_tabs)
  
  resultados_encuesta_server("resultado_satisfaccion", rv_tabs)
  
  certificados_server("certificados", rv_tabs)
  
  # estadisticas_server("estadisticas", rv_tabs)
  
  estado_de_pago_server("estado_de_pago")
  
  pagos_server("pagos", rv_tabs)
  
  herramientas_server("herramientas", rv_tabs)
  
  #registro_resultados_server("registro_resultados", rv_tabs)
  
  # alertas_server("alertas", rv_tabs)
  
  herramientas_internas_server("herramientas_internas", rv_tabs)
  
  gestion_clientes_server("gestion_clientes")
  
  gestion_sistema_server("gestion_sistema")
  
  messageData <- data.frame(
    from = c("Admininstrator", "New User", "Support"),
    message = c(
      "Sales are steady this month.",
      "How do I register?",
      "The new server is ready."
    ),
    stringsAsFactors = FALSE
  )
  
  # output$messageMenu <- renderMenu({
  #   # Code to generate each of the messageItems here, in a list. messageData
  #   # is a data frame with two columns, 'from' and 'message'.
  #   # Also add on slider value to the message content, so that messages update.
  #   
  #   notificaciones <- get_notificaciones(as.numeric(session$userData$id_empresa))
  #   
  #   dropdownMenu(
  #     headerText = "Notificaciones",
  #     badgeStatus = "info",
  #     type = "notifications", 
  #     href = NULL,
  #     lapply(1:nrow(notificaciones), function(i){
  #       notificationItem(inputId = notificaciones[i, "button_id"],
  #                        href = NULL,
  #                        icon = icon(notificaciones[i,]$icono),
  #                        # text = as.character(HTML(notificaciones[i,]$texto)),
  #                        text = tags$b(strsplit(notificaciones[i,]$texto, split = "/n")[[1]][1],
  #                                      tags$br(),
  #                                      strsplit(notificaciones[i,]$texto, split = "/n")[[1]][2]),
  #                        status = notificaciones[i, "estatus"]
  #       )
  #     })
  #     # notificationItem(inputId = "action1",
  #     #   text = "Nuevo modulo de Estado de Pago",
  #     #   status = "success"
  #     # ),
  #     # notificationItem(
  #     #   text = "Modificación horario inscripciones",
  #     #   status = "warning"
  #     # )
  #   )
  # })
  
}

# securing with Polished
# secure_server(server,
#               custom_admin_server = admin_server)


# securing with Auth0
auth0::auth0_server(server, info = a0_info)