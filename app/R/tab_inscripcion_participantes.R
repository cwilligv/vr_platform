# Habilitar carga masiva de participantes mediante archivo CSV. Solo disponible para admin.

inscripcion_participantes_ui <- function(id) {
  #ns <- NS(id)
  
  tabItem(
    tabName = "tab2_inscripciones",
    h1("Inscripción de Participantes", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 8,
          # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
          # actionButton(NS(id, "add_button"), "Inscribir", icon = shiny::icon("plus")),
          # actionButton(NS(id, "edit_button"), "Editar", class = "btn-success", icon("edit")),
          uiOutput(NS(id,"inscribir_editar_buttons"), inline = T),
          uiOutput(NS(id,"admin_buttons"), inline = T)
          # actionButton(NS(id, "edit_button"), "Editar", class = "btn-success", icon("edit")),
          # actionButton(NS(id, "delete_button"), "Borrar", class = "btn-success", icon("trash-alt")),
          # actionButton(NS(id, "carga_masiva"), "Carga masiva", class = "btn-success")
        ),
        column(
          width = 4,
          uiOutput(NS(id, "admin_selector"))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          style = "z-index: 10",
          div(DT::DTOutput(NS(id, "responses_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
        )
      )
    )
  )
}

#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# inscripcion_participantes_server <- function(id, user_rol, rv, telemetry){
inscripcion_participantes_server <- function(id, user_rol, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      inputs_to_track <- c("add_button", "edit_button", "delete_button", "submit", "submit_edit")
      
      # walk(
      #   inputs_to_track,
      #   track_value = TRUE
      # )
      # telemetry$log_input(
      #   input_id = inputs_to_track,
      #   track_value = TRUE,
      #   session = session
      # )
      
      # telemetry$log_navigation_manual(
      #   navigation_id = NS(id, "inscripcion_participantes"),
      #   value = "inscripcion_participantes",
      #   session = session
      # )
      
      dataChangedTrigger <- reactiveVal(0)
      
      output$inscribir_editar_buttons <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('coach')) {
          # tagList(
          #   actionButton(NS(id, "edit_button"), "Editar", class = "btn-success", icon("edit"))
          # )
        } else {
          if (!(session$userData$rol %in% c('asistente'))) {
            tagList(
              actionButton(NS(id, "add_button"), "Inscribir", icon = shiny::icon("plus")),
              actionButton(NS(id, "edit_button"), "Editar", class = "btn-success", icon("edit"))
            )
          } else {
            if (session$userData$rol %in% c('asistente')) {
              tagList(
                # actionButton(NS(id, "add_button"), "Inscribir", icon = shiny::icon("plus")),
                actionButton(NS(id, "edit_button"), "Editar", class = "btn-success", icon("edit"))
              )
            }
          }
        }
      })
    
      output$admin_buttons <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            # actionButton(ns("edit_button"), "Editar", class = "btn-success", icon("edit")),
            actionButton(ns("delete_button"), "Borrar", class = "btn-success", icon("trash-alt")),
            actionButton(ns("carga_masiva"), "Carga masiva", class = "btn-success")
            # selectInput("listado_empresas", "Clientes", choices = get_empresas(session$userData$rol, session$userData$email))
          )
        }
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
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
       # ================= BEGIN: INSCRIPCIONES =======================
       
       #load responses_df and make reactive to inputs  
       responses_df <- reactive({
         
         #make reactive to
         dataChangedTrigger()
         input$btn_carga_masiva
         rv$tab_inscripciones_clicked
         
         updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
         
         # Filtro WHERE
         if (as.numeric(session$userData$id_empresa) == 0) {
           # filtros <- glue::glue_sql("borrado = 0", .con = pool)
           filtros <- glue::glue_sql("1 = 1", .con = pool)
         } else {
           filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
         }
         
         dbExecute(pool, 'SET character set "utf8"')
         tbl <- glue::glue_sql("select * from {`db`}.participantes_view where ({filtros})", .con = pool)
         dbGetQuery(pool, tbl)
         
       })  
       
       
       #save form data into data_frame format
       formData <- reactive({
         print("inside formData")
         formData <- data.frame(#id = paste0(1,input$rut,input$cargo),
                                id_empresa = as.numeric(session$userData$id_empresa),
                                rut = rutifier::rut_hyphen(input$rut),
                                nombres = input$nombres,
                                apellidos = input$apellidos, 
                                telefono = input$telefono,
                                email = trimws(input$email),
                                centro_de_costo = input$centrocosto,
                                cargo = input$cargo,
                                # fecha_solicitud = as.character(today(tzone = "Chile/Continental")),
                                fecha_solicitud = as.character(now(tzone = "Chile/Continental")),
                                fecha_online = as.character(ifelse(!isTruthy(input$fecha_online), NA, as.character(input$fecha_online))),
                                fecha_presencial = as.character(ifelse(!isTruthy(input$fecha_presencial), NA, as.character(input$fecha_presencial))),
                                #date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
                                psicolaboral = if_else(1 %in% as.vector(input$checkBoxGroup), 1, 0),
                                conductual = if_else(2 %in% as.vector(input$checkBoxGroup), 1, 0),
                                conocimiento_seguridad = if_else(3 %in% as.vector(input$checkBoxGroup), 1, 0),
                                vr = if_else(4 %in% as.vector(input$checkBoxGroup), 1, 0),
                                tecnico_teorico = if_else(5 %in% as.vector(input$checkBoxGroup), 1, 0),
                                gestion = if_else(6 %in% as.vector(input$checkBoxGroup), 1, 0),
                                ingresado_por = session$userData$email,
                                urgencia = input$tipo_solicitud,
                                borrado = 0,
                                fecha_borrado = as.character(NA),
                                borrado_por = as.character(NA),
                                stringsAsFactors = FALSE)
         return(formData)
         
       })
       
       #Add participant record to DB
       appendData <- function(data, fecha, horario){
         print(data)
         dbExecute(pool, 'SET character set "utf8"')
         quary <- sqlAppendTable(pool, "participantes", data, row.names = FALSE)
         dbExecute(pool, quary)
         record <- dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id_participante")
         datos_preparacion <- data.frame(
           id_participante = record$id_participante,
           fecha_preparacion = fecha,
           horario = horario,
           estado = 'en coordinacion',
           coach_id = 3,
           observaciones = NA,
           es_horario_especial = input$tipo_solicitud,
           last_change_by = session$userData$email
         )
         print(datos_preparacion)
         quary <- sqlAppendTable(pool, "monitor_preparaciones", datos_preparacion, row.names = FALSE)
         dbExecute(pool, quary)
         new <- dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id_preparacion")
         update_training_blocks(fecha, horario, -1)
         return(new$id_preparacion)
       }
       
       # Boton inscribir participante
       observeEvent(input$add_button,{
         print(paste0("**empresa--> ", session$userData$id_empresa))
         ns <- session$ns
       
         if(as.numeric(session$userData$id_empresa) == 0){
           showModal(
             modalDialog(
               title = "Advertencia",
               paste("Seleccione una empresa antes de incribir." ),
               footer = tagList(
                 modalButton("Cerrar")
               ),
               easyClose = TRUE)
           )
         } else {
           disponible <- interval(lubridate::ymd_hms(paste(today(tzone = "Chile/Continental"),"8:00:00")), lubridate::ymd_hms(paste(today(tzone = "Chile/Continental"),"19:00:00")))
           activar_fuera_horario <- get_system_variable('inscripciones', NULL, 'activar_fuera_horario')
           if (lubridate::ymd_hms(now(tzone = "Chile/Continental")) %within% disponible | session$userData$rol %in% c('admin','coach') | activar_fuera_horario) {
             entry_form("submit", "Inscribir participante")
             updateSelectInput(session, "centrocosto", choices = get_centro_de_costos(as.numeric(session$userData$id_empresa)), selected = '')
             output$checkGroup_capacitaciones <- renderUI({
               checkboxGroupInput(ns("checkBoxGroup"), label = "",
                                  choices = c("Psicolaboral" = 1, "Conductual" = 2, "Conocimientos Seguridad" = 3,
                                              "Identificación de Riesgos" = 4, "Técnico Teórico (*)" = 5, "Gestión" = 6),
                                  inline = F,
                                  selected = NULL)

             })
           } else {
             showModal(
               modalDialog(
                 title = tags$a(style = "color: black", 'Ooops...!', icon('face-sad-tear')),
                 p(paste("Lo sentimos ",session$userData$nombre,", la plataforma se encuentra fuera de horario para realizar inscripciones de capacitación (8:00 a 19:00)" )),
                 HTML("<b>Si se trata de una solicitud urgente, contáctanos directamente por los canales habituales.</b>"),
                 footer = tagList(
                   modalButton("Cerrar")
                 ),
                 easyClose = TRUE)
             )
           }
           
         }
       })
       
       iv <- InputValidator$new()
       iv$add_rule("rut", sv_required(message = "Debe ingresar un rut"))
       iv$add_rule("rut", function(rut_value) {
         if (!validate_rut(rut_value)) {
           "RUT no válido"
         }
       })
       iv$add_rule("nombres", sv_required(message = "Debe ingresar un nombre."))
       iv$add_rule("nombres", function(value) {
         if (grepl("@", value)) {
           return("El símbolo @ no está permitido en este campo")
         }
         return(NULL)  # NULL indicates validation passed
       })
       iv$add_rule("apellidos", sv_required(message = "Debe ingresar un apellido."))
       iv$add_rule("apellidos", function(value) {
         if (grepl("@", value)) {
           return("El símbolo @ no está permitido en este campo")
         }
         return(NULL)  # NULL indicates validation passed
       })
       iv$add_rule("telefono", sv_required(message = "Debe ingresar un teléfono."))
       iv$add_rule("telefono", function(tel_value) {
         # message(paste0("inside telefono rule: ", tel_value))
         if (!grepl("^9\\d{8}$", tel_value)) {
           "Teléfono inválido. Debe comenzar con 9 y tener 9 dígitos."
         }
       })
       iv$add_rule("email", sv_email(message = "Ingresar un email valido"))
       iv$add_rule("cargo", sv_required(message = "Debe ingresar un cargo"))
       iv$add_rule("cargo", function(value) {
         if (grepl("@", value)) {
           return("El símbolo @ no está permitido en este campo")
         }
         return(NULL)  # NULL indicates validation passed
       })
       iv$add_rule("fecha_online", function(value){
         if ((length(value) == 0) && (length(input$fecha_presencial) == 0)) {
           "Debe ingresar una fecha de inicio de Evaluaciones."
         }
       })
       iv$add_rule("fecha_presencial", function(value){
         if ((length(value) == 0) && (length(input$fecha_online) == 0)) {
           "Debe ingresar al menos una fecha, online o presencial."
         }
       })
       iv$add_rule("fecha_solicitud_urgente", function(value){
         
         if (!editing_on()) {
           if((input$tipo_solicitud == 1) && (length(input$fecha_solicitud_urgente) == 0)) {
             "Seleccione una fecha."
           } else {
             if ((input$tipo_solicitud == 1) && (ymd(input$fecha_solicitud_urgente) < lubridate::today(tzone = "Chile/Continental"))) {
               "Fecha incorrecta. Es menor que fecha de inicio."
             }
           }
         }
       })
       iv$add_rule("horario_urgente", function(value){
         
         if (!editing_on()) {
           if((input$tipo_solicitud == 1) && (input$horario_urgente == "")) {
             "Seleccione un horario."
           } else {
             NULL
           }
         }
       })
       
       # Boton submit inscripcion de participante
       observeEvent(input$submit, priority = 20,{
         print("dentro de submit")
         iv$enable()
         req(iv$is_valid())
         withProgress(message = "Procesando inscripción",
                      detail = "Esto podría tomar un momento...", value = 0,{
                        shinyjs::disable("submit")
                        setProgress(0.2, message = "Ingresando participante")
                        # crear_preparacion(input$rut)
                        if (input$tipo_solicitud == 1) {
                          print(paste0("solicitud especial: ",input$tipo_solicitud))
                          print(paste0("fecha solicitud: ", input$fecha_solicitud_urgente))
                          print(paste0("horario solicitud: ", input$horario_urgente))
                          # update_solicitud_especial(input$rut, input$fecha_solicitud_urgente, input$horario_urgente)
                          vfecha = input$fecha_solicitud_urgente
                          vhorario = input$horario_urgente
                        } else {
                          vfecha = input$fecha_prep_autom
                          vhorario = input$hora_prep_autom
                        }
                        
                        id_prep <- appendData(formData(), vfecha, vhorario)
                        setProgress(0.4, message = "Participante inscrito")
                        setProgress(0.7, message = "Enviando correo de notificación")
                        
                        cita <- obtener_fecha_hora_cita(pid_emp = as.numeric(session$userData$id_empresa), prut = rutifier::rut_hyphen(input$rut), pfecha = as.character(today(tzone = "Chile/Continental")), pid_prep = id_prep)
                        envio_email_participante(
                          cita,
                          trimws(input$email),
                          input$nombres,
                          trimws(c(session$userData$email)),
                          # c("capacitacion@mercconsultora.cl", emails_a_notificar()),
                          c("capacitacion@mercconsultora.cl"),
                          session$userData$razon_social,
                          paste(session$userData$nombre, session$userData$apellido),
                          session$userData$cargo,
                          session$userData$telefono,
                          session$userData$email,
                          input$rut,
                          id_prep,
                          'inscripcion',
                          "CAPACITACIÓN"
                        )
                        setProgress(1, message = "Correos de notificación enviados")
                      })
         
         
         shinyjs::reset("entry_form")
         dataChangedTrigger(dataChangedTrigger() + 1)
         removeModal()
         shinyjs::enable("submit")
         v$checkgroupUIDone <- FALSE
       })
       
       #delete data
       observeEvent(input$delete_button, priority = 20,{
         ns <- session$ns
         showModal(
           
           if(length(input$responses_table_rows_selected) < 1 ){
             modalDialog(
               title = "Advertencia",
               paste("Seleccione una fila" ),
               footer = tagList(
                 modalButton("Cerrar")
               ),
               easyClose = TRUE
             )
           }else{
             modalDialog(
               title = "Advertencia",
               h2("Esta seguro de eliminar al participante?"),
               easyClose = F,
               footer = tagList(
                 modalButton("Cancelar"),
                 actionButton(ns("borrar_participante"), "Si, borrar")
               ) 
             )
           }
         )
       })
       
       observeEvent(input$borrar_participante, {
         SQL_df <- responses_df()
         
         row_selection <- SQL_df[input$responses_table_rows_selected, "id"]
         #row_selection <- SQL_df[input$responses_table_row_last_clicked, "id"]
         print(paste0("Borrando participante con ID: ", row_selection))
         ahora <- ymd_hms(now(tzone = "Chile/Continental"))
         sqlq <- glue::glue_sql("UPDATE participantes set 
                                   borrado = 1,
                                   fecha_borrado = {ahora},
                                   borrado_por = {session$userData$email}
                                   WHERE id = {row_selection}", .con = pool)
         print(sqlq)
         dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
         dbExecute(pool, sqlq)
         dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
         showNotification("Participante Eliminado del sistema.", type = "message")
         cita <- obtener_fecha_hora_cita(
           pid_participante = row_selection
         )
         update_training_blocks(pfecha = cita$fecha, phorario = cita$horario, pval = 1)
         showNotification("Fecha/Horario liberado en el sistema.", type = "message")
         removeModal()
         dataChangedTrigger(dataChangedTrigger() + 1)
       })
       
       #edit data
       observeEvent(input$edit_button,{
         ns <- session$ns
         # dbExecute(pool, 'SET character set "utf8"')
         # SQL_df <- dbReadTable(pool, "participantes") %>% 
         #   filter(
         #     if (as.numeric(session$userData$id_empresa) == 0) {
         #       borrado == 0
         #     } else {
         #       borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
         #     }
         #   ) %>%
         #   data.table::setorder(-fecha_solicitud)
         
         SQL_df <- responses_df()
        
         showModal(
           if(length(input$responses_table_rows_selected) > 1 ){
             modalDialog(
               title = "Advertencia",
               paste("Seleccione una fila." ),easyClose = TRUE)
           } else if(length(input$responses_table_rows_selected) < 1){
             modalDialog(
               title = "Advertencia",
               paste("Seleccione una fila." ),
               footer = tagList(
                 modalButton("Cerrar")
               ),
               easyClose = TRUE)
           })  
         
         if (length(input$responses_table_rows_selected) == 1) {
           if(se_puede_editar(SQL_df[input$responses_table_rows_selected, "id"]) | session$userData$rol == 'admin'){
             
             # telemetry$log_input_manual(
             #   input_id = ns("edit_button"),
             #   value = SQL_df[input$responses_table_rows_selected, "rut"],
             #   session = session
             # )
             
             entry_form("submit_edit", "Editar Inscripción")
             
             updateTextInput(session, "rut", value = SQL_df[input$responses_table_rows_selected, "rut"])
             updateTextInput(session, "nombres", value = SQL_df[input$responses_table_rows_selected, "nombres"])
             updateTextInput(session, "apellidos", value = SQL_df[input$responses_table_rows_selected, "apellidos"])
             updateTextInput(session, "telefono", value = SQL_df[input$responses_table_rows_selected, "telefono"])
             updateTextInput(session, "email", value = SQL_df[input$responses_table_rows_selected, "email"])
             updateSelectInput(session, "centrocosto", choices = get_centro_de_costos_por_participante(SQL_df[input$responses_table_rows_selected, "id"]), selected = SQL_df[input$responses_table_rows_selected, "centro_de_costo"])
             updateTextInput(session, "cargo", value = SQL_df[input$responses_table_rows_selected, "cargo"])
             updateDateInput(session, "fecha_online", value = SQL_df[input$responses_table_rows_selected, "fecha_online"])
             updateDateInput(session, "fecha_presencial", value = SQL_df[input$responses_table_rows_selected, "fecha_presencial"])
             # updateCheckboxInput(session, "tipo_solicitud", value = SQL_df[input$responses_table_rows_selected, "urgencia"])
             updateRadioButtons(session, "tipo_solicitud", selected = as.numeric(SQL_df[input$responses_table_rows_selected, "urgencia"]))
             selectedGroup = c(as.numeric(SQL_df[input$responses_table_rows_selected, "psicolaboral"])*1,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "conductual"])*2,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "conocimiento_seguridad"])*3,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "vr"])*4,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "tecnico_teorico"])*5,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "gestion"])*6
             )
             selectedGroup = selectedGroup[!is.na(selectedGroup) & !selectedGroup == 0]
             
             if (session$userData$rol %in% c('cliente')) {
               shinyjs::disable("email")
             }
             shinyjs::disable("fecha_online")
             shinyjs::disable("fecha_presencial")
             shinyjs::disable("tipo_solicitud")
             print("selectedGroup")
             print(selectedGroup)
             print("===========")
             # updateCheckboxGroupInput(session, "checkGroup_capacitaciones", choices = c("Psicolaboral" = 1, "Conductual" = 2, "Conocimientos Seguridad" = 3,
             #                                                                "Identificacion de Riesgos" = 4, "Tecnico Teorico" = 5, "Gestion" = 6),
             #                          
             #                          inline = T,
             #                          selected = c(1,2,5))
             
             
             output$checkGroup_capacitaciones <- renderUI({
               checkboxGroupInput(ns("checkBoxGroup"), label = "", 
                                  choices = c("Psicolaboral" = 1, "Conductual" = 2, "Conocimientos Seguridad" = 3,
                                              "Identificación de Riesgos" = 4, "Técnico Teórico" = 5, "Gestión" = 6), 
                                  inline = F,
                                  selected = selectedGroup
               )
             })
             
           }else{
             showModal(
               modalDialog(
                 title = tags$a(style = "color: black", 'Ooops...!', icon('face-sad-tear')),
                 div(shiny::HTML(paste("Lo sentimos ",session$userData$nombre,", solo es posible editar las inscripciones previo a la capacitación y no es posible realizar modificaciones a la información de inscripciones ya procesadas. <br> <br> Contáctanos si tienes alguna consulta" )), style = "text-align: justify;"),
                 footer = tagList(
                   modalButton("Cerrar")
                 ),
                 easyClose = TRUE)
             )
           }
         }
         
       })
       
       observeEvent(input$submit_edit, priority = 20, {
         # dbExecute(pool, 'SET character set "utf8"')
         # SQL_df <- dbReadTable(pool, "participantes") %>% 
         #   filter(
         #     if (as.numeric(session$userData$id_empresa) == 0) {
         #       borrado == 0
         #     } else {
         #       borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
         #     }
         #   ) %>%
         #   data.table::setorder(-fecha_solicitud)
         SQL_df <- responses_df()
         row_selection <- SQL_df[input$responses_table_row_last_clicked, "id"]
         tipo_solicitud <- SQL_df[input$responses_table_row_last_clicked, "urgencia"]
         f_online <- ifelse(length(input$fecha_online) == 0, as.character(NA), as.character(input$fecha_online))
         f_presencial <- ifelse(length(input$fecha_presencial) == 0, as.character(NA), as.character(input$fecha_presencial))
         print(row_selection)
         ps <- if_else(1 %in% as.vector(input$checkBoxGroup), 1, 0)
         co <- if_else(2 %in% as.vector(input$checkBoxGroup), 1, 0)
         cs <- if_else(3 %in% as.vector(input$checkBoxGroup), 1, 0)
         vr <- if_else(4 %in% as.vector(input$checkBoxGroup), 1, 0)
         tt <- if_else(5 %in% as.vector(input$checkBoxGroup), 1, 0)
         ge <- if_else(6 %in% as.vector(input$checkBoxGroup), 1, 0)
         sqlq <- glue::glue_sql("UPDATE participantes set 
                                 rut = {input$rut},
                                 nombres = {input$nombres}, 
                                 apellidos = {input$apellidos},
                                 telefono = {input$telefono},
                                 email = {input$email},
                                 centro_de_costo = {input$centrocosto},
                                 cargo = {input$cargo},
                                 fecha_online = {f_online},
                                 fecha_presencial = {f_presencial},
                                 psicolaboral = {ps},
                                 conductual = {co},
                                 conocimiento_seguridad = {cs},
                                 vr = {vr},
                                 tecnico_teorico = {tt},
                                 gestion = {ge},
                                 urgencia = {as.numeric(tipo_solicitud)}
                                WHERE id = {row_selection}", .con = pool)

         dbExecute(pool, 'SET character set "utf8"')
         dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
         dbExecute(pool, sqlq)
         dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
         showNotification("Datos modificados.", type = "message")
         shinyjs::reset("entry_form")
         removeModal()
         dataChangedTrigger(dataChangedTrigger() + 1)
         
       })
       
       output$responses_table <- DT::renderDT({
         # table <- responses_df() %>% select(-c(id, id_empresa, fecha_solicitud, ingresado_por, borrado, fecha_borrado, borrado_por)) %>% 
         table <- responses_df() %>% select(-id, -id_empresa, -email, -telefono) %>% 
           mutate(nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
                  #apellidos = str_to_title(apellidos),
                  solicitante = paste0("<strong>", str_to_lower(email_solicitante), "</strong>", "<br>", "<i>", telefono_solicitante, "</i>"),
                  cargo = paste0("<strong>", str_to_title(cargo), "</strong>", "<br>", "<i>", str_to_title(nombre_empresa), "</i>"),
                  # fecha_solicitud = paste0(format(date(fecha_solicitud), format = "%d-%m-%y"), "<br>", sprintf("%02d:%02d", hour(fecha_solicitud), minute(fecha_solicitud))),
                  fecha_solicitud = if_else((lubridate::hour(fecha_solicitud) == 0 & lubridate::minute(fecha_solicitud) == 0),
                                            paste0(format(date(fecha_solicitud), format = "%d-%m-%y"), "<br>", "--:--"),
                                            paste0(format(date(fecha_solicitud), format = "%d-%m-%y"), "<br>", sprintf("%02d:%02d", hour(fecha_solicitud), minute(fecha_solicitud)))),
                  # fecha_solicitud = format(as.Date(fecha_solicitud), format = "%d-%m-%y"),
                  fecha_online = format(as.Date(fecha_online), format = "%d-%m-%y"),
                  # fecha_presencial = format(as.Date(fecha_presencial), format = "%d-%m-%y"),
                  psicolaboral = if_else(psicolaboral == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                  conductual = if_else(conductual == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                  conocimiento_seguridad = if_else(conocimiento_seguridad == 1, as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                  vr = if_else(vr == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                  tecnico_teorico = if_else(tecnico_teorico == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                  gestion = if_else(gestion == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character(""))) %>%
           select(-apellidos, -email_solicitante, -telefono_solicitante, -fecha_presencial, -nombre_empresa) %>%
           relocate(solicitante, .after = nombres) %>%
           mutate(index = row_number()) %>% 
           relocate(index) 
         names(table) <- c("n", "Rut", "Participante","Solicitante","Contrato/Proyecto","Cargo", "Fecha Solicitud",
                           "Fecha Inicio<br>Evaluaciones", "PS", "CO", "CS", "VR", "TT", "GE","Urgencia")
         table <- datatable(table, 
                            rownames = FALSE,
                            escape = FALSE,
                            class = 'cell-border stripe',
                            selection = 'single',
                            options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F, ordering = F,
                                           columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                             list(targets = 14, visible = FALSE)),
                                           language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                           ),
                            callback = JS(paste0("var tips = ['Index', 'Rut', 'Participante', 'Contacto Solicitante', 'Contrato/Proyecto', 'Cargo', 'Fecha de Solicitud de Capacitación','Fecha Inicio de Evaluaciones', 'Psicolaboral', 'Conductual', 'Conocimiento Seguridad', 'Identificación de Riesgos', 'Técnico Teórico', 'Gestión','Urgencia'],
                                          firstRow = $('#",session$ns('responses_table')," thead tr th');
                                          for (var i = 0; i < tips.length; i++) {
                                            $(firstRow[i]).attr('title', tips[i]);
                                          }"))
         ) %>% 
           formatStyle(columns = c("Fecha Solicitud"),
                       valueColumns = c("Urgencia"),
                       border = styleEqual(1, '3px solid #F1C429')
           )
         
       })
       
       # ====
       # This code is triggers when client wants to book urgent and on the same day of booking.
       
       observeEvent(input$fecha_solicitud_urgente, {
         req(input$fecha_solicitud_urgente)
         
         selected_date <- ymd(input$fecha_solicitud_urgente)
         current_date <- lubridate::today(tzone = "Chile/Continental")
         current_time <- lubridate::now(tzone = "Chile/Continental")
         
         # Generate full time vector (all available times)
         full_time_vector <- format(
           seq(as.POSIXct("2017-01-01", tz = "UTC"), 
               as.POSIXct("2017-01-02", tz = "UTC"), 
               by = "30 min"), 
           format = "%I:%M %P"
         )[19:45]
         
         if (selected_date == current_date) {
           # Get current time
           # current_time <- lubridate::ymd_hms("2025-10-09 05:58:00", tz = "Chile/Continental")
           current_time <- lubridate::now(tzone = "Chile/Continental")
           start_time_8am <- lubridate::ymd_hms(paste(lubridate::today(tzone = "Chile/Continental"), "08:00:00"), tz = "Chile/Continental")
           end_time_9pm <- lubridate::ymd_hms(paste(lubridate::today(tzone = "Chile/Continental"), "20:59:59"), tz = "Chile/Continental")
           
           if (between(current_time, start_time_8am, end_time_9pm)) {
             current_hour <- as.numeric(format(current_time, "%H"))
             
             # Calculate the next hour (starting point)
             next_hour <- current_hour + 1
             
             full_time_vector <- format(seq(as.POSIXct("2017-01-01", tz = "UTC"), as.POSIXct("2017-01-02", tz = "UTC"), by = "hour"), format="%I:%M %P")[9:22]
             # Calculate which positions correspond to our desired time range
             # Position 1 in full_time_vector corresponds to 8 AM (hour 8)
             # So hour 'next_hour' corresponds to position (next_hour - 8 + 1)
             start_position <- next_hour - 8 + 1
             end_position <- 14  # Position for 9 PM in the vector (21 - 8 + 1 = 14)
             time_vector <- full_time_vector[start_position:end_position]
           } else {
             time_vector <- c("Sin horarios disponibles")
           }
           
         } else {
           # If selected date is in the future, show all times
           time_vector <- full_time_vector
         }
         
         # Update the selectInput with the filtered time vector
         updateSelectInput(session, "horario_urgente", choices = time_vector)
       })
       
       # ====
       
       # time_vector <- format(seq(as.POSIXct("2017-01-01", tz = "UTC"), as.POSIXct("2017-01-02", tz = "UTC"), by = "30 min"), format="%I:%M %p")[19:45]
       restricted_dates <- seq(ymd('2012-01-01'),lubridate::today(tzone = "Chile/Continental")-1, by = 'days')
       restricted_dates_urgent <- seq(ymd('2012-01-01'),lubridate::today(tzone = "Chile/Continental")-1, by = 'days')
       
       editing_on <- reactiveVal(FALSE)
       
       entry_form <- function(button_id, ptitle){
         ns <- session$ns
         
         if (ptitle == 'Inscribir participante') {
           showModal(modalDialog(
             fluidPage(
               tabsetPanel(
                 id = ns("inTabset"),
                 type = "hidden",
                 tabPanel(
                   title = "Participante",
                   h4("Participante"),
                   br(),
                   p("Ingrese a continuación los datos del participante:"),
                   br(),
                   fluidRow(column(6, textInput(ns("rut"), labelMandatory("Rut"), placeholder = "ej: 12345678-9"))),
                   fluidRow(column(6, textInput(ns("nombres"), labelMandatory("Nombres"), placeholder = "")),
                            column(6, textInput(ns("apellidos"), labelMandatory("Apellidos"), placeholder = ""))),
                   fluidRow(column(6, textInput(ns("telefono"), labelMandatory("Teléfono"), placeholder = "")),
                            column(6, textInput(ns("email"), labelMandatory("Email"), placeholder = ""))),
                   fluidRow(column(6, selectInput(ns("centrocosto"), "Contrato/Proyecto", choices = NULL)),
                            column(6, textInput(ns("cargo"), labelMandatory("Cargo"), placeholder = ""))),
                   # fluidRow(
                   #   h4("Fechas Evaluación"),
                   #   br(),
                   #   p("Indique una o ambas, según evaluaciones a rendir."),
                   # ),
                   h4("Fechas Evaluación"),
                   # p("Indique una o ambas, según evaluaciones a rendir."),
                   p("Indique la fecha que comenzará a rendir las evaluaciones."),
                   fluidRow(column(6, dateInput(ns("fecha_online"), "Fecha de Inicio", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates)),
                            column(6, shinyjs::hidden(dateInput(ns("fecha_presencial"), "Fecha presencial", language = "es", weekstart = 1, autoclose = T, value = NA)))#, datesdisabled = restricted_dates))),
                   ),
                   br(),
                   fluidRow(
                     column(4),
                     column(4),
                     column(4, actionButton(ns("tab_participante_forward_btn"), label = div("Siguiente", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"))
                   )
                 ),
                 tabPanel(
                   title = "Evaluaciones",
                   h4("Capacitación"),
                   br(),
                   p(
                     style="text-align: justify;",
                     "Importante, seleccionar solo aquellas subdimensiones (Evaluaciones) que rendirá el participante y para las cuales debe recibir capacitación:"
                   ),
                   br(),
                   fluidRow(
                     uiOutput(ns("checkGroup_capacitaciones")),
                     p(
                       style = "font-size: 12px;",
                       "(*) Para capacitar en esta subdimensión, requerimos previamente el Temario Técnico (Deben solicitarlo a entidad evaluadora)."
                     )
                   ),
                   textOutput(ns("checkgroup_error_msg")),
                   br(),
                   fluidRow(
                     column(4, actionButton(ns("tab_evaluaciones_back_btn"), "Atras", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                     column(4),
                     column(4, actionButton(ns("tab_evaluaciones_forward_btn"), label = div("Siguiente", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"))
                   )
                 ),
                 tabPanel(
                   title = "Agendamiento",
                   h4("Agendamiento"),
                   br(),
                   # fluidRow(checkboxInput(ns("tipo_solicitud"), "Agendamiento Especial", value = F)),
                   fluidRow(
                     radioButtons(
                       width = 400,
                       ns("tipo_solicitud"),
                       "",
                       choiceNames = list(
                         HTML(paste0('
                     <b style = "margin-left: 5px; text-align: justify;">  Agendamiento Automático (Recomendado)</b>
                     <p style = "font-weight: normal; text-align: justify;">Se ha designado automáticamente como fecha y horario de capacitación el día antes de la evaluación, como se indica a continuación:</p>
                     ', fluidRow(
                       column(6,shinyjs::disabled(textInput(inputId = ns("fecha_prep_autom"), label = "Fecha Capacitación", value = "2024-01-02"))), 
                       column(6,shinyjs::disabled(textInput(inputId = ns("hora_prep_autom"), label = "Horario", value = "09:00 am")))
                     ))),
                         HTML('
                     <b style = "margin-left: 5px; text-align: justify;">  Agendamiento Especial</b>
                     <p style = "font-weight: normal; text-align: justify;">Indíquenos la fecha y hora que estime conveniente. Favor utilice esta opción solo si es necesario, por ejemplo, en casos que el participante tenga disponibilidad limitada. <u>(Agendamiento especial sujeta a disponibilidad)</u></p>
                     ')
                       ),
                       choiceValues = list("0", "1"),
                       selected = "0"
                     )
                   ),
                   conditionalPanel(
                     # condition = "input.tipo_solicitud == '1'",
                     condition = paste0('input[\'', ns('tipo_solicitud'), "\'] == \'1\'"),
                     fluidRow(
                       column(
                         width = 6,
                         dateInput(ns("fecha_solicitud_urgente"), "Fecha Capacitación", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates_urgent),
                       ),
                       column(
                         width = 6,
                         # textInput(ns("horario_urgente"), "Horario", placeholder = "ej: 8:30 am/pm")
                         selectInput(ns("horario_urgente"), "Horario", choices = c(""))
                       )
                     )
                   ),
                   # fluidRow(uiOutput(ns("checkGroup_capacitaciones"))),
                   br(),
                   fluidRow(
                     column(4, actionButton(ns("tab_agendamiento_back_btn"), "Atras", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                     column(4)
                     # column(4, actionButton(ns("tab_agendamiento_forward_btn"), label = div("Siguiente", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"))
                     
                     
                   )
                 )
                 # tabPanel(
                 #   title = "Notificaciones",
                 #   h4("Notificaciones (Opcional)"),
                 #   br(),
                 #   fluidRow(textInput(ns("emails_a_notificar"), "Emails de quienes desea enviar una copia de la notificación (separados por comas):", placeholder = "por ej: email1@email.com, email2@email.com")),
                 #   br(),
                 #   fluidRow(
                 #     column(4, actionButton(ns("tab_notificaciones_back_btn"), "Atras", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                 #     column(4),
                 #     column(4)
                 #   )
                 #   
                 # )
               )
             ),
             tags$div(id = session$ns("constraintPlaceholder1")),
             title = ptitle,
             footer = tagList(
               actionButton(ns("cancel_inscripcion"), "Cancelar"),
               conditionalPanel(
                 condition = paste0('input[\'', ns('inTabset'), "\'] == \'Agendamiento\'"),
                 # modalButton("Cancelar"),
                 # actionButton(ns("cancel_inscripcion"), "Cancelar"),
                 actionButton(ns(button_id), "Inscribir")
               )
             ),
             # footer = conditionalPanel(
             #   condition = paste0('input[\'', ns('inTabset'), "\'] == \'Notificaciones\'"),
             #   # modalButton("Cancelar"),
             #   # actionButton(ns("cancel_inscripcion"), "Cancelar"),
             #   actionButton(ns(button_id), "Inscribir")
             # ),
             easyClose = FALSE
           ))
         } else {
           editing_on(TRUE)
           showModal(modalDialog(
             fluidPage(
               tabsetPanel(
                 id = ns("inTabset"),
                 type = "hidden",
                 tabPanel(
                   title = "Participante",
                   h4("Participante"),
                   br(),
                   p("Ingrese a continuacion los datos del participante:"),
                   br(),
                   fluidRow(column(6, textInput(ns("rut"), labelMandatory("Rut"), placeholder = "ej: 12345678-9"))),
                   fluidRow(column(6, textInput(ns("nombres"), labelMandatory("Nombres"), placeholder = "")),
                            column(6, textInput(ns("apellidos"), labelMandatory("Apellidos"), placeholder = ""))),
                   fluidRow(column(6, textInput(ns("telefono"), labelMandatory("Teléfono"), placeholder = "")),
                            column(6, textInput(ns("email"), labelMandatory("Email"), placeholder = ""))),
                   fluidRow(column(6, selectInput(ns("centrocosto"), "Contrato/Proyecto", choices = NULL)),
                            column(6, textInput(ns("cargo"), labelMandatory("Cargo"), placeholder = ""))),
                   h4("Fechas Evaluación"),
                   # p("Indique una o ambas, según evaluaciones a rendir."),
                   p("Indique la fecha que comenzará a rendir las evaluaciones."),
                   fluidRow(column(6, dateInput(ns("fecha_online"), "Fecha de Inicio", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates)),
                            column(6, shinyjs::hidden(dateInput(ns("fecha_presencial"), "Fecha presencial", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates)))
                   ),
                   br(),
                   fluidRow(
                     column(4),
                     column(4),
                     column(4, actionButton(ns("tab_participante_forward_btn"), label = div("Siguiente", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"))
                   )
                 ),
                 tabPanel(
                   title = "Evaluaciones",
                   h4("Evaluaciones"),
                   br(),
                   p("Importante, seleccionar solo aquellas subdimensiones (Evaluaciones) que debe rendir el participante:"),
                   br(),
                   fluidRow(uiOutput(ns("checkGroup_capacitaciones"))),
                   textOutput(ns("checkgroup_error_msg")),
                   br(),
                   fluidRow(
                     column(4, actionButton(ns("tab_evaluaciones_back_btn"), "Atras", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                     column(4)
                     # column(4, actionButton(ns("tab_evaluaciones_forward_btn"), label = div("Siguiente", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"))
                   )
                 )
               )
             ),
             tags$div(id = session$ns("constraintPlaceholder1")),
             title = ptitle,
             footer = conditionalPanel(
               condition = paste0('input[\'', ns('inTabset'), "\'] == \'Evaluaciones\'"),
               modalButton("Cancelar"),
               actionButton(ns(button_id), "Guardar")
             ),
             easyClose = TRUE
           ))
         }
         
         
       }
       
       observeEvent(input$cancel_inscripcion, {
         v$checkgroupUIDone <- FALSE
         removeModal()
       })
       
       v <- reactiveValues(
         checkgroupUIDone = FALSE
       )
       
       observeEvent(v$checkgroupUIDone, {
         ns <- session$ns
         if (v$checkgroupUIDone) {
           if (length(input$fecha_presencial) == 0) {
             # deshabilitando este codigo para usar solamente la fecha online como fecha de inicio.
             # shinyjs::delay(600,shinyjs::disable(selector = paste0("#",ns("checkBoxGroup"), " input[value='4']")))
           } 
         }
       })
       
       observeEvent(input$tab_participante_forward_btn , {
         ns <- session$ns
         iv$enable()
         req(iv$is_valid())
         updateTabsetPanel(session, "inTabset",selected = "Evaluaciones")
         
         v$checkgroupUIDone <- TRUE
       })
       
       observeEvent(input$tab_evaluaciones_back_btn , {
         updateTabsetPanel(session, "inTabset",selected = "Participante")
       })
       
       observeEvent(input$tab_evaluaciones_forward_btn , {
         
         output$checkgroup_error_msg <- renderText({
           shiny::validate(
             shiny::need(length(input$checkBoxGroup) > 0, "Seleccione al menos una subdimensión a capacitar.")
           )
         })
         
         shiny::req(length(input$checkBoxGroup) > 0)
         updateTabsetPanel(session, "inTabset",selected = "Agendamiento")
         message(paste0("Fecha online: ", input$fecha_online))
         message(paste0("Fecha presencial: ", input$fecha_presencial))
         fecha_posible <- seleccion_fecha_prep(input$fecha_online, input$fecha_presencial)
         updateTextInput(session, "fecha_prep_autom", value = fecha_posible$fecha)
         updateTextInput(session, "hora_prep_autom", value = fecha_posible$horario)
         print(paste0("Fecha posible: ", fecha_posible$fecha, " ", fecha_posible$horario))
         editing_on(FALSE)
       })
       
       observeEvent(input$tab_agendamiento_back_btn , {
         updateTabsetPanel(session, "inTabset",selected = "Evaluaciones")
       })
       
       observeEvent(input$tab_agendamiento_forward_btn , {
         updateTabsetPanel(session, "inTabset",selected = "Notificaciones")
       })
       
       observeEvent(input$tab_notificaciones_back_btn , {
         updateTabsetPanel(session, "inTabset",selected = "Agendamiento")
       })
       
       is_valid_urgent_date <- reactive({
         if (input$tipo_solicitud == 1) {
           !is.na(input$fecha_solicitud_urgente) && !(ymd(input$fecha_solicitud_urgente) < lubridate::today(tzone = "Chile/Continental"))
         } else {
           TRUE
         }
       })
       
       observe({
         if (!editing_on()) {
           toggleState("submit", (input$rut != "" | is.null(input$rut)) && 
                         (input$nombres != "" | is.null(input$nombres)) &&
                         (input$apellidos != "" | is.null(input$apellidos)) &&
                         (input$telefono != "" | is.null(input$telefono)) && 
                         (input$email != "" | is.null(input$email)) &&
                         (input$cargo != "" | is.null(input$cargo)) &&
                         (length(input$checkBoxGroup) > 0) && 
                         (is_valid_urgent_date()))
         }
       })
       
       # ================= END: INSCRIPCIONES =======================
       
       observeEvent(input$carga_masiva, {
         carga_masiva_form("btn_carga_masiva")
         print(getwd())
       })
       
       carga_masiva_form <- function(button_id){
         ns <- session$ns
         showModal(modalDialog(
           fluidPage(
             fluidRow(downloadLink(ns("download_template"), "Descargue plantilla de carga")),
             fluidRow(fileInput(ns("archivo_carga_masiva"), "Seleccione archivo", accept = ".xlsx", buttonLabel = 'Seleccionar archivo', placeholder = 'ningún archivo')),
             fluidRow(textOutput(ns("archivo_carga_audit")))
           ),
           tags$div(id = session$ns("constraintPlaceholder")),
           title = "Carga masiva de participantes",
           footer = tagList(
             modalButton("Cancelar"),
             actionButton(ns(button_id), "Cargar")
           ),
           easyClose = TRUE
         ))
       }
       
       # logica para descargar plantilla de carga
       output$download_template <- downloadHandler(
         filename = function() {
           paste("c3d_plantilla_carga", "xlsx", sep=".")
         },
         content = function(file) {
           file.copy("www/resources/c3d_plantilla_carga.xlsx", file)
         },
         contentType = "application/zip"
       )
       
       # logica que ejecuta la carga masiva de datos a la base de datos
       observeEvent(input$btn_carga_masiva, {
         print("cargando archivo masivo...")
         if (is.null(input$archivo_carga_masiva)) {
           showModal(modalDialog(
             title = "Mensaje Importante",
             "Nada que procesar. Debes cargar un archivo a procesar"
           ))
         }else{
           Data<-input$archivo_carga_masiva$datapath
           #Name<-input$filename
           file <- read_xlsx(Data, sheet = "participantes",
                             col_types = c('text','text','text','numeric','text','text','text','date','date','text','text','text','text','text','text')) %>% 
             clean_names() %>% 
             mutate_at(10:15, ~replace_na(.,'0')) %>%
             mutate(#id = paste0(1,rut,cargo),
                    id_empresa = as.numeric(session$userData$id_empresa),
                    fecha_solicitud = as.character(today(tzone = "Chile/Continental")),
                    psicolaboral = if_else(psicolaboral == "x", '1', '0'),
                    conductual = if_else(conductual == "x", '1', '0'),
                    conocimiento_seguridad = if_else(conocimiento_seguridad == "x", '1', '0'),
                    identificacion_de_riesgos = if_else(identificacion_de_riesgos == "x", '1', '0'),
                    tecnico_teorico = if_else(tecnico_teorico == "x", '1', '0'),
                    gestion = if_else(gestion == "x", '1', '0'),
                    ingresado_por = session$userData$email,
                    urgencia = 0,
                    borrado = 0,
                    fecha_borrado = NA,
                    borrado_por = NA
             ) %>%
             rename(email = correo,
                    vr = identificacion_de_riesgos) %>% 
             relocate(id_empresa) %>% 
             relocate(fecha_solicitud, .after = cargo)
           appendData(file)
           print(file)
           showNotification("Participante(s) inscrito(s).", type = "message")
           # showNotification("Enviando correos....", type = "message")
           # for (i in 1:nrow(file)) {
           #   cita <- obtener_fecha_hora_cita(session$userData$id_empresa, file[i,]$rut, Sys.Date())
           #   envio_email_participante(cita, file[i,]$email, file[i,]$nombres)
           # }
           # 
           # envio_email_solicitante(cita, session$userData$email,session$userData$nombre, trimws(strsplit(input$emails_a_notificar, ",")[[1]]), paste(input$nombres, input$apellidos))
           # showNotification("Email de notificacion y confirmacion enviado.", type = "message")
           dataChangedTrigger(dataChangedTrigger() + 1)
           removeModal()
         }
       })
       }
    
  )
}
