# Habilitar carga masiva de participantes mediante archivo CSV. Solo disponible para admin.

inscripcion_participantes_ui <- function(id) {
  #ns <- NS(id)
  
  tabItem(
    tabName = "tab2",
    h1("Inscripción de Participantes", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 8,
          # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
          actionButton(NS(id, "add_button"), "Inscribir", icon = shiny::icon("plus")),
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

inscripcion_participantes_server <- function(id, user_rol, rv){
  moduleServer(
    id,
    function(input, output, session){
      
        dataChangedTrigger <- reactiveVal(0)
      
        output$admin_buttons <- renderUI({
          ns <- session$ns
          if (session$userData$rol %in% c('admin','coach')) {
            tagList(
              actionButton(ns("edit_button"), "Editar", class = "btn-success", icon("edit")),
              actionButton(ns("delete_button"), "Borrar", class = "btn-success", icon("trash-alt")),
              actionButton(ns("carga_masiva"), "Carga masiva", class = "btn-success"),
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
           # input$submit
           #input$submit_edit
           #input$copy_button
           #input$delete_button
           input$btn_carga_masiva
           rv$tab_inscripciones_clicked
           # rv$cambio_empresa
           
           updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
           dbExecute(pool, 'SET character set "utf8"')
           DBI::dbReadTable(pool, "participantes") %>% 
             filter(
               if (as.numeric(session$userData$id_empresa) == 0) {
                 borrado == 0
               } else {
                 borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
               }
             ) %>% 
             data.table::setorder(-fecha_solicitud)
           
         })  
         
         
         #save form data into data_frame format
         formData <- reactive({
           print("inside formData")
           formData <- data.frame(#id = paste0(1,input$rut,input$cargo),
                                  id_empresa = as.numeric(session$userData$id_empresa),
                                  rut = input$rut,
                                  nombres = input$nombres,
                                  apellidos = input$apellidos, 
                                  telefono = input$telefono,
                                  email = input$email,
                                  centro_de_costo = input$centrocosto,
                                  cargo = input$cargo,
                                  fecha_solicitud = as.character(Sys.Date()),
                                  fecha_online = input$fecha_online,
                                  fecha_presencial = input$fecha_presencial,
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
         
         #Add data
         appendData <- function(data){
           print(data)
           dbExecute(pool, 'SET character set "utf8"')
           quary <- sqlAppendTable(pool, "participantes", data, row.names = FALSE)
           dbExecute(pool, quary)
         }
         
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
             entry_form("submit", "Agregar participante")
             updateSelectInput(session, "centrocosto", choices = get_centro_de_costos(as.numeric(session$userData$id_empresa)), selected = '')
             output$checkGroup_capacitaciones <- renderUI({
               checkboxGroupInput(ns("checkBoxGroup"), label = "Dimensiones a capacitar", 
                                  choices = c("Psicolaboral" = 1, "Conductual" = 2, "Conocimientos Seguridad" = 3,
                                              "Identificacion de Riesgos" = 4, "Técnico Teórico" = 5, "Gestión" = 6), 
                                  inline = F,
                                  selected = NULL)
             })
           }
         })
         
         iv <- InputValidator$new()
         iv$add_rule("rut", sv_required(message = "Debe ingresar un rut"))
         iv$add_rule("rut", function(rut_value) {
           if (!validate_rut(rut_value)) {
             "RUT no valido"
           }
         })
         iv$add_rule("nombres", sv_required(message = "Debe ingresar un nombre"))
         iv$add_rule("apellidos", sv_required(message = "Debe ingresar un apellido"))
         iv$add_rule("telefono", sv_required(message = "Debe ingresar un telefono"))
         iv$add_rule("email", sv_email(message = "Ingresar un email valido"))
         iv$add_rule("fecha_presencial", ~ if(input$fecha_online > input$fecha_presencial) "Fecha presencial no puede ser menor a fecha online")
         
         emails_a_notificar <- reactive({
           if (input$emails_a_notificar == "") {
             NULL
           }else{
             trimws(strsplit(input$emails_a_notificar, ",")[[1]])
           }
         })
         
         observeEvent(input$submit, priority = 20,{
           print("dentro de submit")
           iv$enable()
           req(iv$is_valid())
           
           withProgress(message = "Procesando inscripción",
                        detail = "Esto podría tomar un momento...", value = 0,{
                          setProgress(0.2, message = "Ingresando participante")
                          appendData(formData())
                          print(paste0("solicitud especial: ",input$tipo_solicitud))
                          print(paste0("fecha solicitud: ", input$fecha_solicitud_urgente))
                          print(paste0("horario solicitud: ", input$horario_urgente))
                          if (input$tipo_solicitud) {
                            update_solicitud_especial(input$rut, input$fecha_solicitud_urgente, input$horario_urgente)
                          }
                          # showNotification("Participante inscrito.", type = "message")
                          setProgress(0.4, message = "Participante inscrito")
                          setProgress(0.7, message = "Enviando correo de notificación")
                          cita <- obtener_fecha_hora_cita(as.numeric(session$userData$id_empresa), input$rut, Sys.Date())
                          # showNotification("Enviando correos....", type = "message")
                          # emails_a_notificar <- input$emails_a_notificar
                          # if (input$emails_a_notificar == "") {
                          #   emails_a_notificar <- NULL
                          # }
                          envio_email_participante(cita, input$email, input$nombres, c(session$userData$email, emails_a_notificar()))
                          # envio_email_solicitante(cita, session$userData$email,session$userData$nombre, trimws(strsplit(input$emails_a_notificar, ",")[[1]]), paste(input$nombres, input$apellidos))
                          # showNotification("Email de notificacion y confirmacion enviado.", type = "message")
                          setProgress(1, message = "Correos enviados")
                        })
           
           
           shinyjs::reset("entry_form")
           dataChangedTrigger(dataChangedTrigger() + 1)
           removeModal()
           
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
           SQL_df <- dbReadTable(pool, "participantes") %>% 
             filter(
               if (as.numeric(session$userData$id_empresa) == 0) {
                 borrado == 0
               } else {
                 borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
               }
             ) %>% 
             data.table::setorder(-fecha_solicitud)
           row_selection <- SQL_df[input$responses_table_rows_selected, "id"]
           print(paste0("Borrando participante con ID: ", row_selection))
           ahora <- Sys.time()
           sqlq <- glue::glue_sql("UPDATE participantes set 
                                     borrado = 1,
                                     fecha_borrado = {ahora},
                                     borrado_por = {session$userData$email}
                                     WHERE id_empresa = {as.numeric(session$userData$id_empresa)} AND id = {row_selection}", .con = pool)
           print(sqlq)
           dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
           dbExecute(pool, sqlq)
           dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
           showNotification("Participante Eliminado del sistema.", type = "message")
           removeModal()
           dataChangedTrigger(dataChangedTrigger() + 1)
         })
         
         #edit data
         observeEvent(input$edit_button,{
           ns <- session$ns
           SQL_df <- dbReadTable(pool, "participantes") %>% 
             filter(
               if (as.numeric(session$userData$id_empresa) == 0) {
                 borrado == 0
               } else {
                 borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
               }
             ) %>%
             data.table::setorder(-fecha_solicitud)
           
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
           
           if(length(input$responses_table_rows_selected) == 1 ){
             
             entry_form("submit_edit", "Editar participante")
             
             updateTextInput(session, "rut", value = SQL_df[input$responses_table_rows_selected, "rut"])
             updateTextInput(session, "nombres", value = SQL_df[input$responses_table_rows_selected, "nombres"])
             updateTextInput(session, "apellidos", value = SQL_df[input$responses_table_rows_selected, "apellidos"])
             updateTextInput(session, "telefono", value = SQL_df[input$responses_table_rows_selected, "telefono"])
             updateTextInput(session, "email", value = SQL_df[input$responses_table_rows_selected, "email"])
             updateSelectInput(session, "centrocosto", choices = get_centro_de_costos_por_participante(SQL_df[input$responses_table_rows_selected, "id"]), selected = SQL_df[input$responses_table_rows_selected, "centro_de_costo"])
             updateTextInput(session, "cargo", value = SQL_df[input$responses_table_rows_selected, "cargo"])
             updateDateInput(session, "fecha_online", value = SQL_df[input$responses_table_rows_selected, "fecha_online"])
             updateDateInput(session, "fecha_presencial", value = SQL_df[input$responses_table_rows_selected, "fecha_presencial"])
             updateCheckboxInput(session, "tipo_solicitud", value = SQL_df[input$responses_table_rows_selected, "urgencia"])
             selectedGroup = c(as.numeric(SQL_df[input$responses_table_rows_selected, "psicolaboral"])*1,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "conductual"])*2,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "conocimiento_seguridad"])*3,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "vr"])*4,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "tecnico_teorico"])*5,
                               as.numeric(SQL_df[input$responses_table_rows_selected, "gestion"])*6
             )
             selectedGroup = selectedGroup[!is.na(selectedGroup) & !selectedGroup == 0]
             
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
               checkboxGroupInput(ns("checkBoxGroup"), label = "Dimensiones a capacitar", 
                                  choices = c("Psicolaboral" = 1, "Conductual" = 2, "Conocimientos Seguridad" = 3,
                                              "Identificacion de Riesgos" = 4, "Técnico Teórico" = 5, "Gestión" = 6), 
                                  inline = F,
                                  selected = selectedGroup)
             })
             
           }
           
         })
         
         observeEvent(input$submit_edit, priority = 20, {
           
           SQL_df <- dbReadTable(pool, "participantes") %>% 
             filter(
               if (as.numeric(session$userData$id_empresa) == 0) {
                 borrado == 0
               } else {
                 borrado == 0 & id_empresa == as.numeric(session$userData$id_empresa)
               }
             ) %>%
             data.table::setorder(-fecha_solicitud)
           row_selection <- SQL_df[input$responses_table_row_last_clicked, "id"] 
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
                                   fecha_online = {input$fecha_online},
                                   fecha_presencial = {input$fecha_presencial},
                                   psicolaboral = {ps},
                                   conductual = {co},
                                   conocimiento_seguridad = {cs},
                                   vr = {vr},
                                   tecnico_teorico = {tt},
                                   gestion = {ge},
                                   urgencia = {input$tipo_solicitud}
                                  WHERE id = {row_selection}", .con = pool)
           dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
           dbExecute(pool, sqlq)
           dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
           showNotification("Datos modificados.", type = "message")
           shinyjs::reset("entry_form")
           removeModal()
           dataChangedTrigger(dataChangedTrigger() + 1)
           
         })
         
         output$responses_table <- DT::renderDT({
           table <- responses_df() %>% select(-c(id, id_empresa, fecha_solicitud, ingresado_por, borrado, fecha_borrado, borrado_por)) %>% 
             mutate(nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
                    #apellidos = str_to_title(apellidos),
                    contacto = paste0("<strong>", str_to_lower(email), "</strong>", "<br>", "<i>", telefono, "</i>"),
                    cargo = str_to_title(cargo),
                    fecha_online = format(as.Date(fecha_online), format = "%d-%m-%y"),
                    fecha_presencial = format(as.Date(fecha_presencial), format = "%d-%m-%y"),
                    psicolaboral = if_else(psicolaboral == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                    conductual = if_else(conductual == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                    conocimiento_seguridad = if_else(conocimiento_seguridad == 1, as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                    vr = if_else(vr == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                    tecnico_teorico = if_else(tecnico_teorico == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character("")),
                    gestion = if_else(gestion == '1', as.character(icon("ok", lib = "glyphicon", style = "color:blue;")), as.character(""))) %>%
             select(-apellidos, -email, -telefono) %>%
             relocate(contacto, .after = nombres) %>%
             mutate(index = row_number()) %>% 
             relocate(index) 
           names(table) <- c("n", "Rut", "Participante","Contacto","Centro Costo","Cargo",
                             "Fecha Online", "Fecha Presencial", "PS", "CO", "CS", 
                             "VR", "TT", "GE","Urgencia")
           table <- datatable(table, 
                              rownames = FALSE,
                              escape = FALSE,
                              class = 'cell-border stripe',
                              selection = 'single',
                              options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F, ordering = F,
                                             columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                               list(targets = 14, visible = FALSE)),
                                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                             ),
                              callback = JS(paste0("var tips = ['Index', 'Rut', 'Participante', 'Contacto', 'Centro de Costo', 'Cargo', 'Fecha online', 'Fecha presencial', 'Psicolaboral', 'Conductual', 'Conocimiento Seguridad', 'Identificación de Riesgos', 'Técnico Teórico', 'Gestión','Urgencia'],
                                            firstRow = $('#",session$ns('responses_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
           )
           
         })
         
         time_vector <- format(seq(as.POSIXct("2017-01-01", tz = "UTC"), as.POSIXct("2017-01-02", tz = "UTC"), by = "hour"), format="%H:%M")[10:23]
         restricted_dates <- seq(ymd('2012-01-01'),today(), by = 'days')
         restricted_dates_urgent <- seq(ymd('2012-01-01'),today()-1, by = 'days')
         
         entry_form <- function(button_id, ptitle){
           ns <- session$ns
           showModal(modalDialog(
             fluidPage(
               tabsetPanel(
                 id = ns("inTabset"),
                 type = "hidden",
                 tabPanel(
                   title = "Participante",
                   br(),
                   p("Ingrese a continuacion los datos del participante:"),
                   br(),
                   fluidRow(column(6, textInput(ns("rut"), labelMandatory("Rut"), placeholder = "ej: 12345678-9"))),
                   fluidRow(column(6, textInput(ns("nombres"), labelMandatory("Nombres"), placeholder = "")),
                            column(6, textInput(ns("apellidos"), labelMandatory("Apellidos"), placeholder = ""))),
                   fluidRow(column(6, textInput(ns("telefono"), labelMandatory("Telefono"), placeholder = "")),
                            column(6, textInput(ns("email"), labelMandatory("Email"), placeholder = ""))),
                   fluidRow(column(6, selectInput(ns("centrocosto"), "Centro de Costo", choices = NULL)),
                            column(6, textInput(ns("cargo"), labelMandatory("Cargo"), placeholder = ""))),
                   fluidRow(column(6, dateInput(ns("fecha_online"), "Fecha Online", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates)),
                            column(6, dateInput(ns("fecha_presencial"), "Fecha presencial", language = "es", weekstart = 1, autoclose = T, value = NA, datesdisabled = restricted_dates))),
                   actionButton(ns("tab_solicitud_btn"), "Paso 2: Solicitud >>")
                 ),
                 tabPanel(
                   title = "Solicitud",
                   br(),
                   fluidRow(checkboxInput(ns("tipo_solicitud"), "Especial", value = F)),
                   conditionalPanel(
                     # condition = "input.tipo_solicitud == '1'",
                     condition = paste0('input[\'', ns('tipo_solicitud'), "\'] == \'1\'"),
                     fluidRow(
                       column(
                         width = 6,
                         dateInput(ns("fecha_solicitud_urgente"), "Fecha", language = "es", weekstart = 1, autoclose = T, datesdisabled = restricted_dates_urgent),
                       ),
                       column(
                         width = 6,
                         # textInput(ns("horario_urgente"), "Horario", placeholder = "ej: 8:30 am/pm")
                         selectInput(ns("horario_urgente"), "Horario", choices = time_vector)
                       )
                     )
                   ),
                   fluidRow(uiOutput(ns("checkGroup_capacitaciones"))),
                   fluidRow(
                     actionButton(ns("tab_participante_btn"), "<< Paso 2: Participante"),
                     actionButton(ns("tab_notificaciones_btn"), "Paso 3: Notificaciones >>")
                   )
                 ),
                 tabPanel(
                   title = "Notificaciones",
                   br(),
                   fluidRow(textInput(ns("emails_a_notificar"), "Ingrese emails separados por comas", placeholder = "email1@email.com, email2@email.com")),
                   br(),
                   actionButton(ns("tab_solicitud_back_btn"), "<< Paso 2: Solicitud")
                 )
               )
             ),
             tags$div(id = session$ns("constraintPlaceholder1")),
             title = ptitle,
             footer = tagList(
               modalButton("Cancelar"),
               actionButton(ns(button_id), "Inscribir")
             ),
             easyClose = TRUE
           ))
         }
         
         observeEvent(input$tab_solicitud_btn, {
           iv$enable()
           req(iv$is_valid())
           updateTabsetPanel(session, "inTabset",selected = "Solicitud")
         })
         
         observeEvent(input$tab_notificaciones_btn, {
           updateTabsetPanel(session, "inTabset",selected = "Notificaciones")
         })
         
         observeEvent(input$tab_participante_btn, {
           updateTabsetPanel(session, "inTabset",selected = "Participante")
         })
         
         observeEvent(input$tab_solicitud_back_btn, {
           updateTabsetPanel(session, "inTabset",selected = "Solicitud")
         })
         
         observe({
           toggleState("submit", (input$rut != "" | is.null(input$rut)) && 
                                 (input$nombres != "" | is.null(input$nombres)) &&
                                 (input$apellidos != "" | is.null(input$apellidos)) &&
                                 (input$telefono != "" | is.null(input$telefono)) && 
                                 (input$email != "" | is.null(input$email)) &&
                                 (input$cargo != "" | is.null(input$cargo)) &&
                                 (length(input$checkBoxGroup) > 0))
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
                      fecha_solicitud = as.character(Sys.Date()),
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
