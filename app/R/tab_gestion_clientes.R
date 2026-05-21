gestion_clientes_ui <- function(id){
  tabItem(
    tabName = "tab8",
    h1("Gestión de Clientes", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 12,
          # actionButton(NS(id, "gc_agregar"), "Agregar2", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus")),
          actionButton(NS(id, "agregar_cliente"), "Agregar Cliente", class = "btn-success", icon = shiny::icon("user-plus")),
          actionButton(NS(id, "editar_cliente"), "Editar Cliente", class = "btn-success", icon = shiny::icon("user-pen"))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          style = "z-index: 10",
          div(DT::DTOutput(NS(id, "clientes_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput(NS(id, "submenu_gestion_clientes"))
        )
      )
    )
  )
}

gestion_clientes_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      #*******************************************
      #* DESPLIEGUE DE CLIENTES EN TABLA
      #* *****************************************
      
      output$submenu_gestion_clientes <- renderUI({
        
        if (session$userData$rol %in% c('admin')) {
          tabsetPanel(id = NS(id, "client_tabs"), selected = NULL, 
                      type = "tabs",
                      tabPanel(
                        title = "Usuarios",
                        fluidRow(
                          column(width = 12, align = "left", br(), 
                                 actionButton(NS(id, "add_user"), "Agregar"), 
                                 actionButton(NS(id, "del_user"), "Eliminar"),
                                 actionButton(NS(id, "edit_user"), "Editar"),
                                 br(),br(),
                                 div(DT::dataTableOutput(NS(id, "usuarios_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%"))
                        )
                      ),
                      tabPanel(
                        title = "Centro de Costos",
                        fluidRow(
                          column(width = 4, align = "left", br(),
                                 actionButton(NS(id, "add_cc"), "Agregar"), 
                                 actionButton(NS(id, "del_cc"), "Eliminar"), 
                                 br(),br(),
                                 div(DT::dataTableOutput(NS(id, "centro_de_costos_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%"))
                        )
                      ),
                      tabPanel(
                        title = "Estados de Pago",
                        br(),
                        fluidRow(
                          column(
                            width = 3,
                            textInput(NS(id, "email_estado_pago"), "Email de envio EDP"),
                            textInput(NS(id, "email_estado_pago_cc"), "Email de envio CC EDP"),
                            actionButton(NS(id, "save_estado_de_pago"), "Guardar")
                          )
                        )
                      ),
                      tabPanel(
                        title = "Tarifas",
                        fluidRow(
                          column(
                            width = 4, 
                            align = "left", 
                            br(), 
                            radioButtons(NS(id, "unidad_moneda"), "Unidad Moneda", choices = c("UF" = 1, "CLP" = 0), inline = TRUE),
                            textInput(NS(id, "tarifa_normal"), "Valor Unitario", width = "50%"), 
                            # textInput(NS(id, "tarifa_urgente"), "Servicio Urgente", width = "50%"), 
                            actionButton(NS(id, "save_tarifa"), "Guardar"))
                        )
                      )
                      # tabPanel(
                      #   title = "Email Inscripciones",
                      #   br(),
                      #   fluidRow(actionButton(NS(id, "save_notificacion_xemail"), "Guardar"), br()),
                      #   fluidRow(
                      #     column(width = 4, align = "left", p("Los siguientes emails seran usados para enviar copias de las inscripciones"),
                      #            textInput(NS(id, "emails_notificacion"), "Correos electronicos (Ingrese separado por comas)", placeholder = "email@ejemplo.com, email2@ejemplo.com"))
                      #   )
                      # ),
                      # tabPanel(
                      #   title = "Link facturas",
                      #   fluidRow(
                      #     column(width = 4, align = "left", br(), actionButton(NS(id, "save_link"), "Guardar"), br(),textInput(NS(id, "link"), "Link facturas Duemint"))
                      #   )
                      # ),
                      # tabPanel(
                      #   title = "Otros",
                      #   fluidRow(
                      #     fileInput(NS(id, "company_image"), "Logo corporativo")
                      #   )
                      # )
          )
        } else {
          tabsetPanel(id = NS(id, "client_tabs"), selected = NULL, 
                      type = "tabs",
                      tabPanel(
                        title = "Usuarios",
                        fluidRow(
                          column(width = 12, align = "left", br(), 
                                 actionButton(NS(id, "add_user"), "Agregar"), 
                                 actionButton(NS(id, "del_user"), "Eliminar"),
                                 actionButton(NS(id, "edit_user"), "Editar"),
                                 br(),br(),
                                 div(DT::dataTableOutput(NS(id, "usuarios_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%"))
                        )
                      ),
                      tabPanel(
                        title = "Centro de Costos",
                        fluidRow(
                          column(width = 4, align = "left", br(),
                                 actionButton(NS(id, "add_cc"), "Agregar"), 
                                 actionButton(NS(id, "del_cc"), "Eliminar"), 
                                 br(),br(),
                                 div(DT::dataTableOutput(NS(id, "centro_de_costos_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%"))
                        )
                      )
          )
        }
      })
      
      clientes_df <- reactive({
        
        #make reactive to
        dataChangedTrigger()
        # input$submit
        # input$submit_edit
        
        dbExecute(pool, 'SET character set "utf8"')
        dbReadTable(pool, "clientes")
        
      })
      
      output$clientes_table <- DT::renderDataTable({
        table <- clientes_df() %>% 
          mutate(
            fecha_creacion = format(as.Date(fecha_creacion), format = "%d-%m-%y")
          )
        names(table) <- c("ID", "Rut", "Razón social", "Nombre fantasía", "Fecha creación", "Bloqueado")
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searchHighlight = T, searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                          columnDefs = list(list(targets = 0, visible = FALSE)),
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
      })
      
      output$usuarios_table <- DT::renderDataTable({
        table <- get_users(NULL)
        names(table) <- c("Nombres", "Apellidos", "Rut", "Teléfono", "Correo", "Cargo", "Password", "Bloqueado", "Inactividad", "Último Login")
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                          columnDefs = list(list(targets = c(7), visible = FALSE)))
                           ) %>% 
          formatStyle(
            columns = "Inactividad",
            backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
          )
      })
      
      output$centro_de_costos_table <- DT::renderDataTable({
        table <- get_cc(NULL)
        names(table) <- c("ID", "Nombre")
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searching = TRUE, lengthChange = TRUE, autoWidth = F, 
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
      })
      
      observeEvent(input$clientes_table_rows_selected, {
        SQL_df <- clientes_df()
        id_empresa <- SQL_df[input$clientes_table_row_last_clicked,]$id_empresa
        info <- get_estado_pago_info(id_empresa)
        tarifas <- get_tarifas(id_empresa)
        # lista_emails <- get_lista_emails(id_empresa)
        
        output$usuarios_table <- DT::renderDataTable({
          print("rendering table")
          table <- get_users(id_empresa) %>% 
            mutate(
              ultimo_login = if_else(
                is.na(ultimo_login), 
                "Nunca", 
                format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
              )
            )
          print(table)
          names(table) <- c("Nombres", "Apellidos", "Rut", "Teléfono", "Correo", "Cargo", "Rol", "Bloqueado", "Inactividad", "Último Login")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                            #columnDefs = list(list(targets = c(9), visible = FALSE))
                                            )
                             ) %>% 
            formatStyle(
              columns = "Inactividad",
              backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
            ) 
        })
        
        output$centro_de_costos_table <- DT::renderDataTable({
          table <- get_cc(id_empresa)
          names(table) <- c("ID", "Nombre")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
        })
        
        updateTextInput(session, "email_estado_pago", value = info$email_para_envios)
        updateTextInput(session, "email_estado_pago_cc", value = info$email_para_envios_cc)
        updateTextInput(session, "tarifa_normal", value = as.character(sub(".",",", tarifas$tarifa_normal, fixed = T)))
        # updateTextInput(session, "tarifa_urgente", value = as.character(sub(".",",", tarifas$tarifa_urgente, fixed = T)))
        updateRadioButtons(session, "unidad_moneda", selected = tarifas$unidad_UF)
        # updateTextInput(session, "emails_notificacion", value = lista_emails)
        # updateTextInput(session, "link", value = get_link_factura(id_empresa))
      })
      
      #*******************************************
      #* AGREGAR CLIENTE (EMPRESA)
      #*******************************************
      
      #Add data
      appendData <- function(data){
        print(data)
        dbExecute(pool, 'SET character set "utf8"')
        quary <- sqlAppendTable(pool, "clientes", data, row.names = FALSE)
        print(quary)
        dbExecute(pool, quary)
      }
      
      observeEvent(input$agregar_cliente, priority = 20,{
        
        entry_form("submit_cliente")
        
      })
      
      entry_form <- function(button_id){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, textInput(ns("rut"), labelMandatory("Rut"), placeholder = "ej: 12345678-9")),
                     column(6, dateInput(ns("fecha_creacion"), "Fecha inicio actividades", language = "es", weekstart = 1, autoclose = T, value = NA))),
            fluidRow(column(6, 
                            textInput(ns("razon_social"), labelMandatory("Razón Social"), placeholder = ""),
                            textInput(ns("nombre_fantasia"), labelMandatory("Nombre Fantasía"), placeholder = "")),
                     column(6, br(), checkboxInput(ns("cliente_bloqueado"), "Bloqueado", value = F)))
          ),
          tags$div(id = session$ns("constraintPlaceholder")),
          title = "Agregar nuevo cliente",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Guardar")
          ),
          easyClose = TRUE
        ))
      }
      
      observeEvent(input$submit_cliente, priority = 20,{
        appendData(formData())
        shinyjs::reset("entry_form")
        showNotification("Cliente inscrito.", type = "message")
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
        
      })
      
      #save form data into data_frame format
      formData <- reactive({
        
        formData <- data.frame(rut_cliente = input$rut,
                               razon_social = input$razon_social,
                               nombre_fantasia = input$nombre_fantasia,
                               fecha_creacion = input$fecha_creacion,
                               bloqueado = input$cliente_bloqueado,
                               stringsAsFactors = FALSE)
        return(formData)
        
      })
      
      #************************************
      #* EDITAR CLIENTE (EMPRESA)
      #* **********************************
      
      observeEvent(input$editar_cliente, {
        SQL_df <- dbReadTable(pool, "clientes")
        
        showModal(
          if(length(input$clientes_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Seleccione una fila." ),easyClose = TRUE)
          } else if(length(input$clientes_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })  
        
        if(length(input$clientes_table_rows_selected) == 1 ){
          
          entry_form("submit_edit")
          
          updateTextInput(session, "rut", value = SQL_df[input$clientes_table_rows_selected, "rut_cliente"])
          updateTextInput(session, "razon_social", value = SQL_df[input$clientes_table_rows_selected, "razon_social"])
          updateTextInput(session, "nombre_fantasia", value = SQL_df[input$clientes_table_rows_selected, "nombre_fantasia"])
          updateCheckboxInput(session, "cliente_bloqueado", value = SQL_df[input$clientes_table_rows_selected, "bloqueado"])
          updateDateInput(session, "fecha_creacion", value = SQL_df[input$clientes_table_rows_selected, "fecha_creacion"])
          
        }
      })
      
      observeEvent(input$submit_edit, {
        SQL_df <- dbReadTable(pool, "clientes")
        row_selection <- SQL_df[input$clientes_table_row_last_clicked, "id_empresa"] 
        print(row_selection)
        sqlq <- glue::glue_sql("UPDATE clientes set rut_cliente = {input$rut}, razon_social = {input$razon_social}, nombre_fantasia = {input$nombre_fantasia}, bloqueado = {input$cliente_bloqueado} WHERE id_empresa = {row_selection}", .con = pool)
        print(sqlq)
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        dataChangedTrigger(dataChangedTrigger() + 1)
        showNotification("Cliente modificado.", type = "message")
        removeModal()
      })
            
      #************************************
      #* BOTON AGREGAR USUARIO
      #* **********************************

      observeEvent(input$add_user, {
        print(paste0("Estoy en: ",input$client_tabs))
        if (is.null(input$clientes_table_rows_selected)) {
          print("no cliente seleccionado")
          showNotification("Seleccione un cliente primero.", type = "warning")
        }else{
          new_user_form("submit_nuevo_usuario", "Agregar Usuario")
        }
      })
      
      new_user_form <- function(button_id, title_str, edit_mode = FALSE){
        ns <- session$ns
        default_inactivo <- FALSE
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, textInput(ns("user_rut"), labelMandatory("Rut"), placeholder = "ej: 12345678-9"))),
            fluidRow(column(6, textInput(ns("user_nombres"), labelMandatory("Nombres"), placeholder = "")),
                     column(6, textInput(ns("user_apellidos"), labelMandatory("Apellidos"), placeholder = ""))),
            fluidRow(column(6, textInput(ns("user_telefono"), "Teléfono", placeholder = "")),
                     column(6, textInput(ns("user_email"), "Email", placeholder = ""))),
            fluidRow(column(6, textInput(ns("user_cargo"), "Cargo Empresa", placeholder = "")),
                     column(6, textInput(ns("user_password"), "Password", placeholder = ""))),
            fluidRow(column(6, selectInput(ns("user_rol"), "Rol Plataforma", choices = c("Admin"="admin", "Cliente"="cliente", "Cliente Jefatura"="cliente_jefatura", "Coach"="coach", "Administrativo"="administrativo", "Coordinador"="coordinador"), selected = "cliente")),
                     column(6, div(checkboxInput(ns("user_blocked"), "Usuario Bloqueado", value = FALSE), style = "margin-top: 37px;"))),
            # NEW: Inactivity checkbox (only shown in edit mode)
            if (edit_mode) {
              fluidRow(
                column(12, 
                       hr(),
                       h5("Estado de Actividad"),
                       checkboxInput(
                         ns("user_inactivo"), 
                         tags$span(
                           "Usuario Inactivo",
                           tags$small(
                             style = "color: #666; margin-left: 10px;",
                             "(Marcar si el usuario debe ser desactivado por inactividad)"
                           )
                         ),
                         value = default_inactivo
                       ),
                       conditionalPanel(
                         condition = paste0("input['", ns("user_inactivo"), "'] == false"),
                         tags$small(
                           style = "color: green;",
                           icon("check-circle"),
                           " El usuario podrá acceder normalmente al sistema"
                         )
                       ),
                       conditionalPanel(
                         condition = paste0("input['", ns("user_inactivo"), "'] == true"),
                         tags$small(
                           style = "color: red;",
                           icon("exclamation-triangle"),
                           " El usuario verá un mensaje de cuenta desactivada al intentar ingresar"
                         )
                       )
                )
              )
            }
          ),
          tags$div(id = session$ns("constraintPlaceholder")),
          title = title_str,
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Guardar")
          ),
          easyClose = TRUE
        ))
      }
      
      observeEvent(input$submit_nuevo_usuario, priority = 20,{
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        new_user <- data.frame(nombre = input$user_nombres,
                               apellidos = input$user_apellidos,
                               rut = input$user_rut,
                               telefono = input$user_telefono,
                               email = input$user_email,
                               cargo = input$user_cargo,
                               id_empresa = id_emp,
                               rol = input$user_rol,
                               password = input$user_password,
                               bloqueado = input$user_blocked,
                               inactivo = 0,  # New users are active by default
                               ultimo_login = NA)  # No login yet
        response <- add_new_user(new_user, id_emp)
        showNotification("Usuario ingresado.", type = "message")
        shinyjs::reset("new_user_form")
        
        output$usuarios_table <- DT::renderDataTable({
          table <- get_users(id_emp) %>% 
            # table <- get_users(id_empresa) %>% 
            mutate(
              ultimo_login = if_else(
                is.na(ultimo_login), 
                "Nunca", 
                format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
              )
            )
          names(table) <- c("Nombres", "Apellidos", "Rut", "Telefono", "Correo", "Cargo", "Rol", "Bloqueado", "Inactividad", "Último Login")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                            #columnDefs = list(list(targets = c(9), visible = FALSE))
                                            )
                             ) %>% 
            formatStyle(
              columns = "Inactividad",
              backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
            )
        })
        
        removeModal()
        
      })
      
      #************************************
      #* BOTON ELIMINAR USUARIO
      #* **********************************
      
      observeEvent(input$del_user, priority = 20,{
        if (is.null(input$clientes_table_rows_selected)) {
          showNotification("Seleccione un cliente primero.", type = "warning")
          return()
        }
        
        if (is.null(input$usuarios_table_rows_selected)) {
          showNotification("Seleccione un usuario primero.", type = "warning")
          return()
        }
        
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        SQL_df <- dbReadTable(pool, "usuarios") %>% filter(id_empresa == id_emp)
        row_selection <- SQL_df[input$usuarios_table_rows_selected, "id"]
        
        quary <- lapply(row_selection, function(nr){

          dbExecute(pool, sprintf('DELETE FROM usuarios WHERE id = ("%s")', nr))
        })
        
        output$usuarios_table <- DT::renderDataTable({
          table <- get_users(id_emp) %>% 
            # table <- get_users(id_empresa) %>% 
            mutate(
              ultimo_login = if_else(
                is.na(ultimo_login), 
                "Nunca", 
                format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
              )
            )
          names(table) <- c("Nombres", "Apellidos", "Rut", "Teléfono", "Correo", "Cargo", "Rol", "Bloqueado", "Inactividad", "Último Login")
          # names(table) <- c("Nombres", "Apellidos", "Rut", "Telefono", "Correo", "Cargo", "Bloqueado", "Inactividad", "Último Login")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                            # columnDefs = list(list(targets = c(8), visible = FALSE))
                                            )
                             ) %>% 
            formatStyle(
              columns = "Inactividad",
              backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
            )
        })
        showNotification("Usuario eliminado.", type = "message")
      })
      
      #************************************
      #* BOTON EDITAR USUARIO
      #* **********************************
      
      observeEvent(input$edit_user, {
        if (is.null(input$clientes_table_rows_selected)) {
          showNotification("Seleccione un cliente primero.", type = "warning")
          return()
        }
        
        if (is.null(input$usuarios_table_rows_selected)) {
          showNotification("Seleccione un usuario primero.", type = "warning")
          return()
        }
        
        print(paste0("Estoy editando en: ",input$client_tabs))
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        SQL_df <- dbReadTable(pool, "usuarios") %>% filter(id_empresa == id_emp)
        # row_selection <- SQL_df[input$usuarios_table_rows_selected, "id"]
        
        new_user_form("edit_user_button", "Editar Usuario", edit_mode = TRUE)
        
        updateTextInput(session, "user_rut", value = SQL_df[input$usuarios_table_rows_selected, "rut"])
        updateTextInput(session, "user_nombres", value = SQL_df[input$usuarios_table_rows_selected, "nombre"])
        updateTextInput(session, "user_apellidos", value = SQL_df[input$usuarios_table_rows_selected, "apellidos"])
        updateTextInput(session, "user_telefono", value = SQL_df[input$usuarios_table_rows_selected, "telefono"])
        updateTextInput(session, "user_email", value = SQL_df[input$usuarios_table_rows_selected, "email"])
        updateTextInput(session, "user_cargo", value = SQL_df[input$usuarios_table_rows_selected, "cargo"])
        updateTextInput(session, "user_password", value = SQL_df[input$usuarios_table_rows_selected, "password"])
        updateSelectInput(session,"user_rol", selected = SQL_df[input$usuarios_table_rows_selected, "rol"])
        updateCheckboxInput(session, "user_blocked", value = SQL_df[input$usuarios_table_rows_selected, "bloqueado"])
        updateCheckboxInput(session, "user_inactivo", value = SQL_df[input$usuarios_table_rows_selected, "inactivo"] == 1)
        
        # if (is.null(input$clientes_table_rows_selected)) {
        #   print("no cliente seleccionado")
        #   showNotification("Seleccione un cliente primero.", type = "warning")
        # }else{
        #   new_user_form("edit_user_button", "Editar Usuario", edit_mode = TRUE)
        #   
        #   updateTextInput(session, "user_rut", value = SQL_df[input$usuarios_table_rows_selected, "rut"])
        #   updateTextInput(session, "user_nombres", value = SQL_df[input$usuarios_table_rows_selected, "nombre"])
        #   updateTextInput(session, "user_apellidos", value = SQL_df[input$usuarios_table_rows_selected, "apellidos"])
        #   updateTextInput(session, "user_telefono", value = SQL_df[input$usuarios_table_rows_selected, "telefono"])
        #   updateTextInput(session, "user_email", value = SQL_df[input$usuarios_table_rows_selected, "email"])
        #   updateTextInput(session, "user_cargo", value = SQL_df[input$usuarios_table_rows_selected, "cargo"])
        #   updateTextInput(session, "user_password", value = SQL_df[input$usuarios_table_rows_selected, "password"])
        #   updateSelectInput(session,"user_rol", selected = SQL_df[input$usuarios_table_rows_selected, "rol"])
        #   updateCheckboxInput(session, "user_blocked", value = SQL_df[input$usuarios_table_rows_selected, "bloqueado"])
        #   updateCheckboxInput(session, "user_inactivo", value = SQL_df[input$usuarios_table_rows_selected, "inactivo"] == 1)
        # }
      })
      
      observeEvent(input$edit_user_button, priority = 20, {
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        SQL_df <- dbReadTable(pool, "usuarios") %>% filter(id_empresa == id_emp)
        row_selection <- SQL_df[input$usuarios_table_rows_selected, "id"]
        current_email <- SQL_df[input$usuarios_table_rows_selected, "email"]
        
        was_inactive <- SQL_df[input$usuarios_table_rows_selected, "inactivo"] == 1
        is_now_active <- !input$user_inactivo
        resetting <- was_inactive && is_now_active

        sqlq <- glue::glue_sql("UPDATE usuarios set 
                                   rut = {input$user_rut},
                                   nombre = {input$user_nombres}, 
                                   apellidos = {input$user_apellidos},
                                   telefono = {input$user_telefono},
                                   email = {input$user_email},
                                   cargo = {input$user_cargo},
                                   rol = {input$user_rol},
                                   password = {input$user_password},
                                   bloqueado = {input$user_blocked},
                                   ultimo_login = CASE WHEN {resetting} THEN NOW() ELSE ultimo_login END,
                                   inactivo = {as.numeric(input$user_inactivo)}
                                  WHERE id = {row_selection}", .con = pool)
        
        print(sqlq)
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        
        # update participantes cuando el email del usuario ha cambiado, de esta forma no se pierde referencia
        if (current_email != input$user_email) {
          update_email_sql <- glue::glue_sql("UPDATE participantes set 
                                   ingresado_por = {input$user_email}
                                  WHERE ingresado_por = {current_email} and id_empresa = {id_emp}", .con = pool)
          
          dbExecute(pool, update_email_sql)
        }
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        showNotification("Datos modificados.", type = "message")
        shinyjs::reset("new_user_form")
        removeModal()
        # dataChangedTrigger(dataChangedTrigger() + 1)
        
        output$usuarios_table <- DT::renderDataTable({
          table <- get_users(id_emp) %>% 
            # table <- get_users(id_empresa) %>% 
            mutate(
              ultimo_login = if_else(
                is.na(ultimo_login), 
                "Nunca", 
                format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
              )
            )
          names(table) <- c("Nombres", "Apellidos", "Rut", "Teléfono", "Correo", "Cargo", "Rol", "Bloqueado", "Inactividad", "Último Login")
          # names(table) <- c("Nombres", "Apellidos", "Rut", "Telefono", "Correo", "Cargo", "Bloqueado", "Inactividad", "Último Login")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                            # columnDefs = list(list(targets = c(8), visible = FALSE))
                                            )
                             ) %>% 
            formatStyle(
              columns = "Inactividad",
              backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
            )
        })
        
      })
      
      #************************************
      #* BOTON AGREGAR CENTRO DE COSTO
      #* **********************************
      
      observeEvent(input$add_cc, {
        if (is.null(input$clientes_table_rows_selected)) {
          showNotification("Seleccione un cliente primero.", type = "warning")
          return()
        }
        
        print(paste0("Estoy en: ",input$client_tabs))
        
        if (is.null(input$clientes_table_rows_selected)) {
          print("no cliente seleccionado")
          showNotification("Seleccione un cliente primero.", type = "warning")
        }else{
          new_cc_form("submit_nuevo_cc")
        }
      })
      
      new_cc_form <- function(button_id){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, textInput(ns("nombre_cc"), labelMandatory("Nombre"))))
          ),
          tags$div(id = session$ns("constraintPlaceholder")),
          title = "Agregar centro de costo",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Guardar")
          ),
          easyClose = TRUE
        ))
      }
      
      observeEvent(input$submit_nuevo_cc, priority = 20,{
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        new_cc <- data.frame(nombre = input$nombre_cc,
                               id_empresa = id_emp)
        response <- add_new_cc(new_cc, id_emp)
        showNotification("Centro de Costo ingresado.", type = "message")
        shinyjs::reset("new_cc_form")
        
        output$centro_de_costos_table <- DT::renderDataTable({
          table <- get_cc(id_emp)
          names(table) <- c("ID", "Nombre")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
        })
        
        removeModal()
        
      })
      
      #************************************
      #* BOTON ELIMINAR CENTRO DE COSTO
      #* **********************************
      
      observeEvent(input$del_cc, priority = 20,{
        if (is.null(input$clientes_table_rows_selected)) {
          showNotification("Seleccione un cliente primero.", type = "warning")
          return()
        }
        
        id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        SQL_df <- dbReadTable(pool, "centro_de_costos") %>% filter(id_empresa == id_emp)
        row_selection <- SQL_df[input$centro_de_costos_table_rows_selected, "id"]
        
        quary <- lapply(row_selection, function(nr){
          
          dbExecute(pool, sprintf('DELETE FROM centro_de_costos WHERE id = ("%s")', nr))
        })
        
        output$centro_de_costos_table <- DT::renderDataTable({
          table <- get_cc(id_emp)
          names(table) <- c("ID", "Nombre")
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'single',
                             options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
        })
        showNotification("Centro de Costo eliminado.", type = "message")
      })
      
      #************************************
      #* BOTON GUARDAR INFO ESTADO DE PAGO
      #* **********************************
      
      observeEvent(input$save_estado_de_pago, {
        print(paste0("Estoy en: ",input$client_tabs))
        
        if (is.null(input$clientes_table_rows_selected)) {
          print("no cliente seleccionado")
          showNotification("Seleccione un cliente primero.", type = "warning")
        }else{
          # print(paste0("row clinked: ",input$clientes_table_rows_selected))
          id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
          response <- save_estado_de_pago_info(id_emp, NA, NA, input$email_estado_pago, input$email_estado_pago_cc, NA)
          showNotification("Info para Estado de pago guardada.", type = "message")
        }
      })
      
      #************************************
      #* BOTON GUARDAR TARIFA
      #* **********************************
      observeEvent(input$save_tarifa, {
        print(paste0("Estoy en: ",input$client_tabs))
        
        if (is.null(input$clientes_table_rows_selected)) {
          print("no cliente seleccionado")
          showNotification("Seleccione un cliente primero.", type = "warning")
        }else{
          # print(paste0("row clinked: ",input$clientes_table_rows_selected))
          id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
          response <- save_tarifa(id_emp, input$tarifa_normal, NA, input$unidad_moneda)
          showNotification("Tarifa guardada.", type = "message")
        }
      })
      
      #*****************************************************
      #* BOTON GUARDAR NOTIFICACION de INSCRIPCION POR EMAIL
      #* ***************************************************
      # observeEvent(input$save_notificacion_xemail, {
      #   print(paste0("Estoy en: ",input$client_tabs))
      #   
      #   if (is.null(input$clientes_table_rows_selected)) {
      #     print("no cliente seleccionado")
      #   }else{
      #     # print(paste0("row clinked: ",input$clientes_table_rows_selected))
      #     id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
      #     response <- save_inscripciones_notificacion_por_email(id_emp, input$emails_notificacion)
      #     showNotification("Emails guardados.", type = "message")
      #   }
      # })
      
      #************************************
      #* BOTON GUARDAR LINK A FACTURAS
      #* **********************************
      # observeEvent(input$save_link, {
      #   print(paste0("Estoy en: ",input$client_tabs))
      #   
      #   if (is.null(input$clientes_table_rows_selected)) {
      #     print("no cliente seleccionado")
      #   }else{
      #     # print(paste0("row clinked: ",input$clientes_table_rows_selected))
      #     id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
      #     response <- save_link_factura(id_emp, input$link)
      #     showNotification("Link guardado.", type = "message")
      #   }
      # })
    }
  )
  
}