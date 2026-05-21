usuarios_roles_ui <- function(id){
  fluidRow(
    column(width = 12, align = "left", br(), 
           actionButton(NS(id, "active_btn"), "Activar/Desactivar"),
           br(),br(),
           column(
             width = 8,
             div(DT::dataTableOutput(NS(id, "coachesTable")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
           )
    )
  )
  # tabsetPanel(
  #   id = NS(id, "usuarios_roles_tabs"), 
  #   selected = NULL, 
  #   # vertical = TRUE,
  #   type = "tabs",
  #   tabPanel(
  #     title = "Usuarios",
  #     fluidRow(
  #       column(width = 12, align = "left", br(), 
  #              actionButton(NS(id, "add_user"), "Agregar"), 
  #              actionButton(NS(id, "del_user"), "Eliminar"),
  #              actionButton(NS(id, "edit_user"), "Editar"),
  #              br(),br(),
  #              div(DT::dataTableOutput(NS(id, "usuarios_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
  #       )
  #     )
  #   ),
  #   tabPanel(
  #     title = "Coaches",
  #     br(),
  #     actionButton(NS(id, "active_btn"), "Activar/Desactivar"),
  #     br(),
  #     div(DT::dataTableOutput(NS(id, "coachesTable")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
  #   )
  # )
}

usuarios_roles_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      usersChangedTrigger <- reactiveVal(0)
      
      #*******************************************
      #* DATA SOURCES REACTIVES
      #* *****************************************
      
      coaches_df <- reactive({
        #make reactive to
        dataChangedTrigger()
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.coach", .con = pool)
        dbGetQuery(pool, tbl)
      })
      
      users_df <- reactive({
        usersChangedTrigger()
        dbExecute(pool, 'SET character set "utf8"')
        tbl(pool, "usuarios") %>% 
          left_join(
            tbl(pool, "clientes") %>% select(id_empresa, nombre_fantasia),
            by = "id_empresa"
          ) %>% 
          select(id, nombre, apellidos, rut, telefono, email, cargo, id_empresa, nombre_fantasia, rol, bloqueado, inactivo, ultimo_login) %>% 
          mutate(
            bloqueado = if_else(bloqueado, "Bloqueado", "Desbloqueado"),
            inactivo = if_else(inactivo == 1, "Inactivo", "Activo")
          ) %>% 
          collect()
      })
      
      #*******************************************
      #* USUARIOS
      #* *****************************************
      
      output$usuarios_table <- DT::renderDataTable({
        table <- users_df() %>% 
          select(-id) %>% 
          mutate(
            ultimo_login = if_else(
              is.na(ultimo_login), 
              "Nunca", 
              format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
            )
          )
        table <- datatable(table,
                           colnames = c("Nombres", "Apellidos", "Rut", "Teléfono", "Correo", "Cargo", "id_empresa", "Empresa", "Rol", "Estado", "Inactividad", "Último Login"),
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(
                             searching = TRUE, 
                             lengthChange = TRUE, 
                             autoWidth = TRUE,
                             scrollX = FALSE,
                             columnDefs = list(list(targets = 6, visible = FALSE)),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                           )) %>%
          formatStyle(
            columns = "inactivo",
            backgroundColor = styleEqual(c("Inactivo", "Activo"), c("#ffcccc", "#ccffcc"))
          )
      })
      
      ## AGREGAR USUARIO
      
      observeEvent(input$add_user, {
        new_user_form("submit_nuevo_usuario", "Agregar Usuario")
        updateSelectInput(session, "user_empresa", choices = get_empresas(session$userData$rol, session$userData$email))
      })
      
      new_user_form <- function(button_id, title_str, edit_mode = FALSE, selected_data = NULL){
        ns <- session$ns
        
        # Default values
        default_rut <- ""
        default_nombres <- ""
        default_apellidos <- ""
        default_telefono <- ""
        default_email <- ""
        default_cargo <- ""
        default_password <- ""
        default_rol <- "cliente"
        default_inactivo <- FALSE
        
        # If editing, use selected data
        if (edit_mode && !is.null(selected_data)) {
          default_rut <- selected_data$rut
          default_nombres <- selected_data$nombre
          default_apellidos <- selected_data$apellidos
          default_telefono <- selected_data$telefono
          default_email <- selected_data$email
          default_cargo <- selected_data$cargo
          default_rol <- selected_data$rol
          default_inactivo <- (selected_data$inactivo == "Inactivo")
        }
        
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, textInput(ns("user_rut"), labelMandatory("Rut"), value = default_rut, placeholder = "ej: 12345678-9"))),
            fluidRow(column(6, textInput(ns("user_nombres"), labelMandatory("Nombres"), value = default_nombres, placeholder = "")),
                     column(6, textInput(ns("user_apellidos"), labelMandatory("Apellidos"), value = default_apellidos, placeholder = ""))),
            fluidRow(column(6, textInput(ns("user_telefono"), "Teléfono", value = default_telefono, placeholder = "")),
                     column(6, textInput(ns("user_email"), "Email", value = default_email, placeholder = ""))),
            fluidRow(column(6, selectInput(ns("user_empresa"), "Empresa", choices = NULL)),
                     column(6, textInput(ns("user_cargo"), "Cargo Empresa", value = default_cargo, placeholder = ""))),
            fluidRow(column(6, selectInput(ns("user_rol"), "Rol Plataforma", choices = c("Admin"="admin", "Cliente"="cliente", "Cliente Jefatura"="cliente_jefatura", "Coach"="coach", "Administrativo"="administrativo", "Coordinador"="coordinador"), selected = default_rol)),
                     column(6, textInput(ns("user_password"), "Password", value = default_password, placeholder = ""))),
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
        new_user <- data.frame(nombre = input$user_nombres,
                               apellidos = input$user_apellidos,
                               rut = input$user_rut,
                               telefono = input$user_telefono,
                               email = input$user_email,
                               cargo = input$user_cargo,
                               id_empresa = input$user_empresa,
                               rol = input$user_rol,
                               password = input$user_password,
                               inactivo = 0,  # New users are active by default
                               ultimo_login = NA)  # No login yet
        response <- add_new_user(new_user, input$user_empresa)
        showNotification("Usuario ingresado.", type = "message")
        shinyjs::reset("new_user_form")
        
        usersChangedTrigger(usersChangedTrigger() + 1)
        
        removeModal()
      })
      
      ## EDITAR USUARIOS
      
      observeEvent(input$edit_user, {
        if (length(input$usuarios_table_rows_selected) == 0) {
          showNotification("Por favor, seleccione un usuario.", type = "warning")
          return()
        }
        
        # Get selected row data (including id)
        selected_row <- users_df()[input$usuarios_table_rows_selected, ]
        
        new_user_form("submit_edit_usuario", "Editar Usuario", edit_mode = TRUE, selected_data = selected_row)
        
        # Update empresa selector with current value
        updateSelectInput(
          session, 
          "user_empresa", 
          choices = get_empresas(session$userData$rol, session$userData$email),
          selected = selected_row$id_empresa
        )
      })
      
      observeEvent(input$submit_edit_usuario, priority = 20, {
        selected_row <- users_df()[input$usuarios_table_rows_selected, ]
        user_id <- selected_row$id
        
        # Build update query including inactivo field
        sqlq <- glue::glue_sql("UPDATE usuarios SET 
                                   rut = {input$user_rut},
                                   nombre = {input$user_nombres}, 
                                   apellidos = {input$user_apellidos},
                                   telefono = {input$user_telefono},
                                   email = {input$user_email},
                                   cargo = {input$user_cargo},
                                   id_empresa = {input$user_empresa},
                                   rol = {input$user_rol},
                                   password = {input$user_password},
                                   inactivo = {as.numeric(input$user_inactivo)}
                                  WHERE id = {user_id}", .con = pool)
        
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        
        # If reactivating user (unchecking inactivo), also update ultimo_login
        if (!input$user_inactivo) {
          reactivate_sql <- glue::glue_sql(
            "UPDATE usuarios SET ultimo_login = NOW() WHERE id = {user_id}", 
            .con = pool
          )
          dbExecute(pool, reactivate_sql)
        }
        
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        
        showNotification("Usuario actualizado.", type = "message")
        usersChangedTrigger(usersChangedTrigger() + 1)
        removeModal()
      })
      
      ## ELIMINAR USUARIOS
      
      observeEvent(input$del_user, {
        if (length(input$usuarios_table_rows_selected) == 0) {
          showNotification("Por favor, seleccione un usuario.", type = "warning")
          return()
        }
        
        selected_row <- users_df()[input$usuarios_table_rows_selected, ]
        user_id <- selected_row$id
        
        dbExecute(pool, sprintf('DELETE FROM usuarios WHERE id = ("%s")', user_id))
        
        showNotification("Usuario eliminado.", type = "message")
        usersChangedTrigger(usersChangedTrigger() + 1)
      })
      
      #*******************************************
      #* COACHES
      #* *****************************************
      
      output$coachesTable <- renderDT({
        table <- coaches_df() %>% 
          mutate(
            activo = if_else(activo == TRUE, as.character(icon("check", style = "color:blue;")), as.character(icon("multiply")))
          )
        datatable(
          table,
          colnames = c("id", "Nombre", "Apellidos", "Email", "Activo"),
          selection = 'single',
          rownames = FALSE,
          escape = FALSE,
          options = list(
            scrollX = T, autoWidth = F, ordering = F,
            pageLength = 10,
            dom = 't'
          )
        )
      })
      
      observeEvent(input$active_btn, {
        ns <- session$ns
        showModal(
          if(length(input$coachesTable_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$coachesTable_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          }
        )
        
        if (length(input$coachesTable_rows_selected) == 1 ) {
          SQL_df <- coaches_df()
          
          showModal(
            modalDialog(
              title = "Activación / Desactivación de Coach",
              shinyjs::hidden(textInput(ns("coach_id"), "", value = SQL_df[input$coachesTable_rows_selected, "id"])),
              p(paste0("Coach: ", SQL_df[input$coachesTable_rows_selected, "nombres_coach"], " ", SQL_df[input$coachesTable_rows_selected, "apellidos_coach"])),
              p(paste0("Email: ", SQL_df[input$coachesTable_rows_selected, "email"])),
              selectInput(ns("active_selector"), "Estado:", choices = c("Activo"= 1, "Desactivado"= 0), selected = SQL_df[input$coachesTable_rows_selected, "activo"]),
              br(),
              easyClose = F,
              footer = tagList(
                modalButton("Cancelar"),
                actionButton(ns("cambiar_estado_btn"), "Guardar")
              ) 
            )
          )
        }
      })
      
      observeEvent(input$cambiar_estado_btn, {
        message("Changing coach ",  input$coach_id," status to :", input$active_selector)
        shinyjs::disable("cambiar_estado_btn")
        sqlq <- glue::glue_sql("UPDATE coach
                                set activo = {input$active_selector}
                                WHERE id = {input$coach_id}", .con = pool)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        showNotification("Cambios guardados", type = "message")
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
      })
      
    }
  )
}