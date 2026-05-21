monitor_preparaciones_ui <- function(id){
  tabItem(
    tabName = "tab3_seguimiento",
    h1("Seguimiento Capacitaciones", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      collapsible = F,
      headerBorder = F,
      fluidRow(
        column(
          width = 8,
          # actionButton(NS(id, "mon_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit"))
          uiOutput(NS(id,"admin_buttons"), inline = T)
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
          div(DT::dataTableOutput(NS(id, "monitor_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
        )
      ),
      fluidRow(uiOutput(NS(id, "modal")))
    )
  )
}

monitor_preparaciones_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      # ================= BEGIN: MONITOR =======================
      
      observations_button_server("obs_modal_button", selected_row, dataChangedTrigger, word_pairs_df, obsChangedTrigger)
      dataChangedTrigger <- reactiveVal(0)
      obsChangedTrigger <- reactiveVal(0)
      
      output$admin_buttons <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            actionButton(ns("mon_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
            actionButton(ns("mon_email_resend"), "Correo", class = "btn-success", icon = shiny::icon("paper-plane")),
            # actionButton(ns("mon_suspender"), "Suspender Cita", class = "btn-success", icon = shiny::icon("cancel")),
            # actionButton(ns("mon_observaciones"), "Observaciones", class = "btn-success", icon = shiny::icon("pencil")),
            observations_button_ui(ns("obs_modal_button")),
            downloadButton(ns("mon_download"), "Descargar datos", class = "btn-success", icon = shiny::icon("download"))
          )
        } else {
          if (session$userData$rol %in% c('coordinador')) {
            tagList(
              actionButton(ns("mon_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
              actionButton(ns("mon_email_resend"), "Correo", class = "btn-success", icon = shiny::icon("paper-plane")),
              # actionButton(ns("mon_observaciones"), "Observaciones", class = "btn-success", icon = shiny::icon("pencil")),
              observations_button_ui(ns("obs_modal_button")),
              downloadButton(ns("mon_download"), "Descargar datos", class = "btn-success", icon = shiny::icon("download"))
            )
          } else {
            if (session$userData$rol %in% c('asistente')) {
              tagList(
                actionButton(ns("mon_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
                actionButton(ns("mon_email_resend"), "Correo", class = "btn-success", icon = shiny::icon("paper-plane")),
                observations_button_ui(ns("obs_modal_button")),
              )
            } else {
              if (session$userData$rol %in% c('coach')) {
                tagList(
                  actionButton(ns("mon_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
                  # actionButton(ns("mon_observaciones"), "Observaciones", class = "btn-success", icon = shiny::icon("pencil")),
                  observations_button_ui(ns("obs_modal_button")),
                  # actionButton(ns("mon_email_resend"), "Correo", class = "btn-success", icon = shiny::icon("paper-plane"))
                )
              } else {
                tagList(
                  downloadButton(ns("mon_download"), "Descargar datos", class = "btn-success", icon = shiny::icon("download"))
                )
              }
            }
          }
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
      
      #Carga los datos de monitoreo desde la base de datos  
      monitor_df <- reactive({
        
        #make reactive to
        dataChangedTrigger()
        # input$submit
        input$mon_submit_edit
        input$mon_refresh
        #input$mon_editar
        # input$copy_button
        # input$delete_button
        rv$tab_monitor_clicked
        # rv$cambio_empresa
        
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          if (session$userData$rol %in% c('asistente')) {
            this_month_date <- floor_date(ymd(lubridate::today(tzone = "Chile/Continental")), 'month')
            filtros <- glue::glue_sql("fecha_preparacion >= {this_month_date}", .con = pool)
          } else {
            if (session$userData$rol %in% c('coach')) {
              filtros <- glue::glue_sql("id_coach = {session$userData$id_coach}", .con = pool)
            } else {
              filtros <- glue::glue_sql("1 = 1", .con = pool)
            }
          }
        } else {
          filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.preparaciones_view where ({filtros})", .con = pool)
        print(tbl)
        dbGetQuery(pool, tbl)
        
      })
      
      output$monitor_table <- DT::renderDataTable({
        ns <- session$ns
        table <- monitor_df() %>% 
          select(-id_empresa, -solicitante, -centro_de_costo, -solicitante_email, -fecha_solicitud) %>%
          # rename(comentarios = observaciones) %>%
          mutate(index = row_number(),
                 nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
                 contacto = paste0("<strong>", tolower(email), "</strong>", "<br>", "<i>", telefono, "</i>"),
                 cargo = paste0("<strong>", str_to_title(cargo), "</strong>", "<br>", "<i>", str_to_title(nombre_empresa), "</i>"),
                 # nombres_coach = paste0("<strong>", str_to_title(nombres_coach), "</strong>", "<br>", "<i>", str_to_title(apellidos_coach), "</i>"),
                 nombres_coach = paste0(str_to_title(nombres_coach), " ", str_to_title(apellidos_coach)),
                 # fecha_solicitud = format(as.Date(fecha_solicitud), format = "%d-%m-%y"),
                 # fecha_preparacion = format(as.Date(fecha_preparacion), format = "%d-%m-%y"),
                 # fecha_preparacion = paste0(format(as.Date(fecha_preparacion), format = "%d-%m-%y"), "<br>", horario),
                 # fecha_preparacion = paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", sprintf("%02d:%02d", hour(lubridate::parse_date_time(horario, "%I:%M %p")), minute(lubridate::parse_date_time(horario, "%I:%M %p")))),
                 fecha_preparacion = if_else(is.na(horario),
                                             paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", "--:--"),
                                             paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", sprintf("%02d:%02d", hour(lubridate::parse_date_time(horario, "%I:%M %p")), minute(lubridate::parse_date_time(horario, "%I:%M %p"))))),
                 # texto_comentarios = comentarios,
                 estado = toupper(if_else(estado == 'en coordinacion', 'en coordinación', estado)),
                 observaciones = if_else(observaciones == '' & obs_preparacion == '' & obs_contacto == '', '', glue('<a id="custom_btn" href="#" onclick="Shiny.setInputValue(\'',ns('button_obs'),'\', \'{index}\', {{priority: \'event\'}})"><span class="glyphicon glyphicon-comment" style = "font-size: 24px;color: #FF6600;"></span></a>'))) %>%
                 # comentarios = if_else(comentarios == '', '', as.character(icon("comment-dots")))) %>% 
          relocate(contacto, .after = apellidos) %>% 
          select(-apellidos, -apellidos_coach, -email, -telefono, -horario, -nombre_empresa, -obs_preparacion, -obs_contacto) %>%
          relocate(index)
        names(table) <- c("n", "id", "Rut", "Participante", "Contacto", "Cargo", "Fecha Capacitación", "Estado", "id_coach", "Coach","Obs", "especial")#, "text_obs")
        
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F, ordering = F,
                                          columnDefs = list(list(targets = 0:7, search = FALSE),
                                                            list(targets = c(1,8,11), visible = FALSE),
                                                            list(className = 'dt-center', targets = "_all")),
                                          language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                          # rowCallback = JS(
                                          #   #paste0(
                                          #     "function(row, data) {",
                                          #     "   var text = data[14];",
                                          #     "   if (text !== '') {",
                                          #     "      $('td:eq(10)', row).attr('title', text);",
                                          #     "   }",
                                          #     "}"
                                          #   #)
                                          # )
                                          ),
                           callback = JS(paste0("var tips = ['Index', '', 'Rut', 'Participante', 'Información de Contacto', 'Cargo', 'Fecha Preparación', 'Estado', '','Nombre del Coach','Comentarios','',''],
                                            firstRow = $('#",session$ns('monitor_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
        ) %>% 
          formatStyle(columns = c("Fecha Preparación"),
                      valueColumns = c("especial"),
                      border = styleEqual(1, '1px solid red')
                      )
        
      })
      
      word_pairs_df <- reactive({
        obsChangedTrigger()
        dbExecute(pool, 'SET character set "utf8"')
        tbl(pool, "observaciones") %>% 
          rename(
            description = descripcion,
            word = nombre,
            type = tipo
          ) %>%
          collect()
      })
      
      # Function to generate descriptions for selected words
      getDescriptions <- function(selected_words, type) {
        
        if (length(selected_words) == 0) return("")
        # descriptions <- sapply(selected_words, function(word) {
        #   df <- word_pairs_df()
        #   desc <- df$description[df$word == word & df$type == type]
        #   # paste0("• ", word, ": ", desc)
        #   # paste0("• ", desc)
        #   if (length(desc) > 0) {
        #     paste0("• ", desc)
        #   } else {
        #     ""  # Return empty string if no match found
        #   }
        # })
        selected_words <- unlist(selected_words)
        df <- word_pairs_df()
        descriptions <- character(0)
        
        for(word in selected_words) {
          matches <- df %>% 
            filter(word == !!word, type == !!type) %>% 
            pull(description)
          
          if(length(matches) > 0) {
            descriptions <- c(descriptions, paste0("• ", matches))
          }
        }
        
        paste(descriptions, collapse = "\n")
      }
      
      # popup comentarios
      observeEvent(input$button_obs, {
        # ns <- session$ns
        
        obs <- monitor_df() %>% 
          mutate(
            index = row_number()
          ) %>% 
          filter(index == as.numeric(input$button_obs)) %>% 
          select(rut, nombres, apellidos, observaciones, obs_preparacion, obs_contacto) %>% 
          mutate(
            participante = paste0(str_to_title(nombres), " ",str_to_title(apellidos)),
            obs_preparacion = ifelse(is.na(obs_preparacion), "", ifelse(obs_preparacion != "", list(strsplit(obs_preparacion, ", ")[[1]]), list(character(0)))),
            obs_contacto = ifelse(is.na(obs_contacto), "", ifelse(obs_contacto != "", list(strsplit(obs_contacto, ", ")[[1]]), list(character(0)))),
            # obs_preparacion = ifelse(obs_preparacion != "", strsplit(obs_preparacion, ", ")[[1]], character(0)),
            # observaciones = ifelse(is.na(observaciones), "", paste("\n", "• Cabe comentar que: ", observaciones))
            observaciones = ifelse(is.na(observaciones) | observaciones == "", "", paste0("• ", observaciones))
          ) %>% 
          select(-nombres, -apellidos) 
        
        comments_desc <- ifelse(obs$obs_preparacion == "", "", getDescriptions(obs$obs_preparacion, "comments"))
        comments_contact <- ifelse(obs$obs_contacto == "", "", getDescriptions(obs$obs_contacto, "contact"))
        comments <- paste0(comments_contact, ifelse(comments_contact == "", "", "\n"), comments_desc, ifelse(obs$observaciones == "", "", "\n"), obs$observaciones, collapse = "\n")
        
        # print(obs)
        # output$modal <- renderUI({
        #   print("dentro de modal")
        #   tagList(
        #     bsModal(ns('model'), "", "button_obs", size = "large", 
        #             textAreaInput("text", label = h3("Comentarios/Observaciones") , value = obs, width = "100%", height = "200px", resize = "none")
        #     ))
        # })
        # toggleModal(session,'model', toggle = "Assessment")
        ##Reset the select_button
        # session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
        observaciones_form()
        shinyjs::disable("text")
        # updateTextAreaInput(session, "text", label = paste0(obs$rut, " - ", obs$participante), value = obs$observaciones)
        updateTextAreaInput(session, "text", label = paste0(obs$rut, " - ", obs$participante), value = comments)
      })
      
      observeEvent(input$close_obs, {
        removeModal()
        shinyjs::reset("observaciones_form")
        session$sendCustomMessage(type = 'resetInputValue', message =  "button_obs")
      })
      
      time_vector <- format(seq(as.POSIXct("2017-01-01", tz = "UTC"), as.POSIXct("2017-01-02", tz = "UTC"), by = "30 min"), format="%I:%M %P")[17:45]
      
      #edit data
      observeEvent(input$mon_editar, priority = 30,{
        
        SQL_df <- monitor_df()
        
        showModal(
          if(length(input$monitor_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$monitor_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })  
        
        if(length(input$monitor_table_rows_selected) == 1 ){
          
          monitor_entry_form("mon_submit_edit", SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"])
          
          updateTextInput(session, "mon_id", value = SQL_df[input$monitor_table_rows_selected, "id_preparacion"])
          updateTextInput(session, "mon_rut", value = SQL_df[input$monitor_table_rows_selected, "rut"], label = "Rut")
          updateTextInput(session, "mon_nombres", value = SQL_df[input$monitor_table_rows_selected, "nombres"])
          updateTextInput(session, "mon_apellidos", value = SQL_df[input$monitor_table_rows_selected, "apellidos"])
          updateTextInput(session, "mon_solicitante", value = SQL_df[input$monitor_table_rows_selected, "solicitante"])
          updateTextInput(session, "mon_cargo", value = SQL_df[input$monitor_table_rows_selected, "cargo"])
          updateSelectInput(session, "mon_coach", choices = get_coaches(), selected = SQL_df[input$monitor_table_rows_selected, "id_coach"])
          # updateDateInput(session, "mon_fecha_prep", value = SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"])
          # updateTextInput(session, "mon_horario", value = SQL_df[input$monitor_table_rows_selected, "horario"])
          updateSelectInput(session, "mon_horario", 
                            # choices = c("9:00 am" = "09 am", "12:00 pm" = "12 pm", 
                            #             "3:00 pm" = "03 pm", "6:00 pm" = "06 pm"), 
                            choices = time_vector,
                            selected = SQL_df[input$monitor_table_rows_selected, "horario"])
          updateDateInput(session, "mon_fecha_sol", value = SQL_df[input$monitor_table_rows_selected, "fecha_solicitud"])
          updateSelectInput(session, "mon_estatus", 
                            # choices = c("En coordinación" = "en coordinacion", "Confirmado" = "confirmado", "Capacitado" = "capacitado",
                            #             "Inasistencia" = "inasistencia", "Abandona" = "abandona",
                            #             "Suspendida" = "suspendida"), 
                            choices = get_estados("estado_prep"),
                            selected = SQL_df[input$monitor_table_rows_selected, "estado"])
          # updateTextAreaInput(session, "mon_comentarios", value = SQL_df[input$monitor_table_rows_selected, "observaciones"])
          
        }
      })
      
      observeEvent(input$mon_submit_edit, priority = 30, {
        SQL_df <- monitor_df()
        fecha <- SQL_df[input$monitor_table_row_last_clicked, "fecha_preparacion"]
        horario <- SQL_df[input$monitor_table_row_last_clicked, "horario"]
        especial <- SQL_df[input$monitor_table_row_last_clicked, "es_horario_especial"]
        sqlq <- glue::glue_sql("UPDATE monitor_preparaciones
                            set fecha_preparacion = {input$mon_fecha_prep},
                                horario = {input$mon_horario},
                                estado = {input$mon_estatus},
                                coach_id = {input$mon_coach},
                                es_horario_especial = {especial},
                                last_change_by = {session$userData$email}
                            WHERE id = {input$mon_id}", .con = pool)
        print(sqlq)
        # TODO: ajustar de acuerdo a este post: https://community.rstudio.com/t/shiny-tests-for-database-transactions/2211/2
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        if (horario != input$mon_horario) {
          update_training_blocks(fecha, horario, 1)
        }
        shinyjs::reset("monitor_entry_form")
        showNotification("Datos modificados.", type = "message")
        
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
        
        # telemetry$log_input_manual(
        #   input_id = ns("mon_editar"),
        #   value = input$mon_id,
        #   session = session
        # )
        
      })
      
      monitor_entry_form <- function(button_id, fecha_cap){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, hidden(textInput(ns("mon_id"),"")),shinyjs::disabled(textInput(ns("mon_rut"), "Rut", placeholder = "")))),
            fluidRow(column(6, shinyjs::disabled(textInput(ns("mon_nombres"), "Nombres", placeholder = ""))),
                     column(6, shinyjs::disabled(textInput(ns("mon_apellidos"), "Apellidos", placeholder = "")))),
            fluidRow(column(6, shinyjs::disabled(textInput(ns("mon_solicitante"), "Solicitado por", placeholder = ""))),
                     column(6, shinyjs::disabled(textInput(ns("mon_cargo"), "Cargo", placeholder = "")))),
            # fluidRow(column(6, textInput(ns("mon_coach"), "Coach", placeholder = ""))),
            fluidRow(column(6, selectInput(ns("mon_coach"), "Coach", selectize = FALSE, choices = NULL))),
            fluidRow(column(6, dateInput(ns("mon_fecha_prep"), "Fecha Capacitación", language = "es", weekstart = 1, autoclose = T, value = fecha_cap)),
                     # column(6, textInput("mon_horario", "Horario",placeholder = ""))),
                     column(6, selectInput(ns("mon_horario"), "Horario", 
                                           # choices = c("09 am" = "09 am", 
                                           #                                       "12 pm" = "12 pm", 
                                           #                                       "03 pm" = "03 pm", 
                                           #                                       "06 pm" = "06 pm"),
                                           choices = time_vector,
                                           selectize = FALSE))),
            fluidRow(column(6, shinyjs::disabled(dateInput(ns("mon_fecha_sol"), "Fecha Solicitud", language = "es", weekstart = 1, autoclose = T))),
                     column(6, selectInput(ns("mon_estatus"), "Estado", choices = c("En coordinación" = "en coordinacion",
                                                                                 "Capacitado" = "capacitado",
                                                                                 "Inasistencia" = "inasistencia",
                                                                                 "Cancelada" = "cancelada",
                                                                                 "Suspendida" = "suspendida"),
                                           selectize = FALSE)))
            # fluidRow(textAreaInput(ns("mon_comentarios"), "Observaciones/Comentarios"))
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          # title = "Preparaciones",
          title = "Editar capacitación",
          footer = tagList(
            modalButton("Cancelar"),
            # shinyjs::disable(actionButton(ns(button_id), "Guardar"))
            actionButton(ns(button_id), "Guardar")
          ),
          easyClose = TRUE
        ))
      }

      observaciones_form <- function(){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            textAreaInput(ns("text"), label = "", width = "100%", height = "400px", resize = "none")
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = "Observaciones/Comentarios",
          footer = tagList(
            actionButton(ns("close_obs"), "Cerrar")
          ),
          easyClose = FALSE, size = "l"
        ))
      }
      
      ## COMENTARIOS ##
      # Get selected row data
      selected_row <- reactive({
        # req(input$monitor_table_rows_selected)
        
        SQL_df <- monitor_df()
        SQL_df[input$monitor_table_rows_selected, ]
      })
      
      ## END DIALOG COMENTARIOS ##
      
      reenvio_email_params <- reactiveValues(
        id_empresa = 0,
        rut = NULL,
        id_preparacion = NULL,
        nombres = NULL,
        apellidos = NULL,
        fecha_prep = NULL,
        horario = NULL,
        email = NULL,
        solicitante = NULL,
        email_solicitante = NULL
      )
      
      observeEvent(input$mon_email_resend, {
        ns <- session$ns
        
        showModal(
          if(length(input$monitor_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$monitor_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })
        
        if (length(input$monitor_table_rows_selected) == 1 ) {
          SQL_df <- monitor_df()
          reenvio_email_params$id_empresa <- SQL_df[input$monitor_table_rows_selected, "id_empresa"]
          reenvio_email_params$rut <- SQL_df[input$monitor_table_rows_selected, "rut"]
          reenvio_email_params$id_preparacion <- SQL_df[input$monitor_table_rows_selected, "id_preparacion"]
          reenvio_email_params$nombres <- SQL_df[input$monitor_table_rows_selected, "nombres"]
          reenvio_email_params$apellidos <- SQL_df[input$monitor_table_rows_selected, "apellidos"]
          reenvio_email_params$fecha_prep <- SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"]
          reenvio_email_params$horario <- SQL_df[input$monitor_table_rows_selected, "horario"]
          reenvio_email_params$email <- SQL_df[input$monitor_table_rows_selected, "email"]
          reenvio_email_params$email_solicitante <- SQL_df[input$monitor_table_rows_selected, "solicitante_email"]
          reenvio_email_params$solicitante <- SQL_df[input$monitor_table_rows_selected, "solicitante"]
          
          showModal(
            modalDialog(
              title = "Correo Notificación de Inscripción",
              p(paste0("Para: ", SQL_df[input$monitor_table_rows_selected, "nombres"], SQL_df[input$monitor_table_rows_selected, "apellidos"])),
              p(paste0("Rut: ", SQL_df[input$monitor_table_rows_selected, "rut"])),
              p(paste0("Fecha capacitación: ", SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"])),
              p(paste0("Horario capacitación: ", SQL_df[input$monitor_table_rows_selected, "horario"])),
              # br(),
              # p("Nota: Se notificará automáticamente la suspención al participante y coach asignado."),
              # textAreaInput(ns("mon_razones"), "Justificación (Obligatoria)"),
              layout_columns(
                actionButton(ns("email_resend_btn"), "Reenviar"),
                actionButton(ns("email_reschedule_btn"), "Reagendar")
              ),
              # textInput(ns("email_subject"), "Asunto:", value = "Capacitación (Reagendamiento)"),
              easyClose = F,
              footer = tagList(
                modalButton("Cancelar")
                # actionButton(ns("email_resend_btn"), "Enviar")
              ) 
            )
          )
        }
        
      })
      
      observeEvent(input$email_resend_btn, {
        # reenviar con funcion existente
        withProgress(
          message = "Iniciando reenvio de email...",
          detail = "Esto podria tomar un momento...",
          value = 0, {
            setProgress(0.2, message = "Recolectando datos...")
            shinyjs::disable("email_resend_btn")
            shinyjs::disable("email_reschedule_btn")
            cita <- list(fecha_preparacion = reenvio_email_params$fecha_prep, horario = reenvio_email_params$horario)
            empresa <- get_empresas('cliente', reenvio_email_params$email_solicitante)
            info_solicitante <- get_solicitante_info(reenvio_email_params$email_solicitante)
            setProgress(0.5, message = "Reenviando email...")
            
            envio_email_participante(
              cita, 
              reenvio_email_params$email,
              reenvio_email_params$nombres, 
              reenvio_email_params$email_solicitante,
              c("capacitacion@mercconsultora.cl"),
              rownames(as.data.frame(empresa)),
              reenvio_email_params$solicitante,
              info_solicitante$cargo,
              info_solicitante$telefono,
              reenvio_email_params$email_solicitante,
              reenvio_email_params$rut,
              reenvio_email_params$id_preparacion,
              'reenvio',
              'CAPACITACIÓN (Recordatorio)'
            )
            setProgress(0.7, message = "Reenviando email...")
            Sys.sleep(1)
            setProgress(1, message = "Correo reenviado")
          }
        )
        shinyjs::enable("email_resend_btn")
        shinyjs::enable("email_reschedule_btn")
        removeModal()
      })
      
      observeEvent(input$email_reschedule_btn, {
        # reenviar con funcion existente
        withProgress(
          message = "Iniciando reenvio de email...",
          detail = "Esto podria tomar un momento...",
          value = 0, {
            setProgress(0.2, message = "Recolectando datos...")
            shinyjs::disable("email_resend_btn")
            shinyjs::disable("email_reschedule_btn")
            cita <- list(fecha_preparacion = reenvio_email_params$fecha_prep, horario = reenvio_email_params$horario)
            empresa <- get_empresas('cliente', reenvio_email_params$email_solicitante)
            info_solicitante <- get_solicitante_info(reenvio_email_params$email_solicitante)
            setProgress(0.5, message = "Reenviando email...")
            
            envio_email_participante(
              cita, 
              reenvio_email_params$email,
              reenvio_email_params$nombres, 
              reenvio_email_params$email_solicitante,
              c("capacitacion@mercconsultora.cl"),
              rownames(as.data.frame(empresa)),
              reenvio_email_params$solicitante,
              info_solicitante$cargo,
              info_solicitante$telefono,
              reenvio_email_params$email_solicitante,
              reenvio_email_params$rut,
              reenvio_email_params$id_preparacion,
              'reenvio',
              'CAPACITACIÓN (Reagendamiento)'
            )
            setProgress(0.7, message = "Reenviando email de agendamiento...")
            Sys.sleep(1)
            setProgress(1, message = "Correo reenviado")
          }
        )
        shinyjs::enable("email_resend_btn")
        shinyjs::enable("email_reschedule_btn")
        removeModal()
      })
      
      observeEvent(input$mon_suspender, {
        ns <- session$ns
        
        showModal(
          if(length(input$monitor_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$monitor_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })
        
        if (length(input$monitor_table_rows_selected) == 1 ) {
          SQL_df <- monitor_df()
          fecha <- SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"]
          hora <- SQL_df[input$monitor_table_rows_selected, "horario"]
          fecha_hora <- ymd_hm(paste0(fecha, " ", hora), tz = "Chile/Continental")
          print(paste0("fecha_hora: ", fecha_hora))
          print(paste0("fecha_now: ", now(tz = "Chile/Continental")))
          print(paste0("now menor que fecha de prep?: ", now(tz = "Chile/Continental") <= fecha_hora))
          
          showModal(
            if (!is.na(fecha_hora) & (now(tz = "Chile/Continental") <= fecha_hora)) {
              modalDialog(
                title = "Suspención de Capacitación",
                h3(paste0("¿Quieres suspender la Capacitación de ", SQL_df[input$monitor_table_rows_selected, "nombres"], "?")),
                p(paste0("Rut: ", SQL_df[input$monitor_table_rows_selected, "rut"])),
                br(),
                p("Nota: Se notificará automáticamente la suspención al participante y coach asignado."),
                textAreaInput(ns("mon_razones"), "Justificación (Obligatoria)"),
                easyClose = F,
                footer = tagList(
                  modalButton("Cancelar"),
                  actionButton(ns("suspender_capacitacion"), "Si, suspender")
                ) 
              )
            } else {
              modalDialog(
                title = "Suspención de Capacitación",
                h3(paste0("Lo sentimos, no es posible suspender la cita de ", SQL_df[input$monitor_table_rows_selected, "nombres"])),
                p(paste0("Rut: ", SQL_df[input$monitor_table_rows_selected, "rut"])),
                br(),
                p("Nota: Contáctese con nosotro para revisar su caso."),
                easyClose = F,
                footer = tagList(
                  modalButton("Cerrar")
                ) 
              )
            }
          )
        }
      })
      
      observeEvent(input$suspender_capacitacion, priority = 30, {
        SQL_df <- monitor_df()
        mon_id <- SQL_df[input$monitor_table_rows_selected, "id_preparacion"]
        razones <- paste0("Suspendida por solicitante: <br/> ", input$mon_razones)
        fecha <- SQL_df[input$monitor_table_rows_selected, "fecha_preparacion"]
        horario <- SQL_df[input$monitor_table_rows_selected, "horario"]
        
        sqlq <- glue::glue_sql("UPDATE monitor_preparaciones
                                set estado = 'suspendida',
                                    observaciones = {razones},
                                    last_change_by = {session$userData$email}
                                WHERE id = {mon_id}", .con = pool)
        
        # TODO: ajustar de acuerdo a este post: https://community.rstudio.com/t/shiny-tests-for-database-transactions/2211/2
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        update_training_blocks(fecha, horario, 1)
        showNotification("Cita suspendida.", type = "message")
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
        
        # TODO:
        # enviar email a coach
        # enviar_email_suspencion_coach()
      })
      
      output$mon_download <- downloadHandler(
        # TODO: nombre archivo debe llevar fecha de descarga
        filename = function(){paste0("capacitaciones3d_merc_(",format(today(tzone = "Chile/Continental"), format = "%d-%m-%y"),").xlsx")}, 
        content = function(fname){
          table <- monitor_df() %>% 
            select(-id_empresa, -solicitante, -centro_de_costo, -solicitante_email, -id_preparacion, -id_coach, -es_horario_especial, -nombres_coach, -apellidos_coach, -email, -telefono) %>%
            # rename(comentarios = observaciones) %>% 
            mutate(n = row_number(),
                   nombres = str_to_title(nombres),
                   apellidos = str_to_title(apellidos),
                   cargo = str_to_title(cargo),
                   fecha_solicitud = format(as.Date(fecha_solicitud), format = "%d-%m-%Y"),
                   fecha_preparacion = format(as.Date(fecha_preparacion), format = "%d-%m-%Y"),
                   estado = toupper(if_else(estado == 'en coordinacion', 'en coordinación', estado))
                   # obs_preparacion = ifelse(obs_preparacion == "" | is.na(obs_preparacion), "", list(strsplit(obs_preparacion, ", ")[[1]]))
            ) %>%
            rename(
              # observaciones = comentarios,
              observaciones_adicionales = observaciones,
              observaciones_coordinacion = obs_contacto,
              observaciones_capacitacion = obs_preparacion,
              `fecha solicitud` = fecha_solicitud,
              `fecha capacitación` = fecha_preparacion
            ) %>%
            relocate(n)
          
          # openxlsx::write.xlsx(table, fname)
          ## Create a new workbook
          wb <- openxlsx::createWorkbook("MERC")
          
          ## Add a worksheets
          openxlsx::addWorksheet(wb, "Monitor")
          
          ## write data to worksheet 1
          openxlsx::writeData(wb, sheet = 1, table)
          
          ## create and add a style to the column headers
          headerStyle1 <- openxlsx::createStyle(
            fontSize = 11, fontColour = "#FFFFFF",
            fgFill = "#4F81BD", halign = "center",
            textDecoration = "bold", valign = "center"
          )
          openxlsx::addStyle(wb, sheet = 1, headerStyle1, rows = 1, cols = 1:13, gridExpand = TRUE)
          
          ## create style to column observations
          columnStyle1 <- openxlsx::createStyle(fontSize = 9)
          openxlsx::addStyle(wb, sheet = 1, columnStyle1, rows = 2:nrow(table), cols = 10)
          openxlsx::addStyle(wb, sheet = 1, columnStyle1, rows = 2:nrow(table), cols = 12)
          openxlsx::addStyle(wb, sheet = 1, columnStyle1, rows = 2:nrow(table), cols = 13)
          
          ## set row heights
          openxlsx::setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
          
          ## set auto column width
          openxlsx::setColWidths(wb, sheet = 1, cols = 1:13, widths = "auto")
          
          ## save
          openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
        }
      )
      
      # Historial de cambios
      observeEvent(input$mon_historial, {
        historial_ui()
      })
      
      historial_ui <- function(){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            box(
              width = 12,
              uiOutput(ns("timeline"))
            )
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = "Historial",
          footer = tagList(
            modalButton("Cerrar"),
            #actionButton(ns(button_id), "Submit")
          ),
          easyClose = TRUE,
          size = "l"
        ))
      }
      
      output$timeline <- renderUI({
        #refresh()
        df <- data.frame(
          date = c("2018-01-01", "2018-02-01"),
          title = c("Event A", "Event B")
        )
          
        timelineBlock(
          reversed = TRUE,
          timelineEnd(color = "danger"),
          lapply(split(df, df$date), function(x) {
            list(
              timelineLabel(x$date[1], color = "teal"),
              
              lapply(x$title, function(title) 
                mytimeItem(
                  title = "OGD",
                  icon = "edit",
                  color = "olive",
                  time = "now",
                  footer ="",
                  title
                )
              )
            )}
          ),
          timelineStart(color = "secondary")
          
          
        )
      })
      
      mytimeItem <- function (..., icon = NULL, color = NULL, time = NULL, title = NULL, border = TRUE, footer = NULL){
        data <- paste0(..., collapse = "<br><br>")
        cl <- "fa fa-"
        if (!is.null(icon))
          cl <- paste0(cl, icon)
        if (!is.null(color))
          cl <- paste0(cl, " bg-", color)
        itemCl <- "timeline-header no-border"
        if (isTRUE(border))
          itemCl <- "timeline-header"
        shiny::tags$div(
          shiny::tags$div(class = cl),
          shiny::tags$div(
            class = "timeline-item",
            shiny::tags$span(class = "time", shiny::icon("clock"), time),
            shiny::tags$h3(class = itemCl, title),
            shiny::tags$div(class = "timeline-body",
                            HTML(data)),
            shiny::tags$div(class = "timeline-footer", footer)
          )
        )
      }
      
      # ================= END: MONITOR =======================
    }
  )
}