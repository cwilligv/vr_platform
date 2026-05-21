resultados_encuesta_ui <- function(id){
  tabItem(
    tabName = "tab_encuestas",
    h1("Nivel de Satisfacción", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      collapsible = F,
      headerBorder = F,
      fluidRow(
        column(
          width = 9,
          p(
            "Finalizada cada capacitación, se aplicó a cada participante una encuesta de satisfacción para valorar el servicio. A continuación, se presentan los resultados del nivel de satisfacción de los participantes que accedieron de forma voluntaria a responder la encuesta:"
          )
        ),
        column(
          width = 3
        )
      ),
      fluidRow(
        column(
          width = 4,
          div(
            selectInput(
              NS(id, "filtro_resultados"), 
              "Respuestas", 
              choices = c(
                "Todos" = 100,
                "Muy Mal" = 0,
                "Mal" = 20,
                "Moderada" = 40,
                "Buena" = 60,
                "Muy Buena" = 80
              )
            ),
            style = "margin-top:-5px"
          )
        ),
        column(
          width = 4,
          # uiOutput(NS(id,"score_resumen_encuesta"))
        ),
        column(
          width = 4,
          uiOutput(NS(id, "admin_selector"))
        )
      ),
      fluidRow(
        column(width = 4),
        column(
          width = 4,
          # div(h4("Nivel de Satisfaccion: 96% (Muy Satisfactorio)"))
          uiOutput(NS(id,"score_resumen_encuesta"))
        ),
        column(width = 4)
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          style = "z-index: 10",
          div(DT::dataTableOutput(NS(id, "encuesta_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
        )
      ),
      fluidRow(uiOutput(NS(id, "modal")))
    )
  )
}

resultados_encuesta_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      # ================= BEGIN: MONITOR =======================
      
      # observations_button_server("obs_modal_button", selected_row, dataChangedTrigger, word_pairs_df, obsChangedTrigger)
      dataChangedTrigger <- reactiveVal(0)
      obsChangedTrigger <- reactiveVal(0)
      
      output$admin_selector <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            div(selectInput(ns("listado_empresas"), "Clientes", choices = get_empresas(session$userData$rol, session$userData$email), selected = as.numeric(session$userData$id_empresa)),style = "margin-top:-5px")
          )
        }
      })
      
      observeEvent(input$listado_empresas, {
        session$userData$id_empresa <- input$listado_empresas
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      encuestas <- reactive({
        read.csv("www/resources/encuestas_enriched.csv")
      })
      
      #Carga los datos de monitoreo desde la base de datos  
      encuesta_df <- reactive({ 
        
        #make reactive to
        dataChangedTrigger()
        # input$submit
        input$mon_submit_edit
        input$mon_refresh
        #input$mon_editar
        # input$copy_button
        # input$delete_button
        rv$tab_encuesta_clicked
        if (input$filtro_resultados == 100) {
          lower_bound <- 0
          upper_bound <- 101
        } else {
          lower_bound <- as.numeric(input$filtro_resultados) + 0.1
          upper_bound <- as.numeric(input$filtro_resultados) + 20
          # if (input$filtro_resultados == 80) {
          #   lower_bound <- as.numeric(input$filtro_resultados)
          #   upper_bound <- as.numeric(input$filtro_resultados) + 21
          # } else {
          #   lower_bound <- as.numeric(input$filtro_resultados)
          #   upper_bound <- as.numeric(input$filtro_resultados) + 20
          # }
        }
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          encuestas() %>% 
            filter(between(score, lower_bound, upper_bound)) %>% 
            mutate(fecha_preparacion = as.Date(fecha_preparacion)) %>% 
            arrange(desc(fecha_preparacion))
            # filter(score >= lower_bound & score < upper_bound)
        } else {
          encuestas() %>% 
            # filter(id_empresa == as.numeric(session$userData$id_empresa) & between(score, lower_bound, upper_bound))
            filter(id_empresa == as.numeric(session$userData$id_empresa) & (score >= lower_bound & score < upper_bound)) %>% 
            mutate(fecha_preparacion = as.Date(fecha_preparacion)) %>% 
            arrange(desc(fecha_preparacion))
        }
        
        # dbExecute(pool, 'SET character set "utf8"')
        # tbl <- glue::glue_sql("select * from {`db`}.preparaciones_view where ({filtros})", .con = pool)
        # print(tbl)
        # dbGetQuery(pool, tbl)
        
      })
      
      output$score_resumen_encuesta <- renderUI({
        if (nrow(encuesta_df()) == 0) {
          tagList(
            h4(paste0("No existen registros de satisfacción"))
          )
        }else {
          score <- round(mean(encuesta_df()$score, na.rm = TRUE))
          quality_levels <- c(
            "Muy Mal" = 0,
            "Mal" = 20,
            "Moderada" = 40,
            "Buena" = 60,
            "Muy Buena" = 80
          )
          interval_index <- findInterval(score, quality_levels)
          tagList(
            h4(paste0("Nivel de Satisfacción: ",as.character(score),"%"), style = "text-align: center;"),
            h4(paste0("(", names(quality_levels)[interval_index], ")"), style = "text-align: center;"),
            p(paste0("En base a ",nrow(encuesta_df())," respuestas"), style = "text-align: center;")
          )
        }
      })
      
      output$encuesta_table <- DT::renderDataTable({
        ns <- session$ns
        
        table <- encuesta_df() %>% 
          select(
            -id_preparacion, -id_empresa, -id_coach, -c(pregunta_1:pregunta_7), -nombres_coach, -apellidos_coach, -Rating
          ) %>%
          mutate(
            #index = row_number(),
            nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
            cargo = paste0("<strong>", str_to_title(cargo), "</strong>", "<br>", "<i>", str_to_title(nombre_empresa), "</i>"),
            # nombres_coach = paste0(str_to_title(nombres_coach), " ", str_to_title(apellidos_coach)),
            fecha_preparacion = if_else(is.na(horario),
                                       paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", "--:--"),
                                       paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", sprintf("%02d:%02d", hour(lubridate::parse_date_time(horario, "%I:%M %p")), minute(lubridate::parse_date_time(horario, "%I:%M %p"))))),
            # Rating = case_when(
            #   score > 80 ~ paste(replicate(5, as.character(tags$span(icon("heart"), style = "color: #006ac2;"))), collapse = ""),
            #   score > 60 ~ paste(replicate(4, as.character(tags$span(icon("heart"), style = "color: #006ac2;"))), collapse = ""),
            #   score > 40 ~ paste(replicate(3, as.character(tags$span(icon("heart"), style = "color: #006ac2;"))), collapse = ""),
            #   score > 20 ~ paste(replicate(2, as.character(tags$span(icon("heart"), style = "color: #006ac2;"))), collapse = ""),
            #   TRUE            ~ paste(replicate(1, as.character(tags$span(icon("heart"), style = "color: #006ac2;"))), collapse = "") # For any other case (0-20)
            # ),
            score = paste0(as.character(round(score)), "%"),
            detalles = glue('<a id="custom_btn" onclick="Shiny.setInputValue(\'',ns('boton_detalles'),'\', \'{X}\', {{priority: \'event\'}})"><span class="glyphicon glyphicon-check" style = "font-size: 24px;color: #FF6600;"></span></a>')
          ) %>%
          # relocate(score, .before = detalles) %>%
          select(-apellidos, -horario, -nombre_empresa)
        
        names(table) <- c("n", "Rut", "Participante", "Contrato/Proyecto", "Cargo", "Fecha Capacitación", "Nivel de Satisfacción", "Respuestas")
        
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F, ordering = F,
                                          columnDefs = list(list(targets = 0:7, search = FALSE),
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
                           callback = JS(paste0("var tips = ['Index', 'Rut', 'Participante', 'Contrato/Proyecto', 'Cargo', 'Fecha Preparación', 'Nivel Satisfacción','Detalle de respuestas'],
                                            firstRow = $('#",session$ns('encuesta_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
        )
        
      })
      
      value_map <- c(
        "1" = "Muy en desacuerdo",
        "2" = "En desacuerdo",
        "3" = "Neutral",
        "4" = "De acuerdo",
        "5" = "Muy de acuerdo"
      )
      
      # Function that takes a number and returns the corresponding string
      map_response <- function(x) {
        unname(value_map[as.character(x)])
      }
      
      
      
      observeEvent(input$boton_detalles, {
        respuestas <- encuesta_df()
        index <- input$boton_detalles
        showModal(
          modalDialog(
            title = "Respuestas de la encuesta",
            paste0("Participante: ", respuestas[respuestas$X == index,"nombres"], " ", respuestas[respuestas$X == index,"apellidos"], " - ", respuestas[respuestas$X == index,"rut"]),
            br(),
            br(),
            # fluidRow(
            #   layout_columns(
            #     card(class = "border",
            #       p("1) El relator presentó la información de manera clara y comprensible."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_1"]), "</b>")))
            #     ),
            #     card(class = "border",
            #       p("2) El relator fomentó la participación y respondió de manera receptiva a los participantes."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_2"]), "</b>")))
            #     )
            #   )
            # ),
            fluidRow(
              p("1) El relator presentó la información de manera clara y comprensible:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_1"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_1"]), "</b>")))
            ),
            fluidRow(
              p("2) El relator fomentó la participación y respondió de manera receptiva a los participantes:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_2"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_2"]), "</b>")))
            ),
            # fluidRow(
            #   layout_columns(
            #     class = "border",
            #     card(class = "border",
            #       p("3) El relator se expresó de manera efectiva y respondió a preguntas de manera adecuada."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_3"]), "</b>")))
            #     ),
            #     card(class = "border",
            #       p("4) El material estaba organizado de manera lógica y fácil de seguir."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_4"]), "</b>")))
            #     )
            #   )
            # ),
            fluidRow(
              p("3) El relator se expresó de manera efectiva y respondió a preguntas de manera adecuada:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_3"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_3"]), "</b>")))
            ),
            fluidRow(
              p("4) El material estaba organizado de manera lógica y fácil de seguir:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_4"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_4"]), "</b>")))
            ),
            # fluidRow(
            #   layout_columns(
            #     card(class = "border",
            #       p("5) El material estaba presentado de forma clara y tenía un diseño visual atractivo."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_5"]), "</b>")))
            #     ),
            #     card(class = "border",
            #       p("6) Fue fácil de conectar y acceder a la plataforma de video llamada en línea."),
            #       p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_6"]), "</b>")))
            #     )
            #   )
            # ),
            fluidRow(
              p("5) El material estaba presentado de forma clara y tenía un diseño visual atractivo:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_5"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_5"]), "</b>")))
            ),
            fluidRow(
              p("6) Fue fácil de conectar y acceder a la plataforma de video llamada en línea:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_6"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_6"]), "</b>")))
            ),
            # fluidRow(
            #   card(class = "border",
            #     p("7) Respecto de lo aprendido, ahora me siento capaz de lograr obtener un resultado positivo en las evaluaciones."),
            #     p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_7"]), "</b>"))),
            #   )
            # ),
            fluidRow(
              p("7) Respecto de lo aprendido, ahora me siento capaz de lograr obtener un resultado positivo en las evaluaciones:"),
              # p(HTML("&nbsp")),
              # p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_7"]), "</b>")))
            ),
            fluidRow(
              p(HTML(paste0("<b>",map_response(respuestas[respuestas$X == index, "pregunta_7"]), "</b>")))
            ),
            footer = tagList(
              modalButton("Cerrar"),
            ),
            easyClose = TRUE,
            size = "l"
          )
        )
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
        # req(input$encuesta_table_rows_selected)
        
        SQL_df <- encuesta_df()
        SQL_df[input$encuesta_table_rows_selected, ]
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
          if(length(input$encuesta_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$encuesta_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })
        
        if (length(input$encuesta_table_rows_selected) == 1 ) {
          SQL_df <- encuesta_df()
          reenvio_email_params$id_empresa <- SQL_df[input$encuesta_table_rows_selected, "id_empresa"]
          reenvio_email_params$rut <- SQL_df[input$encuesta_table_rows_selected, "rut"]
          reenvio_email_params$id_preparacion <- SQL_df[input$encuesta_table_rows_selected, "id_preparacion"]
          reenvio_email_params$nombres <- SQL_df[input$encuesta_table_rows_selected, "nombres"]
          reenvio_email_params$apellidos <- SQL_df[input$encuesta_table_rows_selected, "apellidos"]
          reenvio_email_params$fecha_prep <- SQL_df[input$encuesta_table_rows_selected, "fecha_preparacion"]
          reenvio_email_params$horario <- SQL_df[input$encuesta_table_rows_selected, "horario"]
          reenvio_email_params$email <- SQL_df[input$encuesta_table_rows_selected, "email"]
          reenvio_email_params$email_solicitante <- SQL_df[input$encuesta_table_rows_selected, "solicitante_email"]
          reenvio_email_params$solicitante <- SQL_df[input$encuesta_table_rows_selected, "solicitante"]
          
          showModal(
            modalDialog(
              title = "Correo Notificación de Inscripción",
              p(paste0("Para: ", SQL_df[input$encuesta_table_rows_selected, "nombres"], SQL_df[input$encuesta_table_rows_selected, "apellidos"])),
              p(paste0("Rut: ", SQL_df[input$encuesta_table_rows_selected, "rut"])),
              p(paste0("Fecha capacitación: ", SQL_df[input$encuesta_table_rows_selected, "fecha_preparacion"])),
              p(paste0("Horario capacitación: ", SQL_df[input$encuesta_table_rows_selected, "horario"])),
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
          if(length(input$encuesta_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$encuesta_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          })
        
        if (length(input$encuesta_table_rows_selected) == 1 ) {
          SQL_df <- encuesta_df()
          fecha <- SQL_df[input$encuesta_table_rows_selected, "fecha_preparacion"]
          hora <- SQL_df[input$encuesta_table_rows_selected, "horario"]
          fecha_hora <- ymd_hm(paste0(fecha, " ", hora), tz = "Chile/Continental")
          print(paste0("fecha_hora: ", fecha_hora))
          print(paste0("fecha_now: ", now(tz = "Chile/Continental")))
          print(paste0("now menor que fecha de prep?: ", now(tz = "Chile/Continental") <= fecha_hora))
          
          showModal(
            if (!is.na(fecha_hora) & (now(tz = "Chile/Continental") <= fecha_hora)) {
              modalDialog(
                title = "Suspención de Capacitación",
                h3(paste0("¿Quieres suspender la Capacitación de ", SQL_df[input$encuesta_table_rows_selected, "nombres"], "?")),
                p(paste0("Rut: ", SQL_df[input$encuesta_table_rows_selected, "rut"])),
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
                h3(paste0("Lo sentimos, no es posible suspender la cita de ", SQL_df[input$encuesta_table_rows_selected, "nombres"])),
                p(paste0("Rut: ", SQL_df[input$encuesta_table_rows_selected, "rut"])),
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
        SQL_df <- encuesta_df()
        mon_id <- SQL_df[input$encuesta_table_rows_selected, "id_preparacion"]
        razones <- paste0("Suspendida por solicitante: <br/> ", input$mon_razones)
        fecha <- SQL_df[input$encuesta_table_rows_selected, "fecha_preparacion"]
        horario <- SQL_df[input$encuesta_table_rows_selected, "horario"]
        
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
          table <- encuesta_df() %>% 
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