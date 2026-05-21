herramientas_ui <- function(id){
  tabItem(
    tabName = "tab_herramientas",
    h1("Herramientas", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 8,
          p("Grupo de herramientas que facilitan llevar un adecuado seguimiento y control para lo que se deben cargar los datos exportados desde la entidad evaluadora e importalos a continuacion:")
        ),
        column(
          width = 4,
          uiOutput(NS(id,"btn_cargar"), inline = T)
        )
      ),
      bs4Dash::tabsetPanel(
        id = "panel_herramientas",
        tabPanel(
          title = "Resultados",
          registro_resultados_ui(NS(id,"registro_resultados"))
        ),
        tabPanel(
          title = "Alertas",
          alertas_ui(NS(id, "alertas"))
        ),
        tabPanel(
          title = "Análisis",
          estadisticas_ui(NS(id, "estadisticas"))
        )
      )
    )
  )
}

herramientas_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      file_loader <- reactiveValues(
        trigger = 0
      )
      
      registro_resultados_server("registro_resultados", rv, file_loader)
      alertas_server("alertas", rv)
      estadisticas_server("estadisticas", rv)
      
      output$btn_cargar <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin','coach','cliente', 'cliente_jefatura')) {
          tagList(
            actionButton(NS(id, "res_carga_masiva"), "Cargar resultados", class = "btn-success"),
            # br(),
            # actionButton(NS(id, "instructivo_0"), "Instrucciones de carga", icon = icon("info-circle"), class = "btn-success")
          )
        }
        # tagList(
        #   actionButton(NS(id, "res_carga_masiva"), "Cargar resultados", class = "btn-success"),
        #   actionButton(NS(id, "res_carga_masiva_adm"), "Cargar resultados (ADM)", class = "btn-success")
        # )
      })
      
      observeEvent(input$res_carga_masiva, {
        carga_masiva_form("btn_carga_masiva")
      })
      
      carga_masiva_form <- function(button_id){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(p("Carga aquí la planilla 'Gestión Resultados' que proporciona la entidad evaluadora."),
                     br(),
                     p("Importando esta planilla, permitirá que nuestra plataforma pueda desplegar de manera inteligente los resultados, alertas y gráficos, para un mejor control y gestión de los procesos.")),
            br(),
            # fluidRow(downloadLink(ns("download_template"), "Plantilla: Haga clic aquí para descargar plantilla de ejemplo")),
            fluidRow(fileInput(ns("archivo_carga_masiva"), "Cargar archivo", buttonLabel = 'Seleccionar archivo', placeholder = 'ningún archivo', accept = c(".xlsx"))),
            fluidRow(uiOutput(ns("archivo_carga_texto_estructura"))),
            fluidRow(textOutput(ns("archivo_carga_texto_empresa"))),
            fluidRow(textOutput(ns("archivo_carga_texto_participantes"))),
            br(),
            fluidRow(
              column(
                width = 12,
                align="center",
                actionButton(ns("instructivo_0"), "Instrucciones de descarga archivo CEIM", icon = icon("info-circle"), class = "btn-success")
              )
            )
          ),
          tags$div(id = session$ns("constraintPlaceholder")),
          title = "Carga de resultados",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Cargar")
          ),
          easyClose = TRUE
        ))
      }
      
      observeEvent(input$archivo_carga_masiva, {
        print(paste0("Cols diferentes?:", estructure_comparison()))
        if (estructure_comparison() > 0) {
          shinyjs::disable("btn_carga_masiva")
          output$archivo_carga_texto_estructura <- renderUI({
            # paste0("ERROR - Archivo con ", estructure_comparison(), " columna(s) diferente(s)")
            HTML(
              as.character(div(style="color:red;", paste0("ERROR - Archivo con ", estructure_comparison(), " columna(s) diferente(s)")))
            )
          })
        }else{
          shinyjs::enable("btn_carga_masiva")
          output$archivo_carga_texto_estructura <- renderUI({
            # paste0("Archivo con estructura correcta, ", estructure_comparison(), " errores")
            HTML(
              as.character(div(paste0("Archivo con estructura correcta, ", estructure_comparison(), " errores")))
            )
          })
          file <- actual_file()
          nombre_empresa <- file %>% distinct(empresa) %>% pull()
          num_empresas <- file %>% distinct(empresa) %>% count() %>% pull()
          # print(head(file[, c("rut", "estado_vigente_vencido")]))
          
          output$archivo_carga_texto_empresa <- renderText({
            if (num_empresas > 1) {
              paste0("Cargando resultados para ", num_empresas, " empresas")
            }else{
              paste0("Cargando resultados para ", num_empresas, " empresa")
            }
          })
          
          output$archivo_carga_texto_participantes <- renderText({
            if (num_empresas > 1) {
              " "
            }else{
              paste0("# registros: ", nrow(file))
            }
          })
        }
      })
      
      # Esta funcion cambia el nombre del mensaje una vez que se carga un archivo.
      observeEvent(input$archivo_carga_masiva, {
        session$sendCustomMessage("upload_msg", "carga completa")
        # session$sendCustomMessage("upload_txt", "SOME OTHER TEXT")
      })
      
      observeEvent(input$btn_carga_masiva, {
        withProgress(
          message = "Iniciando carga de resultados",
          value = 0, {
            setProgress(0.2, message = "Leyendo archivo")
            shinyjs::disable("btn_carga_masiva")
            print(paste0("File time of saving: ", ymd_hms(now(tzone = "Chile/Continental"))))
            # file <- file_content() %>% select(-empresa) %>% mutate(fecha_carga_datos = lubridate::with_tz(Sys.time(), "Chile/Continental"))
            file <- file_content() %>% 
              select(-empresa) %>% 
              mutate(fecha_carga_datos = lubridate::ymd_hms(now(tzone = "Chile/Continental")),
                     updated_by = session$userData$email
              )
            setProgress(0.5, message = "Guardando datos")
            res <- save_archivo_resultados(file)
            setProgress(0.8, message = "Guardando datos")
            print(paste0("archivo cargado: ", res))
            shinyjs::enable("btn_carga_masiva")
            shinyjs::reset("carga_masiva_form")
            # showNotification("Archivo cargado.", type = "message")
            removeModal()
            setProgress(1, message = "Archivo cargado")
            file_loader$trigger <- file_loader$trigger + 1
          }
        )
      })
      
      # codigo para leer nueva planilla que cliente descarga de CEIM
      file_content <- reactive({
        print("cargando archivo masivo...")
        inputFile <- input$archivo_carga_masiva
        if (is.null(inputFile))
          return()
        Datapath <- input$archivo_carga_masiva$datapath
        resultados_bd <- read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "NO VIGENTE", "ABANDONADO", "EN PROCESO", "SIN ESTADO"), col_types = c("text")) %>% clean_names() %>% 
          select(rut_persona, nombre_persona, email, telefono, cargo_contrato, empresa, rut_empresa, perfil, fecha_evaluacion_psicolaboral, resultado_psicolaboral, puntaje_psicolaboral, resultado_conductual, fecha_evaluacion_conductual,
                 puntaje_conocimiento_seguridad, fecha_evaluacion_conocimiento_seguridad, resultado_conocimiento_seguridad, fecha_vencimiento_conocimiento_seguridad, puntaje_identificacion_riesgos, resultado_identificacion_riesgos,
                 fecha_evaluacion_identificacion_riesgos, puntaje_teorica, fecha_evaluacion_teorica, resultado_teorica, puntaje_practica, fecha_evaluacion_practica, resultado_practica, puntaje_gestion, resultado_gestion, fecha_evaluacion_gestion, 
                 resultado_certificacion, estado_final, fecha_online, fecha_practico, fecha_vr, estado_informe) %>% 
          mutate(estado_final = if_else(estado_informe == "PENDIENTE", "RESULTADO PENDIENTE", estado_final)) %>% 
          select(-estado_informe) %>% 
          mutate(across(-estado_final, ~na_if(., "PENDIENTE"))) %>%
          rename(rut = rut_persona,
                 perfil_3d = perfil,
                 cargo = cargo_contrato,
                 psicolaboral = puntaje_psicolaboral,
                 psicolaboral_categoria = resultado_psicolaboral,
                 psicolaboral_fecha = fecha_evaluacion_psicolaboral,
                 conductas_de_riesgo = resultado_conductual,
                 conductas_de_riesgo_fecha = fecha_evaluacion_conductual,
                 vr = puntaje_identificacion_riesgos,
                 vr_fecha = fecha_evaluacion_identificacion_riesgos,
                 vr_categoria = resultado_identificacion_riesgos,
                 conocimientos_en_seguridad  = puntaje_conocimiento_seguridad,
                 conocimientos_en_seguridad_fecha = fecha_evaluacion_conocimiento_seguridad,
                 conocimientos_en_seguridad_categoria = resultado_conocimiento_seguridad,
                 tecnica_teorica = puntaje_teorica, 
                 tecnica_teorica_fecha = fecha_evaluacion_teorica, 
                 tecnica_teorica_categoria = resultado_teorica, 
                 tecnica_practica = puntaje_practica, 
                 tecnica_practica_fecha = fecha_evaluacion_practica,
                 tecnica_practica_categoria = resultado_practica,
                 gestion = puntaje_gestion,
                 gestion_categoria = resultado_gestion,
                 gestion_fecha = fecha_evaluacion_gestion,
                 certificacion = resultado_certificacion,
                 resultado_final_3d = estado_final,
                 # fecha_examen_presencial = fecha_presencial,
                 fecha_examen_on_line = fecha_online
          ) %>% 
          separate(nombre_persona, c("first","second", "third", "fourth", "fifth", "sixth"), remove = F) %>%
          mutate(
            nombres = if_else(!is.na(sixth), paste(first,second,third,fourth),
                              if_else(!is.na(fifth), paste(first,second,third),paste(first,second))),
            apellidos = if_else(!is.na(sixth), paste(fifth, sixth),
                                if_else(!is.na(fifth), paste(fourth,fifth),paste(third,fourth))),
            psicolaboral = as.numeric(str_replace(psicolaboral, ",", ".")),
            psicolaboral_fecha = dmy(psicolaboral_fecha),
            vr = as.numeric(str_replace(vr, ",", ".")),
            vr_fecha = dmy(vr_fecha),
            conocimientos_en_seguridad = as.numeric(str_replace(conocimientos_en_seguridad, ",", ".")),
            conocimientos_en_seguridad_fecha = dmy(conocimientos_en_seguridad_fecha),
            tecnica_teorica = as.numeric(str_replace(tecnica_teorica, ",", ".")),
            tecnica_teorica_fecha = dmy(tecnica_teorica_fecha),
            tecnica_practica = as.numeric(str_replace(tecnica_practica, ",", ".")),
            tecnica_practica_fecha = dmy(tecnica_practica_fecha),
            gestion = as.numeric(str_replace(gestion, ",", ".")),
            gestion_fecha = dmy(gestion_fecha),
            certificacion = as.numeric(if_else(certificacion == 'COMPETENTE', 100, NA)),
            resultado_final_3d = if_else(resultado_final_3d == 'CON OBSERVACIONES', 'NO COMPETENTE', resultado_final_3d),
            estado_evaluacion_3d = NA,
            fecha_vencimiento_3d = NA,
            estado_vigente_vencido = NA,
            conductas_de_riesgo_fecha = dmy(conductas_de_riesgo_fecha),
            conductas_de_riesgo = if_else(conductas_de_riesgo == 'CON OBSERVACIONES', 'NO COMPETENTE', conductas_de_riesgo),
            vr_categoria = if_else(vr_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', vr_categoria),
            conocimientos_en_seguridad_categoria = if_else(conocimientos_en_seguridad_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', conocimientos_en_seguridad_categoria),
            tecnica_teorica_categoria = if_else(tecnica_teorica_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', tecnica_teorica_categoria),
            tecnica_practica_categoria = if_else(tecnica_practica_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', tecnica_practica_categoria),
            gestion_categoria = if_else(gestion_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', gestion_categoria),
            psicolaboral_categoria = if_else(psicolaboral_categoria == 'CON OBSERVACIONES', 'NO COMPETENTE', psicolaboral_categoria),
            dim_psicolaboral = as.numeric(NA),
            dim_psicolaboral_fecha = NA_Date_,
            dim_psicolaboral_categoria = psicolaboral_categoria,
            dim_seguridad = as.numeric(NA),
            dim_seguridad_categoria = if_else(rowSums(cbind(conductas_de_riesgo == "NO COMPETENTE", vr_categoria == "NO COMPETENTE", conocimientos_en_seguridad_categoria == "NO COMPETENTE"), na.rm = T) > 0, 'NO COMPETENTE', 
                                              if_else(rowSums(cbind(conductas_de_riesgo == "COMPETENTE CON OBSERVACIONES", vr_categoria == "COMPETENTE CON OBSERVACIONES", conocimientos_en_seguridad_categoria == "COMPETENTE CON OBSERVACIONES"), na.rm = T) > 0, 'COMPETENTE CON OBSERVACIONES',
                                                      if_else(rowSums(cbind(conductas_de_riesgo == "COMPETENTE", vr_categoria == "COMPETENTE", conocimientos_en_seguridad_categoria == "COMPETENTE"), na.rm = T) > 0, 'COMPETENTE', as.character(NA)))),
            dim_tecnica = as.numeric(NA),
            dim_tecnica_categoria = if_else(rowSums(cbind(tecnica_teorica_categoria == "NO COMPETENTE", tecnica_practica_categoria == "NO COMPETENTE", gestion_categoria == "NO COMPETENTE"), na.rm = T) > 0, 'NO COMPETENTE', 
                                            if_else(rowSums(cbind(tecnica_teorica_categoria == "COMPETENTE CON OBSERVACIONES", tecnica_practica_categoria == "COMPETENTE CON OBSERVACIONES", gestion_categoria == "COMPETENTE CON OBSERVACIONES"), na.rm = T) > 0, 'COMPETENTE CON OBSERVACIONES',
                                                    if_else(rowSums(cbind(tecnica_teorica_categoria == "COMPETENTE", tecnica_practica_categoria == "COMPETENTE", gestion_categoria == "COMPETENTE"), na.rm = T) > 0, 'COMPETENTE', as.character(NA)))),
            fecha_examen_on_line = dmy(fecha_examen_on_line),
            # fecha_examen_presencial = dmy(fecha_examen_presencial),
            fecha_examen_presencial = pmin(dmy(fecha_practico), dmy(fecha_vr)),
            fecha_de_ultima_evaluacion = pmax(fecha_examen_on_line, fecha_examen_presencial)
          ) %>% 
          select(-first,-second,-third,-fourth,-fifth,-sixth,-nombre_persona) %>%
          select(
            empresa, rut_empresa, rut, nombres, apellidos, cargo, perfil_3d, psicolaboral, psicolaboral_fecha, psicolaboral_categoria, conductas_de_riesgo,
            conductas_de_riesgo_fecha, vr, vr_fecha, vr_categoria, conocimientos_en_seguridad, conocimientos_en_seguridad_fecha, conocimientos_en_seguridad_categoria, 
            tecnica_teorica, tecnica_teorica_fecha, tecnica_teorica_categoria, tecnica_practica, tecnica_practica_fecha, tecnica_practica_categoria, gestion, gestion_fecha, 
            gestion_categoria, certificacion, estado_evaluacion_3d, fecha_de_ultima_evaluacion, fecha_examen_presencial, fecha_examen_on_line, resultado_final_3d, fecha_vencimiento_3d,
            estado_vigente_vencido, dim_seguridad, dim_seguridad_categoria, dim_psicolaboral, dim_psicolaboral_fecha, dim_psicolaboral_categoria,
            dim_tecnica, dim_tecnica_categoria
          )
        resultados_bd
      })
      
      actual_file <- reactive({
        inputFile <- input$archivo_carga_masiva
        if (is.null(inputFile))
          return()
        Datapath <- input$archivo_carga_masiva$datapath
        read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO")) %>% clean_names()
      })
      
      estructure_comparison <- reactive({
        print("Dentro de comparison")
        expected_structure <- read_excel(path = "./www/resources/columnas_esperadas.xlsx", na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO"), n_max = 0) %>% clean_names()
        Datapath <- input$archivo_carga_masiva$datapath
        actual_file <- read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO"), n_max = 0) %>% clean_names()
        janitor::compare_df_cols(expected_structure, actual_file, return = "mismatch", bind_method = "rbind") %>% nrow()
      })
      
      observeEvent(input$instructivo_0, {
        instrucciones_form()
      })
      
      instrucciones_form <- function(){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            tabsetPanel(
              id = ns("inTabset"),
              type = "hidden",
              tabPanel(
                title = "Paso 1",
                tags$img(src="images/ceim - paso1.png", style = "width: 100%; padding: 0;"),
                br(),
                fluidRow(
                  column(4),
                  column(4),
                  column(4, actionButton(ns("paso_12"), label = div("Paso 2", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"), style = "text-align: right")
                )
              ),
              tabPanel(
                title = "Paso 2",
                tags$img(src="images/ceim - paso2.png", style = "width: 100%; padding: 0;"),
                br(),
                fluidRow(
                  column(4, actionButton(ns("paso_21"), "Paso 1", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                  column(4),
                  column(4, actionButton(ns("paso_23"), label = div("Paso 3", icon("caret-right")), width = 120, style = "background-color: #0079b5; color: white"), style = "text-align: right")
                )
              ),
              tabPanel(
                title = "Paso 3",
                tags$img(src="images/ceim - paso3.png", style = "width: 100%; padding: 0;"),
                br(),
                fluidRow(
                  column(4, actionButton(ns("paso_32"), "Paso 2", icon = icon("caret-left"), width = 120, style = "background-color: #0079b5; color: white")),
                  column(4),
                  column(4)
                )
              )
            )
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = "Instrucciones para descargar planilla resultados",
          footer = tagList(
            actionButton(ns("close_obs"), "Cerrar")
          ),
          easyClose = T, size = "l"
        ))
      }
      
      observeEvent(input$paso_12 , {
        updateTabsetPanel(session, "inTabset",selected = "Paso 2")
      })
      
      observeEvent(input$paso_23 , {
        updateTabsetPanel(session, "inTabset",selected = "Paso 3")
      })
      
      observeEvent(input$paso_21 , {
        updateTabsetPanel(session, "inTabset",selected = "Paso 1")
      })
      
      observeEvent(input$paso_32 , {
        updateTabsetPanel(session, "inTabset",selected = "Paso 2")
      })
      
      observeEvent(input$close_obs, {
        removeModal()
        shinyjs::reset("instrucciones_form")
      })
    } # End of moduleServer function
  )
}