alertas_ui <- function(id) {
  tagList(
    br(),
    h1("Alerta de Vencimientos", style = "font-size: 1.8rem;"),
    fluidRow(
      column(
        width = 3,
        div(textOutput(NS(id, "fecha_ultima_actualizacion")), style = "padding:5px;")
        # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
        # actionButton(NS(id, "res_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
        # actionButton(NS(id, "res_carga_masiva"), "Cargar resultados", class = "btn-success"),
        #textOutput(NS(id, "fecha_ultima_actualizacion")),
        # actionButton(NS(id, "res_refresh"), "Borrar", class = "btn-success", icon("trash-alt"))
        # HTML("Sube el archivo que contiene la lista de resultados para desplegar un resumen visual. La plantilla que seguimos esta disponible para descarga")
      ),
      column(
        width = 5,
        align = "left",
        div(
          bslib::layout_columns(
            width = 1/3,
            fillable = T,
            gap = "0px",
            actionButton(NS(id,"filtro_por_vencer"), label = "POR VENCER", width = "100%", style = "color: #fff; background-color: #006ac2; height: 30px", size = "xs"),
            actionButton(NS(id,"filtro_vencidos"), label = "VENCIDOS", width = "100%",  style = "background-color: #f8f9fa; height: 30px", size = "xs"),
            actionButton(NS(id,"filtro_todos"), label = "TODOS", width = "100%",  style = "background-color: #f8f9fa; height: 30px", size = "xs")
          ),
          style = "padding-top: 40px; margin-bottom: -50px"
        )
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
        div(DT::DTOutput(NS(id, "resultados_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
      )
    )
  )
}

alertas_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
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
      
      #Add data
      saveData <- function(data){
        quary <- sqlAppendTable(pool, "resultados_temporal", data, row.names = FALSE) %>% data.table::setorder(-fecha_de_ultima_evaluacion)
        dbExecute(pool, quary)
      }
      
      filtro_alertas <- reactiveValues(
        vencidos = FALSE,
        por_vencer = TRUE,
        todos = FALSE
      )
      
      observeEvent(input$filtro_por_vencer, {
        ns <- session$ns
        filtro_alertas$vencidos <- FALSE
        filtro_alertas$por_vencer <- TRUE
        filtro_alertas$todos <- FALSE
        #444
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.color = "#fff";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#444";'))
      })
      
      observeEvent(input$filtro_vencidos, {
        ns <- session$ns
        filtro_alertas$vencidos <- TRUE
        filtro_alertas$por_vencer <- FALSE
        filtro_alertas$todos <- FALSE
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.color = "#fff";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#444";'))
      })
      
      observeEvent(input$filtro_todos, {
        ns <- session$ns
        filtro_alertas$vencidos <- FALSE
        filtro_alertas$por_vencer <- FALSE
        filtro_alertas$todos <- TRUE
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_por_vencer"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_vencidos"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#fff";'))
      })
      # ================= BEGIN: ALERTAS =======================
      
      #load responses_df and make reactive to inputs  
      resultados_df <- reactive({
        
        #make reactive to
        dataChangedTrigger()
        #input$res_submit_edit
        #input$res_refresh
        rv$tab_resultados_clicked
        # rv$cambio_empresa
        
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        # dbExecute(pool, 'SET character set "utf8"')
        # dbReadTable(pool, "alertas_vencimiento_view") %>% 
        #   filter(
        #     if (as.numeric(session$userData$id_empresa) == 0) {
        #       1 == 1
        #     } else {
        #       id_empresa == as.numeric(session$userData$id_empresa)
        #     }
        #   )
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("1 = 1", .con = pool)
        } else {
          filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.alertas_vencimiento_view where ({filtros})", .con = pool)
        dbGetQuery(pool, tbl)
      })
      
      # TODO: CERTIFICACION no caduca, sacarla de la tabla
      # TODO: remover boton de carga archivo y dejar solamente el boton del modulo resultados
      # TODO: calcular dias restantes a fecha de vencimiento. Si dias < 0 meses --> background rojo, si dias < 3 meses --> background amarillo
      output$resultados_table <- DT::renderDataTable({
        table <- resultados_df() %>% select(-id_empresa, -fecha_carga_datos) %>%
          mutate(nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
                 n = row_number(),
                 psicolaboral_fecha = round(as.numeric(difftime(ymd(psicolaboral_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 conductas_de_riesgo_fecha = round(as.numeric(difftime(ymd(conductas_de_riesgo_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 vr_num = round(as.numeric(difftime(ymd(vr_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 vr_fecha = if_else(is.na(vr_fecha), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(vr_num)),
                 conocimientos_en_seguridad_fecha = round(as.numeric(difftime(ymd(conocimientos_en_seguridad_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 tecnica_teorica_num = round(as.numeric(difftime(ymd(tecnica_teorica_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 tecnica_teorica_fecha = if_else(is.na(tecnica_teorica_fecha), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(tecnica_teorica_num)),
                 tecnica_practica_num = round(as.numeric(difftime(ymd(tecnica_practica_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 tecnica_practica_fecha = if_else(is.na(tecnica_practica_fecha), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(tecnica_practica_num)),
                 gestion_num = round(as.numeric(difftime(ymd(gestion_fecha), now(tzone = "Chile/Continental"), units = "days"))),
                 gestion_fecha= if_else(is.na(gestion_fecha), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(gestion_num)),
                 certificacion = if_else(is.na(certificacion), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), " "),
                 fecha_vencimiento_3d = round(as.numeric(difftime(ymd(fecha_vencimiento_3d), now(tzone = "Chile/Continental"), units = "days")))
                 ) %>%
          # dplyr::filter(
          #   if (filtro_alertas$por_vencer) {
          #     !if_any(c("psicolaboral_fecha", "conductas_de_riesgo_fecha", "vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num"), ~ . < 0 | is.na(.))
          #     # !apply(df[, c("psicolaboral_fecha", "conductas_de_riesgo_fecha", "vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num")], 1, function(row) any(row > 1 & row < 90))
          #   } else if (filtro_alertas$vencidos) {
          #     if_all(c("psicolaboral_fecha", "conductas_de_riesgo_fecha", "vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num"), ~ . >= 0 & . <= 90)
          #     # !apply(df[, c("psicolaboral_fecha", "conductas_de_riesgo_fecha", "vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num")], 1, function(row) any(is.na(row) | row <= 0))
          #   } else {
          #     TRUE
          #   }
          # ) %>% 
          relocate(n) %>%
          relocate(vr_fecha, .after = conocimientos_en_seguridad_fecha) %>% 
          relocate(estado_vigente_vencido, .after = cargo) %>%
          select(-apellidos) %>% 
          dplyr::arrange(fecha_vencimiento_3d)
        
        if (filtro_alertas$por_vencer) {
          verde <- ""
          naranjo <- "#F1C429"
          rojo <- ""
          table <- table %>% 
            filter(if_any(c(psicolaboral_fecha, conductas_de_riesgo_fecha, conocimientos_en_seguridad_fecha, vr_num, tecnica_teorica_num,tecnica_practica_num,gestion_num), ~ between(., 0, 90)))
        }else{
          if (filtro_alertas$vencidos) {
            verde <- ""
            naranjo <- ""
            rojo <- "#E4465C"
            table <- table %>% 
              filter(if_any(c(psicolaboral_fecha, conductas_de_riesgo_fecha, conocimientos_en_seguridad_fecha, vr_num, tecnica_teorica_num,tecnica_practica_num,gestion_num), ~ . < 0))
          }else{
            # verde <- "#77C151"
            verde <- ""
            naranjo <- "#F1C429"
            rojo <- "#E4465C"
          }
        }
        
        names(table) <- c("n","id", "Rut", "Participante", "Cargo", "Estado", "PS","CO", "CS", "VR", "TT", "TP", "GE", "CE", "Final", "vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num")
        
        print(head(table))
        
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'none',
                           options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F,
                                          columnDefs = list(list(visible = F, targets = c(1)), # DT comienza contando desde 0
                                                            list(targets = c(5,14,15,16,17,18), visible = FALSE),
                                                            list(className = 'dt-center', targets = "_all")),
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                          # initComplete = JS(
                                          #   "function(settings, json) {",
                                          #   "$(this.api().table().header()).css({'font-size': '85%'});",
                                          #   "}")
                           ),
                           callback = JS(paste0("var tips = ['Index', 'id', 'Rut', 'Nombres y Apellidos', 'Cargo', 'Estado Evaluación', 'Psicolaboral', 'Conductual', 'Conocimiento Seguridad', 'Identificación de Riesgos', 'Técnico Teórico', 'Técnico Práctico', 'Gestión', 'Certificación', 'Fecha Vencimiento 3D', '1', '2', '3', '4'],
                                            firstRow = $('#",session$ns('resultados_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
        ) %>% 
          formatStyle(columns = c("PS","CO", "CS", "Final"),
                      backgroundColor = styleInterval(c(0,90), c(rojo,naranjo,verde)),
                      fontWeight = 'bold', `text-align` = 'center') %>% 
          formatStyle(columns = c("VR", "TT", "TP", "GE"),
                      valueColumns = c("vr_num", "tecnica_teorica_num", "tecnica_practica_num", "gestion_num"),
                      backgroundColor = styleInterval(c(0,90), c(rojo,naranjo,verde)),
                      fontWeight = 'bold', `text-align` = 'center') 
          # formatStyle(columns = c("CE"),
          #             backgroundColor = styleEqual(c(" "), c("#77C151")),
          #             fontWeight = 'bold', `text-align` = 'center')
        
      })
      
      output$fecha_ultima_actualizacion <- renderText({
        # fecha <- max(unique(resultados_df()$fecha_carga_datos))
        # paste0("Última actualización de resultados: ", format(as.Date(fecha), format = "%d-%m-%y"))
        
        input$listado_empresas
        info <- get_info_actualizacion_resultados(session$userData$id_empresa)
        fecha <- info$fecha
        usuario <- info$updated_by
        if (is.null(usuario)) {
          paste0("Última actualización ", format(as.Date(fecha), format = "%d-%m-%y"), " (Sin nombre)")
        }else{
          paste0("Última actualización ", format(as.Date(fecha), format = "%d-%m-%y"), " (", usuario, ")")
        }
      })
      
      # ************************************************************************************
      
      observeEvent(input$res_carga_masiva, {
        carga_masiva_form("btn_carga_masiva")
      })
      
      #edit data
      observeEvent(input$res_editar, priority = 30,{
        
        SQL_df <- dbReadTable(pool, "resultados_view")
        
        showModal(
          if(length(input$resultados_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una sola fila." ),easyClose = TRUE)
          } else if(length(input$resultados_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),easyClose = TRUE)
          })  
        
        if(length(input$resultados_table_rows_selected) == 1 ){
          
          resultado_entry_form("res_submit_edit")
          
          updateTextInput(session, "res_rut", value = SQL_df[input$resultados_table_rows_selected, "rut"], label = "RUT")
          updateTextInput(session, "res_nombres", value = SQL_df[input$resultados_table_rows_selected, "nombres"])
          updateTextInput(session, "res_apellidos", value = SQL_df[input$resultados_table_rows_selected, "apellidos"])
          updateTextInput(session, "res_cargo", value = SQL_df[input$resultados_table_rows_selected, "cargo"])
          updateTextInput(session, "res_fecha_solicitud", value = SQL_df[input$resultados_table_rows_selected, "fecha_solicitud"])
          updateTextInput(session, "res_psicolaboral", value = SQL_df[input$resultados_table_rows_selected, "psicolaboral"])
          updateTextInput(session, "res_conductual", value = SQL_df[input$resultados_table_rows_selected, "conductual"])
          updateTextInput(session, "res_conoc_seguridad", value = SQL_df[input$resultados_table_rows_selected, "conocimientos_de_seguridad"])
          updateTextInput(session, "res_ident_riesgos", value = SQL_df[input$resultados_table_rows_selected, "identificacion_de_riesgos"])
          updateTextInput(session, "res_tec_teorica", value = SQL_df[input$resultados_table_rows_selected, "tecnica_teorica"])
          updateTextInput(session, "res_tec_practica", value = SQL_df[input$resultados_table_rows_selected, "tecnica_practica"])
          updateTextInput(session, "res_gestion", value = SQL_df[input$resultados_table_rows_selected, "gestion"])
          updateTextInput(session, "res_certificacion", value = SQL_df[input$resultados_table_rows_selected, "certificacion"])
          updateTextInput(session, "res_total", value = SQL_df[input$resultados_table_rows_selected, "resultado_final"])
          
          #print(SQL_df[input$monitor_table_rows_selected, "nombres"])
        }
        
      })
      
      observeEvent(input$res_submit_edit, priority = 30, {
        
        SQL_df <- dbReadTable(pool, "resultados_view")
        operacion <- SQL_df[input$resultados_table_row_last_clicked, "with_records"]
        row_selection <- SQL_df[input$resultados_table_row_last_clicked, "id"] 
        
        if (operacion == 'Y') { # existen registros de resultados por lo que hay que actualizar
          sqlq <- glue::glue_sql("UPDATE resultados 
                                  set psicolaboral = NULLIF({input$res_psicolaboral}, ''),
                                      conductual = NULLIF({input$res_conductual}, ''),
                                      conocimientos_de_seguridad = NULLIF({input$res_conoc_seguridad}, ''),
                                      identificacion_de_riesgos = NULLIF({input$res_ident_riesgos}, ''),
                                      tecnica_teorica = NULLIF({input$res_tec_teorica}, ''),
                                      tecnica_practica = NULLIF({input$res_tec_practica}, ''),
                                      gestion = NULLIF({input$res_gestion}, ''),
                                      certificacion = NULLIF({input$res_certificacion}, ''),
                                      resultado_final = NULLIF({input$res_total}, '')
                                  WHERE id = {row_selection}", .con = pool)
          dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
          dbExecute(pool, sqlq)
          dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
          
        }else{ # no existe registro por lo que hay que insertar
          data <- data.frame(id = row_selection,
                             psicolaboral = if_else(input$res_psicolaboral == '', NA,input$res_psicolaboral),
                             conductual = if_else(input$res_conductual == '', NA,input$res_conductual),
                             conocimientos_de_seguridad = if_else(input$res_conoc_seguridad == '', NA,input$res_conoc_seguridad),
                             identificacion_de_riesgos = if_else(input$res_ident_riesgos == '', NA,input$res_ident_riesgos),
                             tecnica_teorica = if_else(input$res_tec_teorica == '', NA,input$res_tec_teorica),
                             tecnica_practica = if_else(input$res_tec_practica == '', NA,input$res_tec_practica),
                             gestion = if_else(input$res_gestion == '', NA,input$res_gestion),
                             certificacion = if_else(input$res_certificacion == '', NA,input$res_certificacion),
                             resultado_final = if_else(input$res_total == '', NA,input$res_total))
          
          query <- sqlAppendTable(pool, "resultados", data, row.names = FALSE)
          dbExecute(pool, query)
        }
        
        shinyjs::reset("resultado_entry_form")
        showNotification("Resultado registrado.", type = "message")
        removeModal()
        dataChangedTrigger(dataChangedTrigger() + 1)
        
      })
      
      # ================= END: MONITOR =======================
      
      carga_masiva_form <- function(button_id){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(p("Sube el archivo que contiene la lista de resultados para desplegar un resumen visual. La plantilla que seguimos esta disponible para descarga")),
            br(),
            fluidRow(downloadLink(ns("download_template"), "Descargue plantilla de ejemplo")),
            fluidRow(fileInput(ns("archivo_carga_masiva"), "Seleccione archivo")),
            fluidRow(textOutput(ns("archivo_carga_texto_empresa"))),
            fluidRow(textOutput(ns("archivo_carga_texto_participantes")))
          ),
          tags$div(id = session$ns("constraintPlaceholder")),
          title = "Carga masiva de resultados",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Cargar")
          ),
          easyClose = TRUE
        ))
      }
      
      # logica para descargar plantilla de ejemplo
      output$download_template <- downloadHandler(
        filename = function() {
          paste("BaseDatos.xlsx", "xlsx", sep=".")
        },
        content = function(file) {
          file.copy("www/resources/BaseDatos.xlsx.xlsx", file)
        },
        contentType = "application/zip"
      )
      
      # TODO: Valor PENDIENTE en columna Estado debe mantenerse.
      file_content <- reactive({
        print("cargando archivo masivo...")
        inputFile <- input$archivo_carga_masiva
        if (is.null(inputFile))
          return()
        Datapath <- input$archivo_carga_masiva$datapath
        resultados_bd <- read_excel(path = Datapath, skip = 1, na = c("N/A", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE")) %>% clean_names() %>% 
          select(empresa, rut_sin_punto_con_guion, nombres, apellidos, cargo, fecha_de_ultima_evaluacion, fecha_examen_presencial, fecha_examen_on_line, 
                 psicolaboral_percent, psicolaboral_fecha, conductas_de_riesgo_categoria, conductas_de_riesgo_fecha, identificacion_del_riesgo_realidad_virtual_percent, 
                 identificacion_del_riesgo_realidad_virtual_fecha, conocimientos_en_seguridad_percent, conocimientos_en_seguridad_fecha, teorica_percent, teorica_fecha, 
                 practica_percent, practica_fecha, gestion_percent, gestion_fecha, certificacion_percent, estado_evaluacion_3d_cerrado_o_provisorio, 
                 resultado_final_3d_competente_competente_con_observaciones_no_competente, fecha_vencimiento_ev_3d_solo_para_evaluaciones_3d_con_estado_cerrado) %>%
          rename(rut = rut_sin_punto_con_guion,
                 psicolaboral = psicolaboral_percent,
                 conductas_de_riesgo = conductas_de_riesgo_categoria,
                 vr = identificacion_del_riesgo_realidad_virtual_percent,
                 vr_fecha = identificacion_del_riesgo_realidad_virtual_fecha,
                 conocimientos_en_seguridad  = conocimientos_en_seguridad_percent,
                 tecnica_teorica = teorica_percent, 
                 tecnica_teorica_fecha = teorica_fecha, 
                 tecnica_practica = practica_percent, 
                 tecnica_practica_fecha = practica_fecha, 
                 gestion = gestion_percent,
                 certificacion = certificacion_percent,
                 resultado_final_3d = resultado_final_3d_competente_competente_con_observaciones_no_competente,
                 estado_evaluacion_3d = estado_evaluacion_3d_cerrado_o_provisorio,
                 fecha_vencimiento_3d = fecha_vencimiento_ev_3d_solo_para_evaluaciones_3d_con_estado_cerrado) %>% 
          relocate(fecha_de_ultima_evaluacion, fecha_examen_presencial, fecha_examen_on_line, .before = resultado_final_3d) %>% 
          mutate(id_empresa = 1) %>% 
          relocate(id_empresa)
        resultados_bd
      })
      
      observeEvent(input$archivo_carga_masiva, {
        file <- file_content()
        #saveData(file)
        nombre_empresa <- file %>% distinct(empresa) %>% pull()
        num_empresas <- file %>% distinct(empresa) %>% count() %>% pull()
        
        output$archivo_carga_texto_empresa <- renderText({
          if (num_empresas > 1) {
            "Error, demasiadas empresas en archivo"
          }else{
            paste0("Empresa: ", nombre_empresa)
          }
        })
        
        output$archivo_carga_texto_participantes <- renderText({
          if (num_empresas > 1) {
            " "
          }else{
            paste0("# participantes: ", n_distinct(file$rut))
          }
        })
      })
      
      observeEvent(input$btn_carga_masiva, {
        print(paste0("File time of saving: ", ymd_hms(now(tzone = "Chile/Continental"))))
        # file <- file_content() %>% select(-empresa) %>% mutate(fecha_carga_datos = lubridate::with_tz(Sys.time(), "Chile/Continental"))
        file <- file_content() %>% select(-empresa) %>% mutate(fecha_carga_datos = lubridate::ymd_hms(now(tzone = "Chile/Continental")))
        res <- save_archivo_resultados(file)
        print(paste0("archivo cargado: ", res))
        shinyjs::reset("carga_masiva_form")
        showNotification("Archivo cargado.", type = "message")
        removeModal()
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      resultado_entry_form <- function(button_id){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(column(6, shinyjs::disabled(textInput(ns("res_rut"), "Rut", placeholder = ""))),
                     column(6, shinyjs::disabled(textInput(ns("res_nombres"), "Nombres", placeholder = "")))),
            fluidRow(column(6, shinyjs::disabled(textInput(ns("res_cargo"), "Cargo", placeholder = ""))),
                     column(6, shinyjs::disabled(textInput(ns("res_apellidos"), "Apellidos", placeholder = "")))),
            fluidRow(column(6, shinyjs::disabled(textInput(ns("res_fecha_solicitud"), "Fecha Solicitud", placeholder = "")))),
            fluidRow(column(3, textInput(ns("res_psicolaboral"), "Psicolaboral", placeholder = "")),
                     column(3, textInput(ns("res_conductual"), "Conductual", placeholder = "")),
                     column(3, textInput(ns("res_conoc_seguridad"), "Conocimientos Seguridad", placeholder = ""))),
            fluidRow(column(3, textInput(ns("res_ident_riesgos"), "Identificacion de Riesgos", placeholder = "")),
                     column(3, textInput(ns("res_tec_teorica"), "Tecnico Teorica", placeholder = "")),
                     column(3, textInput(ns("res_tec_practica"), "Tecnico Practica", placeholder = ""))),
            fluidRow(column(3, textInput(ns("res_gestion"), "Gestion", placeholder = "")),
                     column(3, textInput(ns("res_certificacion"), "Certificacion", placeholder = "")),
                     column(3, textInput(ns("res_total"), "Resultado Final", placeholder = "")))
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = "Modificar Resultados",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns(button_id), "Guardar")
          ),
          easyClose = TRUE
        ))
      }
    }
  )
}