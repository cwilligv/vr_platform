registro_resultados_ui <- function(id){
  tagList(
    # tabName = "tab4",
    # h1("Registro de resultados", style = "font-size: 1.8rem;"),
    # bs4Dash::box(
    #   title = h1("Registro de resultados", style = "font-size: 1.8rem;"),
    #   width = 12,
    #   headerBorder = F,
    #   collapsible = F,
    #   fluidRow(
    #     column(
    #       width = 2,
    #       # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
    #       # actionButton(NS(id, "res_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
    #       # actionButton(NS(id, "res_carga_masiva"), "Cargar resultados", class = "btn-success"),
    #       uiOutput(NS(id,"buttons"), inline = T)
    #       #textOutput(NS(id, "fecha_ultima_actualizacion")),
    #       # actionButton(NS(id, "res_refresh"), "Borrar", class = "btn-success", icon("trash-alt"))
    #       # HTML("Sube el archivo que contiene la lista de resultados para desplegar un resumen visual. La plantilla que sugerimos esta disponible para descarga")
    #     ),
    #     column(
    #       width = 6,
    #       div(textOutput(NS(id, "fecha_ultima_actualizacion")), style = "padding:5px;")
    #       # br(),
    #       # actionButton(NS(id, "instructivo_0"), "Instrucciones de descarga archivo CEIM", icon = icon("info-circle"), class = "btn-success")
    #       # HTML("Sube el archivo que contiene la lista de resultados para desplegar un resumen visual. La plantilla que seguimos esta disponible para descarga")
    #     ),
    #     column(
    #       width = 4,
    #       uiOutput(NS(id, "admin_selector"))
    #     )
    #   ),
    #   br(),
    #   fluidRow(
    #     column(
    #       width = 12,
    #       align = "center",
    #       style = "z-index: 10",
    #       div(DT::DTOutput(NS(id, "resultados_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
    #     )
    #   )
    # )
    br(),
    h1("Registro de resultados", style = "font-size: 1.8rem;"),
    fluidRow(
      column(
        width = 3,
        # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
        # actionButton(NS(id, "res_editar"), "Editar", class = "btn-success", icon = shiny::icon("edit")),
        # actionButton(NS(id, "res_carga_masiva"), "Cargar resultados", class = "btn-success"),
        # uiOutput(NS(id,"buttons"), inline = T)
        div(textOutput(NS(id, "fecha_ultima_actualizacion")), style = "padding:5px;")
        #textOutput(NS(id, "fecha_ultima_actualizacion")),
        # actionButton(NS(id, "res_refresh"), "Borrar", class = "btn-success", icon("trash-alt"))
        # HTML("Sube el archivo que contiene la lista de resultados para desplegar un resumen visual. La plantilla que sugerimos esta disponible para descarga")
      ),
      column(
        width = 5,
        align = "left",
        div(
          bslib::layout_columns(
            width = 1/2,
            fillable = T,
            gap = "0px",
            actionButton(NS(id, "filtro_competentes_final"), label = "COMPETENTE FINAL", width = "100%", style = "color: #fff; background-color: #006ac2; height: 30px", size = "xs"),
            actionButton(NS(id, "filtro_brechas"), label = "BRECHAS", width = "100%",  style = "background-color: #f8f9fa; height: 30px", size = "xs"),
            actionButton(NS(id, "filtro_todos"), label = "TODOS", width = "100%", style = "background-color: #f8f9fa; height: 30px", size = "xs")
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

registro_resultados_server <- function(id, rv, file_loader){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      filtro_resultados <- reactiveValues(
        competentes_final = TRUE,
        brechas = FALSE,
        todos = FALSE
      )
      
      # begin rm
      output$buttons <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin','coach','cliente')) {
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
      # end rm
      
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
      
      observeEvent(input$filtro_competentes_final, {
        ns <- session$ns
        filtro_resultados$competentes_final <- TRUE
        filtro_resultados$brechas <- FALSE
        filtro_resultados$todos <- FALSE
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.color = "#fff";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#444";'))
      })
      
      observeEvent(input$filtro_brechas, {
        ns <- session$ns
        filtro_resultados$competentes_final <- FALSE
        filtro_resultados$brechas <- TRUE
        filtro_resultados$todos <- FALSE
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.color = "#fff";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#444";'))
      })
      
      observeEvent(input$filtro_todos, {
        ns <- session$ns
        filtro_resultados$competentes_final <- FALSE
        filtro_resultados$brechas <- FALSE
        filtro_resultados$todos <- TRUE
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_competentes_final"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.background = "#f8f9fa";'))
        runjs(paste0('document.getElementById("',ns("filtro_brechas"),'").style.color = "#444";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.background = "#006ac2";'))
        runjs(paste0('document.getElementById("',ns("filtro_todos"),'").style.color = "#fff";'))
      })
      # ================= BEGIN: MONITOR =======================
      
      #load responses_df and make reactive to inputs  
      resultados_df <- reactive({
        print("dentro de resultados reactive")
        #make reactive to
        dataChangedTrigger()
        file_loader$trigger
        rv$tab_herramientas_clicked
        
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("1 = 1", .con = pool)
        } else {
          filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool)
        }
        
        if(filtro_resultados$competentes_final){
          filtro2 <- glue::glue_sql("resultado_final_3d = 'COMPETENTE'", .con = pool)
        } else {
          if (filtro_resultados$brechas) {
            filtro2 <- glue::glue_sql("(psicolaboral_categoria != 'COMPETENTE' AND psicolaboral_categoria IS NOT NULL) OR 
          	  (conductas_de_riesgo != 'COMPETENTE' AND conductas_de_riesgo IS NOT NULL) OR 
          	  (vr_categoria != 'COMPETENTE' AND vr_categoria IS NOT NULL) OR 
          	  (conocimientos_en_seguridad_categoria != 'COMPETENTE' AND conocimientos_en_seguridad_categoria IS NOT NULL) OR
          	  (tecnica_teorica_categoria != 'COMPETENTE' AND tecnica_teorica_categoria IS NOT NULL) OR 
          	  (tecnica_practica_categoria  != 'COMPETENTE' AND tecnica_practica_categoria IS NOT NULL) OR 
          	  (gestion_categoria != 'COMPETENTE' AND gestion_categoria IS NOT NULL) OR 
          	  (certificacion != '100' AND certificacion IS NOT NULL) AND
          	  NOT (psicolaboral_categoria IS NULL AND conductas_de_riesgo IS NULL AND vr_categoria IS NULL AND 
          	  conocimientos_en_seguridad_categoria IS NULL AND tecnica_teorica_categoria IS NULL AND tecnica_practica_categoria IS NULL AND 
          	  gestion_categoria IS NULL AND certificacion IS NULL)
            ", .con = pool)
          } else {
            filtro2 <- glue::glue_sql("1 = 1", .con = pool)
          }
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.resultados_planilla where ({filtros}) and ({filtro2}) order by CAST(fecha_de_ultima_evaluacion as date) DESC", .con = pool)
        print(tbl)
        dbGetQuery(pool, tbl)
      })
      
      output$resultados_table <- DT::renderDataTable({
        print("rendering table resultados")
        table <- resultados_df() %>% 
          select(-id_empresa,-psicolaboral_fecha,-conductas_de_riesgo_fecha,-vr_fecha,-conocimientos_en_seguridad_fecha,
                 -tecnica_teorica_fecha,-tecnica_practica_fecha,-gestion_fecha,-fecha_examen_presencial,-fecha_examen_on_line, 
                 -fecha_carga_datos, -fecha_vencimiento_3d, -psicolaboral_categoria, -vr_categoria, -conductas_de_riesgo_categoria,
                 -conocimientos_en_seguridad_categoria, -tecnica_teorica_categoria, -tecnica_practica_categoria, -gestion_categoria, -perfil_3d,
                 -dim_seguridad, -dim_seguridad_categoria, -dim_psicolaboral, -dim_psicolaboral_fecha, -dim_psicolaboral_categoria, -dim_tecnica, 
                 -dim_tecnica_categoria, -estado_vigente_vencido, -estado_evaluacion_3d, -updated_by) %>% 
          mutate(nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
                 n = row_number(),
                 fecha_de_ultima_evaluacion  = format(as.Date(fecha_de_ultima_evaluacion ), format = "%d-%m-%y"),
                 vr = if_else(is.na(vr), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(vr)),
                 tecnica_teorica = if_else(is.na(tecnica_teorica), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(tecnica_teorica)),
                 tecnica_practica = if_else(is.na(tecnica_practica), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(tecnica_practica)),
                 gestion = if_else(is.na(gestion), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(gestion)),
                 certificacion = if_else(is.na(certificacion), as.character(icon("ban", style = "font-size: 24px; color:lightgray;")), as.character(certificacion)),
                 conductas_de_riesgo = if_else(conductas_de_riesgo == 'COMPETENTE', "C", 
                                              if_else(conductas_de_riesgo == 'COMPETENTE CON OBSERVACIONES', "C/O",
                                                      if_else(conductas_de_riesgo == 'NO COMPETENTE', "N/C", ""))),
                 resultado_final_3d = if_else(resultado_final_3d == 'COMPETENTE', as.character(icon("smile", class = "fa-solid", style = "font-size: 24px;color: #77C151;")), 
                                           if_else(resultado_final_3d == 'COMPETENTE CON OBSERVACIONES', as.character(icon("meh", class = "fa-solid", style = "font-size: 24px;color: #F1C429;")),
                                                   if_else(resultado_final_3d == 'NO COMPETENTE', as.character(icon("frown", class = "fa-solid", style = "font-size: 24px;color: #E4465C;")),as.character(""))))) %>%
          relocate(n) %>%
          relocate(fecha_de_ultima_evaluacion, .after = cargo) %>%
          relocate(vr, .after = conocimientos_en_seguridad) %>% 
          # relocate(estado_evaluacion_3d, .after = fecha_de_ultima_evaluacion) %>%
          select(-apellidos)
        
        if (filtro_resultados$competentes_final) {
          verde <- ""
          naranjo <- ""
          rojo <- ""
        }else{
          if (filtro_resultados$brechas) {
            verde <- ""
            naranjo <- "#F1C429"
            rojo <- "#E4465C"
          }else{
            verde <- "#77C151"
            naranjo <- "#F1C429"
            rojo <- "#E4465C"
          }
        }
        
        names(table) <- c("n","id", "Rut", "Participante", "Cargo", "Fecha Evaluación", "PS","CO", "CS", "VR", "TT", "TP", "GE", "CE", "Final")
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'none',
                           extensions = 'Buttons',
                           options = list(searchHighlight = T, searching = T, scrollX = T, autoWidth = F, 
                                          buttons = c('pdf', 'csv', 'excel'),
                                          columnDefs = list(list(visible = F, targets = c(1)), # DT comienza contando desde 0
                                                            list(className = 'dt-center', targets = "_all")),
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                          # initComplete = JS(
                                          #   "function(settings, json) {",
                                          #   "$(this.api().table().header()).css({'font-size': '85%'});",
                                          #   "}")
                           ),
                           callback = JS(paste0("var tips = ['Index', 'id', 'Rut', 'Nombres y Apellidos', 'Cargo', 'Fecha Última Evaluación', 'Psicolaboral', 'Conductual', 'Conocimiento Seguridad', 'Identificación de Riesgos', 'Técnico Teórico', 'Técnico Práctico', 'Gestión', 'Certificación', 'Resultado Final'],
                                            firstRow = $('#",session$ns('resultados_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
        ) %>% 
          formatStyle(columns = c("PS"),
                      backgroundColor = styleInterval(c(77,89), c(rojo,naranjo,verde)),
                      fontWeight = 'bold', `text-align` = 'center') %>%
          formatStyle(columns = c("CO"),
                      backgroundColor = styleEqual(c("N/C", "C/O", "C"), c(rojo,naranjo,verde)),
                      fontWeight = 'bold', `text-align` = 'center') %>%
          formatStyle(c("VR", "CS"),
                      backgroundColor = styleInterval(c(69.3,81.2), c(rojo,naranjo,verde)),
                      fontWeight = 'bold', `text-align` = 'center') %>%
          formatStyle(c("TT", "TP", "GE"),
                      backgroundColor = styleInterval(c(65.3,85.7,100), c(rojo,naranjo,verde,"")),
                      fontWeight = 'bold', `text-align` = 'center') %>%
          formatStyle(columns = c("CE"),
                      backgroundColor = styleEqual(100, verde),
                      fontWeight = 'bold', `text-align` = 'center')
        
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
      
      # begin rm
      observeEvent(input$res_carga_masiva, {
        carga_masiva_form("btn_carga_masiva")
      })
      # end rm
      
      # observeEvent(input$res_carga_masiva_adm, {
      #   carga_masiva_form("btn_carga_masiva")
      # })
      
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
      
      # begin rm
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
            fluidRow(textOutput(ns("archivo_carga_texto_estructura"))),
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
      # end rm
      
      # begin rm
      observeEvent(input$archivo_carga_masiva, {
        session$sendCustomMessage("upload_msg", "carga completa")
        # session$sendCustomMessage("upload_txt", "SOME OTHER TEXT")
      })
      # end rm
      
      # logica para descargar plantilla de ejemplo
      output$download_template <- downloadHandler(
        filename = function() {
          paste("Gestion_Resultados_plantilla", "xlsx", sep=".")
        },
        content = function(file) {
          file.copy("www/resources/Gestion_Resultados_plantilla.xlsx", file)
        },
        contentType = "application/zip"
      )
      
      # codigo para leer planilla anterior
      # file_content <- reactive({
      #   print("cargando archivo masivo...")
      #   inputFile <- input$archivo_carga_masiva
      #   if (is.null(inputFile))
      #     return()
      #   Datapath <- input$archivo_carga_masiva$datapath
      #   resultados_bd <- read_excel(path = Datapath, skip = 1, na = c("N/A", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE")) %>% clean_names() %>% 
      #     select(empresa, rut_empresa, rut_sin_punto_con_guion, nombres, apellidos, cargo, perfil_3d_supervisor_staff_o_tecnico, fecha_de_ultima_evaluacion, fecha_examen_presencial, fecha_examen_on_line, 
      #            psicolaboral_percent, psicolaboral_fecha, psicolaboral_categoria, conductas_de_riesgo_categoria, conductas_de_riesgo_fecha, identificacion_del_riesgo_realidad_virtual_percent, 
      #            identificacion_del_riesgo_realidad_virtual_fecha, identificacion_del_riesgo_realidad_virtual_categoria, conocimientos_en_seguridad_percent, conocimientos_en_seguridad_fecha, 
      #            conocimientos_en_seguridad_categoria, teorica_percent, teorica_fecha, teorica_categoria, practica_percent, practica_fecha, practica_categoria, gestion_percent, gestion_fecha, 
      #            gestion_categoria, certificacion_percent, #estado_evaluacion_3d_cerrado_o_provisorio, 
      #            resultado_final_3d_competente_competente_con_observaciones_no_competente, 
      #            fecha_vencimiento_ev_3d_solo_para_evaluaciones_3d_con_estado_cerrado,
      #            seguridad_percent, seguridad_categoria, psicolaboral_fecha, psicolaboral_categoria, tecnica_percent, tecnica_categoria) %>%
      #     bind_cols(
      #       read_excel(path = Datapath, skip = 1, range = cell_cols("AN")) %>% clean_names()
      #     ) %>% 
      #     bind_cols(
      #       read_excel(path = Datapath, skip = 1, range = cell_cols("AQ")) %>% clean_names()
      #     ) %>% 
      #     rename(rut = rut_sin_punto_con_guion,
      #            perfil_3d = perfil_3d_supervisor_staff_o_tecnico,
      #            psicolaboral = psicolaboral_percent,
      #            conductas_de_riesgo = conductas_de_riesgo_categoria,
      #            vr = identificacion_del_riesgo_realidad_virtual_percent,
      #            vr_fecha = identificacion_del_riesgo_realidad_virtual_fecha,
      #            vr_categoria = identificacion_del_riesgo_realidad_virtual_categoria,
      #            conocimientos_en_seguridad  = conocimientos_en_seguridad_percent,
      #            tecnica_teorica = teorica_percent, 
      #            tecnica_teorica_fecha = teorica_fecha, 
      #            tecnica_teorica_categoria = teorica_categoria, 
      #            tecnica_practica = practica_percent, 
      #            tecnica_practica_fecha = practica_fecha,
      #            tecnica_practica_categoria = practica_categoria,
      #            gestion = gestion_percent,
      #            certificacion = certificacion_percent,
      #            resultado_final_3d = resultado_final_3d_competente_competente_con_observaciones_no_competente,
      #            estado_evaluacion_3d = estado_evaluacion_3d_cerrado_o_provisorio,
      #            fecha_vencimiento_3d = fecha_vencimiento_ev_3d_solo_para_evaluaciones_3d_con_estado_cerrado,
      #            estado_vigente_vencido = estado_vigente_o_vencido,
      #            dim_seguridad = seguridad_percent,
      #            dim_seguridad_categoria = seguridad_categoria,
      #            dim_tecnica = tecnica_percent,
      #            dim_tecnica_categoria = tecnica_categoria) %>% 
      #     mutate(#id_empresa = session$userData$id_empresa,
      #            dim_psicolaboral = psicolaboral,
      #            dim_psicolaboral_fecha = psicolaboral_fecha,
      #            dim_psicolaboral_categoria = psicolaboral_categoria) %>%
      #     relocate(fecha_de_ultima_evaluacion, fecha_examen_presencial, fecha_examen_on_line, .before = resultado_final_3d) %>% 
      #     relocate(estado_evaluacion_3d, .after = fecha_examen_on_line) %>% 
      #     relocate(dim_psicolaboral, dim_psicolaboral_fecha, dim_psicolaboral_categoria, .after = dim_seguridad_categoria) %>%
      #     relocate(estado_vigente_vencido, .after = fecha_vencimiento_3d)
      #   resultados_bd
      # })
      
      # begin rm
      # codigo para leer nueva planilla que cliente descarga de CEIM
      file_content <- reactive({
        print("cargando archivo masivo...")
        inputFile <- input$archivo_carga_masiva
        if (is.null(inputFile))
          return()
        Datapath <- input$archivo_carga_masiva$datapath
        resultados_bd <- read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "EN PROCESO", "SIN ESTADO")) %>% clean_names() %>% 
          select(rut_persona, nombre_persona, email, telefono, cargo_contrato, empresa, rut_empresa, perfil, fecha_evaluacion_psicolaboral, resultado_psicolaboral, puntaje_psicolaboral, resultado_conductual, fecha_evaluacion_conductual,
                 puntaje_conocimiento_seguridad, fecha_evaluacion_conocimiento_seguridad, resultado_conocimiento_seguridad, fecha_vencimiento_conocimiento_seguridad, puntaje_identificacion_riesgos, resultado_identificacion_riesgos,
                 fecha_evaluacion_identificacion_riesgos, puntaje_teorica, fecha_evaluacion_teorica, resultado_teorica, puntaje_practica, fecha_evaluacion_practica, resultado_practica, puntaje_gestion, resultado_gestion, fecha_evaluacion_gestion, 
                 resultado_certificacion, estado_final, fecha_online, fecha_practico, fecha_vr) %>%
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
                 # fecha_examen_presencial = pmin(dmy(fecha_practico), dmy(fecha_vr)),
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
      # end rm
      
      actual_file <- reactive({
        inputFile <- input$archivo_carga_masiva
        if (is.null(inputFile))
          return()
        Datapath <- input$archivo_carga_masiva$datapath
        read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO")) %>% clean_names()
      })
      
      # begin rm
      estructure_comparison <- reactive({
        print("Dentro de comparison")
        expected_structure <- read_excel(path = "./www/resources/columnas_esperadas.xlsx", na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO"), n_max = 0) %>% clean_names()
        Datapath <- input$archivo_carga_masiva$datapath
        actual_file <- read_excel(path = Datapath, na = c("N/A", "NO APLICA", "SIN PORCENTAJE", "PENDIENTE", "NO VIGENTE", "ABANDONADO", "SIN ESTADO"), n_max = 0) %>% clean_names()
        janitor::compare_df_cols(expected_structure, actual_file, return = "mismatch", bind_method = "rbind") %>% nrow()
      })
      # end rm
      
      # begin rm
      observeEvent(input$archivo_carga_masiva, {
        print(paste0("Cols diferentes?:", estructure_comparison()))
        if (estructure_comparison() > 0) {
          shinyjs::disable("btn_carga_masiva")
          output$archivo_carga_texto_estructura <- renderText({
            paste0("ERROR - Archivo con ", estructure_comparison(), " columna(s) diferente(s)")
          })
        }else{
          shinyjs::enable("btn_carga_masiva")
          output$archivo_carga_texto_estructura <- renderText({
            paste0("Archivo con estructura correcta, ", estructure_comparison(), " errores")
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
      # end rm
      
      # begin rm
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
      # end rm
      
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
            fluidRow(column(3, textInput(ns("res_ident_riesgos"), "Identificación de Riesgos", placeholder = "")),
                     column(3, textInput(ns("res_tec_teorica"), "Técnico Teórica", placeholder = "")),
                     column(3, textInput(ns("res_tec_practica"), "Técnico Practica", placeholder = ""))),
            fluidRow(column(3, textInput(ns("res_gestion"), "Gestión", placeholder = "")),
                     column(3, textInput(ns("res_certificacion"), "Certificación", placeholder = "")),
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
      
      # being rm
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
      # end rm
    }
  )
}