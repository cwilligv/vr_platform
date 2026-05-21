pagos_ui <- function(id) {
  #ns <- NS(id)
  
  tabItem(
    tabName = "tab9_pagos",
    h1("Estados de Pago", style = "font-size: 1.8rem;"),
    shinyalert::useShinyalert(),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 12, 
          align = "center", 
          # div(htmlOutput(NS(id, "frame")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"))
          tabsetPanel(
            id = NS(id, "pagosTabset"),
            type = "tabs",
            tabPanel(
              title = "Prefacturas",
              br(),
              fluidRow(
                column(
                  width = 4,
                  uiOutput(NS(id, "selector_clientes"))
                ),
                column(
                  width = 4,
                  uiOutput(NS(id, "btn_estado_de_pago"), style = "margin-top: 27px;")
                ),
                column(
                  width = 4
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = "center",
                  style = "z-index: 10",
                  div(DT::dataTableOutput(NS(id, "prefacturas_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
                )
              )
            )
            # tabPanel(
            #   title = "Facturas",
            #   div(htmlOutput(NS(id, "frame")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"))
            # )
          )
        )
      )
    )
  )
}

pagos_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      #Carga los datos de estados de pago (view) desde la base de datos  
      prefacturas_df <- reactive({
        
        #make reactive to
        dataChangedTrigger()
        rv$tab_pagos
        
        updateSelectInput(session, "listado_empresas", selected = as.numeric(session$userData$id_empresa))
        # Filtro WHERE
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("1 = 1", .con = pool)
        } else {
          filtros <- glue::glue_sql("id_empresa = {as.numeric(session$userData$id_empresa)} and estado_id in (3, 4)", .con = pool)
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.prefacturas_view where ({filtros})", .con = pool)
        dbGetQuery(pool, tbl)
        
      })
      
      output$frame <- renderUI({
        tags$iframe(
          seamless = "seamless",
          src = link_factura(),
          width = "100%",
          style="height: 100vh;"
        )
      })
      
      link_factura <- reactive({
        rv$cambio_empresa
        print(paste0("getting factura for empresa: ", session$userData$id_empresa))
        url <- get_link_factura(as.numeric(session$userData$id_empresa))
        #"https://www.duemint.com/portal/2392702/p62ded315d86bf"
        url
      })
      
      output$selector_clientes <- renderUI({
        ns <- session$ns
        tagList(
          selectInput(ns("listado_empresas"), "Empresa", choices = get_empresas(session$userData$rol, session$userData$email), selected = as.numeric(session$userData$id_empresa))
        )
      })
      
      output$btn_estado_de_pago <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            actionButton(ns("generar_prefactura"), "Generar EP", class = "btn-success", icon = shiny::icon("cog")),
            actionButton(ns("editar_prefactura"), "Editar EP", class = "btn-success", icon = shiny::icon("edit"))
            # actionButton(ns("enviar_prefactura"), "Enviar EP", class = "btn-success", icon = shiny::icon("envelope"))
          )
        }
      })
      
      observeEvent(input$listado_empresas, {
        session$userData$id_empresa <- input$listado_empresas
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      output$prefacturas_table <- DT::renderDataTable({
        ns <- session$ns
        table <- prefacturas_df() %>% 
          # select(-id_empresa, -rut_cliente, -razon_social, -valor_unitario) %>% 
          select(fecha_servicio, nombre_fantasia, cantidad, total, estado) %>%
          mutate(index = row_number(),
                 total = if_else(is.na(total), NA, paste0("$", formatC(as.numeric(total), format="f", digits=0, decimal.mark = ",", big.mark = "."))),
                 detalles = glue('<a id="custom_btn" onclick="Shiny.setInputValue(\'',ns('boton_detalles'),'\', \'{index}\', {{priority: \'event\'}})"><span class="glyphicon glyphicon-list-alt" style = "font-size: 24px;color: black;"></span></a>')) %>%
          relocate(index)
        
        names(table) <- c("n", "Fecha Servicio", "Empresa", "Cantidad", "Total", "Estado", "Detalles")
        
        table <- datatable(table,
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searchHighlight = T, searching = T, lengthChange = F, scrollX = T, autoWidth = F, ordering = F,
                                          columnDefs = list(list(targets = 0:6, search = FALSE),
                                                            list(targets = c(0), visible = FALSE),
                                                            list(className = 'dt-center', targets = "_all")),
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
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
                           callback = JS(paste0("var tips = ['Index', 'Fecha de Servicio', 'Empresa', 'Cantidad', 'Total', 'Estado OC', 'Detalles de Estado de Pago'],
                                            firstRow = $('#",session$ns('prefacturas_table')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
        )
        
      })
      
      # popup detalle prefactura
      observeEvent(input$boton_detalles, {
        # ns <- session$ns
        obs <- prefacturas_df() %>% 
          mutate(index = row_number()) %>% 
          filter(index == as.numeric(input$boton_detalles)) %>% 
          select(id_empresa, razon_social, nombre_fantasia, rut_cliente, valor_unitario, valor_unitario_uf, fecha_uf, fecha_servicio, cantidad, total, moneda, email_enviados) %>% 
          mutate(
            # valor_unitario = if_else(is.na(valor_unitario), '', paste0("$", formatC(as.numeric(valor_unitario), format="f", digits=0, decimal.mark = ",", big.mark = "."))),
            valor_unitario_uf = paste0(formatC(as.numeric(valor_unitario_uf), format="f", digits=ifelse(moneda == 'UF', 1, 0), decimal.mark = ",", big.mark = "."), if_else(moneda == 'UF',paste0(" UF al ", format(as.Date(fecha_uf), "%d-%m-%y")),' CLP')),
            total = if_else(is.na(total), '', paste0("$", formatC(as.numeric(total), format="f", digits=0, decimal.mark = ",", big.mark = ".")))
          )
        print(paste0("id clicked: ", as.numeric(input$boton_detalles)))
        print(obs)
        
        prefactura_detalle_params$id_empresa <- obs$id_empresa
        prefactura_detalle_params$nombre_fantasia <- obs$nombre_fantasia
        prefactura_detalle_params$mes <- obs$fecha_servicio
        prefactura_detalle_params$destinatarios <- get_info_ep(obs$id_empresa)$email_para_envios
        prefactura_detalle_params$destinatarios_cc <- get_info_ep(obs$id_empresa)$email_para_envios_cc
        prefactura_detalle_params$email_count <- get_num_ep_emails(obs$id_empresa, obs$fecha_servicio)
        
        detalle_prefactura_form()
        
        if (is.na(prefactura_detalle_params$destinatarios)) {
          shinyjs::disable("send_email_btn")
        }
        
        # removing as now the label of the button is updated within the renderUI function
        # updateActionButton(session, "send_email_btn", label = paste0("Enviar EDP (",prefactura_detalle_params$email_count,")"))
        
        # output$detalles_subtitulo_empresa <- renderText({paste0("Empresa: ", obs$nombre_fantasia)})
        # output$detalles_subtitulo_fecha_servicio <- renderText({paste0("Fecha Servicio: ", obs$fecha_servicio)})
        
        tbl1 <- data.frame(
          titulo = c("Empresa:", "Rut:","Fecha Servicio:","Descripcion:",paste0("Valor Unitario (", obs$moneda,"):"),"Cantidad:","Total (Exento):", "Condiciones:"),
          valor = c(obs$razon_social, obs$rut_cliente, obs$fecha_servicio, "Capacitación 3D", obs$valor_unitario_uf, obs$cantidad, obs$total, "Saldo a pagar en 30 días (Servicio Exento de IVA).")
        )
        
        prefactura_detalle_params$resumen <- tbl1 
        
        gt_tbl1 <- gt(tbl1) %>% 
          opt_table_lines("all") %>%
          tab_header(
            title = md("**Resumen Servicio**")
          ) %>%
          tab_options(
            column_labels.hidden = TRUE,
            heading.background.color = "#0068C2",
            # table.border.top.style = "hidden",
            # table.border.right.style = "hidden",
            # table.border.left.style = "hidden",
            table.width = pct(100),
            data_row.padding = px(4),
            table.border.right.style = "solid",
            table.border.right.width = px(2),
            table.border.right.color = "gray",
            table.border.left.style = "solid",
            table.border.left.width = px(2),
            table.border.left.color = "gray",
            table.border.top.style = "solid",
            table.border.top.width = px(2),
            table.border.top.color = "gray"
          ) %>% 
          cols_width(
            titulo ~ pct(30),
            valor ~ pct(70)
          ) %>% 
          tab_style(
            style = list(
              cell_fill(color = "gray"),
              cell_text(weight = "bold", size = "x-smaller")
            ),
            locations = cells_body(
              columns = titulo
            )
          ) %>% 
          tab_style(
            style = list(
              cell_text(size = "x-smaller")
            ),
            locations = cells_body(
              columns = valor
            )
          )
          # tab_source_note(
          #   md("Servicio Exento de IVA")
          # )
        
        output$tbl_resumen_servicio <- render_gt(expr = gt_tbl1)

        output$detalles_prefactura <- renderDataTable({
          dbExecute(pool, 'SET character set "utf8"')
          table <- tbl(pool, "prefactura_details_view") %>% 
            filter(id_empresa == !!obs$id_empresa & fecha_servicio == !!obs$fecha_servicio) %>% 
            select(-id_empresa, -fecha_servicio, -nombre_fantasia) %>% 
            collect() %>% 
            mutate(
              participante = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
              cargo = str_to_title(cargo),
              valor_unitario = if_else(is.na(valor_unitario), '', paste0("$", formatC(as.numeric(valor_unitario), format="f", digits=0, decimal.mark = ",", big.mark = "."))),
              index = row_number()
            ) 
            # mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "utf-8"))})
          
          prefactura_detalle_params$detalles <- table
          
          table <- table %>% 
            select(index, rut, participante, cargo, centro_de_costo, fecha_solicitud, fecha_preparacion, valor_unitario)
            
          
          names(table) <- c('n', 'Rut', 'Participante', 'Cargo', 'Contrato', 'Fecha Solicitud', 'Fecha Capacitación', 'Valor')
          
          table <- datatable(table,
                             #filter = "top",
                             rownames = FALSE,
                             escape = FALSE,
                             class = 'cell-border stripe',
                             selection = 'none',
                             options = list(searchHighlight = F, searching = F, scrollX = T, autoWidth = F, ordering = F, 
                                            dom = 'lfrtip',
                                            lengthChange = T,
                                            # pageLength = 25,
                                            # lengthMenu = c(5, 10, 15, 20),
                                            columnDefs = list(list(targets = 0:4, search = FALSE),
                                                              list(className = 'dt-center', targets = "_all")),
                                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
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
                             callback = JS(paste0("var tips = ['Index','Rut', 'Nombre Participante', 'Cargo', 'Contrato/Proyecto', 'Fecha de Solicitud', 'Fecha de Capacitación', 'Valor Unitario'],
                                            firstRow = $('#",session$ns('detalles_prefactura')," thead tr th');
                                            for (var i = 0; i < tips.length; i++) {
                                              $(firstRow[i]).attr('title', tips[i]);
                                            }"))
          )
        })
        
      })
      
      detalle_prefactura_form <- function(){
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(style = "margin-top:-20px",
              column(
                9,
                gt_output(ns("tbl_resumen_servicio"))
              ),
              column(
                3,
                div(
                  div(
                    style = "margin-bottom: 15px;",
                    # actionButton(ns("send_email_btn"),
                    #              label = paste0("Enviar EDP"),
                    #              # class = "btn-info",
                    #              style = "width: 100%; background-color: #0079b5; color: white"),
                    uiOutput(ns("send_email_btn_placeholder")),
                  ),
                  downloadButton(ns("ep_download_excel"), 
                                 label = tagList(
                                   tags$img(src = "images/excel_icon.png", width = "30", height = "30"),
                                   tags$span("Descargar Estado de Pago")
                                 ),
                                 class = "btn-success"
                  ),
                  tags$script(HTML(paste0("$('#",ns("ep_download_excel"),"').css('padding-left',2).find('>i').remove();"))),
                  style = paste0("padding-left: 20px; padding-top: ",if(session$userData$rol %in% c('admin', 'administrativo')) "170px" else "210px", ";")
                  
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(DT::dataTableOutput(ns("detalles_prefactura")), style = "font-size:75%")
              )
            )
            # textOutput(ns("detalles_subtitulo_empresa")),
            # textOutput(ns("detalles_subtitulo_fecha_servicio")),
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = tags$span("Estado de Pago", tags$img(src = "images/merc_720.png", width = "15%", height = "15%", style = "vertical-align: middle; float: right;")),
          footer = tagList(
            actionButton(ns("close_obs"), "Cerrar")
          ),
          easyClose = TRUE, size = "l"
        ))
      }
      
      output$send_email_btn_placeholder <- renderUI({
        # Dynamically determine the button label
        button_label <- if (prefactura_detalle_params$email_count > 0) {
          paste0("Reenviar EDP (", prefactura_detalle_params$email_count, ")")
        } else {
          "Enviar EDP"
        }
        
        # Append the count to the label for debugging/info, similar to your previous approach
        # final_label <- paste0(button_label, " (", prefactura_detalle_params$email_count, ")")
        
        if(session$userData$rol %in% c('admin', 'administrativo')) {
          tagList(
            actionButton(NS(id, "send_email_btn"),
                         # label = paste0("Enviar EDP"),
                         label = button_label,
                         # class = "btn-info",
                         style = "width: 100%; background-color: #0079b5; color: white")
          )
        }
      })
      
      observeEvent(input$close_obs, {
        removeModal()
        shinyjs::reset("detalle_prefactura_form")
        session$sendCustomMessage(type = 'resetInputValue', message =  "boton_detalles")
      })
      
      observeEvent(input$send_email_btn, {
        shinyjs::disable("send_email_btn")
        
        # Determine email subject and body based on the current count.
        current_count <- prefactura_detalle_params$email_count
        
        if (current_count == 0) {
          # First email (original email)
          email_asunto <- paste0("Prefactura - Servicio Capacitación 3D (", prefactura_detalle_params$mes, ")")
          # Assuming the first email body is handled inside envio_email_pagos
          # or is the default if no body is passed.
          email_body <- NULL 
        } else {
          # Second email (reminder) or subsequent emails
          month_year <- prefactura_detalle_params$mes
          email_asunto <- paste0("Recordatorio Prefactura - Servicio Capacitación 3D (", month_year, ")")
          email_body <- "Junto con saludar, recordarles que se encuentra <b>Pendiente Orden de Compra</b> correspondiente al <b>Estado de Pago</b> enviado anteriormente, según detalle en documento adjunto y resumen a continuación:"
        }
        
        withProgress(
          message = "Enviado estado de pago a cliente...",
          detail = "Esto toma unos minutos...", value = 0, {
            setProgress(0.3, message = "Recolectando datos")
            Sys.sleep(1)
            setProgress(0.6, message = "Generando archivo")
            filename = paste0("EDP_Capacitación3D_", prefactura_detalle_params$nombre_fantasia, "_",prefactura_detalle_params$mes,".xlsx")
            table <- prefactura_detalle_params$detalles %>% 
              select(index, rut, nombres, apellidos, cargo, centro_de_costo, -fecha_solicitud, fecha_preparacion, valor_unitario) %>% 
              mutate(
                nombres = str_to_title(nombres),
                apellidos = str_to_title(apellidos),
                cargo = str_to_title(cargo),
                centro_de_costo = str_to_title(centro_de_costo)
              ) %>% 
              rename(n = index, 
                     Rut = rut, 
                     Nombres = nombres,
                     Apellidos = apellidos,
                     Cargo = cargo, 
                     Contrato = centro_de_costo, 
                     # `Fecha Solicitud` = fecha_solicitud,
                     `Fecha Capacitacion` = fecha_preparacion, 
                     Valor = valor_unitario
              )
            
            excel <- generar_edp_excel(table, filename, prefactura_detalle_params$resumen)
            if (!excel) {
              setProgress(0.8, message = "Error generando excel")
            }
            dest <- prefactura_detalle_params$destinatarios
            dest_cc <- prefactura_detalle_params$destinatarios_cc
            
            setProgress(0.8, message = "Enviando email")
            
            out <- envio_email_pagos(
              ep = prefactura_detalle_params$resumen,
              dest = dest,
              emails_cc = dest_cc, # "administracion@mercconsultora.cl",
              filename = filename,
              # asunto = paste0("Prefactura - Servicio Capacitación 3D (", prefactura_detalle_params$mes, ")"),
              asunto = email_asunto,
              body = email_body
            )
            if (out != 250) {
              setProgress(1, message = "Error al enviar email")
            } else {
              setProgress(1, message = "EP enviado")
              unlink(filename)
              
              email_ep_enviado(prefactura_detalle_params$id_empresa, prefactura_detalle_params$mes, prefactura_detalle_params$email_count)
              prefactura_detalle_params$email_count <- prefactura_detalle_params$email_count+1
              # updateActionButton(session, "send_email_btn", label = paste0("Send Email (",prefactura_detalle_params$email_count,")"))
              shinyjs::enable("send_email_btn")
              Sys.sleep(1)
            }
          }
        )
      })
      
      prefactura_detalle_params <- reactiveValues(
        id_empresa = 0,
        nombre_fantasia = NULL,
        mes = NULL,
        resumen = NULL,
        detalles = NULL,
        email_count = 0,
        destinatarios = NULL,
        destinatarios_cc = NULL
      )
      
      ## Generacion Excel detalle estado de pago ##
      #############################################
      
      output$ep_download_excel <- downloadHandler(
        # TODO: nombre archivo debe llevar fecha de descarga
        # filename = function(){paste0("estado_pago_merc_(",prefactura_detalle_params$mes,").xlsx")},
        filename = function(){paste0("EDP_Capacitación3D_", prefactura_detalle_params$nombre_fantasia, "_(",prefactura_detalle_params$mes,").xlsx")},
        content = function(fname){
          table <- prefactura_detalle_params$detalles %>% 
            select(index, rut, nombres, apellidos, cargo, centro_de_costo, -fecha_solicitud, fecha_preparacion, valor_unitario) %>% 
            mutate(
              nombres = str_to_title(nombres),
              apellidos = str_to_title(apellidos),
              cargo = str_to_title(cargo),
              centro_de_costo = str_to_title(centro_de_costo)
            ) %>% 
            rename(n = index, 
                   Rut = rut, 
                   Nombres = nombres,
                   Apellidos = apellidos,
                   Cargo = cargo, 
                   Contrato = centro_de_costo, 
                   # `Fecha Solicitud` = fecha_solicitud,
                   `Fecha Capacitacion` = fecha_preparacion, 
                   Valor = valor_unitario
            )
          
          # openxlsx::write.xlsx(table, fname)
          ## Create a new workbook
          # wb <- openxlsx::createWorkbook("MERC")
          # 
          # ## Add a worksheets
          # openxlsx::addWorksheet(wb, "Estado de Pago")
          # 
          # ## Write title
          # openxlsx::writeData(wb, sheet = 1, "ESTADO DE PAGO", startCol = 1, startRow = 1)
          # 
          # # Merge cells from A1 to H3
          # openxlsx::mergeCells(wb, sheet = 1, rows = 1:3, cols = 1:9)
          # 
          # # Create a style for the merged cells
          # title_style <- createStyle(
          #   fontName = "Calibri",
          #   fontSize = 20,
          #   fontColour = "white",
          #   halign = "center",
          #   valign = "center",
          #   textDecoration = "bold",
          #   fgFill = "#4F81BD"
          # )
          # 
          # # Apply the style to the merged cell range
          # openxlsx::addStyle(wb, sheet = 1, style = title_style, rows = 1:3, cols = 1:9)
          # 
          # ## write data to worksheet 1
          # openxlsx::writeData(wb, sheet = 1, table, startCol = 1, startRow = 5)
          # 
          # ## create and add a style to the column headers
          # headerStyle1 <- openxlsx::createStyle(
          #   fontSize = 11, fontColour = "#FFFFFF",
          #   fgFill = "#4F81BD", halign = "center",
          #   textDecoration = "bold", valign = "center"
          # )
          # openxlsx::addStyle(wb, sheet = 1, headerStyle1, rows = 5, cols = 1:9, gridExpand = TRUE)
          # 
          # ## set row heights
          # openxlsx::setRowHeights(wb, sheet = 1, rows = 5, heights = 30)
          # 
          # ## set auto column width
          # openxlsx::setColWidths(wb, sheet = 1, cols = 1:9, widths = "auto")
          # 
          # ## save
          # openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
          
          generar_edp_excel(table, fname, prefactura_detalle_params$resumen)
        }
      )
      
      ## Generacion documento PDF estado de pago ##
      ############################################
      
      detalle_prefactura <- reactive({
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql("select * from {`db`}.prefactura_details_view where id_empresa = {prefactura_detalle_params$id_empresa} and fecha_servicio = {prefactura_detalle_params$mes}", .con = pool)
        table <- dbGetQuery(pool, tbl) %>% 
          mutate(
                nombres = str_to_title(nombres),
                apellidos = str_to_title(apellidos),
                index = row_number()
              ) %>%
          select(index, rut, nombres, apellidos, cargo, centro_de_costo, fecha_solicitud, fecha_preparacion)
        # table <- tbl(pool, "prefactura_details_view") %>% 
        #   filter(id_empresa == !!prefactura_detalle_params$id_empresa & fecha_servicio == !!prefactura_detalle_params$fecha_servicio) %>% 
        #   select(-id_empresa, - fecha_servicio, -nombre_fantasia) %>% 
        #   collect() %>% 
        #   mutate(
        #     nombres = paste0("<strong>", str_to_title(nombres), "</strong>", "<br>", "<i>", str_to_title(apellidos), "</i>"),
        #     index = row_number()
        #   ) %>% 
        #   select(-apellidos) %>% 
        #   relocate(index)
        print(head(table))
        names(table) <- c('n', 'Rut', 'Participante', 'Cargo', 'Contrato/Proyecto', 'Fecha Solicitud', 'Fecha Capacitación')
        table
      })
      
      output$downloadReport <- downloadHandler(
        filename = function() {
          paste('estado_de_pago', sep = '.', 'pdf')
        },
        
        content = function(file) {
          withProgress(
            message = "Descarga en proceso...",
            detail = "Esto toma unos minutos...", value = 0, {
              setProgress(0.3, message = "Recolectando datos")
              # src <- normalizePath('templates\\estado_de_pago.Rmd')
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              # owd <- setwd(tempdir())
              # on.exit(setwd(owd))
              # file.copy(src, 'estado_de_pago.Rmd', overwrite = TRUE)
              Sys.sleep(2)
              setProgress(0.6, message = "Generando PDF")
              # out <- rmarkdown::render(
              #   'estado_de_pago.Rmd',
              #   params = list(
              #     # mes_servicio = prefactura_detalle_params$mes, 
              #     data_source = prefactura_detalle_params$detalles,
              #     resumen = prefactura_detalle_params$resumen
              #   )
              # )
              out <- chrome_print(
                rmarkdown::render(paste0(getwd(),"/templates/estado_de_pago.Rmd"), 
                                  params = list(
                                    # mes_servicio = "Septiembre", 
                                    data_source = prefactura_detalle_params$detalles,
                                    resumen = prefactura_detalle_params$resumen
                                  )
                ), 
                output = "estado_de_pago.pdf",
                extra_args = c("--no-sandbox")
              )
              setProgress(0.8, message = "Generando PDF")
              
              file.rename(out, file)
              setProgress(1, message = "Listo PDF")
            }
          )
        }
      )
      
      ## Botones de estado de pago ##
      ###############################
      # modal_rv <- reactiveVal(0)
      
      ## Generar Prefactura
      observeEvent(input$generar_prefactura, {
        ns <- session$ns
        showModal(modalDialog(
          fluidPage(
            fluidRow(
              selectInput(ns("empresa_seleccionada"), "Clientes", choices = c('Seleccionar cliente'=-1, get_empresas(session$userData$rol, session$userData$email)), selected = " "),
              selectInput(ns("mes_seleccionado"), "Periodo de Servicio", choices = format(rev(seq(ymd('2020-01-01'),ymd(today(tzone = "Chile/Continental")), by = 'months')),'%m-%Y')),
              layout_column_wrap(
                width = 1/2,
                textInput(ns("valor_unitario"), "Valor Unitario", value = ""),
                div(radioButtons(ns("valor_unidad"), label = "Moneda", choices = c("UF" = 1, "CLP" = 0), selected = 1, inline = TRUE), style = "margin-top: 30px;")
              ),
              checkboxInput(ns("usar_valor_unitario"), "Usar otro valor unitario", value = FALSE),
              layout_column_wrap(
                width = 1/2,
                dateInput(ns("fecha_uf"), label = "Fecha UF", value = NA),
                textInput(ns("valor_uf"), label = "Valor UF", value = 0)
              ),
              shinyjs::disabled(checkboxInput(ns("usar_valor_uf"), "Usar otro valor UF", value = FALSE))
              # conditionalPanel(
              #   condition = paste0("input.",ns("valor_unidad"), " == '0'"),
              #   shinyjs::disabled(checkboxInput(ns("usar_valor_uf"), "Usar otro valor UF", value = FALSE))
              # ),
              # conditionalPanel(
              #   condition = paste0("input.",ns("valor_unidad"), " == '1'"),
              #   checkboxInput(ns("usar_valor_uf"), "Usar otro valor UF", value = FALSE)
              # )
            )
          ),
          tags$div(id = session$ns("constraintPlaceholder2")),
          title = "Generar Estado de pago",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("generar_btn"), "Generar")
          ),
          easyClose = TRUE, size = "s"
        ))
        
        shinyjs::disable("generar_btn")
        shinyjs::disable("valor_unitario")
        shinyjs::disable("valor_unidad")
        shinyjs::disable("valor_uf")
        shinyjs::disable("fecha_uf")

      })
      
      observeEvent(input$empresa_seleccionada, {
        tarifa <- get_valores_unitarios(as.numeric(input$empresa_seleccionada))
        updateTextInput(session, "valor_unitario", value = as.character(sub(".",",", tarifa$tarifa_normal, fixed = T)))
        updateRadioButtons(session, inputId = "valor_unidad", selected = tarifa$unidad_UF)
        # updateSwitchInput(session, "valor_unidad", value = tarifa$unidad_UF)
        print(paste0("Es TRUE?: ", tarifa$unidad_UF))

        if (input$valor_unidad == 1) {
          shinyjs::enable("usar_valor_uf")
        } else {
          shinyjs::disable("usar_valor_uf")
        }

        if (as.numeric(input$empresa_seleccionada) >= 0) {
          shinyjs::enable("generar_btn")
        }else{
          shinyjs::disable("generar_btn")
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input$mes_seleccionado, {
        print(paste0("Mes seleccionado: ", lubridate::my(input$mes_seleccionado)))
        
        withProgress(
          message = "Carga UF desde SII",
          value = 0, {
            incProgress(1/3, message = "Calculando ultimo dia de periodo")
            ultimo_dia_periodo <- lubridate::rollforward(lubridate::my(input$mes_seleccionado))
            
            incProgress(2/3, message = "Extrayendo valores")
            valor_uf <- get_uf_periodo(lubridate::my(input$mes_seleccionado), fin_de_mes = T)
            
            incProgress(3/3, message = "Actualizando valores")
            updateDateInput(session, "fecha_uf", value = ultimo_dia_periodo)
            updateTextInput(session, "valor_uf", value = valor_uf)
          }
        )
      }, ignoreInit = TRUE)
      
      # observeEvent(input$fecha_uf, {
      #   updateTextInput(session, "valor_uf", value = get_uf_fin_periodo(lubridate::ymd(input$fecha_uf)))
      # }, ignoreInit = TRUE)
      
      observeEvent(input$usar_valor_unitario, {
        if(input$usar_valor_unitario){
          shinyjs::enable(id = "valor_unitario")
          # shinyjs::enable(id = "valor_unidad")
          shinyjs::disable(id = "usar_valor_uf")
        } else {
          shinyjs::disable(id = "valor_unitario")
          # shinyjs::disable(id = "valor_unidad")
          shinyjs::enable(id = "usar_valor_uf")
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input$usar_valor_uf, {
        if(input$usar_valor_uf){
          shinyjs::enable(id = "fecha_uf")
        } else {
          shinyjs::disable(id = "fecha_uf")
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input$fecha_uf, {
        if(input$usar_valor_uf){
          print("Fecha seleccionada")
          print(input$fecha_uf)
          
          withProgress(
            message = "Carga UF desde SII",
            value = 0, {
              incProgress(1/2, message = "Trayendo valores")
              valor_uf <- get_uf_periodo(lubridate::ymd(input$fecha_uf), fin_de_mes = F)
              
              incProgress(2/2, message = "Actualizando valores")
              updateTextInput(session, "valor_uf", value = valor_uf)
            }
          )
        }
      })
      
      observeEvent(input$generar_btn, {
        withProgress(
          message = "Generando Estado de Pago...",
          detail = "Iniciando procedimiento...", value = 0,{
            
            if (input$usar_valor_unitario) {
              # nuevo_valor_unitario <- as.numeric(input$valor_unitario)
              # nuevo_valor_unitario <- as.numeric(sub(",", ".", input$valor_unitario, fixed = TRUE))
              nuevo_valor_unitario <- convert_spanish_number(input$valor_unitario)
            } else {
              nuevo_valor_unitario <- NA
            }
            valor_cambio <- as.numeric(input$valor_uf)
            if (input$usar_valor_unitario && !as.numeric(input$valor_unidad)) {
              valor_cambio <- 1
            }
            resultado <- generar_estado_de_pago(input$empresa_seleccionada, input$mes_seleccionado, now(tzone = "Chile/Continental"), input$usar_valor_unitario, nuevo_valor_unitario, valor_cambio, input$fecha_uf, session$userData$email)
            setProgress(0.4, message = "Contando participantes")
            Sys.sleep(1)
            m1 <- "Guardando datos"
            m2 <- "Estado de Pago generado"
            if (resultado == 0) {
              m1 <- "No existen datos para generar EP"
              m2 <- "No existen datos para generar EP"
              shinyalert::shinyalert(
                title = "No es posible generar EP",
                text = "No se realizaron capacitaciones en este periodo",
                type = "warning"
              )
            }
            setProgress(0.8, message = m1)
            Sys.sleep(1)
            setProgress(1, message = m2)
          }
        )
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
      })
      
      
      ## Editar estados de Prefactura
      observeEvent(input$editar_prefactura, {
        SQL_df <- prefacturas_df()
        
        showModal(
          if(length(input$prefacturas_table_rows_selected) > 1 ){
            modalDialog(
              title = "Advertencia",
              paste("Seleccione una fila." ),easyClose = TRUE)
          } else if(length(input$prefacturas_table_rows_selected) < 1){
            modalDialog(
              title = "Advertencia",
              paste("Seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          }) 
        
        if(length(input$prefacturas_table_rows_selected) == 1 ){
          ns <- session$ns
          showModal(modalDialog(
            fluidPage(
              fluidRow(
                hidden(textInput(ns("ep_id"),"")),
                textInput(ns("fecha_servicio_ep"), "Fecha de Servicio"),
                textInput(ns("empresa_ep"), "Empresa")
              ),
              fluidRow(
                column(5, textInput(ns("cantidad_ep"), "Cantidad", value = 0)),
                column(5, textInput(ns("total_ep"), "Total", value = 0))
              ),
              fluidRow(
                textInput(ns("valor_unitario_ep"), "Valor unitario")
              ),
              fluidRow(
                # column(5, selectInput(ns("estado_oc"), "Estado OC", choices = NULL)),
                column(5, selectInput(ns("estado_pago_factura"), "Estado Facturación", choices = NULL))
              )
            ),
            tags$div(id = session$ns("constraintPlaceholder2")),
            title = "Editar Estado",
            footer = tagList(
              modalButton("Cancelar"),
              actionButton(ns("guardar_ep_btn"), "Guardar")
            ),
            easyClose = TRUE, size = "m"
          ))
          
          updateTextInput(session, "ep_id", value = SQL_df[input$prefacturas_table_rows_selected, "id"])
          updateTextInput(session, "fecha_servicio_ep", value = SQL_df[input$prefacturas_table_rows_selected, "fecha_servicio"])
          updateTextInput(session, "empresa_ep", value = SQL_df[input$prefacturas_table_rows_selected, "nombre_fantasia"])
          updateTextInput(session, "cantidad_ep", value = SQL_df[input$prefacturas_table_rows_selected, "cantidad"])
          updateTextInput(session, "total_ep", value = SQL_df[input$prefacturas_table_rows_selected, "total"])
          updateTextInput(session, "valor_unitario_ep", value = paste0("$", formatC(as.numeric(SQL_df[input$prefacturas_table_rows_selected, "valor_unitario"]), format="f", digits=0, decimal.mark = ",", big.mark = ".")))
          # updateSelectInput(session, "estado_oc", choices = get_estados('oc'), selected = SQL_df[input$prefacturas_table_rows_selected, "oc_id"])
          updateSelectInput(session, "estado_pago_factura", choices = get_estados('factura'), selected = SQL_df[input$prefacturas_table_rows_selected, "estado_id"])
          
          shinyjs::disable("fecha_servicio_ep")
          shinyjs::disable("empresa_ep")
          shinyjs::disable("cantidad_ep")
          shinyjs::disable("total_ep")
          shinyjs::disable("valor_unitario_ep")
          
        }
      })
      
      observeEvent(input$guardar_ep_btn, {
        withProgress(
          message = "Guardando cambios en Estado de Pago...",
          detail = "Iniciando procedimiento...", value = 0,{
            guardar_estados_ep(input$ep_id, input$estado_oc, input$estado_pago_factura)
            setProgress(0.4, message = "Guardando estado oc")
            Sys.sleep(1)
            setProgress(0.8, message = "Guardando estado factura")
            Sys.sleep(1)
            setProgress(1, message = "Cambios guardados")
          }
        )
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
      })
      
      
    }
  )
}