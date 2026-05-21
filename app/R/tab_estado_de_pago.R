estado_de_pago_ui <- function(id) {
  #ns <- NS(id)
  
  tabItem(
    tabName = "tab6",
    h1("Estado de Pago", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        column(
          width = 12,
          # actionButton(NS(id, "gc_agregar"), "Agregar Cliente", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus"))
          actionButton(NS(id, "enviar_x_email"), "Enviar por email", class = "btn-success", icon("plus")),
          actionButton(NS(id, "details"), "Ver Detalles", icon("edit")),
          # actionButton(NS(id, "generar_estado_pago"), "Generar Estado de Pago")),
          bsModal("modalExample", "Participantes Capacitados", NS(id, "details"), size = "large",
                            dataTableOutput(NS(id, "estado_de_pago_detalle")))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          style = "z-index: 10",
          div(dataTableOutput(NS(id, "estado_de_pago_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
        )
      )
    )
  )
}

estado_de_pago_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      #******************************************
      #* DESPLIEGUE DE DATOS EN TABLA
      #* ****************************************
      
      estado_de_pago_df <- reactive({
        
        #make reactive to
        # input$submit
        # input$submit_edit
        # input$copy_button
        # input$delete_button
        # input$btn_carga_masiva
        
        dbReadTable(pool, "estado_de_pago")
        
      })
      
      output$estado_de_pago_table <- DT::renderDataTable({
        
        table <- estado_de_pago_df() %>% select(-id_empresa)
        names(table) <- c("ID", "Fecha de Servicio", "Descripcion","Cantidad","Valor Unitario","IVA","Total")
        table <- datatable(table, 
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE, autoWidth = F, 
                                          columnDefs = list(list(visible=FALSE, targets=c(0))),
                                          extensions="Responsive",
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))
        )
        
      })
      
      #veer detalles del estado de pago
      # observeEvent(input$details, {
      #   
      #   # SQL_df <- dbReadTable(pool, "estado_de_pago_detalle")
      #   
      #   showModal(
      #     if(length(input$estado_de_pago_table_rows_selected) > 1 ){
      #       modalDialog(
      #         title = "Advertencia",
      #         paste("Seleccione una fila." ),easyClose = TRUE)
      #     } else if(length(input$estado_de_pago_table_rows_selected) < 1){
      #       modalDialog(
      #         title = "Advertencia",
      #         paste("Seleccione una fila." ),easyClose = TRUE)
      #     })  
      #   
      #   if(length(input$estado_de_pago_table_selected) == 1 ){
      #     
      #     # showModal(modalDialog(
      #     #   fluidPage(
      #     #     fluidRow(dataTableOutput(session$ns(id, "estado_de_pago_detalle")))
      #     #   ),
      #     #   tags$div(id = session$ns("constraintPlaceholder")),
      #     #   title = "Detalle Estado de Pago",
      #     #   footer = tagList(
      #     #     modalButton("Cerrar")
      #     #     # actionButton(ns(button_id), "Submit")
      #     #   ),
      #     #   easyClose = TRUE
      #     # ))
      #     output$estado_de_pago_detalle <- DT::renderDataTable({
      #       print("en DT detalle")
      #       SQL_df <- dbReadTable(pool, "estado_de_pago")
      #       id <- SQL_df[input$estado_de_pago_table_rows_selected, "id"]
      #       table <- estado_de_pago_detalle_df(id)
      #       names(table) <- c("Rut", "Nombres","Apellidos","Centro de Costo","Cargo","Fecha Solicitud", "Fecha Capacitacion")
      #       table <- datatable(table, 
      #                          rownames = FALSE,
      #                          escape = FALSE,
      #                          class = 'cell-border stripe',
      #                          selection = 'single',
      #                          options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE, autoWidth = F, 
      #                                         extensions="Responsive",
      #                                         language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))
      #       )
      #       
      #     })
      # 
      #   }
      #   
      # })
      
      output$estado_de_pago_detalle <- DT::renderDataTable({
        print("en DT detalle")
        SQL_df <- dbReadTable(pool, "estado_de_pago")
        p_id <- SQL_df[input$estado_de_pago_table_rows_selected, "id"]
        table <- estado_de_pago_detalle_df() %>% filter(id_estado_de_pago == p_id) %>% select(-id_detalle, -id_estado_de_pago)
        names(table) <- c("Rut", "Nombres","Apellidos","Centro de Costo","Cargo","Fecha Solicitud", "Fecha Capacitacion")
        table <- datatable(table,
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searching = FALSE, lengthChange = FALSE, autoWidth = F,
                                          extensions="Responsive",
                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))
        )

      })
      
      # estado_de_pago_detalle <- reactive({
      # 
      #   dbReadTable(pool, "estado_de_pago")
      #   
      # })
      
      estado_de_pago_detalle_df <- reactive({
        table <- dbReadTable(pool, "estado_de_pago_detalle")
      })
      
      #******************************************
      #* ENVIAR ESTADO DE PAGO POR EMAIL
      #* ****************************************
      
      # observeEvent(input$enviar_x_email, {
      #   # SQL_df <- dbReadTable(pool, "estado_de_pago")
      #   email_de_envio <- get_email_estado_pago(session$userData$user()$email)
      #   
      #   Server <- list(smtpServer= "mercconsultora.cl")
      #   
      #   from <- "<info.c3d@mercconsultora.cl>"
      #   to <- email_de_envio
      #   subject <- "Plataforma C3D - Estado de Pago"
      #   body <- "EStimado cliente, adjunto se encuentra el estado de pago del mes de Mayo 2023"
      #   attachmentPath <-"C://LocalData//cw102//Personal//MERC//capacitacion3d//Estado de Pago//two_columns.pdf"
      #   attachmentObject <-mime_part(x=attachmentPath,name="Estado de Pago.pdf")
      #   bodyWithAttachment <- list(body,attachmentObject)
      #   sendmail(from,to,subject,bodyWithAttachment,
      #            engine = "curl",
      #            engineopts = list(username = "info.c3d@mercconsultora.cl", password = "=9Si##([ACG2Lyo"),
      #            control=list(smtpServer= "mercconsultora.cl"))
      #   showNotification("Estado de Pago enviado.", type = "message")
      #   
      # })
    }
  )
}