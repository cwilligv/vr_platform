config_seguimiento_ui <- function(id){
  # tabsetPanel(
  #   id = NS(id, "seguimiento_tabs"), 
  #   selected = NULL, 
  #   # vertical = TRUE,
  #   type = "tabs",
  #   tabPanel(
  #     title = "Observaciones",
  #     fluidRow(
  #       column(width = 12, align = "left", br(), 
  #              actionButton(NS(id, "add_obs"), "Agregar"), 
  #              actionButton(NS(id, "edit_obs"), "Editar"),
  #              br(),br(),
  #              column(
  #                width = 8,
  #                div(DT::dataTableOutput(NS(id, "observaciones_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
  #              )
  #       )
  #     )
  #   )
  # )
  fluidRow(
    column(width = 12, align = "left", br(), 
           actionButton(NS(id, "add_obs"), "Agregar"), 
           actionButton(NS(id, "edit_obs"), "Editar"),
           br(),br(),
           column(
             width = 8,
             div(DT::dataTableOutput(NS(id, "observaciones_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px"), style = "font-size:75%")
           )
    )
  )
}

config_seguimiento_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      
      obsChangedTrigger <- reactiveVal(0)
      
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
      
      observaciones_df <- reactive({
        obsChangedTrigger()
        dbExecute(pool, 'SET character set "utf8"')
        tbl(pool, "observaciones") %>% 
          collect()
      })
      
      #*******************************************
      #* USUARIOS
      #* *****************************************
      
      output$observaciones_table <- DT::renderDataTable({
        # obs_tipo_vec = c("capacitacion" = "Capacitación", "coordinacion" = "Coordinación")
        obs_tipo_vec = c("comments" = "Capacitación", "contact" = "Coordinación")
        table <- observaciones_df() %>% 
          mutate(
            tipo = obs_tipo_vec[tipo],
            active = ifelse(active, "Activa", "Inactiva")
          )
        table <- datatable(table,
                           colnames = c("ID", "Condición", "Observación", "Tipo", "Estado"),
                           #filter = "top",
                           rownames = FALSE,
                           escape = FALSE,
                           class = 'cell-border stripe',
                           selection = 'single',
                           options = list(searching = TRUE, lengthChange = TRUE, autoWidth = TRUE, 
                                     # list(
                                     #   targets = 4,  # Index of 'deleted' column (0-based)
                                     #   visible = FALSE
                                     # ),
                                     columnDefs = list(list(targets = 2, visible = FALSE)),
                                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
          # formatStyle(
          #   columns = 1:ncol(table), 
          #   target = "cell", 
          #   color = JS("\"unset\""),
          #   backgroundColor = JS("\"unset\"")
          # ) %>%
          # formatStyle(
          #   'active',
          #   target = 'row',
          #   backgroundColor = styleEqual(c(0), c('gray'))
          # )
      })
      
      ## AGREGAR observación
      
      observeEvent(input$add_obs, {
        ns <- session$ns
        # new_obs_form("submit_nuevo_obs", "Agregar Observación")
        # updateSelectInput(session, "user_empresa", choices = get_empresas(session$userData$rol, session$userData$email))
        showModal(modalDialog(
          title = "Agregar Observación",
          radioButtons(ns("obs_tipo"), "Tipo", choices = c("Coordinación"="contact", "Capacitación"="comments"), inline = TRUE),
          checkboxInput(ns("obs_activo"), "Activo", value = TRUE),
          textInput(ns("obs_name"), "Condición"),
          textAreaInput(ns("obs_description"), "Observación", rows = 5),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("obs_save"), "Guardar")
            # actionButton("add_cancel", "Cancelar")
          ),
          easyClose = TRUE
        ))
      })
      
      observeEvent(input$obs_save, priority = 20,{
        # id_emp <- dbReadTable(pool, "clientes") %>% filter(row_number() == input$clientes_table_rows_selected) %>% select(id_empresa) %>% pull()
        new_obs <- data.frame(
          nombre = input$obs_name,
          descripcion = input$obs_description,
          tipo = input$obs_tipo,
          active = input$obs_activo
        )
        response <- add_new_obs(new_obs)
        showNotification("Observación ingresada.", type = "message")
        # shinyjs::reset("new_user_form")
        
        obsChangedTrigger(obsChangedTrigger() + 1)
        
        removeModal()
        
      })
      
      ## EDITAR OBSERVACIONES
      observeEvent(input$edit_obs, {
        ns <- session$ns
        
        # Check if row is selected
        if (length(input$observaciones_table_rows_selected) == 0) {
          showNotification(
            "Por favor, seleccione una fila.",
            type = "warning"
          )
          return()
        }
        
        # Get selected row data
        selected_row <- observaciones_df()[input$observaciones_table_rows_selected, ]
        
        showModal(modalDialog(
          title = "Editar Observación",
          shinyjs::hidden(textInput(ns("edit_id"), "", value = selected_row$id)),
          radioButtons(ns("edit_tipo"), "Tipo", choices = c("Coordinación"="contact", "Capacitación"="comments"), inline = TRUE, selected = selected_row$tipo),
          checkboxInput(ns("edit_activo"), "Activo", value = selected_row$active),
          textInput(ns("edit_name"), "Condición", value = selected_row$nombre),
          textAreaInput(ns("edit_description"), "Observación", rows = 5, value = selected_row$descripcion),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("edit_save"), "Guardar"),
            # actionButton("edit_cancel", "Cancel")
          )
        ))
      })
      
      observeEvent(input$edit_save, {
        
        # Update selected row
        current_data <- data.frame(
          id = as.integer(input$edit_id),
          nombre = input$edit_name,
          descripcion = input$edit_description,
          tipo = input$edit_tipo,
          active = input$edit_activo
        )
        # Update table
        editar_obs(current_data)
        showNotification("Observación editada.", type = "message")
        # shinyjs::reset("new_user_form")
        
        obsChangedTrigger(obsChangedTrigger() + 1)
        
        removeModal()
      })
      
      ## END EDITAR
      
    }
  )
}