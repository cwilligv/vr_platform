# Este modulo despliega solamente el manejo de observaciones
observations_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns("showModal"), "Show Details", class = "btn-primary", disabled = TRUE)
    actionButton(ns("mon_observaciones"), "Observaciones", class = "btn-success", icon = shiny::icon("pencil"))
  )
}

observations_button_server <- function(id, selected_data, dataChangedTrigger, word_pairs_df, obsChangedTrigger){
  moduleServer(
    id,
    function(input, output, session){
      
      # Helper function to safely handle NA or empty values
      safe_value <- function(x) {
        if (is.na(x) || is.null(x)) return("")
        return(x)
      }
      
      # word_pairs_df <- data.frame(
      #   word = c(
      #     "Serendipity",
      #     "Nebulous",
      #     "Ephemeral",
      #     "Mellifluous",
      #     "Pristine",
      #     "Luminous",
      #     "Cascade",
      #     "Whimsical",
      #     "Radiant",
      #     "Ethereal",
      #     "Magical"
      #   ),
      #   description = c(
      #     "unexpected magical chance discovery",
      #     "vague mysterious cloudy thoughts",
      #     "lasting for brief time",
      #     "sweet flowing like honey",
      #     "pure unspoiled natural state",
      #     "bright radiating warm light",
      #     "flowing water falling freely",
      #     "playfully quaint or fanciful",
      #     "glowing with warm light",
      #     "delicate light as air",
      #     "unexpected magical chance discovery"
      #   ),
      #   type = c(
      #     "comments",
      #     "comments",
      #     "comments",
      #     "comments",
      #     "comments",
      #     "comments",
      #     "contact",
      #     "contact",
      #     "contact",
      #     "contact",
      #     "contact"
      #   ),
      #   stringsAsFactors = FALSE
      # )
      
      # word_pairs_df <- reactive({
      #   obsChangedTrigger()
      #   dbExecute(pool, 'SET character set "utf8"')
      #   tbl(pool, "observaciones") %>% 
      #     rename(
      #       description = descripcion,
      #       word = nombre,
      #       type = tipo
      #     ) %>%
      #     collect()
      # })
      
      # Show add comment modal
      observeEvent(input$mon_observaciones, {
        
        print("in module modal")
        data <- selected_data() %>% 
          rename(
            Comments = obs_preparacion,
            ContactComments = obs_contacto,
            ExtraComments = observaciones
          )
        
        if (nrow(data) < 1 | nrow(data) > 1) {
          showModal(
            modalDialog(
              title = "Advertencia",
              paste("Porfavor seleccione una fila." ),
              footer = tagList(
                modalButton("Cerrar")
              ),
              easyClose = TRUE)
          )
        } else {
          
          # existing_comments <- data$Comments[selected_row]
          parsed_values <- parseComments(data[,c('Comments', 'ContactComments', 'ExtraComments')])
          obsChangedTrigger(obsChangedTrigger() + 1)
          showCommentModal(
            parsed_values = parsed_values,
            data
          )
          
          shinyjs::disable("contact_area")
          shinyjs::disable("comments_area")
        }
      })
      
      # Function to generate descriptions for selected words
      getDescriptions <- function(selected_words, type) {
        if (length(selected_words) == 0) return("")
        descriptions <- sapply(selected_words, function(word) {
          desc <- word_pairs_df()$description[word_pairs_df()$word == word & word_pairs_df()$type == type]
          paste0("• ", word, ": ", desc)
        })
        paste(descriptions, collapse = "\n")
      }
      
      # Function to parse existing comments
      parseComments <- function(row_data) {
        comments <- safe_value(row_data$Comments)
        contact_comments <- safe_value(row_data$ContactComments)
        extra_comments <- safe_value(row_data$ExtraComments)
        
        list(
          comments_words = if (comments != "") strsplit(comments, ", ")[[1]] else character(0),
          contact_words = if (contact_comments != "") strsplit(contact_comments, ", ")[[1]] else character(0),
          extra_comments = extra_comments
        )
      }
      
      # Function to create checkbox group with custom layout
      createCheckboxGroup <- function(inputId, choices, selected = NULL) {
        choices_list <- lapply(choices, function(choice) {
          div(
            class = "col-3", # This creates 4 columns
            checkboxInput(paste0(inputId, "_", choice), 
                          label = choice,
                          value = choice %in% selected)
          )
        })
        
        div(class = "row", choices_list)
      }
      
      # Function to show modal with optional pre-filled values
      showCommentModal <- function(parsed_values = NULL, row_data) {
        ns <- session$ns
        if (is.null(parsed_values)) {
          parsed_values <- list(
            comments_words = character(0),
            contact_words = character(0),
            extra_comments = ""
          )
        }
        
        nombre <- row_data$nombres
        apellido <- row_data$apellidos
        rut <- row_data$rut
        
        # Generate initial descriptions
        initial_comments_desc <- getDescriptions(parsed_values$comments_words, "comments")
        initial_contact_desc <- getDescriptions(parsed_values$contact_words, "contact")
        
        showModal(modalDialog(
          # title = "Asistente de Observaciones",
          title = paste0("Observaciones: ", nombre, ' ', apellido, ' (', rut,')'),
          size = "l",
          
          # First section with tabs
          tabsetPanel(
            id = "wordTabs",
            tabPanel("Coordinación",
                     div(
                       # h4("Seleccione frases:"),
                       br(),
                       createCheckboxGroup(
                         ns("contact_words"),
                         choices = word_pairs_df()$word[word_pairs_df()$type == "contact" & word_pairs_df()$active == TRUE],
                         selected = parsed_values$contact_words
                       )
                     )
            ),
            tabPanel("Capacitación",
                     div(
                       # h4("Seleccione frases:"),
                       br(),
                       createCheckboxGroup(
                         ns("comments_words"),
                         choices = word_pairs_df()$word[word_pairs_df()$type == "comments" & word_pairs_df()$active == TRUE],
                         selected = parsed_values$comments_words
                       )
                     )
            )
          ),
          
          # Second section with text areas
          textAreaInput(ns("contact_area"), "Observaciones Coordinación:", 
                        width = "100%", height = "100px", 
                        value = initial_contact_desc, 
                        resize = "none") %>% 
            tagAppendAttributes(readonly = TRUE),
          
          textAreaInput(ns("comments_area"), "Observaciones Capacitación:", 
                        width = "100%", height = "100px",
                        value = initial_comments_desc,
                        resize = "none") %>% 
            tagAppendAttributes(readonly = TRUE),
          
          # Third section for additional comments
          textAreaInput(ns("extra_comments"), "Comentarios Adicionales:",
                        width = "100%", height = "100px",
                        value = parsed_values$extra_comments, 
                        resize = "vertical"),
          
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("save_comments"), "Publicar")
          )
        ))
      }
      
      getSelectedCheckboxes <- function(prefix, choices) {
        selected <- logical(length(choices))
        for (i in seq_along(choices)) {
          selected[i] <- isTRUE(input[[paste0(prefix, "_", choices[i])]])
        }
        choices[selected]
      }
      
      # Update comments text area based on selected words
      observe({
        comments_choices <- word_pairs_df()$word[word_pairs_df()$type == "comments"]
        selected_comments <- getSelectedCheckboxes("comments_words", comments_choices)
        comments_desc <- getDescriptions(selected_comments, "comments")
        updateTextAreaInput(session, "comments_area", value = comments_desc)
      })
      
      # Update contact text area based on selected words
      observe({
        contact_choices <- word_pairs_df()$word[word_pairs_df()$type == "contact"]
        selected_contact <- getSelectedCheckboxes("contact_words", contact_choices)
        contact_desc <- getDescriptions(selected_contact, "contact")
        updateTextAreaInput(session, "contact_area", value = contact_desc)
      })
      
      # Handle saving comments
      observeEvent(input$save_comments, {
        # current_data <- table_data()
        
        # Get selected words
        comments_choices <- word_pairs_df()$word[word_pairs_df()$type == "comments"]
        contact_choices <- word_pairs_df()$word[word_pairs_df()$type == "contact"]
        selected_comments <- getSelectedCheckboxes("comments_words", comments_choices)
        selected_contact <- getSelectedCheckboxes("contact_words", contact_choices)
        
        # Combine all selected descriptions and extra comments
        # all_comments <- c(
        #   if (length(selected_comments) > 0) paste("Comments:", paste(selected_comments, collapse = ", ")),
        #   if (length(selected_contact) > 0) paste("Contact:", paste(selected_contact, collapse = ", ")),
        #   if (input$extra_comments != "") paste("Additional:", input$extra_comments)
        # )
        
        observations <- NA
        contact_comments <- NA
        additional_comments <- NA
        
        if (length(selected_comments) > 0) observations <- paste(selected_comments, collapse = ", ")
        if (length(selected_contact) > 0) contact_comments <- paste(selected_contact, collapse = ", ")
        if (input$extra_comments != "") additional_comments <- input$extra_comments
        # current_data$Comments[selected_row] <- paste(all_comments, collapse = " | ")
        # table_data(current_data)
        # removeModal()
        id_prep <- selected_data()$id_preparacion
        
        sqlq <- glue::glue_sql("UPDATE monitor_preparaciones
                                set 
                                  obs_preparacion = {observations},
                                  observaciones = {additional_comments},
                                  obs_contacto = {contact_comments},
                                  last_change_by = {session$userData$email}
                                WHERE id = {id_prep}", .con = pool)
        print(sqlq)
        
        # TODO: ajustar de acuerdo a este post: https://community.rstudio.com/t/shiny-tests-for-database-transactions/2211/2
        dbExecute(pool, 'SET character set "utf8"')
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
        dbExecute(pool, sqlq)
        dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
        
        showNotification("Observaciones Ingresadas.", type = "message")
        
        dataChangedTrigger(dataChangedTrigger() + 1)
        removeModal()
      })
      
    } # end of function
  ) # end of moduleServer function
  
}