certificados_ui <- function(id){
  tabItem(
    tabName = "tab_certificados",
    h1("Certificados Participación", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      collapsible = F,
      headerBorder = F,
      fluidRow(
        column(
          width = 8,
          p(style = "text-align: justify;",
            "A continuación los certificados de participación a Capacitación de Modelamiento en Competencias para la minería, 
            orientada al desarrollo y nivelación de brechas del modelo de competencias laborales 3D. Estos certificados tienen 
            como propósito constatar la asistencia de los participantes a la instancia formativa y pueden ser presentados a minería 
            mandante como evidencia del compromiso de la empresa."
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
          div(
            DT::dataTableOutput(NS(id, "monitor_table")) %>%
              shinycssloaders::withSpinner(type = 8, proxy.height = "300px"),
            style = "font-size:75%"
          )
        )
      ),
      fluidRow(uiOutput(NS(id, "modal")))
    )
  )
}

certificados_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      dataChangedTrigger <- reactiveVal(0)
      obsChangedTrigger  <- reactiveVal(0)
      
      # ── UI: company selector (admin only) ──────────────────────────────────
      output$admin_selector <- renderUI({
        ns <- session$ns
        if (session$userData$rol %in% c('admin')) {
          tagList(
            div(
              selectInput(
                ns("listado_empresas"), "Clientes",
                choices  = get_empresas(session$userData$rol, session$userData$email),
                selected = as.numeric(session$userData$id_empresa)
              ),
              style = "margin-top:-30px"
            )
          )
        }
      })
      
      observeEvent(input$listado_empresas, {
        session$userData$id_empresa <- input$listado_empresas
        dataChangedTrigger(dataChangedTrigger() + 1)
      })
      
      # ── Data: load from DB ─────────────────────────────────────────────────
      monitor_df <- reactive({
        dataChangedTrigger()
        input$mon_submit_edit
        input$mon_refresh
        rv$tab_monitor_clicked
        
        updateSelectInput(session, "listado_empresas",
                          selected = as.numeric(session$userData$id_empresa))
        
        if (as.numeric(session$userData$id_empresa) == 0) {
          filtros <- glue::glue_sql("1 = 1", .con = pool)
        } else {
          filtros <- glue::glue_sql(
            "id_empresa = {as.numeric(session$userData$id_empresa)}", .con = pool
          )
        }
        
        dbExecute(pool, 'SET character set "utf8"')
        tbl <- glue::glue_sql(
          "select * from {`db`}.certificados_view where ({filtros})", .con = pool
        )
        print(tbl)
        dbGetQuery(pool, tbl)
      })
      
      # ── Table render ───────────────────────────────────────────────────────
      output$monitor_table <- DT::renderDataTable({
        ns <- session$ns
        
        table <- monitor_df() %>%
          select(-id_empresa, -solicitante, -solicitante_email,
                 -email, -nombres_coach, -apellidos_coach, -estado) %>%
          mutate(
            index = row_number(),
            nombres = paste0(
              "<strong>", str_to_title(nombres), "</strong>",
              "<br>",
              "<i>", str_to_title(apellidos), "</i>"
            ),
            centro_de_costo = str_to_title(centro_de_costo),
            cargo = paste0(
              "<strong>", str_to_title(cargo), "</strong>",
              "<br>",
              "<i>", str_to_title(nombre_empresa), "</i>"
            ),
            fecha_preparacion = if_else(
              is.na(horario),
              paste0(format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>", "--:--"),
              paste0(
                format(date(fecha_preparacion), format = "%d-%m-%y"), "<br>",
                sprintf(
                  "%02d:%02d",
                  hour(lubridate::parse_date_time(horario, "%I:%M %p")),
                  minute(lubridate::parse_date_time(horario, "%I:%M %p"))
                )
              )
            ),
            certificado = glue(
              '<a id="custom_btn" href="#" onclick="Shiny.setInputValue(\'',
              ns('button_cert'),
              '\', \'{index}\', {{priority: \'event\'}})">',
              '<span class="glyphicon glyphicon-file" style="font-size:24px;color:#FF6600;"></span>',
              '</a>'
            )
          ) %>%
          select(index, id_preparacion, rut, nombres,
                 centro_de_costo, cargo, fecha_preparacion, certificado)
        
        names(table) <- c("n", "id", "Rut", "Participante",
                          "Contrato/Proyecto", "Cargo", "Fecha Capacitación", "Certificado")
        
        datatable(
          table,
          rownames  = FALSE,
          escape    = FALSE,
          class     = 'cell-border stripe',
          selection = 'single',
          options   = list(
            searchHighlight = TRUE,
            searching       = TRUE,
            scrollX         = TRUE,
            autoWidth       = FALSE,
            ordering        = FALSE,
            columnDefs      = list(
              list(targets = 0:7, search = FALSE),
              list(targets = c(1), visible = FALSE),
              list(className = 'dt-center', targets = "_all")
            ),
            language = list(
              url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
            )
          ),
          callback = JS(paste0(
            "var tips = ['Index','','Rut','Participante','Contrato/Proyecto','Cargo','Fecha Preparación','Certificado'],",
            "firstRow = $('#", session$ns('monitor_table'), " thead tr th');",
            "for (var i = 0; i < tips.length; i++) {",
            "  $(firstRow[i]).attr('title', tips[i]);",
            "}"
          ))
        )
      })
      
      # ── Certificate generation ─────────────────────────────────────────────
      observeEvent(input$button_cert, {
        
        selected_row <- as.integer(input$button_cert)
        row_data     <- monitor_df()[selected_row, ]
        
        withProgress(message = "Generando certificado...", value = 0, {
          
          incProgress(0.2, detail = "Preparando datos...")
          
          # Building data for saving into DB ────────────────────────────────
          
          # Check if certificate already exists and reuse its folio
          existing_cert <- get_certificado_folio(row_data$id_preparacion)
          
          if (!is.null(existing_cert) && nrow(existing_cert) > 0) {
            folio <- existing_cert$folio
          } else {
            # Build folio: MERC-CC-YYYY-MMDD-INITIALS-correlative
            initials <- paste0(
              toupper(substr(row_data$nombres,   1, 1)),
              toupper(substr(row_data$apellidos, 1, 1))
            )
            folio <- paste0(
              "MERC-CC-",
              format(as.Date(row_data$fecha_preparacion), "%Y"),
              "-",
              format(as.Date(row_data$fecha_preparacion), "%m%d"),
              "-", initials,
              "-", generate_cert_id()
            )
          }
          
          # Build subdimensiones JSON array from skills that are active (== 1)
          skills_map <- c(
            psicolaboral           = "Aspectos Psicolaborales",
            conductual             = "Conductual",
            conocimiento_seguridad = "Estándares de Seguridad",
            vr                     = "Operación Segura",
            tecnico_teorico        = "Contenidos Técnico-Teóricos",
            gestion                = "Gestión"
          )
          active_skills <- skills_map[
            names(skills_map)[sapply(names(skills_map), function(s) {
              as.integer(row_data[[s]]) == 1
            })]
          ]
          subdimensiones <- paste0(
            '[',
            paste0('"', unname(active_skills), '"', collapse = ", "),
            ']'
          )
          
          # ==========================================================
          
          # ── Paths ────────────────────────────────────────────────────────
          # All images used by the Rmd live in R/certificate/images/
          cert_dir  <- normalizePath(file.path(getwd(), "R", "certificate"))
          logo_path <- normalizePath(file.path(cert_dir, "images", "merc_720.png"))
          rmd_file  <- file.path(cert_dir, "certificate_template_rmd.Rmd")
          
          # ── QR code → temp file ──────────────────────────────────────────
          # qr_file <- tempfile(fileext = ".png")
          qr_file <- file.path(cert_dir, "images", paste0("qr_", row_data$rut, ".png"))
          qr_url  <- paste0(
            "https://mercconsultora.cl/verificar/?folio=",
            folio
          )
          qr <- qrcode::qr_code(qr_url)
          png(qr_file, width = 300, height = 300, bg = "white")
          par(mar = c(0, 0, 0, 0))
          plot(qr)
          dev.off()
          
          # Output goes to www/ so Shiny can serve it as a static file
          # www_dir     <- normalizePath(file.path(getwd(), "www"))
          output_name <- paste0(
            "certificado_", row_data$rut, "_", format(Sys.Date(), "%Y%m%d"), ".pdf"
          )
          # output_file <- file.path(www_dir, output_name)
          output_file <- file.path(tempdir(), output_name)
          
          # ── Params for the Rmd ───────────────────────────────────────────
          params <- list(
            name            = paste(
              stringr::str_to_title(row_data$nombres),
              stringr::str_to_title(row_data$apellidos)
            ),
            rut      = row_data$rut,
            cargo    = stringr::str_to_title(row_data$cargo),
            empresa  = stringr::str_to_title(row_data$nombre_empresa),
            course   = "Competencias Laborales (Estándar 3D)",
            date     = format_date_cert(row_data$fecha_preparacion),
            hora     = format_horario(row_data$horario),
            duration = "1 hora",
            skill1   = if_else(as.integer(row_data$psicolaboral)          == 1, "Psicolaboral",             ""),
            skill2   = if_else(as.integer(row_data$conductual)            == 1, "Conductual",               ""),
            skill3   = if_else(as.integer(row_data$conocimiento_seguridad)== 1, "Conocimiento Seguridad",   ""),
            skill4   = if_else(as.integer(row_data$vr)                    == 1, "VR",                       ""),
            skill5   = if_else(as.integer(row_data$tecnico_teorico)       == 1, "Técnico Teórico",          ""),
            skill6   = if_else(as.integer(row_data$gestion)               == 1, "Gestión",                  ""),
            instructor      = paste(
              stringr::str_to_title(row_data$nombres_coach),
              stringr::str_to_title(row_data$apellidos_coach),
              "- Psicólogo Capacitador"
            ),
            instructor_name = paste(
              stringr::str_to_title(row_data$nombres_coach),
              stringr::str_to_title(row_data$apellidos_coach)
            ),
            # cert_id         = generate_cert_id(),
            qr_path         = paste0("images/qr_", row_data$rut, ".png"),
            logo_path       = logo_path
          )
          
          incProgress(0.5, detail = "Generando PDF...")
          
          tryCatch({
            rmarkdown::render(
              input       = rmd_file,
              output_file = output_file,
              params      = params,
              quiet       = TRUE,
              envir       = new.env()
            )
            
            incProgress(0.3, detail = "Listo!")
            
            # Only record in DB if not already saved
            if (!certificado_exists(row_data$id_preparacion)) {
              save_certificado(
                folio             = folio,
                participante      = paste(
                  stringr::str_to_title(row_data$nombres),
                  stringr::str_to_title(row_data$apellidos)
                ),
                rut               = row_data$rut,
                cargo             = stringr::str_to_title(row_data$cargo),
                empresa           = stringr::str_to_title(row_data$nombre_empresa),
                contrato_proyecto = row_data$centro_de_costo,
                tipo_actividad    = "Modelamiento Competencias Laborales – Estándar 3D",
                fecha_realizacion = as.character(as.Date(row_data$fecha_preparacion)),
                duracion          = "1 hora",
                subdimensiones    = subdimensiones,
                alcance           = "Instancia de capacitación orientada a modelar competencias, para la preparación y nivelación de brechas de trabajadores, previo a la rendición de evaluación de competencias laborales exigidas por empresas mandante.",
                id_preparacion    = row_data$id_preparacion
              )
            }
            
            showNotification(
              "Certificado generado exitosamente.",
              type     = "message",
              duration = 3
            )
            
            # # Open the PDF served directly from www/ — browser will download it
            # shinyjs::runjs(sprintf(
            #   "window.open('%s', '_blank');", output_name
            # ))
            # 
            # # Clean up QR code image after successful render
            # if (file.exists(qr_file)) {
            #   file.remove(qr_file)
            # }
            
            # Encode PDF as base64 and trigger browser download directly
            # without storing in www/
            pdf_base64 <- base64enc::base64encode(output_file)
            shinyjs::runjs(sprintf("
              var link = document.createElement('a');
              link.href = 'data:application/pdf;base64,%s';
              link.download = '%s';
              document.body.appendChild(link);
              link.click();
              document.body.removeChild(link);
            ", pdf_base64, output_name))
            
            # Clean up both the temp PDF and QR code image immediately
            if (file.exists(output_file)) file.remove(output_file)
            if (file.exists(qr_file))     file.remove(qr_file)
            
          }, error = function(e) {
            # Clean up QR code image even if render fails
            # if (file.exists(qr_file)) {
            #   file.remove(qr_file)
            # }
            # Clean up both temp PDF and QR code image even if render fails
            if (file.exists(output_file)) file.remove(output_file)
            if (file.exists(qr_file))     file.remove(qr_file)
            showNotification(
              paste("Error al generar certificado:", e$message),
              type     = "error",
              duration = 10
            )
            message("Certificate render error: ", e$message)
          })
        })
      })
      
    } # function(input, output, session)
  ) # moduleServer
} # certificados_server