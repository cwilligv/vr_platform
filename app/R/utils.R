#' Remove the URL query
#'
#' Remove the entire query string from the URL.  This function should only be called
#' inside the server function of your Shiny app.
#'
#' @param session the Shiny \code{session}
#' @param mode the mode to pass to \code{shiny::updateQueryString()}.  Valid values are
#' \code{"replace"} or \code{"push"}.
#'
#' @importFrom shiny updateQueryString getDefaultReactiveDomain
#'
#' @return \code{invisible(NULL)}
#'
#' @export
#'
#'
remove_query_string <- function(session = shiny::getDefaultReactiveDomain(), mode = "replace") {
  
  shiny::updateQueryString(
    "?",
    mode = mode,
    session = session
  )
  
  invisible(NULL)
}

#' get_cookie
#'
#' Get a cookie value by name from a cookie string
#'
#' @param cookie_string the cookie string
#' @param name the name of the cookie
#'
#' @importFrom dplyr filter pull %>%
#' @importFrom tidyr separate
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @noRd
#'
#'
get_cookie <- function(cookie_string, name) {
  
  cookies <- strsplit(cookie_string , split = "; ", fixed = TRUE)
  
  tibble::tibble(cookie = unlist(cookies)) %>%
    tidyr::separate(.data$cookie, into = c("key", "value"), sep = "=", extra = "merge") %>%
    dplyr::filter(.data$key == name) %>%
    dplyr::pull("value")
}



#' create UI for checkbox to send an email invite
#'
#' @param ns the Shiny namespace function
#' @param app_url the app url
#'
#' @noRd
#'
#' @importFrom shinyjs disabled
#' @importFrom shinyWidgets prettyCheckbox
#' @importFrom htmltools tags
send_invite_checkbox <- function(ns, app_url) {
  # check if the app has an app url.  If the app has an app_url, allow the
  # user to send an invite email.
  if (!is.null(app_url) && !is.na(app_url) && app_url != "") {
    email_invite_checkbox <- shinyWidgets::prettyCheckbox(
      ns("send_invite_email"),
      "Send Invite Email?",
      value = FALSE,
      status = "primary"
    )
  } else {
    email_invite_checkbox <- tags$div(
      tags$span(
        shinyjs::disabled(shinyWidgets::prettyCheckbox(
          ns("send_invite_email"),
          "Send Invite Email?",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ))
      ),
      tags$span(
        style = "display: inline-block; margin-left: -15px;",
        id = ns("checkbox_question"),
        icon("question-circle"),
        `data-toggle` = "tooltip",
        `data-placement`= "top",
        title = "You must set the App URL to send email invites. Go to https://dashboard.polished.tech to set your app URL."
      )
    )
  }
  
  email_invite_checkbox
}

#' normalize UI
#'
#' the UI passed a shiny app can be a function HTML.  This function normalized the 2 different
#' formats so that they both use the character
#'
#' @param ui the Shiny ui
#' @param request_ the request environment passed to the first argument of the UI
#' function
#'
#' @export
#'
#' @return the Shiny UI
#'
normalize_ui <- function(ui, request_) {
  if (is.function(ui)) {
    if (length(formals(ui)) > 0) {
      ui <- ui(request_)
    }  else {
      ui <- ui()
    }
  } else {
    ui <- ui
  }
  ui
}

# Default `.options` for `showToast`
polished_toast_options <- list(
  positionClass = "toast-top-center",
  showDuration = 1000,
  newestOnTop = TRUE
)

#' is_valid_email
#'
#' function for email validation (Sign in & Registration)
#'
#' @param x email address to check
#'
#' @noRd
#'
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}


#' is_email_registered
#'
#' Check if an email address is already registered.  This function is used in our
#' sign in modules to redirect the user from the sign in inputs to the registration
#' inputs if the user is attempting to sign in before they have registered.
#'
#' @param email the email address to check
#'
#' @return a list with the a boolean element named "is_registered", and an "error"
#' element if there is an error status code returned from the Polished Auth API request.
#'
#' @noRd
#'
is_email_registered <- function(email) {
  
  user_res <- httr::GET(
    paste0(.polished$api_url, "/users"),
    query = list(
      email = email
    ),
    httr::authenticate(
      user = get_api_key(),
      password = ""
    )
  )
  
  user_res_content <- jsonlite::fromJSON(
    httr::content(user_res, "text", encoding = "UTF-8")
  )
  
  if (!identical(httr::status_code(user_res), 200L)) {
    out <- list(
      is_registered = FALSE,
      error = user_res_content
    )
  } else {
    if (isTRUE(user_res_content$is_password_set)) {
      out <- list(
        is_registered = TRUE
      )
      
    } else {
      out <- list(
        is_registered = FALSE
      )
    }
  }
  
  
  out
}

set_html_attribs <- function(title = "", lang = "es", dir = "ltr") {
  
  # validate
  if (!is.character(title)) stop("arugment 'title' must be a string")
  if (title == "") warning("value for 'title' is missing.")
  if (!is.character(lang)) stop("argument 'lang' must be a character")
  if (!is.character(dir)) stop("argument 'dir' must be a character")
  if (!dir %in% c("ltr", "rtl", "auto")) {
    stop("value for 'dir' is invalid. Use 'ltr', 'rtl', or 'dir'.")
  }
  
  # content to append to <head> + html attributes 
  tagList(
    
    # <head>
    tags$head(
      
      # document encoding
      tags$meta(charset = "utf-8"),
      
      # for MS Edge
      tags$meta(
        `http-quiv` = "x-ua-compatible",
        content = "ie=edge"
      ),
      
      # mobile optimization
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      
      # link to stylesheets
      tags$link(
        rel = "stylesheet",
        href = "path/to/stylesheet"
      ),
      
      # document title
      tags$title(title)
    ),
    
    # render hidden inline <span> element
    tags$span(
      id = "shiny__html_attribs",
      style = "display: none;",
      `data-html-lang` = lang,
      `data-html-dir` = dir
    )
  )
}


#' Calcula los dias que faltan para el vencimiento del resultado.
#'
#'
#' @param sub_dim nombre de la sub-dimension.
#' @param fecha_evaluacion fecha en la que se realizo la evaluacion.
#' @param resultado resultado de la evaluacion (competente, competente con observaciones y competente)
#'
#' @return numero de dias
#'
#' @export
#'
#'
calcular_vencimiento <- function(sub_dim, fecha_evaluacion, resultado){
  futuro <- case_when(
    tolower(resultado) == 'no competente' ~ months(6),
    tolower(resultado) == 'competente c/o' ~ months(6),
    tolower(resultado) == 'competente' & !(sub_dim %in% c("tecnico_teorico", "tecnico_practica", "gestion")) ~ years(2),
    .default = days(0)
  )
  
  dias <- NA
  
  if (!is.na(fecha_evaluacion) && !is.na(resultado)) {
    dias <- round(as.numeric(difftime(ymd(fecha_evaluacion)+futuro, now(tzone = "Chile/Continental"), units = "days")))
  }
  
  return(dias)
}

seleccion_fecha_prep <- function(pfecha_online, pfecha_presencial){
  # pfecha_online <- ifelse(pfecha_online == '', "2300-01-01", pfecha_online)
  # pfecha_presencial <- ifelse(pfecha_presencial == '', "2300-01-01", pfecha_presencial)

  if (length(pfecha_online) == 0 || is.na(pfecha_online)) {
    pfecha_online <- "2300-01-01"
  }
  
  if (length(pfecha_presencial) == 0 || is.na(pfecha_presencial)) {
    pfecha_presencial <- "2300-01-01"
  }
  selected_date <- list(fecha = NA, horario = NA)
  # fecha <- as.Date(pfecha) = as.difftime(2, unit = "days")
  fecha <- min(ymd(pfecha_online, tz = 'Chile/Continental'), ymd(pfecha_presencial, tz = 'Chile/Continental'))
  selected_date$fecha <- ymd(fecha) - days(1)
  
  if (selected_date$fecha <  lubridate::today(tzone = "Chile/Continental")) {
    selected_date$fecha <- ymd(fecha)
  }
  
  df <- get_available_slots(selected_date$fecha) %>% 
    mutate(avail_slots = rowSums(across(where(is.numeric))))
  
  tnow <- lubridate::now(tzone = "Chile/Continental")
  # tnow <- lubridate::ymd_hms('2025-01-09 10:00:00', tz = 'Chile/Continental')
  
  # Check if there is availability the day before
  if (df$avail_slots > 0) {
    selected_date$horario <- case_when(
      df$`9am` > 0 & (lubridate::ymd_hms(paste(selected_date$fecha, "09:00:00 am"), tz = "Chile/Continental") > tnow) ~ '09:00 am',
      df$`12pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "12:00:00 pm"), tz = "Chile/Continental") > tnow) ~ '12:00 pm',
      df$`3pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "15:00:00"), tz = "Chile/Continental") > tnow) ~ '03:00 pm',
      df$`6pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "18:00:00"), tz = "Chile/Continental") > tnow) ~ '06:00 pm',
      df$`8pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "20:00:00"), tz = "Chile/Continental") > tnow) ~ '08:00 pm',
      .default = NA
    )
  } else { # if there is no slots available check two days before
    selected_date$fecha <- ymd(pfecha_online) - days(2)
    df <- get_available_slots(selected_date$fecha) %>% 
      mutate(avail_slots = rowSums(across(where(is.numeric))))
    
    if (df$avail_slots > 0) {
      selected_date$horario <- case_when(
        df$`9am` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "09:00:00 am"), tz = "Chile/Continental") > tnow) ~ '09:00 am',
        df$`12pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "12:00:00 pm"), tz = "Chile/Continental") > tnow) ~ '12:00 pm',
        df$`3pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "15:00:00"), tz = "Chile/Continental") > tnow) ~ '03:00 pm',
        df$`6pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "18:00:00"), tz = "Chile/Continental") > tnow) ~ '06:00 pm',
        df$`8pm` > 0 & (lubridate::ymd_hms(paste0(selected_date$fecha, "20:00:00"), tz = "Chile/Continental") > tnow) ~ '08:00 pm',
        .default = NA
      )
    }
  }
  
  return(selected_date)
  
  
}

meses <- data.frame(
  eng = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  spa = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
)

get_uf_periodo <- function(periodo, fin_de_mes){
  
  if (fin_de_mes) {
    mes_periodo <- meses[meses$eng == month(periodo, label = T, abbr = T),]$spa
    annio_periodo <- year(periodo)
    last_day <- day(rollforward(periodo))
    
    valor_uf <- get_uf_mensual(annio_periodo) %>% filter(mes == mes_periodo & day == last_day) %>% select(value) %>% pull()
    
  } else { # esta logica corresponde a fecha especifica de un dia.
    mes_periodo <- meses[meses$eng == month(periodo, label = T, abbr = T),]$spa
    annio_periodo <- year(periodo)
    day_periodo <- day(periodo)
    
    valor_uf <- get_uf_mensual(annio_periodo) %>% filter(mes == mes_periodo & day == day_periodo) %>% select(value) %>% pull()
  }
  
  return(round(valor_uf,2))
}

get_uf_mensual <- function(y) {
  
  # y <- 2016
  
  # message(y)
  
  seleccionador <- ifelse(y <= 2012, first, last) 
  direccion <- ifelse(y <= 2012, "http://www.sii.cl/pagina/valores/uf/uf{ year }.htm",
                      "http://www.sii.cl/valores_y_fechas/uf/uf{ year }.htm")
  
  d <- y %>% 
    str_glue(direccion, year = .) %>% 
    read_html() %>% 
    html_table() %>% 
    seleccionador() %>% 
    gather(key, value, -Día) %>% 
    as_tibble() %>% 
    mutate(
      value = str_replace(value, "\\.", ""),
      value = str_replace(value, ",", "."),
      value = as.numeric(value),
      year = y,
    ) %>% 
    rename(
      day = Día,
      mes = key
    )
  
  d 
  
}

# Function to convert spanish number commas to dots
convert_spanish_number <- function(x) {
  # Remove spaces
  x <- gsub(" ", "", x)
  
  # Replace Spanish decimal point with a temporary placeholder (e.g., "#")
  x <- gsub(".", "#", x, fixed = TRUE) 
  
  # Replace Spanish thousand separator (comma) with English decimal point (dot)
  x <- gsub(",", ".", x)
  
  # Replace temporary placeholder with English thousand separator (comma)
  x <- gsub("#", "", x)
  
  # Convert to numeric
  as.numeric(x)
}

generar_observacion <- function(notas) {
  chat <- chat_groq(
    api_key = env$LLM_API,
    system_prompt = "Eres un psicologo que capacita a personas para rendir pruebas online."
  )
  
  peticion <- "
  Escribe un parrafo sobre una persona que fue capacitada por ti, el psicologo. El parrafo debe ser conciso y al punto.El parrafo debe resumir la capacitacion Usando las siguientes notas:
  "
  # prompt <- paste(peticion, notas)
  
  response <- chat$chat(
    paste(peticion, notas)
  )
  
  return(response)
}

generar_edp_excel <- function(table, fname, resumen){
  ## Create a new workbook
  wb <- openxlsx::createWorkbook("MERC")
  
  ## Add a worksheets
  openxlsx::addWorksheet(wb, "Estado de Pago")
  
  ## Write title
  openxlsx::writeData(wb, sheet = 1, "ESTADO DE PAGO", startCol = 1, startRow = 1)
  
  # Merge cells from A1 to H3
  openxlsx::mergeCells(wb, sheet = 1, rows = 1:3, cols = 1:9)
  
  # Create a style for the merged cells
  title_style <- createStyle(
    fontName = "Calibri",
    fontSize = 20,
    fontColour = "white",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    fgFill = "#4F81BD"
  )
  
  # Apply the style to the merged cell range
  openxlsx::addStyle(wb, sheet = 1, style = title_style, rows = 1:3, cols = 1:9, gridExpand = T)
  
  ## Write tabla de resumen
  openxlsx::writeData(wb, sheet = 1, "RESUMEN DE SERVICIO", startCol = 3, startRow = 6)
  openxlsx::mergeCells(wb, sheet = 1, rows = 6, cols = 3:5)
  resumen_title_style <- createStyle(
    fontName = "Calibri",
    fontSize = 11,
    fontColour = "white",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    fgFill = "#4F81BD"
  )
  openxlsx::addStyle(wb, sheet = 1, style = resumen_title_style, rows = 6, cols = 3:5, gridExpand = T)
  
  resumen_columna_style <- createStyle(
    fontName = "Calibri",
    fontSize = 11,
    fontColour = "black",
    halign = "left",
    textDecoration = "bold",
    fgFill = "#d9d9d9"
  )
  
  openxlsx::writeData(wb, sheet = 1, "EMPRESA:", startCol = 3, startRow = 7)
  openxlsx::writeData(wb, sheet = 1, resumen[1,]$valor, startCol = 4, startRow = 7)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 7, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 7, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "RUT:", startCol = 3, startRow = 8)
  openxlsx::writeData(wb, sheet = 1, resumen[2,]$valor, startCol = 4, startRow = 8)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 8, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 8, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "FECHA SERVICIO:", startCol = 3, startRow = 9)
  openxlsx::writeData(wb, sheet = 1, resumen[3,]$valor, startCol = 4, startRow = 9)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 9, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 9, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "DESCRIPCIÓN:", startCol = 3, startRow = 10)
  openxlsx::writeData(wb, sheet = 1, resumen[4,]$valor, startCol = 4, startRow = 10)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 10, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 10, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "CANTIDAD:", startCol = 3, startRow = 11)
  openxlsx::writeData(wb, sheet = 1, resumen[6,]$valor, startCol = 4, startRow = 11)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 11, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 11, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "VALOR UNITARIO (UF):", startCol = 3, startRow = 12)
  openxlsx::writeData(wb, sheet = 1, resumen[5,]$valor, startCol = 4, startRow = 12)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 12, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 12, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "IVA (Exento):", startCol = 3, startRow = 13)
  openxlsx::writeData(wb, sheet = 1, "$ 0", startCol = 4, startRow = 13)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 13, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 13, cols = 4:5)
  
  openxlsx::writeData(wb, sheet = 1, "TOTAL:", startCol = 3, startRow = 14)
  openxlsx::writeData(wb, sheet = 1, resumen[7,]$valor, startCol = 4, startRow = 14)
  openxlsx::addStyle(wb, sheet = 1, style = resumen_columna_style, rows = 14, cols = 3)
  openxlsx::mergeCells(wb, sheet = 1, rows = 14, cols = 4:5)
  
  # Top border style (row 1, cols 1-8)
  top_style <- createStyle(border = c("top"), borderColour = "black", borderStyle = "medium")
  addStyle(wb, sheet = 1, style = top_style, rows = 6, cols = 3:5, gridExpand = TRUE, stack = TRUE)
  
  # Bottom border style (row 3, cols 1-8)
  bottom_style <- createStyle(border = c("bottom"), borderColour = "black", borderStyle = "medium")
  addStyle(wb, sheet = 1, style = bottom_style, rows = 14, cols = 3:5, gridExpand = TRUE, stack = TRUE)
  
  # Left border style (rows 1-3, col 1)
  left_style <- createStyle(border = c("left"), borderColour = "black", borderStyle = "medium")
  addStyle(wb, sheet = 1, style = left_style, rows = 6:14, cols = 3, gridExpand = TRUE, stack = TRUE)
  
  # Right border style (rows 1-3, col 8)
  right_style <- createStyle(border = c("right"), borderColour = "black", borderStyle = "medium")
  addStyle(wb, sheet = 1, style = right_style, rows = 6:14, cols = 5, gridExpand = TRUE, stack = TRUE)
  
  # Insert MERC's logo
  openxlsx::insertImage(wb, sheet = 1, file = paste0(getwd(),'/www/images/merc_720.png'), width = 576, height = 168, units = "px", startCol =  7, startRow = 8)
  
  ## write data to worksheet 1
  openxlsx::writeData(wb, sheet = 1, table, startCol = 1, startRow = 18)
  
  ## create and add a style to the column headers
  headerStyle1 <- openxlsx::createStyle(
    fontSize = 11, fontColour = "#FFFFFF",
    fgFill = "#4F81BD", halign = "center",
    textDecoration = "bold", valign = "center"
  )
  openxlsx::addStyle(wb, sheet = 1, headerStyle1, rows = 18, cols = 1:9, gridExpand = TRUE)
  
  ## Create and add style for table content
  tableContentStyle <- openxlsx::createStyle(
    fontSize = 10
  )
  openxlsx::addStyle(wb, sheet = 1, tableContentStyle, rows = 19:(19+nrow(table)), cols = 1:9, gridExpand = TRUE)
  
  ## set row heights
  openxlsx::setRowHeights(wb, sheet = 1, rows = 18, heights = 30)
  
  ## set column width
  openxlsx::setColWidths(wb, sheet = 1, cols = 1, widths = 4.57)
  openxlsx::setColWidths(wb, sheet = 1, cols = 2:9, widths = "auto")
  
  ## Page setup
  openxlsx::pageSetup(wb, sheet = 1, orientation = "landscape", paperSize = 9)
  
  ## save
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE, returnValue = TRUE)
}

# Function to convert month names to Spanish (Modulo Avances Encuesta)
format_month_spanish <- function(date_string) {
  date_obj <- as.Date(paste0(date_string, "-01"))
  month_num <- as.numeric(format(date_obj, "%m"))
  year <- format(date_obj, "%Y")
  
  spanish_months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  paste(spanish_months[month_num], year)
}

get_category_label <- function(category) {
  category_labels <- c(
    "1" = "Muy en desacuerdo",
    "2" = "En desacuerdo", 
    "3" = "Neutral",
    "4" = "De acuerdo",
    "5" = "Muy de acuerdo"
  )
  return(as.character(category_labels[category]))
}

# Helper: human-readable date
format_date_cert <- function(date) {
  tolower(format(as.Date(date), "%d de %B de %Y"))
}

format_horario <- function(horario){
  sprintf(
    "%02d:%02d",
    hour(lubridate::parse_date_time(horario, "%I:%M %p")),
    minute(lubridate::parse_date_time(horario, "%I:%M %p"))
  )
}

# Helper: 8-char hex certificate ID
generate_cert_id <- function() {
  paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")
}