# user_access_module_ui <- function (id) 
# {
#   ns <- shiny::NS(id)
#   shinydashboard::tabItem(tabName = "user_access", 
#                           shiny::fluidRow(tags$style(paste0("#", ns("users_table"), " .dataTables_length {\npadding-top: 10px;\n}")), 
#                                           shinydashboard::box(width = 12, title = "Usuarios", 
#                                                               shiny::fluidRow(shiny::column(12, shiny::actionButton(ns("add_user"), "Agregar", class = "btn-success", style = "color: #fff;", icon = shiny::icon("user-plus")))), 
#                                                               shiny::fluidRow(shiny::column(12, style = "z-index: 10", DT::DTOutput(ns("users_table")) %>% shinycssloaders::withSpinner(type = 8, proxy.height = "300px")))
#                                           ), 
#                                           shiny::column(12, 
#                                                         br(), 
#                                                         br())
#                           ), 
#                           tags$script(src = "polish/js/user_access_module.js?version=2"), 
#                           tags$script(paste0("user_access_module('", ns(""), "')"))
#   )
# }

# admin_server <- function (input, output, session) 
# {
#   shiny::callModule(profile_module, "polish__profile")
#   shiny::observeEvent(input$go_to_shiny_app, {
#     remove_query_string(mode = "push")
#     session$reload()
#   }, ignoreInit = TRUE)
#   shiny::callModule(user_access_module, "user_access")
#   invisible(NULL)
# }

# user_access_module <- function (input, output, session) 
# {
#   ns <- session$ns
#   users_trigger <- reactiveVal(0)
#   users <- reactive({
#     users_trigger()
#     out <- NULL
#     tryCatch({
#       app_users_res <- get_app_users(app_uid = .polished$app_uid)
#       app_users <- app_users_res$content
#       app_users <- app_users %>% mutate(created_at = as.POSIXct(created_at))
#       res <- httr::GET(url = paste0(.polished$api_url, 
#                                     "/last-active-session-time"), query = list(app_uid = .polished$app_uid), 
#                        httr::authenticate(user = get_api_key(), password = ""))
#       httr::stop_for_status(res)
#       last_active_times <- jsonlite::fromJSON(httr::content(res, 
#                                                             "text", encoding = "UTF-8"))
#       last_active_times <- tibble::as_tibble(last_active_times)
#       last_active_times <- last_active_times %>% mutate(last_sign_in_at = lubridate::force_tz(lubridate::as_datetime((last_sign_in_at)), 
#                                                                                               tzone = "UTC"))
#       out <- app_users %>% left_join(last_active_times, 
#                                      by = "user_uid")
#     }, error = function(err) {
#       msg <- "unable to get users"
#       warning(msg)
#       warning(conditionMessage(err))
#       showToast("error", msg, .options = polished_toast_options)
#     })
#     out
#   })
#   users_table_prep <- reactiveVal(NULL)
#   observeEvent(users(), {
#     out <- users()
#     n_rows <- nrow(out)
#     if (n_rows == 0) {
#       actions <- character(0)
#     }
#     else {
#       actions <- purrr::map_chr(seq_len(n_rows), function(row_num) {
#         the_row <- out[row_num, ]
#         if (isTRUE(the_row$is_admin)) {
#           buttons_out <- paste0("<div class=\"btn-group\" style=\"width: 105px\" role=\"group\" aria-label=\"User Action Buttons\">\n            <button class=\"btn btn-default btn-sm sign_in_as_btn\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Sign In As\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\"><i class=\"fas fa-user-astronaut\"></i></button>\n            <button class=\"btn btn-primary btn-sm edit_btn\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Edit User\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\"><i class=\"fa fa-pencil-square-o\"></i></button>\n            <button class=\"btn btn-danger btn-sm delete_btn\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\" disabled><i class=\"fa fa-trash-o\"></i></button>\n          </div>")
#         }
#         else {
#           buttons_out <- paste0("<div class=\"btn-group\" style=\"width: 105px\" role=\"group\" aria-label=\"User Action Buttons\">\n            <button class=\"btn btn-default btn-sm sign_in_as_btn\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Sign In As\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\"><i class=\"fas fa-user-astronaut\"></i></button>\n            <button class=\"btn btn-primary btn-sm edit_btn\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Edit User\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\"><i class=\"fa fa-pencil-square-o\"></i></button>\n            <button class=\"btn btn-danger btn-sm delete_btn\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Delete User\" id = ", 
#                                 the_row$user_uid, " style=\"margin: 0\"><i class=\"fa fa-trash-o\"></i></button>\n          </div>")
#         }
#         buttons_out
#       })
#       out <- cbind(tibble::tibble(actions = actions), 
#                    out) %>% dplyr::mutate(invite_status = ifelse(is.na(last_sign_in_at), 
#                                                                  "Pending", "Accepted")) %>% dplyr::select(actions, 
#                                                                                                            email, invite_status, is_admin, 
#                                                                                                            last_sign_in_at)
#     }
#     if (is.null(users_table_prep())) {
#       users_table_prep(out)
#     }
#     else {
#       shinyjs::runjs("$('.btn-sm').tooltip('hide')")
#       DT::replaceData(users_proxy, out, resetPaging = FALSE, 
#                       rownames = FALSE)
#     }
#   })
#   output$users_table <- DT::renderDT({
#     shiny::req(users_table_prep())
#     out <- users_table_prep()
#     DT::datatable(out, rownames = FALSE, colnames = c("", 
#                                                       "Email", "Invite Status", "Is Admin?", "Last Sign In"), 
#                   escape = -1, selection = "none", callback = DT::JS("$( table.table().container() ).addClass( 'table-responsive' ); return table;"), 
#                   options = list(dom = "ftlp", columnDefs = list(list(targets = 0, 
#                                                                       orderable = FALSE), list(targets = 0, class = "dt-center"), 
#                                                                  list(targets = 0, width = "105px")), order = list(list(4, 
#                                                                                                                         "desc")), drawCallback = JS("function(settings) {\n          $('.tooltip').remove();\n        }"))) %>% 
#       DT::formatDate(5, method = "toLocaleString")
#   })
#   users_proxy <- DT::dataTableProxy("users_table")
#   add_user_return <- shiny::callModule(user_edit_module, "add_user", 
#                                        modal_title = "Agregar Usuario", user_to_edit = function() NULL, 
#                                        open_modal_trigger = reactive({
#                                          input$add_user
#                                        }), existing_users = users)
#   observeEvent(add_user_return$users_trigger(), {
#     users_trigger(users_trigger() + 1)
#   }, ignoreInit = TRUE)
#   user_to_edit <- reactiveVal(NULL)
#   observeEvent(input$user_uid_to_edit, {
#     out <- users() %>% dplyr::filter(user_uid == input$user_uid_to_edit)
#     user_to_edit(out)
#   }, priority = 1)
#   edit_user_return <- shiny::callModule(user_edit_module, 
#                                         "edit_user", modal_title = "Edit User", user_to_edit = user_to_edit, 
#                                         open_modal_trigger = reactive({
#                                           input$user_uid_to_edit
#                                         }), existing_users = users)
#   observeEvent(edit_user_return$users_trigger(), {
#     users_trigger(users_trigger() + 1)
#   }, ignoreInit = TRUE)
#   user_to_delete <- reactiveVal(NULL)
#   observeEvent(input$user_uid_to_delete, {
#     out <- users() %>% dplyr::filter(user_uid == input$user_uid_to_delete)
#     user_to_delete(out)
#   }, priority = 1)
#   observeEvent(input$user_uid_to_delete, {
#     hold_user <- user_to_delete()
#     shiny::req(nrow(hold_user) == 1)
#     shiny::showModal(shiny::modalDialog(title = "Delete User", 
#                                         footer = list(modalButton("Cancel"), actionButton(ns("submit_user_delete"), 
#                                                                                           "Delete User", class = "btn-danger", style = "color: white", 
#                                                                                           icon = icon("times"))), size = "m", tags$div(class = "text-center", 
#                                                                                                                                        style = "padding: 30px;", tags$h3(style = "line-height: 1.5;", 
#                                                                                                                                                                          HTML(paste0("Are you sure you want to delete ", 
#                                                                                                                                                                                      tags$b(hold_user$email), "?"))), tags$br())))
#   })
#   shiny::observeEvent(input$submit_user_delete, {
#     shiny::removeModal()
#     user_uid <- user_to_delete()$user_uid
#     app_uid <- .polished$app_uid
#     tryCatch({
#       res <- httr::DELETE(url = paste0(.polished$api_url, 
#                                        "/app-users"), body = list(user_uid = user_uid, 
#                                                                   app_uid = app_uid, req_user_uid = session$userData$user()$user_uid), 
#                           httr::authenticate(user = get_api_key(), password = ""), 
#                           encode = "json")
#       httr::stop_for_status(res)
#       shinyFeedback::showToast("success", "User successfully deleted", 
#                                .options = polished_toast_options)
#       users_trigger(users_trigger() + 1)
#     }, error = function(err) {
#       msg <- "unable to delete delete user"
#       warning(msg)
#       shinyFeedback::showToast("error", msg, .options = polished_toast_options)
#       warning(conditionMessage(err))
#       invisible(NULL)
#     })
#   })
#   shiny::observeEvent(input$sign_in_as_btn_user_uid, {
#     hold_user <- session$userData$user()
#     user_to_sign_in_as <- users() %>% filter(user_uid == 
#                                                input$sign_in_as_btn_user_uid) %>% dplyr::pull("user_uid")
#     update_session(session_uid = hold_user$session_uid, 
#                    session_data = list(signed_in_as = user_to_sign_in_as))
#     remove_query_string(mode = "push")
#     session$reload()
#   }, ignoreInit = TRUE)
#   invisible(NULL)
# }

# user_edit_module <- function (input, output, session, modal_title, user_to_edit, open_modal_trigger, existing_users) 
# {
#   ns <- session$ns
#   app_url <- reactiveVal(NULL)
#   observeEvent(open_modal_trigger(), {
#     tryCatch({
#       res <- httr::GET(url = paste0(.polished$api_url, 
#                                     "/apps"), query = list(app_uid = .polished$app_uid), 
#                        httr::authenticate(user = get_api_key(), password = ""))
#       res_content <- jsonlite::fromJSON(httr::content(res, 
#                                                       type = "text", encoding = "UTF-8"))
#       if (!identical(httr::status_code(res), 200L)) {
#         app_url(NULL)
#         stop(res_content, call. = FALSE)
#       }
#       else {
#         app_url(res_content$app_url)
#       }
#     }, error = function(err) {
#       warning(conditionMessage(err))
#       invisible(NULL)
#     })
#   }, priority = 1)
#   shiny::observeEvent(open_modal_trigger(), {
#     hold_user <- user_to_edit()
#     hold_app_url <- app_url()
#     if (is.null(hold_user)) {
#       is_admin_value <- "No"
#       email_input <- shiny::textInput(ns("user_email"), 
#                                       "Email", value = if (is.null(hold_user)) 
#                                         ""
#                                       else hold_user$email)
#       send_invite_ui <- tagList(br(), send_invite_checkbox(ns, 
#                                                            hold_app_url))
#     }
#     else {
#       if (isTRUE(hold_user$is_admin)) {
#         is_admin_value <- "Yes"
#       }
#       else {
#         is_admin_value <- "No"
#       }
#       email_input <- NULL
#       send_invite_ui <- list()
#     }
#     shiny::showModal(shiny::modalDialog(title = modal_title, 
#                                         footer = list(modalButton("Cancel"), actionButton(ns("submit"), 
#                                                                                           "Submit", class = "btn-success", icon = icon("plus"), 
#                                                                                           style = "color: white")), size = "s", htmltools::br(), 
#                                         email_input, htmltools::br(), htmltools::div(class = "text-center", 
#                                                                                      shiny::radioButtons(ns("user_is_admin"), "Is Admin?", 
#                                                                                                          choices = c("Yes", "No"), selected = is_admin_value, 
#                                                                                                          inline = TRUE), send_invite_ui), tags$script(src = "polish/js/user_edit_module.js?version=2"), 
#                                         tags$script(paste0("user_edit_module('", ns(""), 
#                                                            "')"))))
#     if (!is.null(email_input)) {
#       observeEvent(input$user_email, {
#         hold_email <- tolower(input$user_email)
#         if (is_valid_email(hold_email)) {
#           shinyFeedback::hideFeedback("user_email")
#           shinyjs::enable("submit")
#         }
#         else {
#           shinyjs::disable("submit")
#           if (hold_email != "") {
#             shinyFeedback::showFeedbackDanger("user_email", 
#                                               text = "Invalid email")
#           }
#           else {
#             shinyFeedback::hideFeedback("user_email")
#           }
#         }
#       })
#     }
#   })
#   users_trigger <- reactiveVal(0)
#   shiny::observeEvent(input$submit, {
#     input_email <- tolower(input$user_email)
#     input_is_admin <- input$user_is_admin
#     is_admin_out <- if (input_is_admin == "Yes") 
#       TRUE
#     else FALSE
#     hold_user <- user_to_edit()
#     if (is.null(hold_user)) {
#       tryCatch({
#         res <- httr::POST(url = paste0(.polished$api_url, 
#                                        "/app-users"), body = list(email = input_email, 
#                                                                   app_uid = .polished$app_uid, is_admin = is_admin_out, 
#                                                                   req_user_uid = session$userData$user()$user_uid, 
#                                                                   send_invite_email = input$send_invite_email), 
#                           httr::authenticate(user = get_api_key(), password = ""), 
#                           encode = "json")
#         if (!identical(httr::status_code(res), 200L)) {
#           err <- jsonlite::fromJSON(httr::content(res, 
#                                                   "text", encoding = "UTF-8"))
#           stop(err$error, call. = FALSE)
#         }
#         shiny::removeModal()
#         users_trigger(users_trigger() + 1)
#         shinyFeedback::showToast("success", "User successfully added!", 
#                                  .options = polished_toast_options)
#       }, error = function(err) {
#         err_msg <- conditionMessage(err)
#         shinyFeedback::showToast("error", err_msg, .options = polished_toast_options)
#         warning(err_msg)
#         invisible(NULL)
#       })
#     }
#     else {
#       shiny::removeModal()
#       tryCatch({
#         res <- httr::PUT(url = paste0(.polished$api_url, 
#                                       "/app-users"), body = list(user_uid = hold_user$user_uid, 
#                                                                  app_uid = .polished$app_uid, is_admin = is_admin_out, 
#                                                                  req_user_uid = session$userData$user()$user_uid), 
#                          httr::authenticate(user = get_api_key(), password = ""), 
#                          encode = "json")
#         if (!identical(httr::status_code(res), 200L)) {
#           err <- jsonlite::fromJSON(httr::content(res, 
#                                                   "text", encoding = "UTF-8"))
#           stop(err, call. = FALSE)
#         }
#         users_trigger(users_trigger() + 1)
#         shinyFeedback::showToast("success", "User successfully edited!", 
#                                  .options = polished_toast_options)
#       }, error = function(err) {
#         msg <- "unable to edit user"
#         warning(msg)
#         shinyFeedback::showToast("error", msg, .options = polished_toast_options)
#         warning(conditionMessage(err))
#         invisible(NULL)
#       })
#     }
#   }, ignoreInit = TRUE)
#   return(list(users_trigger = users_trigger))
# }

# sign_in_module_ui <- function (id, register_link = "First time user? Register here!", password_reset_link = "Forgot your password?") 
# {
#   ns <- shiny::NS(id)
#   providers <- .polished$sign_in_providers
#   continue_sign_in <- div(id = ns("continue_sign_in"), shiny::actionButton(inputId = ns("submit_continue_sign_in"), 
#                                                                            label = "Continue", width = "100%", class = "btn btn-primary btn-lg"))
#   sign_in_password_ui <- div(id = ns("sign_in_password_ui"), 
#                              div(class = "form-group", style = "width: 100%;", tags$label(tagList(icon("unlock-alt"), 
#                                                                                                   "password"), `for` = "sign_in_password"), tags$input(id = ns("sign_in_password"), 
#                                                                                                                                                        type = "password", class = "form-control", value = "")), 
#                              br(), shinyFeedback::loadingButton(ns("sign_in_submit"), 
#                                                                 label = "Sign In", class = "btn btn-primary btn-lg text-center", 
#                                                                 style = "width: 100%", loadingLabel = "Authenticating...", 
#                                                                 loadingClass = "btn btn-primary btn-lg text-center", 
#                                                                 loadingStyle = "width: 100%"))
#   continue_registration <- div(id = ns("continue_registration"), 
#                                shiny::actionButton(inputId = ns("submit_continue_register"), 
#                                                    label = "Continue", width = "100%", class = "btn btn-primary btn-lg"))
#   register_passwords <- div(id = ns("register_passwords"), 
#                             div(class = "form-group", style = "width: 100%", tags$label(tagList(icon("unlock-alt"), 
#                                                                                                 "password"), `for` = ns("register_password")), tags$input(id = ns("register_password"), 
#                                                                                                                                                           type = "password", class = "form-control", value = "")), 
#                             br(), div(class = "form-group shiny-input-container", 
#                                       style = "width: 100%", tags$label(tagList(shiny::icon("unlock-alt"), 
#                                                                                 "verify password"), `for` = ns("register_password_verify")), 
#                                       tags$input(id = ns("register_password_verify"), 
#                                                  type = "password", class = "form-control", value = "")), 
#                             br(), div(style = "text-align: center;", shinyFeedback::loadingButton(ns("register_submit"), 
#                                                                                                   label = "Register", class = "btn btn-primary btn-lg", 
#                                                                                                   style = "width: 100%;", loadingLabel = "Registering...", 
#                                                                                                   loadingClass = "btn btn-primary btn-lg text-center", 
#                                                                                                   loadingStyle = "width: 100%;")))
#   if (is.null(password_reset_link)) {
#     pass_link_ui <- NULL
#   }
#   else {
#     pass_link_ui <- send_password_reset_email_module_ui(ns("reset_password"), 
#                                                         password_reset_link)
#   }
#   email_ui <- tags$div(id = ns("email_ui"), tags$div(id = ns("sign_in_panel_top"), 
#                                                      htmltools::h1(class = "text-center", style = "padding-top: 0;", 
#                                                                    "Sign In"), tags$br(), email_input(inputId = ns("sign_in_email"), 
#                                                                                                       label = tagList(icon("envelope"), "email"), value = ""), 
#                                                      tags$br()), tags$div(id = ns("sign_in_panel_bottom"), 
#                                                                           if (isTRUE(.polished$is_invite_required)) {
#                                                                             tagList(continue_sign_in, shinyjs::hidden(sign_in_password_ui))
#                                                                           }
#                                                                           else {
#                                                                             sign_in_password_ui
#                                                                           }, div(style = "text-align: center;", if (is.null(register_link)) {
#                                                                             list()
#                                                                           }
#                                                                           else {
#                                                                             list(hr(), shiny::actionLink(inputId = ns("go_to_register"), 
#                                                                                                          label = register_link))
#                                                                           }, br(), pass_link_ui)), shinyjs::hidden(div(id = ns("register_panel_top"), 
#                                                                                                                        h1(class = "text-center", style = "padding-top: 0;", 
#                                                                                                                           "Register"), tags$br(), email_input(inputId = ns("register_email"), 
#                                                                                                                                                               label = tagList(icon("envelope"), "email"), value = ""), 
#                                                                                                                        tags$br())), shinyjs::hidden(div(id = ns("register_panel_bottom"), 
#                                                                                                                                                         if (isTRUE(.polished$is_invite_required)) {
#                                                                                                                                                           tagList(continue_registration, shinyjs::hidden(register_passwords))
#                                                                                                                                                         }
#                                                                                                                                                         else {
#                                                                                                                                                           register_passwords
#                                                                                                                                                         }, div(style = "text-align: center", hr(), shiny::actionLink(inputId = ns("go_to_sign_in"), 
#                                                                                                                                                                                                                      label = "Already a user? Sign in!"), br(), br()))))
#   if (length(providers) == 1 && providers == "email") {
#     ui_out <- email_ui
#   }
#   else {
#     hold_providers_ui <- providers_ui(ns, providers)
#     email_ui <- shinyjs::hidden(email_ui)
#     ui_out <- tagList(hold_providers_ui, email_ui)
#   }
#   htmltools::tagList(shinyjs::useShinyjs(), tags$div(class = "auth_panel", 
#                                                      ui_out), tags$script(src = "polish/js/auth_keypress.js?version=4"), 
#                      tags$script(paste0("auth_keypress('", ns(""), "')")), 
#                      tags$script("$('input').attr('autocomplete', 'off');"), 
#                      sign_in_js(ns))
# }

# my_custom_sign_in_page <- sign_in_ui_default(
#   sign_in_module = sign_in_module_ui("sign_in", NULL, password_reset_link = "Olvidaste tu contraseña?"),
#   color = "#FFFFFF",
#   company_name = "MERC Consultora",
#   logo_top = tags$img(
#     src = "images/logo1.png",
#     alt = "Logo merc consultora",
#     style = "width: 300px; margin-top: 30px; margin-bottom: 30px;"
#   ),
#   button_color = "#000000"
#     # logo_bottom = tags$img(
#   #   src = "images/tychobra_logo_blue_co_name.png",
#   #   alt = "Tychobra Logo",
#   #   style = "width: 200px; margin-bottom: 15px; padding-top: 15px;"
#   # ),
#   # icon_href = "images/tychobra_icon_blue.png",
#   # background_image = "images/milky_way.jpeg"
# )

# admin_ui <- function(options = default_admin_ui_options()) {
#   
#   # don't show profile dropdown if in Admin mode.  User cannot log out of admin mode.
#   
#   head <- shinydashboard::dashboardHeader(
#     title = "C3D Usuarios",#options$title,
#     profile_module_ui("polish__profile")
#   )
#   
#   
#   sidebar <- shinydashboard::dashboardSidebar(
#     shinydashboard::sidebarMenu(
#       id = "sidebar_menu",
#       shinydashboard::menuItem(
#         text = "Agregar Usuarios",
#         tabName = "user_access",
#         icon = shiny::icon("users")
#       ),
#       
#       
#       options$sidebar_branding
#     )
#   )
#   
#   tab_items <- shinydashboard::tabItems(
#     user_access_module_ui("user_access")
#   )
#   
#   body <- shinydashboard::dashboardBody(
#     htmltools::tags$head(
#       options$browser_tab_icon,
#       htmltools::tags$link(rel = "stylesheet", href = "polish/css/styles.css?version=1")
#     ),
#     shinyjs::useShinyjs(),
#     shinyFeedback::useShinyFeedback(),
#     
#     htmltools::tags$div(
#       style = "position: fixed; bottom: 15px; right: 15px; z-index: 1000;",
#       shiny::actionButton(
#         "go_to_shiny_app",
#         "Volver a plataforma",
#         icon = shiny::icon("rocket"),
#         class = "btn-primary btn-lg",
#         style = "color: #FFFFFF;"
#       )
#     ),
#     
#     tab_items
#   )
#   
#   shinydashboard::dashboardPage(
#     head,
#     sidebar,
#     body,
#     title = "Polished",
#     skin = "black"
#   )
# }

# login function for shinymanager
# my_custom_check_creds <- function(con) {
#   
#   # finally one function of user and password
#   function(user, password) {
#     
#     #on.exit(dbDisconnect(con))
#     
#     req <- glue_sql("SELECT * FROM usuarios WHERE email = {user} AND password = {password}", 
#                     user = user, password = password, .con = con)
#     
#     # req <- dbGetQuery(con, req)
#     res <- dbGetQuery(con, req)
#     if (nrow(res) > 0) {
#       sid = stringi::stri_rand_strings(1, 100)
#       info <- tbl(con, "usuarios") %>% 
#         filter(email == user) %>% 
#         select(nombre, apellidos, email, cargo, id_empresa, rol) %>%
#         inner_join(tbl(con, "clientes") %>% select(id_empresa, bloqueado), by = c("id_empresa")) %>% 
#         mutate(session_cookie = sid) %>%
#         dplyr::collect()
#       
#       add_session_to_db(user = user, sessionid = sid, conn = con)
#       # shinyjs::js$setcookie(sid)
#       # list(result = TRUE, user_info = list(user = user, something = 123))
#       print(as.list(info))
#       list(result = TRUE, user_info = as.list(info))
#     } else {
#       list(result = FALSE)
#     }
#   }
# }

# add_session_to_db <- function(user, sessionid, conn = pool) {
#   # tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
#   #   #dbWriteTable(conn, "sessions", ., append = TRUE)
#   #   sqlAppendTable(conn, "sessions", row.names = FALSE) %>%
#   #   dbExecute(conn)
#   tbl <- data.frame(user = user, sessionid = sessionid, login_time = as.character(Sys.time()))
#   query <- sqlAppendTable(conn, "sessions", tbl, row.names = FALSE)
#   result <- dbExecute(conn, query)
#   
# }

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.
# get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
#   tbl(conn, "sessions") %>%
#     mutate(login_time = ymd_hms(login_time)) %>%
#     as_tibble() %>%
#     filter(login_time > Sys.time() - days(expiry))
# }
