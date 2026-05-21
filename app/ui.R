#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.port = 3838)

spinner <- tagList(
  spin_chasing_dots(),
  span("Loading stuff...", style="color:white;")
)

mytheme <- create_theme(
  bs4dash_layout(
    sidebar_width = "200px",
    main_bg = "#EFF3F6"
  ),
  bs4dash_sidebar_dark(
    bg = "#FFFFFF",
    color = "#000000",
    hover_bg = "#EFF3F6",
    header_color = "#FFFFFF",
    hover_color = "#000000",
    active_color = "#EFF3F6"
  ),
  bs4dash_color(
    #blue = "#EFF3F6"
  ),
  bs4dash_status(
    success = "#006ac2",
    warning = "#ff5a00",
    danger = "#becede"
  )
)

custom_css <- '
.nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
    color: #0A66C2;
}

.sidebar-dark-primary .nav-sidebar>.nav-item>.nav-link.active, .sidebar-light-primary .nav-sidebar>.nav-item>.nav-link.active {
    background-color: #EFF3F6; color: #0A66C2
}

a {
    color: black;
}

.shiny-output-error-validation {
        color: red;
}
'

jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

jscode_upload_msg <- " 
  Shiny.addCustomMessageHandler('upload_msg', function(msg) {
    var target = $('#herramientas-archivo_carga_masiva_progress').children()[0];
    target.innerHTML = msg;
  }); 
"

ui <- dashboardPage(
  scrollToTop = T,
  dark = NULL, 
  help = NULL,
  useShinyalert(),
  useShinyjs(),
  freshTheme = mytheme,
  title = "MERC Training 3D",
  header = dashboardHeader(set_html_attribs(title = "Capacitación 3D"), title = tags$a(tags$img(src='images/merc_720.png', width=180, style = "margin: 10px 0px 0px 10px"), href = "https://www.mercconsultora.cl", target="_blank"),
                           #title = tags$img(src='images/kdmindustrial.jpg', height=72,width=200),
                           titleWidth = "220px",
                           #tags$style(".layout-navbar-fixed .wrapper .sidebar-dark-primary .brand-link {Width:220px}"),
                           fixed = T,
                           # rightUi = tags$li(class = "dropdown",
                           #   div(selectInput("selector_empresas", "", choices = c("A", "B")), style="height:30px; margin-top:-18px"),
                           #   div(tags$li(class = "dropdown", userOutput("user")))
                           # ),
                           # rightUi = tags$li(class = "dropdown", dropdownMenuOutput("messageMenu"), userOutput("user")),
                           rightUi = tags$li(class = "dropdown", userOutput("user")),
                           leftUi = tags$li(class = "dropdown", span(h2(strong(uiOutput("texto_saludo_inicio")))), style = "font-size:1.8rem;"),
                           tags$script(src = "set_html_attribs.js")
  ),
  sidebar = dashboardSidebar(status = "primary",
                             minified = F,
                             inputId = "sidebarState",
                             skin = "dark",
                             # tags$head(
                             #   tags$style(
                             #     HTML(
                             #       "
                             #       #tab-tab11 {
                             #        position:absolute;
                             #        wisth:inherit;
                             #        bottom:0;
                             #        left:0;
                             #       }
                             #       "
                             #     )
                             #   )
                             # ),
                             #disconnectMessage(text = "TU sesion en la plataforma ha terminado. Presiona refrescar para iniciar nuevamente"),
                             sidebarMenu(
                               id = "sidebar_menu",
                               menuItem(
                                 text = "Resumen",
                                 tabName = "tab1_inicio",
                                 icon = icon("chart-simple", lib = "font-awesome"),
                                 selected = T
                               ),
                               menuItemOutput("menu_inscripciones"), #tab2,
                               menuItemOutput("menu_monitor"), #tab3,
                               menuItemOutput("menu_satisfaccion"),
                               # menuItemOutput("menu_certificados"),
                               menuItemOutput("menu_herramientas"), #tab_herramientas
                               menuItemOutput("menu_pagos"), # tab9
                               # menuItemOutput("menu_soporte"), # tab7
                               menuItemOutput("menu_monitor_interno"),
                               menuItemOutput("menu_administracion"), # tab8
                               menuItemOutput("menu_sistema"), # tab_sistema
                               tags$div(
                                 style = "position: fixed; bottom: 20px; width: 200px; text-align: center; padding: 10px;",
                                 tags$hr(style = "border-color: #ddd; margin: 5px 15px;"),
                                 tags$p(
                                   style = "margin: 0; font-size: 1rem; color: #444; font-weight: bold;",
                                   "¿Necesitas ayuda?"
                                 ),
                                 tags$p(
                                   style = "margin: 2px 0; font-size: 0.93rem; color: #666;",
                                   "Contáctanos si tienes consultas"
                                 ),
                                 tags$p(
                                   style = "margin: 6px 0 0 0; font-size: 0.85rem; color: #ff5a00; font-weight: bold;",
                                   HTML("<i class='fa fa-phone' style='color: #ff5a00;'></i> <span style='color: #000000;'>+56 22 756 7186</span>")
                                 )
                               )
                             )
  ),
  body = dashboardBody( useShinyjs(), 
                        # use_telemetry(),
                        tags$script(src = "https://kit.fontawesome.com/<you>.js"),
                        tags$script(HTML(jscode_upload_msg)),
                        tags$head(
                          tags$link(rel = "icon", href = "logo2.ico"),
                          tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js"),
                          # js for cleaning up the url after two seconds
                          tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'Page Title', window.location.pathname);},2000);")),
                          tags$style(HTML("
                            .invalid-feedback {
                              color: red !important;
                            }
                            .sa-icon.sa-error.animateErrorIcon {
                              border-color: #ff5a00 !important;
                            }
                            .sa-line.sa-left,
                            .sa-line.sa-right {
                              background-color: #ff5a00 !important;
                            }
                          ")),
                        ),
                        extendShinyjs(text = jsCode, functions = c("getcookie", "setcookie", "rmcookie")),
                        tags$style(HTML(custom_css)),
                        
                      tabItems(
                        inicio_ui("inicio"),
                        inscripcion_participantes_ui("inscripcion_participantes"),
                        monitor_preparaciones_ui("monitor_preparaciones"),
                        resultados_encuesta_ui("resultado_satisfaccion"),
                        # certificados_ui("certificados"),
                        herramientas_ui("herramientas"),
                        estado_de_pago_ui("estado_de_pago"),
                        pagos_ui("pagos"),
                        soporte_ui("soporte"),
                        herramientas_internas_ui("herramientas_internas"),
                        gestion_clientes_ui("gestion_clientes"),
                        gestion_sistema_ui("gestion_sistema"),
                        tabItem(tabName = "tab11", h2("Terminos"))
                      )
  ),
  # controlbar = dashboardControlbar(
  #   skin = "light",
  #   sliderInput(
  #     inputId = "controller",
  #     label = "Update the first tabset",
  #     min = 1,
  #     max = 6,
  #     value = 2
  #   )
  # ),
  footer = bs4DashFooter(left = HTML("Plataforma Gestión de Realidad Virtual"), 
                         right = paste0("Creado por Innovación y Desarrollo Tecnológico, MERC Consultora Ltda. Antofagasta, Chile - ", format(Sys.Date(), "%Y")))
)


# securing with Auth0
auth0::auth0_ui(ui, info = a0_info)
