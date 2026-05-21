#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
mytheme <- create_theme(
  bs4dash_layout(
    sidebar_width = "220px",
    main_bg = "#EFF3F6"
  ),
  bs4dash_sidebar_light(
    bg = "#FFFFFF",
    hover_bg = "#EFF3F6",
    header_color = "#FFFFFF",
    hover_color = "#000000",
    active_color = "#0A66C2"
  ),
  bs4dash_color(
    blue = "#EFF3F6"
  )
)


ui = dashboardPage(scrollToTop = T,
  dark = NULL,
  useShinyalert(),
  useShinyjs(),
  freshTheme = mytheme,
  header = dashboardHeader(set_html_attribs(title = "Capacitación 3D"), title = tags$img(src='images/kdmindustrial.jpg', height=72,width=220),
                           #title = tags$img(src='images/kdmindustrial.jpg', height=72,width=200),
                           titleWidth = "220px",
                           #tags$style(".layout-navbar-fixed .wrapper .sidebar-dark-primary .brand-link {Width:220px}"),
                           fixed = T,
                           rightUi = userOutput("user"),
                           leftUi = tags$li(class = "dropdown", span(h2(strong(textOutput("texto_saludo_inicio")))), style = "font-size:1.8rem;"),
                           tags$script(src = "set_html_attribs.js")
  ),
  sidebar = dashboardSidebar(status = "primary",
                             minified = F,
                             inputId = "sidebarState",
                             skin = "light",
                             #img(src='images/kdmindustrial.jpg', height=72,width=200),
                             #br(),
                             disconnectMessage(text = "TU sesion en la plataforma ha terminado. Presiona refrescar para iniciar nuevamente"),
                             sidebarMenu(
                               id = "sidebar",
                               menuItem(
                                 text = "Inicio",
                                 tabName = "tab1-inicio",
                                 icon = icon("bar-chart", lib = "font-awesome"),
                                 selected = T
                               ),
                               menuItem(
                                 text = "Inscripciones",
                                 tabName = "tab2",
                                 icon = icon("user-plus", lib = "font-awesome")
                               ),
                               menuItem(
                                 text = "Monitor",
                                 tabName = "tab3",
                                 icon = icon("traffic-light", lib = "font-awesome")
                               ),
                               menuItem(
                                 text = "Resultados",
                                 tabName = "tab4",
                                 icon = icon("gauge", lib = "font-awesome")
                               ),
                               menuItem(
                                 text = "Alertas",
                                 tabName = "tab10",
                                 icon = icon("triangle-exclamation", lib = "font-awesome")
                               ),
                               # menuItem(
                               #   text = "Estadisticas",
                               #   tabName = "tab5",
                               #   icon = icon("chart-simple", lib = "font-awesome")
                               # ),
                               menuItem(
                                 text = "Estado de Pago",
                                 tabName = "tab6",
                                 icon = icon("receipt", lib = "font-awesome")
                               ),
                               menuItem(
                                 text = "Facturas",
                                 tabName = "tab9",
                                 icon = icon("dollar", lib = "font-awesome")
                               ),
                               menuItem(
                                 text = "Soporte",
                                 tabName = "tab7",
                                 icon = icon("circle-info", lib = "font-awesome")
                               ),
                               menuItemOutput("menu_administracion")
                             )
  ),
  body = dashboardBody( useShinyjs(), tags$script(src = "https://kit.fontawesome.com/<you>.js"),
                        tags$style(HTML(".main-sidebar { font-size: 14px; }")),
                        
    tabItems(
      inicio_ui("inicio"),
      inscripcion_participantes_ui("inscripcion_participantes"),
      monitor_preparaciones_ui("monitor_preparaciones"),
      registro_resultados_ui("registro_resultados"),
      alertas_ui("alertas"),
      estadisticas_ui("estadisticas"),
      estado_de_pago_ui("estado_de_pago"),
      facturas_ui("facturas"),
      soporte_ui("soporte"),
      gestion_clientes_ui("gestion_clientes")
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
  footer = bs4DashFooter(left = "Plataforma Capacitación Competencias Laborales", right = "Desarrollado por el area de Sistemas de MERC consultora SpA. Antofagasta")
)


secure_ui(ui,
          sign_in_page_ui = my_custom_sign_in_page,
          custom_admin_button_ui = NULL,#admin_button_ui(align = "left", vertical_align = "bottom"),
          custom_admin_ui = admin_ui)
