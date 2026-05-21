if (interactive()) {
  library(shiny)
  library(bs4Dash)
  
  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "Capacitacion3d",
                               leftUi = tags$li(class="navbar-collapse collapse dropdown",
                                                tags$ul(class="nav navbar-nav sidebar-menu",
                                                        bs4SidebarMenuItem("Page 1", tabName="page-1", selected=TRUE),
                                                        bs4SidebarMenuItem("Page 2", tabName="page-2"),
                                                        bs4SidebarMenuItem("Page 3", tabName="page-3")
                                                )
                               )
      ),
      sidebar = dashboardSidebar(disable = TRUE),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      title = "Handle tabs",
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "page-1",
                  h1("Welcome!")),
          tabItem(tabName = "page-2",
                  h1("Good bye!")),
          tabItem(tabName = "page-3",
                  h1("Not again!"))
        ),
        actionButton("add", "Add 'Dynamic' tab"),
        actionButton("remove", "Remove 'Foo' tab"),
        actionButton("hideTab", "Hide 'Foo' tab"),
        actionButton("showTab", "Show 'Foo' tab"),
        br(), br(),
        tabBox(
          id = "tabs",
          title = "A card with tabs",
          selected = "Bar",
          status = "primary",
          solidHeader = FALSE, 
          type = "tabs",
          tabPanel("Hello", "This is the hello tab"),
          tabPanel("Foo", "This is the foo tab"),
          tabPanel("Bar", "This is the bar tab")
        )
      )
    ),
    server = function(input, output, session) {
      observeEvent(input$add, {
        insertTab(
          inputId = "tabs",
          tabPanel("Dynamic", "This a dynamically-added tab"),
          target = "Bar",
          select = TRUE
        )
      })
      
      observeEvent(input$remove, {
        removeTab(inputId = "tabs", target = "Foo")
      })
      
      observeEvent(input$hideTab, {
        hideTab(inputId = "tabs", target = "Foo")
      })
      
      observeEvent(input$showTab, {
        showTab(inputId = "tabs", target = "Foo")
      })
    }
  )
}


# bs4DashNavbar()
# =============================

library(shiny)
library(bs4Dash)
library(fresh)

navbarTab <- function(tabName, ..., icon = NULL) {
  tags$li(
    class = "nav-item",
    tags$a(
      class = "nav-link",
      id = paste0("tab-", tabName),
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      icon,
      tags$p(...)
    )
  )
}


navbarMenu <- function(..., id = NULL) {
  if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  
  tags$ul(
    class = "navbar-nav dropdown", 
    role = "menu",
    id = "sidebar-menu",
    ...,
    div(
      id = id,
      class = "sidebarMenuSelectedTabItem",
      `data-value` = "null",
      
    )
  )
}

shinyApp(
  ui = dashboardPage(title = "Capacitacion3d",
    freshTheme = create_theme(
      bs4dash_vars("navbar-light-active-color" = "purple"),
      bs4dash_status(primary = "#414769")#"#5E81AC")
    ),
    header = dashboardHeader(title = "Capacitacion3d", rightUi = userOutput("user"), status = "primary",
      navbarMenu(
        navbarTab(tabName = "Tab1", "Inicio"),
        navbarTab(tabName = "Tab2", "Inscripciones"),
        navbarTab(tabName = "Tab2", "Monitor"),
        navbarTab(tabName = "Tab2", "Resultados"),
        navbarTab(tabName = "Tab2", "Estadisticas"),
        navbarTab(tabName = "Tab2", "Estados de Pago")
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "Tab1",
          h1("Inicio")
        ),
        tabItem(
          tabName = "Tab2",
          h1("Inscripciones")
        )
      )
    ),
    sidebar = dashboardSidebar(disable = TRUE)
  ),
  server = function(input, output, session) {
    
    output$user <- renderUser({
      dashboardUser(
        name = "Divad Nojnarg",
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        title = "shinydashboardPlus",
        subtitle = "Author",
        footer = p("The footer", class = "text-center"),
        fluidRow(
          dashboardUserItem(
            width = 6,
            "Item 1"
          ),
          dashboardUserItem(
            width = 6,
            "Item 2"
          )
        )
      )
    })
    
  }
)
