header <- dashboardHeader(title = "Shiny Auth", uiOutput("logout_button"))
header$children[[2]]$children <-  tags$img(src='logo.png',height='40')


sidebar <- dashboardSidebar(collapsed = TRUE, sidebarMenuOutput("sidebar"))

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css"),
    tags$style(".table{margin: 0 auto;}")
  ),

  loginUI("login"),

  includeCSS("slider_theme.css"),
  
  pwcTheme,
  
  tabItems(
    tabItem(
      "data",
      uiOutput("data_tab")
    ),
    tabItem(
      "segments",
      uiOutput("segments_tab")
    )
  )
)



dashboardPage(
  skin = "black",
  header, 
  sidebar,
  body
)


