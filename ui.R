###   UI

dashboardPage(skin = "black",
              title = "OWEN Wockhardt",
              #Define Dashboard Header
              dashboardHeader(title = tags$a(href='https://www.i-cube.in/OWEN',
                                             tags$img(src="OWEN_Logo.png")),
                              titleWidth = 260),
              
              #Define Dashboard Sidebar
              dashboardSidebar(
                width = 260,
                uiOutput("sidebarpanel")
              ),
              
              #Define Dashboard Body
              dashboardBody(
                tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                useShinyjs(),
                extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("dashboardBody").addClass("sidebar-collapse") }'),
                extendShinyjs(text = 'shinyjs.showSidebar = function(params) { $("dashboardBody").removeClass("sidebar-collapse") }'),
                uiOutput("dashboardBody")
              )
)