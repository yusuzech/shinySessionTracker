header <- dashboardHeader(
    title = "shinySessionTracker"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Home",tabName = "home"),
        menuItem(text = "Home",tabName = "playground")
    )
)

body <- dashboardBody(
    
)


ui <- dashboardPage( 
    header = header,
    sidebar =  sidebar,
    body = body
)
