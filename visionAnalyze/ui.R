ui <- function(request){
  
  
  
  shinydashboard::dashboardPage(
    
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML(paste0(strong('Vision Analyze'))),
      titleWidth=200),
    
    ##### Sidebar #####
    sidebar = shinydashboard::dashboardSidebar(
      width = 200,
      shinydashboard::sidebarMenu(
        id = 'id_sidebar',
        
        shinydashboard::menuItem(text = 'Analyze Image',
                                 tabName = 'analyze_image',
                                 icon = shiny::icon('binoculars'))
        
      )
      
    ),
    
    ##### Body #####
    body = shinydashboard::dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "custom.css"
        )
      ),
      shinydashboard::tabItems(
        ### Body: Data Select ###                
        shinydashboard::tabItem("analyze_image", 
                                uiOutput("image_analyze")
        )
      )
    )
  )
}