ui <- function(request){
  
  
  
  shinydashboard::dashboardPage(
    ### changing theme
    
    # shinyDashboardThemes(
    #  theme = "grey_dark"
    #),
    dashboardthemes::shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    header = shinydashboard::dashboardHeader(
      title = shiny::HTML(paste0(strong('Google Vision R'))),
      titleWidth=250),
    
    ##### Sidebar #####
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = 'id_sidebar',
        
        shinydashboard::menuItem(text = 'Anaylze Image',
                                 tabName = 'analyze_image',
                                 icon = shiny::icon('binoculars'))
        
      )
      
    ),
    
    ##### Body #####
    body = shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        ### Body: Data Select ###                
        shinydashboard::tabItem("analyze_image", 
                                uiOutput("image_analyze")
                                )
      )
    )
  )
}