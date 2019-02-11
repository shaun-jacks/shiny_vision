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
                                shiny::sidebarLayout(
                                  shiny::sidebarPanel(
                                 
                                      title = "Image and Analysis Selection",
                                      shiny::fileInput("file1", label = "Input Image"),
                                      selectInput("analysis_type", "Analysis Type", 
                                                  c("Facial Detection" = "FACE_DETECTION",
                                                    "Label Detection" = "LABEL_DETECTION",
                                                    "Landmark Detection" = "LANDMARK_DETECTION",
                                                    "Logo Detection" = "LOGO_DETECTION",
                                                    "Object Localization" = "OBJECT_LOCALIZATION",
                                                    "Text Detection" = "TEXT_DETECTION")
                                      ),
                                      actionButton("analyze", "Analyze!")
                                    
                                  ),
                                  shiny::mainPanel(
                                    uiOutput("image_results")
                                    )
                                  )
                                )
      )
    )
  )
}