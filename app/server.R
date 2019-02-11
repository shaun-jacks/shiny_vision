server <- function(input, output, session) {
  values <- reactiveValues(upload_step = 1,
                           file_uploaded = F,
                           analysis_selected = F)
  
  
  observeEvent(input$file1, {
    values$file_uploaded = T
    values$file_path = input$file1$datapath
  })
  
  observeEvent(input$analysis_type, {
    values$analysis_selected = T
    values$analysis_type = input$analysis_type
  })
  
  observeEvent(input$analyze, {
    if (!((values$file_uploaded) & (values$analysis_selected)))
      return(NULL)
    else {
      cat(values$analysis_type)
      values$img_res <- getGoogleVisionResponse(input$file1$datapath, feature = values$analysis_type)
      values$upload_step = values$upload_step + 1
      
    }
  })
  
  
  #photo <- magick::image_read(input$file1$datapath)
  
  output$picture2 <- renderPlot({
    photo = image_convert(magick::image_read(values$file_path), "jpg")
    photo = as_EBImage(photo)
    width = dim(photo)[1]
    height = dim(photo)[2]
    graphics::plot(photo, xaxs="i", yaxs="i")
    if (values$analysis_type == "FACE_DETECTION") {
      bounding_boxes = values$img_res$fdBoundingPoly$vertices
      mapply(x = bounding_boxes, y = c(1:length(bounding_boxes)),  function(x, y) {
        polygon(x = x[[1]], y = x[[2]], border = 'red', lwd = 4)
        text(x = x[[1]][[3]], y = x[[2]][[3]], 
             labels = as.character(y), 
             offset = 1,
             cex = 1, 
             font = 2,
             col = "green")
      })
    } else if (values$analysis_type == "LANDMARK_DETECTION") {
      if("No features detected!" %in% as.character(o)) {
        
      } else {
        bounding_boxes = values$img_res$boundingPoly$vertices
        name_land = values$img_res$description
        pred_score = values$img_res$score
        mapply(vertice = bounding_boxes, name_land = name_land, pred_score = pred_score,
               function(vertice, name_land, pred_score) {
          polygon(x = vertice[[1]], y = vertice[[2]], border = 'red', lwd = 4)
          text(x = vertice[[1]][[3]], y = vertice[[2]][[3]], 
               labels = paste(as.character(name_land), as.character(pred_score)),
               offset = 1,
               cex = 1, 
               font = 2,
               col = "green")
        })
      }
      
    } else if (values$analysis_type == "OBJECT_LOCALIZATION") {
        bounding_boxes <- values$img_res$boundingPoly$normalizedVertices
        mapply(x = bounding_boxes, y = values$img_res$name, function(x, y) {
          polygon(x = x[[1]]*width, 
                  y = x[[2]]*height, 
                  border = 'white', 
                  lwd = 4)
          text(x = x[[1]][[3]]*width, 
               y = x[[2]][[3]]*height, 
               labels = as.character(y),
               offset = 1,
               cex = 1, 
               font = 2,
               col = "green")
        }) 
      
    }
    
    
  })
  
  output$image_results <- renderUI({
    if (values$upload_step == 2) {
      shiny::tagList(
        shiny::fluidRow(plotOutput("picture2")),
        br(),
        br(),
        shinydashboard::box(
          width=12, 
          title = "Analysis Results",
          solidHeader = TRUE,
          DT::dataTableOutput("results"),
          collapsible = TRUE,
          collapsed = FALSE,
          status = "primary"
        )
      )
    }
  })
  
  output$results <- DT::renderDataTable({
    if (values$analysis_type == "FACE_DETECTION") {
      as.data.frame(
        list(
          joy = values$img_res$joyLikelihood, 
          sorrow = values$img_res$sorrowLikelihood,
          anger = values$img_res$angerLikelihood, 
          surprise = values$img_res$surpriseLikelihood
          )
        )
    } else if (values$analysis_type == "OBJECT_LOCALIZATION") {
      as.data.frame(
        list(
          `Object Detected` = values$img_res$name,
          Score = values$img_res$score
          )
        )
    } else if (values$analysis_type == "LANDMARK_DETECTION") {
      as.data.frame(
        list(
          Landmark = values$img_res$description, 
          Score = values$img_res$score
          )
        )
    } else if (values$analysis_type == "LABEL_DETECTION") {
      as.data.frame(
        list(
          Description = values$img_res$description,
          Score = values$img_res$score,
          topicality = values$img_res$topicality
          )
        )
    }
  }, options = list(scrollX = TRUE, searching = TRUE, scrollY = "600px", scrollCollapse = TRUE, dom = 't'))
  
}