server <- function(input, output, session) {
  values <- reactiveValues(upload_step = 1,
                           file_uploaded = F,
                           analysis_selected = F,
                           warning_inputs = "")
  
  
  observeEvent(input$file1, {
    values$file_uploaded = T
    values$file_path = input$file1$datapath
  })
  
  observeEvent(input$analysis_type, {
    if (input$analysis_type != "analysis_select") {
      values$analysis_selected = T
    } else if (input$analysis_type == "analysis_select") {
      values$analysis_selected = F
    }
    values$analysis_type = input$analysis_type
  })
  
  observeEvent(input$analyze, {
    if (!((values$file_uploaded) & (values$analysis_selected))) {
      values$warning_inputs = "Must select an analysis and upload a file"
    } else {
      cat(values$analysis_type)
      values$img_res <- getGoogleVisionResponse(input$file1$datapath, feature = values$analysis_type)
      values$upload_step = values$upload_step + 1
      
    }
  })
  
  
  #photo <- magick::image_read(input$file1$datapath)
  
  output$picture2 <- renderImage({
    req(values$file_path)
    photo = image_convert(magick::image_read(values$file_path), "jpg")
    photo_info = magick::image_info(photo)
    width = photo_info$width
    height = photo_info$height
    if (values$analysis_type == "FACE_DETECTION") {
      if (!is.null(values$img_res$fdBoundingPoly$vertices)) {
        magick::image_draw(photo)
        bounding_boxes = values$img_res$fdBoundingPoly$vertices
        points_lst = values$img_res$landmarks
        mapply(x = bounding_boxes, y = c(1:length(bounding_boxes)), z = points_lst, function(x, y, z) {
          polygon(x = x[[1]], y = x[[2]], border = 'red', lwd = 4*height/400)
          text(x = x[[1]][[3]], y = x[[2]][[3]], 
               labels = as.character(y), 
               offset = 1,
               cex = 1, 
               font = 2,
               col = "green")
          points(x = z$position$x, y = z$position$y, col = "green", lwd = height/300, pch = 3)
        })
        photo = magick::image_capture()
        dev.off()
        photo = magick::image_resize(photo, "500x500")
        tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
      } else {
        text(x = width/2, y = height/2, labels = "No features detected", 
             col = "red", font = 2, offset = 1, cex = 1)
      }

    } else if (values$analysis_type == "LANDMARK_DETECTION") {
      if(is.null(values$img_res$boundingPoly$vertices)) {
        text(x = width/2, y = height/2, labels = "No features detected", col = "red", font = 2,
             offset = 1,
             cex = 1)
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
      if (!is.null( values$img_res$boundingPoly$normalizedVertices)) {
        bounding_boxes <- values$img_res$boundingPoly$normalizedVertices
        mapply(x = bounding_boxes, y = values$img_res$name, function(x, y) {
          polygon(x = x[[1]]*width, 
                  y = x[[2]]*height, 
                  border = 'white', 
                  lwd = 2)
          text(x = x[[1]][[3]]*width, 
               y = x[[2]][[3]]*height, 
               labels = as.character(y),
               offset = 1,
               cex = 1, 
               font = 2,
               col = "green")
        }) 
      } else {
        text(x = width/2, y = height/2, labels = "No features detected", 
             col = "red", font = 2, offset = 1, cex = 1)
      }
      
    } else if (values$analysis_type == "TEXT_DETECTION") {
      if (!is.null(values$img_res$boundingPoly$vertices)) {
        bounding_boxes <- values$img_res$boundingPoly$vertices
        mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), function(x, y) {
          polygon(x = x[[1]], 
                  y = x[[2]], 
                  border = 'green', 
                  lwd = 1)
          text(x = x[[1]][[3]], 
               y = x[[2]][[3]], 
               labels = as.character(y),
               offset = 1,
               cex = 1, 
               font = 2,
               col = "white")
        }) 
      } else {
        text(x = width/2, y = height/2, labels = "No features detected", 
             col = "red", font = 2, offset = 1, cex = 1)
      }
    } else if (values$analysis_type == "LOGO_DETECTION") {
      if (!is.null(values$img_res$boundingPoly$vertices)) {
        bounding_boxes <- values$img_res$boundingPoly$vertices
        mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), function(x, y) {
          polygon(x = x[[1]], 
                  y = x[[2]], 
                  border = 'green', 
                  lwd = 1)
          text(x = x[[1]][[3]], 
               y = x[[2]][[3]], 
               labels = as.character(y),
               offset = 1,
               cex = 1, 
               font = 2,
               col = "white")
        }) 
      } else {
        text(x = width/2, y = height/2, labels = "No features detected", 
             col = "red", font = 2, offset = 1, cex = 1)
      }
    }
    
    list(src = tmpFile)
    
  })

  
  observeEvent(input$reset, {
    values$img_res = NULL
    values$file_path = ""
    photo = NULL
    values$analysis_type = ""
    values$analysis_selected = F
    values$warning_inputs = ""
    values$file_uploaded = F
    values$upload_step = 1
  })
  
  output$image_analyze <- renderUI({
    if (values$upload_step == 1) {
      shiny::sidebarLayout(
        shiny::sidebarPanel(
            title = "Image and Analysis Selection",
            shiny::fileInput("file1", label = "Input Image"),
            selectInput("analysis_type", "Analysis Type", 
                        c("Select Analysis" = "analysis_select",
                          "Facial Detection" = "FACE_DETECTION",
                          "Label Detection" = "LABEL_DETECTION",
                          "Landmark Detection" = "LANDMARK_DETECTION",
                          "Logo Detection" = "LOGO_DETECTION",
                          "Object Localization" = "OBJECT_LOCALIZATION",
                          "Text Detection" = "TEXT_DETECTION")
            ),
            actionButton("analyze", "Analyze!"),
            br(),
            values$warning_inputs
          ),
        shiny::mainPanel(
        )
      )
    } else if (values$upload_step == 2) {
      shiny::fluidPage(
        shiny::tagList(
          shiny::fluidRow(imageOutput("picture2")),
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
          ),
          br(),
          br(),
          actionButton("reset", "Restart")      
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
    } else if (values$analysis_type == "TEXT_DETECTION") {
      as.data.frame(
        list(
          Language = values$img_res$locale,
          Words = values$img_res$description
        )
      )
    } else if (values$analysis_type == "LOGO_DETECTION") {
      as.data.frame(
        list(
          Logo = values$img_res$description,
          Score = values$img_res$score
        )
      )
    }
  }, options = list(scrollX = TRUE, searching = TRUE, scrollY = "600px", scrollCollapse = TRUE, dom = 't'))
  
}

# FACE_DETECTION, LANDMARK_DETECTION, LOGO_DETECTION, LABEL_DETECTION, TEXT_DETECTION, IMAGE_PROPERTIES, SAFE_SEARCH_DETECTION