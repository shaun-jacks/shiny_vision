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
      shiny::withProgress(message = "Obtaining Google Vision Analysis", value = 0.1, {
        
        shiny::incProgress(amount = 0, detail = "sending photo to Google Vision")
        values$img_res <- getGoogleVisionResponse(input$file1$datapath, feature = values$analysis_type)
        shiny::incProgress(amount = .8, detail = "Done")
        
      })
      values$upload_step = values$upload_step + 1
      
    }
  })
  
  

  output$picture2 <- renderImage({
    
    shiny::withProgress(message = "Rendering Image", value = 0.1, {
      
      req(values$file_path)
      
      shiny::incProgress(amount = 0, detail = "Reading image and converting to image pointer with Magick")
      photo = magick::image_read(values$file_path)
      shiny::incProgress(0.1)
      # obtain photo dimensions
      photo_info = magick::image_info(photo)
      width = photo_info$width
      height = photo_info$height
      max_height <- 400
      rescale_lwd <- 2/max_height * height
 
      ########################
      #### FACE DETECTION ####
      ########################
      
      if (values$analysis_type == "FACE_DETECTION") {
        # if faces were detected: draw landmarks and bounding polygons
        if (!is.null(values$img_res$fdBoundingPoly$vertices)) {
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo) # draw photo with graphics device
          shiny::incProgress(amount = 0.3)
          shiny::incProgress(amount = 0, detail = "Drawing bounding boxes and facial landmarks")
          bounding_boxes = values$img_res$fdBoundingPoly$vertices # save list of bounding boxes
          points_lst = values$img_res$landmarks # save list of point locations of facial landmarks
          mapply(x = bounding_boxes, y = c(1:length(bounding_boxes)), z = points_lst, function(x, y, z) {
            polygon(x = x[[1]], y = x[[2]], border = 'red', lwd = rescale_lwd)
            text(x = x[[1]][[3]], y = x[[2]][[3]], 
                 labels = as.character(y), # label each image with an id that will be referenced in table
                 offset = 1,
                 cex = 3, # text magnified by this factor relative to default
                 col = "white")
            points(x = z$position$x, # plot the x coordinates of facial landmarks
                   y = z$position$y, # plot the y coordinates of facial landmarks
                   col = "green", 
                   lwd = rescale_lwd, 
                   pch = 3)
          })
          shiny::incProgress(amount = 0.3, detail = "Capturing image and storing into magick image pointer")
          photo = magick::image_capture() # This saves the graphics output into a magick image object
          shiny::incProgress(0.1)
          dev.off()
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg') # tmpfile for output of image to shiny
          shiny::incProgress(0.1)
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red",  offset = 1, cex = 4)
        }
      
      ############################
      #### Landmark Detection ####
      ############################
      
      } else if (values$analysis_type == "LANDMARK_DETECTION") {
        # If no features detected
        if(is.null(values$img_res$boundingPoly$vertices)) {
          text(x = width/2, y = height/2, labels = "No features detected", col = "red", font = 2,
               offset = 1,
               cex = 1)
        } else {
          # draw magick object with graphics device
          magick::image_draw(photo)
          # save bounding boxes, and vector of descriptions and predictions
          bounding_boxes = values$img_res$boundingPoly$vertices
          name_land = values$img_res$description
          pred_score = values$img_res$score
          # output bounding boxes, descriptions, and predictions, with graphics
          mapply(vertice = bounding_boxes, name_land = name_land, pred_score = pred_score,
                 function(vertice, name_land, pred_score) {
                   polygon(x = vertice[[1]], y = vertice[[2]], border = 'red', lwd = rescale_lwd)
                   text(x = vertice[[1]][[3]], y = vertice[[2]][[3]], 
                        labels = paste(as.character(name_land), as.character(pred_score)),
                        offset = 1,
                        cex = 5, 
                        col = "green")
                 })
          # save output onto magick object
          photo = magick::image_capture()
          dev.off()
          # wtite magick object onto temp file used for image shiny output
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
        }
      
      #############################
      #### Object Localization ####
      #############################
        
      } else if (values$analysis_type == "OBJECT_LOCALIZATION") {
        if (!is.null( values$img_res$boundingPoly$normalizedVertices)) {
          magick::image_draw(photo)
          
          bounding_boxes <- values$img_res$boundingPoly$normalizedVertices
          mapply(x = bounding_boxes, y = values$img_res$name, function(x, y) {
            polygon(x = x[[1]]*width, 
                    y = x[[2]]*height, 
                    border = 'white', 
                    lwd = rescale_lwd)
            text(x = x[[1]][[3]]*width, 
                 y = x[[2]][[3]]*height, 
                 labels = as.character(y),
                 offset = 1,
                 cex = 3, 
                 col = "green")
          }) 
          photo = magick::image_capture()
          dev.off()
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", font = 2, offset = 1, cex = 1)
        }
        
      ########################
      #### Text Detection ####
      ########################
        
      } else if (values$analysis_type == "TEXT_DETECTION") {
        if (!is.null(values$img_res$boundingPoly$vertices)) {
          magick::image_draw(photo)
          
          bounding_boxes <- values$img_res$boundingPoly$vertices
          mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), function(x, y) {
            polygon(x = x[[1]], 
                    y = x[[2]], 
                    border = 'green', 
                    lwd = rescale_lwd)
            text(x = x[[1]][[3]], 
                 y = x[[2]][[3]], 
                 labels = as.character(y),
                 offset = 1,
                 cex = 1, 
                 font = rescale_lwd * 6,
                 col = "white")
          }) 
          photo = magick::image_capture()
          dev.off()
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", font = 2, offset = 1, cex = 1)
        }
        
      ########################
      #### Logo Detection ####
      ########################
        
      } else if (values$analysis_type == "LOGO_DETECTION") {
        if (!is.null(values$img_res$boundingPoly$vertices)) {
          magick::image_draw(photo)
          
          bounding_boxes <- values$img_res$boundingPoly$vertices
          mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), function(x, y) {
            polygon(x = x[[1]], 
                    y = x[[2]], 
                    border = 'green', 
                    lwd = rescale_lwd)
            text(x = x[[1]][[3]], 
                 y = x[[2]][[3]], 
                 labels = as.character(y),
                 offset = 1,
                 cex = 3, 
                 col = "white")
          }) 
          photo = magick::image_capture()
          dev.off()
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", font = 2, offset = 1, cex = 1)
        }
      }
      
    })
    
   
    list(src = tmpFile, height = 400)
    
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
            shiny::fileInput("file1", label = "Input Image",
                             accept = c(
                               "image/png",
                               "image/jpeg"
                             )),
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
          shiny::fluidRow(
            column(8, align = "center", 
                   shinycssloaders::withSpinner(
                     imageOutput("picture2"),
                     color = '#999999'
                   )
            )
          ),
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