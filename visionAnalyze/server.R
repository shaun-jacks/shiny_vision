server <- function(input, output, session) {
  values <- reactiveValues(upload_step = 1,
                           file_uploaded = F,
                           analysis_selected = F,
                           warning_inputs = "")
  
  #### When image has been uploaded ####
  observeEvent(input$file1, {
    values$file_uploaded = T
    values$file_path = input$file1$datapath # store uploaded image filepath
  })
  
  #### Store analysis type selected ####
  observeEvent(input$analysis_type, {
    if (input$analysis_type != "analysis_select") {
      values$analysis_selected = T
    } else if (input$analysis_type == "analysis_select") {
      values$analysis_selected = F
    }
    values$analysis_type = input$analysis_type
  })
  
  #### When Analyze button toggled ####
  observeEvent(input$analyze, {
    if (!((values$file_uploaded) & (values$analysis_selected))) {
      values$warning_inputs = "Must select an analysis and upload a file"
    } else {
      # Begin progress bar
      shiny::withProgress(message = "Obtaining Google Vision Analysis", value = 0.1, {
        
        shiny::incProgress(amount = 0, detail = "sending photo to Google Vision")
        # send to Google Vision
        values$img_res <- getGoogleVisionResponse(input$file1$datapath, feature = values$analysis_type)
        shiny::incProgress(amount = .8, detail = "Done")
        
      })
      # Increment step to render new UI
      values$upload_step = values$upload_step + 1
      
    }
  })
  
  
  #### Render Image with analysis results ####
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
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)
          shiny::incProgress(amount = 0.3)
          shiny::incProgress(amount = 0, detail = "Drawing landmark bounding boxes detected")
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
          shiny::incProgress(amount = 0.3, detail = "Capturing image and storing into magick image pointer")
          photo = magick::image_capture()
          shiny::incProgress(0.1)
          dev.off()
          # wtite magick object onto temp file used for image shiny output
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
          shiny::incProgress(0.1)
        }
        
        #############################
        #### Object Localization ####
        #############################
        
      } else if (values$analysis_type == "OBJECT_LOCALIZATION") {
        if (!is.null( values$img_res$boundingPoly$normalizedVertices)) {
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)
          shiny::incProgress(amount = 0.3)
          
          shiny::incProgress(amount = 0, detail = "Drawing bounding boxes detected for object localization")
          bounding_boxes <- values$img_res$boundingPoly$normalizedVertices
          mapply(x = bounding_boxes, y = values$img_res$name, function(x, y) {
            polygon(x = x[[1]]*width, # since vertices are normalized, multiply by width
                    y = x[[2]]*height, # since vertices are normalized, multiply by height
                    border = 'white', 
                    lwd = rescale_lwd)
            text(x = x[[1]][[1]]*width, 
                 y = x[[2]][[1]]*height, 
                 labels = as.character(y),
                 offset = 1,
                 cex = 3, 
                 col = "green")
          }) 
          
          shiny::incProgress(amount = 0.3, detail = "Capturing image and storing into magick image pointer")
          photo = magick::image_capture()
          shiny::incProgress(0.1)
          dev.off()
          # wtite magick object onto temp file used for image shiny output
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
          shiny::incProgress(0.1)
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", offset = 1, cex = 3)
        }
        
        ########################
        #### Text Detection ####
        ########################
        
      } else if (values$analysis_type == "TEXT_DETECTION") {
        if (!is.null(values$img_res$boundingPoly$vertices)) {
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)
          shiny::incProgress(amount = 0.3)
          
          shiny::incProgress(amount = 0, detail = "Drawing bounding boxes detected for text detection")
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
          
          shiny::incProgress(amount = 0.3, detail = "Capturing image and storing into magick image pointer")
          photo = magick::image_capture()
          shiny::incProgress(0.1)
          dev.off()
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
          shiny::incProgress(0.1)
          
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", font = 2, offset = 1, cex = 1)
        }
        
        ########################
        #### Logo Detection ####
        ########################
        
      } else if (values$analysis_type == "LOGO_DETECTION") {
        if (!is.null(values$img_res$boundingPoly$vertices)) {
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)
          shiny::incProgress(amount = 0.3)
          shiny::incProgress(amount = 0, detail = "Drawing bounding boxes detected for logo detection")
          
          bounding_boxes <- values$img_res$boundingPoly$vertices
          mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), 
                 function(x, y) {
                   polygon(x = x[[1]], 
                           y = x[[2]], 
                           border = 'green', 
                           lwd = rescale_lwd)
                   text(x = x[[1]][[1]], 
                        y = x[[2]][[1]], 
                        labels = as.character(y),
                        offset = 1,
                        cex = 3, 
                        col = "white")
                 }) 
          
          shiny::incProgress(amount = 0.3, detail = "Capturing image and storing into magick image pointer")
          # save output onto magick object
          photo = magick::image_capture()
          shiny::incProgress(0.1)
          dev.off()
          # wtite magick object onto temp file used for image shiny output
          tmpFile <- image_write(photo, tempfile(fileext='jpg'), format = 'jpg')
          shiny::incProgress(0.1)
          
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", font = 2, offset = 1, cex = 1)
        }
      }
      
    }) # end withProgress bar
    
    # src is image to output, and height is max height of image on shiny output
    list(src = tmpFile, height = 400)
    
  })
  
  # on click of restart button, reset all reactive values
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
  
  ### Body of image_analyze tab ###
  output$image_analyze <- renderUI({
    ### Image upload UI Step ###
    if (values$upload_step == 1) {
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          title = "Image and Analysis Selection",
          shiny::fileInput("file1", 
                           label = "Input Image",
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
      ### Image Analysis UI Step ###
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
  
  
  #### Analysis Table ####
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