server <- function(input, output, session) {
  values <- reactiveValues(analysis_stage = 1,
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
      shiny::withProgress(message = "Obtaining Google Vision Analysis", value = 0.1, {
        
        shiny::incProgress(amount = 0, detail = "sending photo to Google Vision")
        # send to Google Vision
        values$img_res <- getGoogleVisionResponse(
          values$file_path, 
          feature = values$analysis_type
        )
        
        shiny::incProgress(amount = .8, detail = "Done")
        
      })
      # Increment step to render new UI
      values$analysis_stage = values$analysis_stage + 1
      
    }
  })
  
  
  #### Render Image with analysis results ####
  output$picture2 <- renderImage({
    
    shiny::withProgress(message = "Rendering Image", value = 0.1, {
      
      req(values$file_path)
      
      shiny::incProgress(
        amount = 0, 
        detail = "Reading image and converting to image pointer with Magick"
      )
      
      photo = magick::image_read(values$file_path)
      shiny::incProgress(0.1)
      # obtain photo dimensions
      photo_info = magick::image_info(photo)
      width = photo_info$width
      height = photo_info$height
      max_height <- 400
      rescale_lwd <- 2/max_height * height

      shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
      magick::image_draw(photo)  # draw photo with graphics device
      shiny::incProgress(amount = 0.3)
      
      ########################
      #### FACE DETECTION ####
      ########################
      
      if (values$analysis_type == "FACE_DETECTION") {
        face_detected <- !is.null(values$img_res$fdBoundingPoly$vertices)
        if (face_detected) {
          shiny::incProgress(
            amount = 0, 
            detail = "Drawing bounding boxes and facial landmarks"
          )
          bounding_boxes = values$img_res$fdBoundingPoly$vertices  # save list of bounding boxes
          points_lst = values$img_res$landmarks  # save list of point locations of facial landmarks
          mapply(x = bounding_boxes, 
                 y = c(1:length(bounding_boxes)), 
                 z = points_lst, 
                 function(x, y, z) {
            polygon(x = x[[1]], y = x[[2]], border = 'red', lwd = rescale_lwd)
                   # label each image as referenced in table
                   text(x = x[[1]][[3]], y = x[[2]][[3]], 
                        labels = as.character(y),
                        offset = 1,
                        cex = 3,  # text magnified by this factor relative to default
                        col = "white")
                   points(x = z$position$x,  # plot the x coordinates of facial landmarks
                          y = z$position$y,  # plot the y coordinates of facial landmarks
                          col = "green", 
                          lwd = rescale_lwd, 
                          pch = 3)
          })
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red",  offset = 1, cex = 5)
        }
        
        ############################
        #### Landmark Detection ####
        ############################
        
      } else if (values$analysis_type == "LANDMARK_DETECTION") {
        # If no features detected
        landmark_detected <- !is.null(values$img_res$boundingPoly$vertices)
        if(!landmark_detected) {
          text(x = width/2, y = height/2, labels = "No features detected",
               col = "red", offset = 1, cex = 5)
          
        } else {
          shiny::incProgress(
            amount = 0, 
            detail = "Drawing landmark bounding boxes detected"
          )
          # save bounding boxes, and vector of descriptions and predictions
          bounding_boxes = values$img_res$boundingPoly$vertices
          name_land = values$img_res$description
          pred_score = values$img_res$score
          # output bounding boxes, descriptions, and predictions, with graphics
          mapply(vertice = bounding_boxes,
                 name_land = name_land, 
                 pred_score = pred_score,
                 function(vertice, name_land, pred_score) {
                   polygon(x = vertice[[1]], y = vertice[[2]], border = 'red', 
                           lwd = rescale_lwd)
                   text(x = vertice[[1]][[3]], y = vertice[[2]][[3]], 
                        labels = paste(as.character(name_land), 
                                       as.character(pred_score)),
                        offset = 1,
                        cex = 5, 
                        col = "green")
                 })
        }
        
        #############################
        #### Object Localization ####
        #############################
        
      } else if (values$analysis_type == "OBJECT_LOCALIZATION") {
        
        objects_detected <- !is.null( values$img_res$boundingPoly$normalizedVertices)
        if (objects_detected) {
          shiny::incProgress(
            amount = 0, 
            detail = "Drawing bounding boxes detected for object localization"
          )
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
          
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", offset = 1, cex = 5)
        }
        
        ########################
        #### Text Detection ####
        ########################
        
      } else if (values$analysis_type == "TEXT_DETECTION") {
        
        text_detected <- !is.null(values$img_res$boundingPoly$vertices)
        if (text_detected) {
          shiny::incProgress(
            amount = 0, 
            detail = "Drawing bounding boxes detected for text detection"
          )
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
          
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", offset = 1, cex = 5)
        }
        
        ########################
        #### Logo Detection ####
        ########################
        
      } else if (values$analysis_type == "LOGO_DETECTION") {
        
        logo_detected <- !is.null(values$img_res$boundingPoly$vertices)
        if (logo_detected) {
          shiny::incProgress(
            amount = 0, 
            detail = "Drawing bounding boxes detected for logo detection"
          )
          
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
        } else {
          text(x = width/2, y = height/2, labels = "No features detected", 
               col = "red", offset = 1, cex = 5)
        }
      }
      
      shiny::incProgress(
        amount = 0.3, 
        detail = "Capturing image and storing into magick image pointer"
      )
      # Save the graphics output into a magick image object
      photo = magick::image_capture()  
      shiny::incProgress(0.1)
      dev.off()
      
      tmpFile <- image_write(
                   photo, 
                   tempfile(fileext='jpg'),
                   format = 'jpg'
                 )  # tmpfile for output of image to shiny
    
      shiny::incProgress(0.1)
      
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
    values$analysis_stage = 1
  })
  
  ### Body of image_analyze tab ###
  ### Body of image_analyze tab ###
  output$image_analyze <- renderUI({
    ### Image upload UI Step ###
    if (values$analysis_stage == 1) {
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          title = "Image and Analysis Selection",
          # ask for file input from user
          shiny::fileInput(
            inputId = "file1",
            label   = "Input Image",
            accept  = c(
              "image/png",
              "image/jpeg"
            )
          ),
          # provide dropdown menu to select analysis type
          shiny::selectInput(
            inputId = "analysis_type",
            label   = "Analysis Type",
            choices = c(  # key (seen from user): value (for code)
              "Select Analysis"     = "analysis_select",
              "Facial Detection"    = "FACE_DETECTION",
              "Label Detection"     = "LABEL_DETECTION",
              "Landmark Detection"  = "LANDMARK DETECTION",
              "Logo Detection"      = "LOGO_DETECTION",
              "Object Localization" = "OBJECT_LOCALIZATION",
              "Text Detection"      = "TEXT_DETECTION"
            )
          ),
          # Allow for user to choose when to analyze with button
          shiny::actionButton("analyze", "Analyze!"),
          br(),
          # If file not uploaded or analysis type not selected, warning
          values$warning_inputs
        ),
        shiny::mainPanel(
          # do not need anything in here
        )
      )
      ### Image Analysis UI Step ###
    } else if (values$analysis_stage == 2) {
      # stage 2 within our renderUI function
      shiny::fluidPage(
        shiny::tagList(
          # Row 1: our analyzed image
          shiny::fluidRow(
            column(8, align = "center", 
                   shinycssloaders::withSpinner(  # Spinner loading bar
                     imageOutput("picture2"),  # Outputted Image
                     color = '#999999'
                   )
            )
          ),
          br(),
          br(),
          # Row 2: our table results
          shiny::fluidRow(
            column(8, align = "center",
                   shinydashboard::box(
                     width=12, 
                     title = "Analysis Results",
                     solidHeader = TRUE,
                     DT::dataTableOutput("results"),  # Outputted table
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = "primary"
                   )
            )
          ),
          br(),
          br(),
          # Row three: our restart button
          shiny::fluidRow(
            column(8, align = "center",
                   shiny::actionButton("reset", "Restart") 
            )
          )
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
  }, options = list(scrollX = TRUE, searching = TRUE, scrollY = "600px", 
                    scrollCollapse = TRUE, dom = 't'))
  
}