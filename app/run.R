run <- function(){
  shiny::runApp(
    shiny::shinyApp(
      ui = ui,
      server = server,
      enableBookmarking = 'server'
    ))
}