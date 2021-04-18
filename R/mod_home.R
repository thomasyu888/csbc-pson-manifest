#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("CSBC/PS-ON Manifest Curator", align = "center"),
    h4("Add metadata to the Cancer Complexity Knowledge Portal",
       align = "center"),
    br(),
    h4(strong("Overview")),
    p("So far, we have annotated..."),
    fluidRow(
      #valueBoxOutput("num_grants", width = 3),
      valueBoxOutput(ns("num_pubs"), width = 3),
      valueBoxOutput(ns("num_datasets"), width = 3),
      valueBoxOutput(ns("num_files"), width = 3),
      valueBoxOutput(ns("num_tools"), width = 3),
    ),
    p("Let's annotate even more! Go to ", strong("Validate and Upload"),
      " to start.")
  )
}


#' home Server Function
#'
#' @noRd 
mod_home_server <- function(input, output, session, values){
  ns <- session$ns
  # tables <- reactive({
  #   get_tables()
  # })
  # tables <<- get_tables()
  output$num_pubs <- shinydashboard::renderValueBox({
    tables <- isolate(values$tables)
    shinydashboard::valueBox(
      formatC(nrow(tables$publications), format = "d", big.mark = ","),
      "Publications",
      icon = icon("book-open"),
      color = "maroon"
    )
  })
  output$num_datasets <- renderValueBox({
    tables <- isolate(values$tables)
    valueBox(
      nrow(tables$datasets), "Datasets",
      icon = icon("cubes"), color = "olive"
    )
  })
  output$num_files <- renderValueBox({
    tables <- isolate(values$tables)
    valueBox(
      formatC(nrow(tables$files), format = "d", big.mark = ","),
      "Data files",
      icon = icon("file"), color = "yellow"
    )
  })
  output$num_tools <- renderValueBox({
    tables <- isolate(values$tables)
    valueBox(
      nrow(tables$tools), "Tools",
      icon = icon("tools"), color = "light-blue"
    )
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# callModule(mod_home_server, "home_ui_1")
 
