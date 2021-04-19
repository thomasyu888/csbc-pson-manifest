#' quickview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quickview_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Portal Quickview"),
    p("This page will display selected columns of the latest 10 rows
         of each portal table. For a more in-depth look, go ", 
      a(href = "https://www.synapse.org/#!Synapse:syn7080714/tables/",
        target = "_blank",
        "here"),
      "."),
    tabBox(
      id = "quickview_tabs",
      width = "100%",
      tabPanel(
        "Grants",
        style = "height:68vh; overflow-y:scroll; overflow-x:scroll;",
        DT::DTOutput(ns("grants_table"))
      ),
      tabPanel(
        "Publications",
        style = "height:68vh; overflow-y:scroll; overflow-x:scroll;",
        DT::DTOutput(ns("pubs_table"))
      ),
      tabPanel(
        "Datasets",
        style = "height:68vh; overflow-y:scroll; overflow-x:scroll;",
        DT::DTOutput(ns("datasets_table"))
      ),
      tabPanel(
        "Files",
        style = "height:68vh; overflow-y:scroll; overflow-x:scroll;",
        DT::DTOutput(ns("files_table"))
      ),
      tabPanel(
        "Tools",
        style = "height:68vh; overflow-y:scroll; overflow-x:scroll;",
        DT::DTOutput(ns("tools_table"))
      )
    )
  )
}
    
#' quickview Server Function
#'
#' @noRd 
mod_quickview_server <- function(input, output, session, values){
  ns <- session$ns
  output$grants_table <- DT::renderDT(
    tail(isolate(values$tables)$grants, 10),
    options = list(dom = 't'), rownames = FALSE
  )
  output$pubs_table <- DT::renderDT(
    tail(isolate(values$tables)$publications, 10),
    options = list(dom = 't'), rownames = FALSE
  )
  output$datasets_table <- DT::renderDT(
    tail(isolate(values$tables)$datasets, 10),
    options = list(dom = 't'), rownames = FALSE
  )
  output$files_table <- DT::renderDT(
    tail(isolate(values$tables)$files, 10),
    options = list(dom = 't'), rownames = FALSE
  )
  output$tools_table <- DT::renderDT(
    tail(isolate(values$tables)$tools, 10),
    options = list(dom = 't'), rownames = FALSE
  )
}
    
## To be copied in the UI
# mod_quickview_ui("quickview_ui_1")
    
## To be copied in the server
# callModule(mod_quickview_server, "quickview_ui_1")
 
