#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinyBS
#' @import reticulate
#' @import dplyr
#' @import tibble
#' @import waiter
#' @import dccvalidator
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  #Add synapse login
  session$sendCustomMessage(type = "readCookie", message = list())
  source_python("R/synapse_funcs.py")
  
  syn <- .GlobalEnv$synapseclient$Synapse()
  values <- reactiveValues()
  
  observeEvent(input$cookie, {

    if (input$cookie == "unauthorized") {
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png",
              height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    } else {
      ### login and update session; otherwise, notify to login to Synapse first
      tryCatch({
        syn$login(sessionToken = input$cookie)
        values$tables <- get_tables(syn)
        # get controlled-vocabulary list
        values$cv_terms <- get_synapse_annotations("syn25322361", syn) %>%
          select(key, value, columnType) %>%
          unique()
        ### update waiter loading screen once login successful
        waiter::waiter_update(
          html = tagList(
            img(src = "www/synapse_logo.png",
                height = "120px"),
            h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
          )
        )
        Sys.sleep(2)
        waiter::waiter_hide()
      }, error = function(err) {
        print(err)
        Sys.sleep(2)
        waiter::waiter_update(
          html = tagList(
            img(src = "www/synapse_logo.png", height = "120px"),
            h3("Login error"),
            span(
              "There was an error with the login process. Please refresh your Synapse session by logging out of and back in to",
              a("Synapse", href = "https://www.synapse.org/", target = "_blank"),
              ", then refresh this page."
            )
          )
        )
        
      })

    }
    # output$title <- shiny::renderUI({
    #   shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
    # })
    shiny::callModule(mod_home_server, "home_ui_1", values)
    shiny::callModule(mod_quickview_server, "quickview_ui_1", values)
    shiny::callModule(mod_validator_server, "validator_ui_1", values)
    
  })
}
