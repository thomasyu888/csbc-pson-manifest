#' Customizations
COLOR = "blue"
SIDE_WIDTH = 260
TITLE = "CSBC/PS-ON Manifests"

#' Sidebar
sidebar <- dashboardSidebar(
  width = SIDE_WIDTH,
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "home", icon = icon("dashboard")),
    menuItem("Validate and Upload", tabName = "validator",
             icon = icon("cloud-upload-alt")
    ),
    menuItem("Portal Quickview", tabName = "quickview",
             icon = icon("desktop")
    )
  ),
  HTML("<footer>
            Made with ♥ and dccvalidator functions<br/>
            Supported by U24-CA209923<br/>
            Powered by Sage Bionetworks
      </footer>"
  )
)


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import waiter
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = tagList(
        img(src = "www/loading.gif"),
        h4("Retrieving Synapse information...")
      ),
      color = "#424874"
    ),
    # Dashboard page
    dashboardPage(
      skin = COLOR,
      dashboardHeader(
        titleWidth = SIDE_WIDTH,
        title = TITLE
      ),
      sidebar,
      dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("home", mod_home_ui("home_ui_1")),
          shinydashboard::tabItem("validator", mod_validator_ui("validator_ui_1")),
          shinydashboard::tabItem("quickview", mod_quickview_ui("quickview_ui_1"))
        ),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    golem::activate_js(),
    # favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'csbcPsonManifest'
    ),
    includeScript(system.file("inst/app/www/read_cookie.js",
                              package = "csbcPsonManifest"))# ,
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
  )
}

