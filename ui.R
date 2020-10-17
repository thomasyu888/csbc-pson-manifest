## Customizations
SIDE_WIDTH = 260

ui <- dashboardPage(
  skin = "blue",
  
  ## Top Nav
  dashboardHeader(
    titleWidth = SIDE_WIDTH,
    title = "CSBC/PS-ON Manifests"
  ),
  
  ## Sidebar
  dashboardSidebar(
    width = SIDE_WIDTH,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Templates", tabName = "download", icon = icon("file-download")),
      menuItem("Validations", tabName = "validate", icon = icon("tasks")),
      menuItem("Upload", tabName = "upload", icon = icon("cloud-upload-alt")),
      hr(),
      menuItem("Source Code", 
               href = "https://github.com/vpchung/csbc-pson-data-portal",
               icon = icon("github"))
    ),
    HTML("<footer>
            Made with ♥<br/>
            Powered by Sage Bionetworks
        </footer>")
  ),
  
  ## Main
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      singleton(
        includeScript("www/read_cookie.js")
      )
    ),

    uiOutput("diag"),
    
    # loading screen
    use_waiter(),
    waiter_show_on_load(
      html = tagList(
        img(src = "loading.gif"),
        h4("Retrieving Synapse information...")
      ),
      color = "#424874"
    )
  )
)
