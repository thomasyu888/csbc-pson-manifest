## Customizations
COLOR = "purple"
SIDE_WIDTH = 260
TITLE = "CSBC/PS-ON Manifests"


## Sidebar
sidebar <- dashboardSidebar(
  width = SIDE_WIDTH,
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "home", icon = icon("dashboard")),
    menuItem("Validate and Upload", tabName = "validator", icon = icon("cloud-upload-alt"))
  ),
  HTML("<footer>
            Made with ♥<br/>
            Powered by Sage Bionetworks
      </footer>"
  )
)


## Body
body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    singleton(includeScript("www/read_cookie.js"))
  ),

  tabItems(
    tabItem(
      tabName = "home",
      h2("CSBC/PS-ON Manifest Curator", align = "center"),
      h4("Add metadata to the Cancer Complexity Knowledge Portal", align = "center"),
      br(),
      h4(strong("Overview")),
      fluidRow(
        valueBoxOutput("num_grants", width = 3),
        valueBoxOutput("num_pubs", width = 3),
        valueBoxOutput("num_datasets", width = 3),
        valueBoxOutput("num_tools", width = 3)
      ),
    ),

    tabItem(
      tabName = "validator",
      sidebarLayout(
        sidebarPanel(
          box(
            title = "Instructions",
            width = "100%", collapsible = TRUE, collapsed = TRUE,
            p("In order to add new metadata to the portal, the values must ",
              "first be validated. Select the type of manifest to be uploaded, ",
            "then upload the file. Note that it must be a .xlsx file with at ",
            "least two sheets: one that contains the new metadata and another ",
            "named `standard_terms` that contains a list of acceptable values."),
            p("Templates are available below if needed:"),
            tags$ul(
              tags$li("Publications"),
              tags$li("Datasets"),
              tags$li("Files"),
              tags$li("Tools")
            )
          ),

          selectizeInput(
            "type",
            label = "Manifest type:",
            choices = c(
              "--choose one--" = "",
              "Publications" = "publication",
              "Datasets" = "dataset",
              "Files" = "file",
              "Tools" = "tool"
            )
          ),

          fileInput(
            "manifest_file",
            label = "Upload manifest (.xlsx)",
            accept = c(
              ".xlsx",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            )
          ),

          div(
            align = "center",
            hidden(
              span(id = "warning", style = "color:red",
                em("File is missing a `standard_terms` sheet. See ",
                   strong("Instructions"), " for more details."), br(), br()
              )
            ),
            disabled(
              actionButton(
                "validate_btn",
                label = "Validate",
                class = "btn-primary", style = "color: white"
              )
            )
          )
        ),

        mainPanel(
          tabBox(
            id = "validator_tabs",
            width = "100%",
            tabPanel(
              "File Preview",
              value = "preview_tab",
              style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
              DT::DTOutput("preview")
            ),
            tabPanel(
              "Validation Results",
              value = "results_tab",
              style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
              uiOutput("validation_results")
            )
          )
        )
      )
    )
  ),

  # loading screen
  use_waiter(spinners = 6),
  waiter_show_on_load(
    html = tagList(
      img(src = "loading.gif"),
      h4("Retrieving Synapse information...")
    ),
    color = "#424874"
  )
)


ui <- dashboardPage(
  skin = COLOR,
  
  dashboardHeader(
    titleWidth = SIDE_WIDTH,
    title = TITLE
  ),
  sidebar,
  body
)
