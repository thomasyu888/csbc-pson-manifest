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
    menuItem("Validate and Upload", tabName = "validator", 
      icon = icon("cloud-upload-alt")
    )
  ),
  uiOutput("diag"),
  HTML("<footer>
            Made with ♥ and dccvalidator functions<br/>
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
      h4("Add metadata to the Cancer Complexity Knowledge Portal", 
        align = "center"),
      br(),
      h4(strong("Overview")),
      p("So far, we have annotated..."),
      fluidRow(
        valueBoxOutput("num_grants", width = 3),
        valueBoxOutput("num_pubs", width = 3),
        valueBoxOutput("num_datasets", width = 3),
        valueBoxOutput("num_tools", width = 3)
      ),
      p("Let's annotate even more! Go to ", strong("Validate and Upload"),
        " to start.")
    ),

    tabItem(
      tabName = "validator",
      sidebarLayout(
        sidebarPanel(
          box(
            title = "Instructions",
            width = "100%", collapsible = TRUE, collapsed = TRUE,
            p("In order to add new metadata to the portal, the values must
               first be validated. Select the type of manifest to be uploaded,
               then upload the file. A file preview will be displayed on the
               right. Click on ", strong("Validate."), "If all checks pass,
               feel free to upload; otherwise, edit accordingly then re-upload
               the manifest to validate again."),
            p(strong("Note:"), "the file must be .xlsx and it is expected to
              have a sheet called standard_terms (list of the acceptable 
              values). If needed, templates are available below:"),
            tags$ul(
              tags$li(a(href = "templates/publications_manifest.xlsx",
                "Publications", download = NA, target = "_blank")),
              tags$li(a(href = "templates/datasets_manifest.xlsx", "Datasets",
                download = NA, target = "_blank")),
              tags$li(a(href = "templates/files_manifest.xlsx", "Files",
                download = NA, target = "_blank")),
              tags$li(a(href = "templates/tools_manifest.xlsx", "Tools",
                download = NA, target = "_blank")),
              tags$li(a(href = "templates/additional_standard_terms.xlsx",
                "Additional Terms", download = NA, target = "_blank"))
            ),
            span(style = "font-size:smaller",
              em("Templates last updated 10/30/2020."))
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
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" #nolint
            )
          ),

          fileInput(
            "new_std_terms",
            label = "Additional Standard Terms (.xlsx)",
            accept= c(
              ".xlsx",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" #nolint
            )
          ),

          div(
            align = "center",
            hidden(
              span(id = "missing_warning", style = "color:red",
                em("File is missing a `standard_terms` sheet. See ",
                   strong("Instructions"), " for more details."), br()
              )
            ),
            hidden(
              span(id = "empty_warning", style = "color:red",
                em("File is empty. Double-check the manifest and try again."),
                br()
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
              "Standard Terms",
              value = "terms_tab",
              style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
              p("This will display the list of standard terms used to validate
                the manifest."),
              DT::DTOutput("terms"),
            ),
            tabPanel(
              "Validation Results",
              value = "results_tab",
              style = "height:78vh",
              results_boxes_ui("validation_results")
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
