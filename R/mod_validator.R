#' validator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_validator_ui <- function(id){
  ns <- NS(id)
  tagList(
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
          p(strong("Note:"), "the file must be .xlsx. If needed, templates 
               are available below:"),
          tags$ul(
            tags$li(a(href = "www/templates/publications_manifest.xlsx",
                      "Publications", download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/datasets_manifest.xlsx", "Datasets",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/files_manifest.xlsx", "Files",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/tools_manifest.xlsx", "Tools",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/additional_standard_terms.xlsx",
                      "Additional Terms", download = NA, target = "_blank"))
          ),
          span(style = "font-size:smaller",
               em("Templates last updated Mar 2021."))
        ),
        
        span(style = "font-size:smaller",
             strong("Important!"), br(),
             "When uploading multiple manifests, upload the publications one first."
        ), br(), br(),
        
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
          "new_cv_terms",
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
            DT::DTOutput("preview"),
            DT::DTOutput("diag")
          ),
          tabPanel(
            "Standard Terms",
            value = "terms_tab",
            style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
            p("Displayed below is the list of standard terms used to validate
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
}
    
#' validator Server Function
#'
#' @noRd 
mod_validator_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_validator_ui("validator_ui_1")
    
## To be copied in the server
# callModule(mod_validator_server, "validator_ui_1")
 
