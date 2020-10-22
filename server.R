source_python("synapse_funcs.py")
source("helper_funcs.R")
source("col_values.R")

# get count from current tables
count <- function(syn_id) {
  query <- sprintf("SELECT COUNT(*) AS n FROM %s", syn_id)
  return(syn_table_query(query)$asDataFrame()$n)
}

# modal with next step (based on dccvalidator's next_step_modal)
next_step_upload <- function(results) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  if (!any(is_failure)) {
    showModal(
      modalDialog(
        title = "Great work!",
        HTML(
          "All validations have cleared. You may now upload the manifest."
        ),
        easyClose = TRUE
      )
    )
  }
}

server <- function(input, output, session) {

  ## SYNAPSE LOGIN ############
  session$sendCustomMessage(type = "read_cookie", message = list())
  observeEvent(input$cookie, {
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### display overview stats
      output$num_grants <- renderValueBox({
        valueBox(
          count("syn21918972"), "Grants",
          icon = icon("award"), color = "yellow"
        )
      })
      output$num_pubs <- renderValueBox({
        valueBox(
          formatC(count("syn21868591"), format = "d", big.mark = ","),
          "Publications",
          icon = icon("book-open"), color = "maroon"
        )
      })
      output$num_datasets <- renderValueBox({
        valueBox(
          count("syn21897968"), "Datasets",
          icon = icon("cubes"), color = "olive"
        )
      })
      output$num_tools <- renderValueBox({
        valueBox(
         count("syn21930566"), "Tools",
          icon = icon("tools"), color = "light-blue"
        )
      })

      ### update waiter loading screen once login successful
      waiter_update(
        html = tagList(
          img(src = "synapse_logo.png", height = "120px"),
          h3(sprintf("Welcome, %s!", syn_user_profile()$userName))
        )
      )
      Sys.sleep(2)
      waiter_hide()
    }, error = function(err) {

      ### notify user to login if unsuccessful
      Sys.sleep(2)
      waiter_update(
        html = tagList(
          img(src = "synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    })
  })

  ## BUTTON ACTIVATION ########
  valid_upload <- TRUE
  observe({
    cond <- input$type != "" & !is.null(input$manifest_file) & valid_upload
    toggleState("validate_btn", condition = cond)
  })

  ## MANIFEST VALIDATOR #######
  w <- Waiter$new(
    id = "shiny-tab-validator",
    html = tagList(
      spin_loaders(15),
      h4("Validating...")
    ),
    color = "#424874"
  )
  manifest <- tibble()
  std_terms <- tibble()

  observeEvent(input$manifest_file, {
    valid_upload <<- TRUE
    hide("missing_warning")
    hide("empty_warning")
    manifest <<- readxl::read_excel(input$manifest_file$datapath)
    if (nrow(manifest) == 0) {
      valid_upload <<- FALSE
      show("empty_warning")
    }
    tryCatch({
      std_terms <<- readxl::read_excel(
        input$manifest_file$datapath, 
        sheet = "standard_terms"
      )
      output$preview <- DT::renderDT(manifest)
    }, error = function(err) {
      valid_upload <<- FALSE
      show("missing_warning")
      output$preview <- DT::renderDT({})
    })
    updateTabsetPanel(session, "validator_tabs", selected = "preview_tab")
  })

  observeEvent(input$validate_btn, {
    w$show()
    updateTabsetPanel(session, "validator_tabs", selected = "results_tab")

    checks <- list(
      missing_cols = check_col_names(
        manifest,
        template[[input$type]],
        success_msg = "All required columns are present",
        fail_msg = "Missing columns in the manifest"
      ),
      dup_ids = check_id_dups(manifest, id[[input$type]]),
      invalid_cols = check_annotation_keys(
        manifest,
        std_terms,
        whitelist_keys = setdiff(template[[input$type]], std_cols),
        success_msg = "All column names are valid",
        fail_msg = "Some column names are invalid"
      ),
      incomplete_cols = check_cols_complete(
        manifest,
        required_cols = complete_cols[[input$type]],
        success_msg = "All necessary columns have annotations",
        fail_msg = "Some necessary columns are missing annotations"
      ),
      invalid_vals = check_listed_values(
        manifest,
        std_terms
      )
    )

    callModule(results_boxes_server, "validation_results", checks)
    Sys.sleep(2)
    w$hide()

    Sys.sleep(1)
    next_step_upload(checks)
  })
}

