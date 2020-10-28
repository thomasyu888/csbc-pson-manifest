synapseclient <- reticulate::import('synapseclient')

source_python("synapse_funcs.py")
source("validation_funcs.R")
source("col_values.R")
source("synapse_ids.R")

# get count from current tables
count <- function(syn_id) {
  query <- sprintf("SELECT COUNT(*) AS n FROM %s", syn_id)
  return(syn_table_query(query)$asDataFrame()$n)
}

# modal with next step (based on dccvalidator's next_step_modal)
next_step_modal <- function(results, type) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  if (type == "file") {
    instructions = p("File manifests are manually added by our internal team.
                     Please email the manifest file to Verena at
                     verena.chung@sagebase.org to add the new annotations.")
  } else {
    instructions = tagList(
      p("You may now upload the manifest by clicking the button below."),
      actionButton("upload_btn", class="btn-lg btn-block",
        icon = icon("cloud-upload-alt"), "Upload to Synapse")
    )
  }
  if (length(results) & !any(is_failure)) {
    Sys.sleep(1)
    showModal(
      modalDialog(
        id = "next_step",
        title = "Validation Checks Passed!",
        instructions,
        easyClose = TRUE
      )
    )
  }
}

server <- function(input, output, session) {

  ### grant information for annotations later
  grants <- tibble()

  ## SYNAPSE LOGIN ############
  session$sendCustomMessage(type = "read_cookie", message = list())
  observeEvent(input$cookie, {
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### display overview stats
      grants <<- syn_table_query(sprintf(
        "SELECT grantId, grantName, grantNumber FROM %s", "syn21918972")
        )$asDataFrame()
      output$num_grants <- renderValueBox({
        valueBox(
          nrow(grants), "Grants",
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
          span("Please ",
            a("login", 
              href = "https://www.synapse.org/#!LoginPlace:0", 
              target = "_blank"
            ),
            " to Synapse, then refresh this page."
          )
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
  v_waiter <- Waiter$new(
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
      output$terms <- DT::renderDT(
        std_terms,
        options = list(pageLength = 50))
    }, error = function(err) {
      valid_upload <<- FALSE
      show("missing_warning")
      output$preview <- DT::renderDT({})
      output$terms <- DT::renderDT({})
    })
    updateTabsetPanel(session, "validator_tabs", selected = "preview_tab")
  })

  observeEvent(input$validate_btn, {
    v_waiter$show()

    tryCatch({
      results <- list(
        missing_cols = check_col_names(
          manifest,
          template[[input$type]],
          success_msg = "All required columns are present",
          fail_msg = "Missing columns in the manifest"
        ),
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
      if (input$type != "file") {
        dup_ids = check_id_dups(manifest, id[[input$type]])
      }
      updateTabsetPanel(session, "validator_tabs", selected = "results_tab")
      callModule(results_boxes_server, "validation_results", results)
      Sys.sleep(3)
      next_step_modal(results, input$type)
    }, error = function(err) {
      showModal(
        modalDialog(
          title = "Uh oh, something went wrong!",
          HTML(
            "`standard_terms` must have the following columns: 'key', 'value',
            and 'columnType'. See templates for an example."),
          easyClose = TRUE
        )
      )
      updateTabsetPanel(session, "validator_tabs", selected = "terms_tab")
    })
    v_waiter$hide()
  })

  ## UPLOAD MANIFEST #######
  u_waiter <- Waiter$new(
    id = "next_step",
    html = div(style = "color:#465362",
      spin_loaders(15, color = "#465362"),
      h4("Uploading...")
    ),  
    color = "white"
  )
  observeEvent(input$upload_btn, {
    u_waiter$show()
    Sys.sleep(2)

    if (input$type == "tool") {
      ### create new entity in parent folder
      apply(manifest, 1, function(row) {
        name <- row[[ id[[input$type]] ]]

        ### create dummy file to upload to Synapse
        write(name, file = name)
        new_file <- synapseclient$File(
          path = name,
          name = name,
          parent = parent_folder[[input$type]],
          annotations = list(
            displayName = name,
            grantId = grants[grants$grantNumber == row[["grantNumber"]], ]$grantId, #nolint
            toolType = row[["toolType"]]
          )
        )
        new_file <- syn$store(new_file)

        ### remove dummy file
        file.remove(name)
        new_file$id
      })
    }

    u_waiter$update(
      html = div(style = "color:#465362",
        span(style = "color:green", icon("check-circle", "fa-2x")),
        strong("Upload complete!")
      )
    )
  })
}

