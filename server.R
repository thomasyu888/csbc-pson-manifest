synapseclient <- reticulate::import('synapseclient')

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

  ### grant and publication information for annotations later
  grants <- tibble()
  publications <- tibble()

  ## SYNAPSE LOGIN ############
  session$sendCustomMessage(type = "read_cookie", message = list())
  observeEvent(input$cookie, {
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### display overview stats
      grants <<- syn_table_query(sprintf(
        "SELECT grantId, grantName, grantNumber, themeId, theme, grantInstitution, consortiumId, consortium FROM %s", #nolint
        portal_table[["grant"]])
      )$asDataFrame()
      output$num_grants <- renderValueBox({
        valueBox(
          nrow(grants), "Grants",
          icon = icon("award"), color = "yellow"
        )
      })
      publications <<- syn_table_query(sprintf(
        "SELECT publicationId, publicationTitle FROM %s", 
        portal_table[["publication"]])
      )$asDataFrame()
      output$num_pubs <- renderValueBox({
        valueBox(
          formatC(nrow(publications), format = "d", big.mark = ","),
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
    manifest <<- readxl::read_excel(input$manifest_file$datapath) %>%
      plyr::rename(
        replace = c(fileURL = "fileUrl", datasetURL = "datasetUrl", toolName = "tool", homepageUrl = "externalLink"),
        warn_missing = FALSE
      )
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
    type <- input$type

    tryCatch({
      results <- list(
        missing_cols = check_col_names(
          manifest,
          template[[type]],
          success_msg = "All required columns are present",
          fail_msg = "Missing columns in the manifest"
        ),
        invalid_cols = check_annotation_keys(
          manifest,
          std_terms,
          whitelist_keys = setdiff(template[[type]], std_cols),
          success_msg = "All column names are valid",
          fail_msg = "Some column names are invalid"
        ),
        incomplete_cols = check_cols_complete(
          manifest,
          required_cols = complete_cols[[type]],
          success_msg = "All necessary columns have annotations",
          fail_msg = "Some necessary columns are missing annotations"
        ),
        invalid_vals = check_listed_values(
          manifest,
          std_terms
        )
      )
      if (type != "file") {
        dup_ids = check_id_dups(manifest, id[[type]])
      }
      updateTabsetPanel(session, "validator_tabs", selected = "results_tab")
      callModule(results_boxes_server, "validation_results", results)
      Sys.sleep(3)
      next_step_modal(results, type)
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
    type <- input$type

    apply(manifest, 1, function(row) {
      name <- row[[ id[[type]] ]]
      if (type == "publication") {
        name <- paste0("pmid_", name)
      }
      annotations <- switch(type,
        "publication" = publication_annots(row),
        "dataset" = dataset_annots(row),
        "tool" = tool_annots(row, grants)
      )
      if (type == "dataset") {
        syn_id <- save_folder_to_synapse(
          synapseclient,
          name,
          parent_folder[[type]],
          annotations
        )
      } else {
        syn_id <- save_file_to_synapse(
          synapseclient,
          name,
          parent_folder[[type]],
          annotations
        )
      }
      new_portal_row <- switch(type,
        "publication" = publication_row(row, syn_id),
        "dataset" = dataset_row(row, syn_id),
        "tool" = tool_row(row, syn_id, publications, grants)
      )
    })

    u_waiter$update(
      html = div(style = "color:#465362",
        span(style = "color:green", icon("check-circle", "fa-2x")),
        strong("Upload complete!")
      )
    )
  })
}

