synapseclient <- reticulate::import('synapseclient')

# Get selected coloumns from a Synapse table.
get_portal_table <- function(table_id, cols) {
  query <- sprintf("SELECT %s FROM %s", paste0(cols, collapse=","), table_id)
  return(syn_table_query(query)$asDataFrame())
}

# Get Synapse tables used for the portal.
get_tables <- function() {
  grants <- get_portal_table(
    portal_table[["grant"]],
    c("grantId", "grantName", "grantNumber", "grantInstitution",
      "themeId", "theme", "consortiumId", "consortium")
  )   
  publications <- get_portal_table(
    portal_table[["publication"]],
    c("publicationId", "publicationTitle", "grantId", "grantNumber",
      "grantName", "themeId", "theme", "consortiumId", "consortium"
    )
  )   
  datasets <- get_portal_table(
    portal_table[["dataset"]],
    c("datasetId", "datasetName", "datasetAlias")
  )   
  tools <- get_portal_table(
    portal_table[["tool"]],
    c("toolId", "toolName")
  )
  files <- get_portal_table(
    fileview,
    c("fileName", "datasets", "parentId")
  )
  list(
    "grants" = grants,
    "publications" = publications,
    "datasets" = datasets,
    "files" = files,
    "tools" = tools
  )
}

display_overview_stats <- function(output, tables) {
  output$num_pubs <- renderValueBox({
    valueBox(
      formatC(nrow(tables$publications), format = "d", big.mark = ","),
      "Publications",
      icon = icon("book-open"), color = "maroon"
    )
  })
  output$num_datasets <- renderValueBox({
    valueBox(
      nrow(tables$datasets), "Datasets",
      icon = icon("cubes"), color = "olive"
    )
  })
  output$num_files <- renderValueBox({
    valueBox(
      formatC(nrow(tables$files), format = "d", big.mark = ","),
      "Data files",
      icon = icon("file"), color = "yellow"
    )
  })
  output$num_tools <- renderValueBox({
    valueBox(
      nrow(tables$tools), "Tools",
      icon = icon("tools"), color = "light-blue"
    )
  })
}

display_quickview <- function(output, tables) {
  output$grants_table <- DT::renderDT(
    tail(tables$grants, 10),
    options = list(dom = 't'))
  output$pubs_table <- DT::renderDT(
    tail(tables$publications, 10),
    options = list(dom = 't'))
  output$datasets_table <- DT::renderDT(
    tail(tables$datasets, 10),
    options = list(dom = 't'))
  output$files_table <- DT::renderDT(
    tail(tables$files, 10),
    options = list(dom = 't'))
  output$tools_table <- DT::renderDT(
    tail(tables$tools, 10),
    options = list(dom = 't'))
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

  ### portal table information for annotations later
  tables <- list()

  ## SYNAPSE LOGIN ############
  session$sendCustomMessage(type = "read_cookie", message = list())
  observeEvent(input$cookie, {
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### get portal tables and display overview stats
      tables <<- get_tables()
      display_overview_stats(output, tables)
      display_quickview(output, tables)

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
      mutate_all(~ tidyr::replace_na(.x, "")) %>%
      plyr::rename(
        replace = c(
          fileURL = "fileUrl", datasetURL = "datasetUrl", 
          toolName = "tool", homepageUrl = "externalLink", 
          dpgapAccns = "dbgapAccns", dpgapUrls = "dbgapUrls"
        ),
        warn_missing = FALSE
      ) 

    ### Rancho provides some cols we don't need, so remove them
    manifest <<- manifest[, !(names(manifest) %in% c("Rancho comments", "tumorType_"))]
    if (nrow(manifest) == 0) {
      valid_upload <<- FALSE
      show("empty_warning")
    }
    tryCatch({
      terms <- readxl::read_excel(
        input$manifest_file$datapath, 
        sheet = "standard_terms"
      )

      ### dccvalidator requires annotations to have a `columnType` column,
      ### so add it if not provided in `standard_terms`
      if (!"columnType" %in% names(std_terms)) {
        terms$columnType <- "STRING"
      }
      std_terms <<- unique(bind_rows(std_terms, terms))
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

  observeEvent(input$new_std_terms, {
    tryCatch({
      new_terms <- readxl::read_excel(input$new_std_terms$datapath)
      if (nrow(new_terms) > 0) {
        new_terms <- new_terms[, names(new_terms) %in% c("Category", "standard_name", "key", "value")] %>% #nolint
          plyr::rename(
            replace = c(Category = "key", standard_name = "value"), 
            warn_missing = FALSE
          ) %>%
          mutate(
            key = stringr::str_replace_all(
              key, 
              c("outDataType" = "outputDataType", "Assay" = "assay", "Tumor Type" = "tumorType")) #nolint
          ) %>%
          add_column(columnType = "STRING")
        std_terms <<- unique(bind_rows(std_terms, new_terms))
        output$terms <- DT::renderDT(
          std_terms,
          options = list(pageLength = 50))
      }
    }, error = function(err) {
      showModal(
        modalDialog(
          title = "Uh oh, something went wrong!",
          span(
            "File of additional standard terms is not correctly formatted.
            There should be a column named `Category` or `key` and another
            column named `standard_name` or `value`.", br(), br(), "See", 
            strong("Instructions"), "for a template file."),
          easyClose = TRUE
        )
      )
    })
  })

  observeEvent(input$validate_btn, {
    v_waiter$show()
    type <- input$type

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
      results$dup_ids = check_id_dups(manifest, id[[type]])
    }
    updateTabsetPanel(session, "validator_tabs", selected = "results_tab")
    callModule(results_boxes_server, "validation_results", results)
    next_step_modal(results, type)
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
        "tool" = tool_annots(row, tables$grant)
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
#      syn_id <- "syn123"
      new_portal_row <- switch(type,
        "publication" = publication_row(
          syn_id, row, 
          tables$grants, 
          tables$datasets
        ),
        "dataset" = dataset_row(
          syn_id, row,
          tables$publications
        ),
        "tool" = tool_row(
          syn_id, row, 
          tables$publications
        )
      )
#      output$diag <- DT::renderDT(new_portal_row)
      syn_store(synapseclient$Table(portal_table[[type]], new_portal_row))

      ### update stats on overview tab
      tables <<- get_tables()
      display_overview_stats(output, tables)
      display_quickview(output, tables)
    })

    u_waiter$update(
      html = div(style = "color:#465362",
        span(style = "color:green", icon("check-circle", "fa-2x")),
        strong("Upload complete!")
      )
    )
  })
}

