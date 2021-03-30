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
    portal_table[["file"]],
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
    options = list(dom = 't'), rownames = FALSE)
  output$pubs_table <- DT::renderDT(
    tail(tables$publications, 10),
    options = list(dom = 't'), rownames = FALSE)
  output$datasets_table <- DT::renderDT(
    tail(tables$datasets, 10),
    options = list(dom = 't'), rownames = FALSE)
  output$files_table <- DT::renderDT(
    tail(tables$files, 10),
    options = list(dom = 't'), rownames = FALSE)
  output$tools_table <- DT::renderDT(
    tail(tables$tools, 10),
    options = list(dom = 't'), rownames = FALSE)
}

# modal with next step (based on dccvalidator's next_step_modal)
next_step_modal <- function(results, type) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  instructions = tagList(
    p("You may now upload the manifest by clicking the button below."),
    actionButton("upload_btn", class="btn-lg btn-block",
      icon = icon("cloud-upload-alt"), "Upload to Synapse")
  )
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
  cv_terms <- tibble()

  ## SYNAPSE LOGIN ############
  session$sendCustomMessage(type = "read_cookie", message = list())
  observeEvent(input$cookie, {
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### get portal tables and display overview stats
      tables <<- get_tables()
      display_overview_stats(output, tables)
      display_quickview(output, tables)

      # get controlled-vocabulary list
      cv_terms <<- get_synapse_annotations("syn25322361", syn) %>% 
        select(key, value, columnType) %>% 
        unique()
      output$terms <- DT::renderDT(
        cv_terms,
        options = list(pageLength = 50) 
      )

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

    ### Show warning if uploaded manifest is empty
    if (nrow(manifest) == 0) {
      valid_upload <<- FALSE
      show("empty_warning")
    } else {
      output$preview <- DT::renderDT(manifest)
    }
    
    updateTabsetPanel(session, "validator_tabs", selected = "preview_tab")
  })

  observeEvent(input$new_cv_terms, {
    tryCatch({
      new_terms <- readxl::read_excel(input$new_cv_terms$datapath)
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
        cv_terms <<- unique(bind_rows(cv_terms, new_terms))
        output$terms <- DT::renderDT(
          cv_terms,
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
        cv_terms,
        whitelist_keys = c(setdiff(template[[type]], std_cols), "Notes", "notes"),
        success_msg = "All column names are valid",
        fail_msg = "Some column names are invalid",
        annots_link = "https://www.synapse.org/#!Synapse:syn25322361/tables/"
      ),
      incomplete_cols = check_cols_complete(
        manifest,
        required_cols = complete_cols[[type]],
        success_msg = "All necessary columns have annotations",
        fail_msg = "Some necessary columns are missing annotations"
      ),
      invalid_vals = check_listed_values(
        manifest,
        cv_terms
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

      if (type == "dataset") {
        annotations <- dataset_annots(row)
        syn_id <- save_folder_to_synapse(
          synapseclient,
          name,
          parent_folder[["dataset"]],
          annotations
        )
      } else if (type == "file") {

        # Get parent ID (dataset folder ID) for data file
        res <- tables$datasets[grepl(paste0("^", row[["datasetId"]], "$"), tables$datasets[["datasetAlias"]], ignore.case = TRUE), ][["datasetId"]] #nolint
        dataset_folder <- unlist(res[!is.na(res)]) %>%
          stringr::str_replace_all(c("\\[" = "", "\\\"" = "", "\\]" = ""))

        annotations <- file_annots(row, dataset_folder, tables$grants, tables$datasets)
        syn_id <- save_file_to_synapse(
          synapseclient,
          name,
          dataset_folder,
          annotations
        )
      } else {

### Skip making entities for publications and tools for now
#        if (type == "publication") {
#          name <- paste0("pmid_", name)
#        }
#        annotations <- switch(type,
#          "publication" = publication_annots(row),
#          "tool" = tool_annots(row, tables$grant)
#        )
#        syn_id <- save_file_to_synapse(
#          synapseclient,
#          name,
#          parent_folder[[type]],
#          annotations
#        )
        syn_id <- ""
      }      
      
      if (type != "file") {
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
        syn_store(synapseclient$Table(portal_table[[type]], new_portal_row))
      }
#      output$diag <- DT::renderDT(new_portal_row)
#      syn_store(synapseclient$Table(portal_table[[type]], new_portal_row))

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

