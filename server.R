source_python("synapse_funcs.py")

# helper: get count from current tables
count <- function(syn_id) {
  query <- sprintf("SELECT COUNT(*) AS n FROM %s", syn_id)
  return(syn_table_query(query)$asDataFrame()$n)
}

# helper: validate tool manifest
validate_tool <- function(df, standard_terms) {
  return(standard_terms$key[1])
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
    hide("warning")
    manifest <<- read_excel(input$manifest_file$datapath)
    tryCatch({
      std_terms <<- read_excel(input$manifest_file$datapath, sheet = "standard_terms")
      output$preview <- renderDT(manifest)
    }, error = function(err) {
      valid_upload <<- FALSE
      show("warning")
      output$preview <- renderDT({})
    })
  })

  observeEvent(input$validate_btn, {
    w$show()
    updateTabsetPanel(session, "validator_tabs", selected = "results_tab")
    output$validation_results <- renderUI({
      if (input$type == "tool") {
        validate_tool(manifest, std_terms)
      }
    })
    Sys.sleep(2)
    w$hide()
  })
}

