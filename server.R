source_python("synapse_funcs.py")

server <- function(input, output, session) {
  session$sendCustomMessage(type = "read_cookie", message = list())
  
  observeEvent(input$cookie, {
    
    ### login and update session; otherwise, notify to login to Synapse first
    tryCatch({
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)

      ### update waiter loading screen once login successful
      waiter_update(
        html = h3(sprintf("Welcome, %s!", syn_user_profile()$userName))
      )
      Sys.sleep(2)
      waiter_hide()
    }, error = function(err) {
      print(err)

      Sys.sleep(2)
      waiter_update(
        html = tagList(
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    })
  })
}

