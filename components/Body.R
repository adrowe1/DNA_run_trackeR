## Dashboard Body    ---------------------


componentBody <- dashboardBody(

  ## Tabs ---------------------------
  tabItems(
    ## Import tab ------------------
    tabItem(tabName = "import",
            fluidRow(
              box(width = 6, title = "Choose file to import", solidHeader = TRUE, background = "black",
                  selectizeInput("importableFiles", label = "", choices = NULL, selected = NULL)
              )
            ),
            fluidRow(
              box(width = 12, title = "tmp", solidHeader = FALSE,
                  dataTableOutput("tmp")
              )
            )
    )

    ## Next tab ------------------

  )


)


