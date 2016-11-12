## Dashboard Body    ---------------------


componentBody <- dashboardBody(

  ## Tabs ---------------------------
  tabItems(
    ## Import tab ------------------
    tabItem(tabName = "import",
            fluidRow(
              box(width = 8, height = "100px", title = "Choose file to import", solidHeader = TRUE, background = "black",
                  selectizeInput("importableFiles", label = NULL, choices = NULL, selected = NULL)
              ),
              box(width = 4, height = "100px", title = "Import", solidHeader = TRUE, background = "black",
                  actionButton("buttonImport", "Import", icon = icon("import"))
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


