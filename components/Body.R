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
              box(width = 12, title = "Plot data", solidHeader = FALSE,
                  # dataTableOutput("tmp")
                  div(style = 'height: calc(100vh - 270px) !important;width:100%;background-color: #FFFFFF',
                      plotOutput("currentImportPlot")
                  )
              )
            )
    ),
    ## END TAB

    ## Tuning tab ------------------
    tabItem(tabName = "tuning",
            fluidRow(
              box(width = 3, height = "150px", title = "Choose dataset to tune", solidHeader = TRUE, background = "black",
                  selectizeInput("tunableFiles", label = "", choices = NULL, selected = NULL)
              ),
              box(width = 6, height = "150px", title = "Drift compensation", solidHeader = TRUE, background = "black",
                column(width = 4, numericInput("driftDeltaX", label = "X drift", value = 0)),
                column(width = 4, numericInput("driftDeltaY", label = "Y drift", value = 0)),
                column(width = 4, numericInput("driftDeltaFrame", label = "Drift frames", value = 1))
              ),
              box(width = 3, height = "150px", title = "Rotate", solidHeader = TRUE, background = "black",
                  sliderInput("rotatePlot", "Rotate", min = -90, max = +90, value = 0)
              )
            ),
            fluidRow(
              box(width = 12, title = "Plot data", solidHeader = FALSE,
                  # dataTableOutput("tmp")
                  div(style = 'height: calc(100vh - 270px) !important;width:100%;background-color: #FFFFFF',
                      # plotOutput("currentImportPlot")
                      p("")
                  )
              )
            )
    )



    ## END TAB





    ## Next tab ------------------




  )


)


