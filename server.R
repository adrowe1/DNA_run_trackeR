## app.R ##


server <- function(session, input, output) {

  ## INPUTS ------------------------

  # input component for selecting a directory containing raw data
  shinyDirChoose(input, "dataDir", root=rootOptions, session=session, filetypes=c("", "json"))

  # input component for selecting a database file containing imported data
  shinyFileChoose(input, "analysisFile", root=rootOptions, session=session, filetypes=c("", "sqlite"))

  ## END INPUTS


  ## DB SETUP ---------------------
  dbCon <- reactive({
    if (is.null(input$analysisFile))
      return("./data/imported.sqlite")
    # otherwise use defined file
    parseFilePaths(rootOptions, input$analysisFile)
  })




  ## END DB SETUP



  ## PROCESS ---------------------
  filesToImport <- reactive({
    # If no directory selected, pass a NULL
    if (is.null(input$dataDir))
      return(NULL)
    # else get selected path
    basePath <- parseDirPath(rootOptions, input$dataDir)
    # then list all json files in path and make a data frame with checksums
    files <- data_frame(path=list.files(basePath, recursive = TRUE, pattern = ".json"),
               checksum=list.files(basePath, recursive = TRUE, full.names = TRUE, pattern = ".json") %>% tools::md5sum())

    # exclude any files with checksums already in database
    con <- dbConnect(SQLite(), dbCon())
    alreadyImported <- dbGetQuery(conn=con, "SELECT checksum FROM import_record")

    # return data frame of file paths and checksums
    files %>% filter(!checksum %in% alreadyImported)
  })



  ## END PROCESS



  ## REACTIVES -------------------

  # FIXME When data becomes available for import, select import tab
  observe({
    # dependecy on filesToImport
    filesToImport()

    # make import tab active
    updateTabItems(session, "tabs", "import")
  })

  ## END REACTIVES




  ## OUTPUTS ---------------------

  # Sidebar
  output$menuImport <- renderMenu({
    # If no directory selected, don't show menu
    if (is.null(filesToImport()))
      return(NULL)

    # count number of files available for import
    numFiles <- nrow(filesToImport())

    sidebarMenu(
      menuItem("Import", icon = icon("upload"), tabName = "import",
        badgeLabel = numFiles, badgeColor = ifelse(numFiles>0, "green", "navy"))
    )
  })

  output$sidebarButton <- renderUI({
    column(width = 12, offset = 0,
           # Spacing to tidy up
           br(),
           # Button controls for selecting data
           shinyFilesButton(id = "analysisFile",
                            label = 'Use alternate DB file',
                            title = 'Select a DB file containing imported data',
                            multiple = FALSE,
                            buttonType = ifelse(file.exists("./data/imported.sqlite"), "primary", "warning") )
    )
  })

  ## END OUTPUTS


}


