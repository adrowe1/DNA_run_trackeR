## app.R ##


server <- function(session, input, output) {

  ## INPUTS ------------------------

  volumes <- getVolumes()

  # input component for selecting a directory containing raw data
  shinyDirChoose(input, "dataDir", root=volumes(), session=session, filetypes=c("", "json"))

  # input component for selecting a database file containing imported data
  shinyFileChoose(input, "analysisFile", root=volumes(), session=session, filetypes=c("", "sqlite")) # FIXME root failing

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

  # files which can be imported
  filesToImport <- reactive({
    # If no directory selected, pass a NULL
    if (is.null(input$dataDir))
      return(NULL)
    # else get selected path
    basePath <- parseDirPath(volumes(), input$dataDir)
    # then list all json files in path and make a data frame with checksums
    files <- data_frame(path=list.files(basePath, recursive = TRUE, pattern = ".json"),
                        checksum=list.files(basePath, recursive = TRUE, full.names = TRUE, pattern = ".json") %>% tools::md5sum())

    # exclude any files with checksums already in database
    con <- dbConnect(SQLite(), dbCon())
    alreadyImported <- dbGetQuery(conn=con, "SELECT checksum FROM import_record")

    cat(dim(files), "\n")
    cat(dim(alreadyImported), "\n")
    cat(dim(files %>% anti_join(., alreadyImported)), "\n")


    # return data frame of file paths and checksums
    files %>% anti_join(., alreadyImported)
  })

  # parse a filename for DB write
  importRecord <- reactive({
    if (is.null(input$importableFiles))
      return(NULL)


    tmp <- filesToImport()$path



    firstCol <- file.path(parseDirPath(volumes(), input$dataDir), tmp[str_detect(tmp, input$importableFiles)]) %>%
      as_data_frame() %>%
      set_colnames("filename")

    values <- input$importableFiles %>%
      basename %>%
      str_split_fixed(pattern = "_", n = 8) %>%
      set_colnames(c("protein", "NaCl", "date", "framerate", "pixelsize", "batch", "dilution", "metadata")) %>%
      as_data_frame() %>%
      mutate(NaCl = str_replace_all(NaCl, "[A-z]", "") %>% as.numeric(),
             date = str_sub(date, 1, 8) %>% ymd,
             framerate = str_replace_all(framerate, "[A-z]", "") %>% as.numeric(),
             pixelsize = str_replace_all(pixelsize, "[A-z]", "") %>% as.numeric(),
             dilution = str_replace_all(dilution, "[A-z]", "") %>% as.numeric()
      )


    # output a data frame
    bind_cols(firstCol, values) %>% mutate(checksum = tools::md5sum(filename),
                                           filename = basename(filename))
  })


  ## END PROCESS



  ## REACTIVES -------------------

  # FIXME When data becomes available for import, select import tab
  observe({
    # dependency on filesToImport
    filesToImport()

    # make import tab active
    updateTabItems(session, "tabs", "import")

    # add list of possible imports to selectize input
    isolate({
      if (!is.null(filesToImport())) {
        chooseBetween <- filesToImport() %>% extract2("path") %>% basename()
        updateSelectizeInput(session, "importableFiles", choices = chooseBetween, selected=chooseBetween[1])
      }

    })

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


  output$tmp <- renderDataTable({
    importRecord()
  })

  ## END OUTPUTS


}


