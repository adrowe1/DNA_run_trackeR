## Dashboard Sidebar ---------------------


componentSidebar <- dashboardSidebar(

  ## Menu components -------

  sidebarMenu(
    id = "tabs",
    sidebarMenuOutput("menuImport"),
    menuItem("Menu item", icon = icon("calendar"))
  ),

  ## Raw data directory choice -------

  column(width = 12, offset = 0,
    # Spacing to tidy up
    br(), br(),
    # Button controls for selecting data
    shinyDirButton(id = "dataDir",
      label = 'Import data from a directory',
      title = 'Select a directory containing raw data',
      buttonType = "success")
    ),


  ## Imported data analysis file choice -------

  uiOutput("sidebarButton")


)



