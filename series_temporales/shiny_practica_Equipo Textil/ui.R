############################## CREATE THE SIDEBAR ##############################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Serie Temporal", tabName = "Tab0", icon = icon("line-chart"))
    
  ),
  
  # Logo in sidebar menu
  div(style = "position: fixed; bottom: 20px; left: 20px;",
      img(src = 'external_logos/timeseries.jpg', width = 197)
  )
)

############################### CREATE THE BODY ################################
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "_all-skins.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "skin-yellow.min.css")
  ),
  
  # Write the UI reference of the modules
  tabItems(
    tabItem(tabName = "Tab0", tab_0_ui("tab_0"))
  )
)

#################### PUT THEM TOGETHER INTO A DASHBOARDPAGE ####################
dashboardPage(skin = "blue",
              dashboardHeader(title = "Predicciones ML"),
              sidebar,
              body
)