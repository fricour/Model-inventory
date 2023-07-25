#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard leaflet reactable
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "Model Inventory"),
      dashboardSidebar(
        selectInput(inputId="var",label="Variable", choices = ModelInventory::dfvartype$var[which(ModelInventory::dfvartype$type == 'cat.excl')], selected = 'VertCoord'),
        selectInput(inputId="var_type",label="Type", choices = ModelInventory::dfvartype$var[which(ModelInventory::dfvartype$type == 'cat.mult')], selected = 'AssimPlatform')
      ),
      dashboardBody(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        fluidRow(
          box(width = 8, leafletOutput("map", width = "100%", height = 600)),
          box(width = 4, plotOutput("PiePlot", width = "100%", height = 600)),
          box(width = 8, reactableOutput("model_table")),
          box(width = 4, plotOutput("BarPlot"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ModelInventory"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
