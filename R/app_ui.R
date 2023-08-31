#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import waiter
#' @noRd
app_ui <- function(request) {
  tagList(
    useWaiter(),
    waiterOnBusy(color = "rgba(0,0,0,0.5)"),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    dashboardPage(
      title = "OpenAir Dashboard",
      # md = TRUE,
      # skin = "green",
      # controlbar = dashboardControlbar(),
      dashboardHeader(title = "OpenAir Dashboard"),
      dashboardSidebar(
        sidebarMenu(
        menuItem("玫瑰图",tabName = "windrose", icon = icon("th")),
        menuItem("极地图分析", tabName = "polarFre", icon = icon("th")),
        menuItem("百分比玫瑰图", tabName = "percent", icon = icon("th")),
        menuItem("极地图", tabName = "polar", icon = icon("th")),
        menuItem("极环图", tabName = "annulus", icon = icon("th")),
        menuItem("时序图", tabName = "time", icon = icon("th")),
        menuItem("时间变化", tabName = "timeviaria", icon = icon("th")),
        menuItem("日历图", tabName = "calendar", icon = icon("th")),
        menuItem("时间比例图",tabName = "timeprop", icon = icon("th")),
        menuItem("趋势热图",tabName = "trendlevel", icon = icon("th"))
      )),
      controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
      dashboardBody(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      mod_condition_module_ui("condition_module"),
      mod_summaryplot_module_ui("summaryplot_module_1"),
      tabItems(

        # First tab content
        tabItem(tabName = "polarFre",
                mod_polarFreq_module_ui("polarFreq_module_1")),
        # Second tab content
        tabItem(tabName = "windrose",
                mod_rose_module_ui("rose_module_1")
                ),
        tabItem(tabName = "percent",
                mod_percentile_roseplot_module_ui("percentile_roseplot_module_1")
                ),
        tabItem(tabName = "polar",
                mod_polar_module_ui("polar_module_1")),
        tabItem(tabName = "annulus",
                mod_polar_annuluslot_module_ui("polar_annuluslot_module_1")),
        tabItem(tabName = "time",
                mod_timePlot_module_ui("timePlot_module_1")),
        tabItem(tabName = "timeviaria",
                mod_timevariationPlot_module_ui("timevariationPlot_module_1")),
        tabItem(tabName = "calendar",
                mod_calendarPlot_module_ui("calendarPlot_module_1")),
        tabItem(tabName = "timeprop",
                mod_timeProp_module_ui("timeProp_module_1")),
        tabItem(tabName = "trendlevel",
                mod_trendLevel_module_ui("trendLevel_module_1"))



      ))
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
      app_title = "OpenAirApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
