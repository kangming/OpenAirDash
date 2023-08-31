#' summaryplot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summaryplot_module_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      box(
        title = "数据汇总图",
        width = 12,
        closable = FALSE,
        status = "warning",
        solidHeader = FALSE,
        collapsible = T,

        jqui_resizable(plotOutput(ns("summaryPlot"),height = 650))
      )
    )
  )
}

#' summaryplot_module Server Functions
#'
#' @noRd
mod_summaryplot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$summaryPlot <- renderPlot(summaryPlot(ImProxy$data),res=96)
  })
}

## To be copied in the UI
# mod_summaryplot_module_ui("summaryplot_module_1")

## To be copied in the server
# mod_summaryplot_module_server("summaryplot_module_1")
