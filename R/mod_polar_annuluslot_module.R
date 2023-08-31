#' polar_annuluslot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_polar_annuluslot_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "全局配置",
        closable = FALSE,
        width = 12,

        solidHeader = T,
        collapsible = T,
        column(
          width = 6,
          prettyRadioButtons(ns("pol"),
                              inline = T,"指标:",
                              choices = cfg_info$pol,
                              selected="PM10")),
        column(
          width = 6,
          prettyRadioButtons(
            ns("cols"),
            "配色",
            inline = T,
            choices = cfg_info$cols,
            selected = 'default'
          )
        )

      )
    ),

    fluidRow(
      box(
        title = "极环图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        jqui_resizable(plotOutput(ns("annulus_plot"),height = 650))

      )
    )
  )
}

#' polar_annuluslot_module Server Functions
#'
#' @noRd
mod_polar_annuluslot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    getAnnulusPlot <- function(data,period,title) {
      p <- polarAnnulus(data,cols = input$cols,pollutant=input$pol,main=getTile(ImProxy,title),period = period)
    }

    getPlot <- function(){
      trend <- getAnnulusPlot(ImProxy$data,'trend',"趋势分析")
      season <- getAnnulusPlot(ImProxy$data,'season',"季节分析")
      weekday <- getAnnulusPlot(ImProxy$data,'weekday',"工作日分析")
      hour <- getAnnulusPlot(ImProxy$data,'hour',"小时分析")
      print(trend, split = c(1, 1, 2, 2))
      print(season, split = c(2, 1, 2, 2), newpage = FALSE)
      print(trend, split = c(1, 2, 2, 2), newpage = FALSE)
      print(hour, split = c(2, 2, 2, 2), newpage = FALSE)
    }
    output$annulus_plot <- renderPlot(getPlot(),res=96)
  })
}

## To be copied in the UI
# mod_polar_annuluslot_module_ui("polar_annuluslot_module_1")

## To be copied in the server
# mod_polar_annuluslot_module_server("polar_annuluslot_module_1")
