#' polarFreq_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_polarFreq_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      jqui_sortable(div(
        fluidRow(
          box(
            title = "全局配置",
            closable = FALSE,
            width = 12,

            solidHeader = T,
            collapsible = T,
            column(
              width = 6,
              prettyRadioButtons(ns("pol"),  inline = T,"指标:",choices = cfg_info$pol)
            ),
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
            title = "风频图",
            closable = FALSE,
            width = 6,
            solidHeader = T,
            collapsible = T,
            style = "overflow-x: scroll",
            dropdownMenu = boxDropdown(
              div(
                style = 'padding:15px;width:115px',

                prettyRadioButtons(
                  ns("polarWsCols"),
                  "配色",
                  inline = F,
                  status = "primary",
                  choices = cfg_info$cols
                )
              )
            ),
            jqui_resizable(plotOutput(ns("wsPolarFreq"),height = 650))
          ),
          box(
            title = "污染来源特征图",
            closable = FALSE,
            width = 6,
            solidHeader = FALSE,
            collapsible = T,
            style = "overflow-x: scroll",
            dropdownMenu = boxDropdown(
              div(
                style = 'padding:15px;width:250px',
                prettyRadioButtons(
                  ns("polarPol"),
                  "指标",
                  inline = T,
                  choices = cfg_info$pol
                ),
                prettyRadioButtons(
                  ns("statisticPol"),
                  "统计方法",
                  inline = T,
                  choices = list(
                    "weighted.mean" = "weighted.mean",
                    "mean" = 'mean',
                    "median" = 'median',
                    "max" = "max",
                    "stdev" = "stdev"

                  ),
                  selected = 'mean'
                ),
                prettyRadioButtons(
                  ns("polarPolCols"),
                  "配色",
                  inline = T,
                  choices = cfg_info$cols
                )
              )
            ),

            jqui_resizable(plotOutput(ns("polPolarFreq"),height = 650))
          )
        ),
        fluidRow(
          box(
            title = "风向对污染物平均浓度贡献比",
            width = 12,
            closable = FALSE,
            solidHeader = FALSE,
            collapsible = T,
            style = "overflow-x: scroll",
            dropdownMenu = boxDropdown(
              div(
                style = 'padding:15px;width:250px',
                prettyRadioButtons(
                  ns("polarCPol"),
                  "指标",
                  inline = T,
                  choices = cfg_info$pol
                ),
                prettyRadioButtons(
                  ns("pNRoseCols"),
                  "配色",
                  inline = T,
                  choices = cfg_info$cols
                )
              )
            ),
            jqui_resizable(plotOutput(ns("polPolar"),height = 500))
            # column(width = 6, plotOutput(ns("pollutionRoseForNorm"))),
            # column(width = 6,plotOutput(ns("polNormaliseRose")))
          )
        ),
        fluidRow(
          box(
            title = "污染来源分析-多尺度",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            collapsible = T,
            style = "overflow-x: scroll",
            dropdownMenu = boxDropdown(
              div(
                style = 'padding:15px;width:250px',
                prettyRadioButtons(
                  ns( "polarDatePol"),
                  "指标",
                  inline = T,
                  choices = cfg_info$pol_wea
                ),
                prettyRadioButtons(
                  ns("statisticDatePol"),
                  "统计方法",
                  inline = T,
                  choices = list(
                    "weighted.mean" = "weighted.mean",
                    "mean" = 'mean',
                    "median" = 'median',
                    "max" = "max",
                    "stdev" = "stdev"

                  ),
                  selected = 'weighted.mean'
                ),

                prettyRadioButtons(
                  ns( "polarPolDType"),
                  "时间尺度",
                  inline = T,
                  choices = cfg_info$dtype
                ),
                prettyRadioButtons(
                  ns("polarPolDateCols"),
                  "配色",
                  inline = F,
                  status = "primary",
                  choices =cfg_info$cols
                )
              )
            ),
            jqui_resizable(plotOutput(ns("polDatePolarFreq"),height = 500))
          )
        ),
        fluidRow(
          box(
            title = "风频图-多尺度",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            collapsible = T,
            style = "overflow-x: scroll",
            dropdownMenu = boxDropdown(
              div(
                style = 'padding:15px;width:250px',
                prettyRadioButtons(
                  ns("polarWsDType"),
                  "时间尺度",
                  inline = T,
                  choices = cfg_info$dtype
                ),

                prettyRadioButtons(
                  ns("polarWsDateCols"),
                  "配色",
                  inline = T,
                  choices = cfg_info$cols
                )
              )
            ),
            jqui_resizable(plotOutput(ns("wsDatePolarFreq"),height = 500))
          )

        )
      ))
    )
  )
}

#' polarFreq_module Server Functions
#'
#' @noRd
mod_polarFreq_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #---------------------Draw Plot function----------------------------
    getWsPolarFreq <- function() {
      p <- polarFreq(ImProxy$data,cols = input$polarWsCols,main=getTile(ImProxy,NULL))
    }

    getPolPolarFreq <- function() {
      p <- polarFreq(ImProxy$data,cols = input$polarPolCols,pollutant = input$polarPol,statistic = input$statisticPol,min.bin = 2,main=getTile(ImProxy,input$polarPol))
    }

    getWsDatePolarFreq <- function() {
      p <- polarFreq(ImProxy$data,cols = input$polarWsDateCols,type=input$polarWsDType,main=getTile(ImProxy,NULL))
    }

    getPolDatePolarFreq <- function() {
      p <- polarFreq(ImProxy$data,cols = input$polarPolDateCols,pollutant=input$polarDatePol,type=input$polarPolDType,statistic = input$statisticDatePol,min.bin = 2,main=getTile(ImProxy,input$polarDatePol))
    }

    getPolPolar <- function() {
      p <- polarFreq(ImProxy$data,cols = input$pNRoseCols,pollutant=input$polarCPol, ws.int = 30,
                     statistic = "weighted.mean",
                     offset = 80, trans = FALSE,main=getTile(ImProxy,input$polarCPol))
    }

    output$wsPolarFreq <- renderPlot({
      getWsPolarFreq()
    },res=96)

    output$polPolarFreq <- renderPlot({
      getPolPolarFreq()
    },res=96)

    output$wsDatePolarFreq <- renderPlot({
      getWsDatePolarFreq()
    },res=96)

    output$polDatePolarFreq <- renderPlot({
      getPolDatePolarFreq()
    },res=96)


    output$polPolar <- renderPlot({
      getPolPolar()
    },res=96)



    observe({
      color <- input$cols
      if(!is.null(color)){
        colsIds = c(
          "polarWsCols",
          "polarPolCols",
          "polarWsDateCols",
          "polarPolDateCols",
          "pNRoseCols"
        )
        for (id in colsIds) {
          updatePrettyRadioButtons(session = session,
                                   inputId = id,
                                   selected = color)
        }
      }

    })

    observe({
      pol <- input$pol
      if(!is.null(pol)){
        polIds = c(
          "polarPol",
          "polarCPol",
          "polarDatePol"
        )
        for (id in polIds) {
          updatePrettyRadioButtons(session = session,
                                   inputId = id,
                                   selected = pol)
        }
      }
    })
  })
}

## To be copied in the UI
# mod_polarFreq_module_ui("polarFreq_module_1")

## To be copied in the server
# mod_polarFreq_module_server("polarFreq_module_1")
