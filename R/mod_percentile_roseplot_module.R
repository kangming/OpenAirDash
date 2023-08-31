#' percentile_roseplot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_percentile_roseplot_module_ui <- function(id){
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
        title = "风玫瑰图",
        closable = FALSE,
        width = 6,
        solidHeader = T,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:115px',
            prettyRadioButtons(
              ns("per_wtype"),
              "类型",
              inline = T,
              choices = list("桨状" = 0, "扇状" = 1),
              selected = 0
            ),
            prettyRadioButtons(
              ns("per_wRoseCols"),
              "配色",
              inline = F,
              status = "primary",
              choices = cfg_info$cols
            )

            # downloadButton('Save as Image')
          )

        ),
        jqui_resizable(plotOutput(ns("per_windRose"),height = 650))

      ),
      box(
        title = "污染物风玫瑰",
        closable = FALSE,
        width = 6,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns("per_pol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),
            prettyRadioButtons(
              ns("per_pRoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("per_polRose"),height = 650))
      )
    ),
    fluidRow(
      box(
        title = "指标时间变化统计",
        closable = FALSE,
        width = 12,
        height = 600,
        solidHeader = T,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:270px',
            prettyCheckboxGroup(
              ns("per_datepol"),
              "指标",
              status = "primary",
              inline = T,
              choices = cfg_info$pol_wea,
              selected = c('PM25','PM10')
            ),
            prettyRadioButtons(
              ns("per_dateType"),
              "时间尺度",
              inline = T,
              choices = list(
                "全时段" = 'all',
                "年" = 'year',
                "季" = 'season',
                "月" = 'month',
                "周" = 'weekday',

                "小时" = "hour",
                "周内/周末" = "weekend"
                ,
                "季-日/夜"="daylightseason",
                "月-年" = "monthyear",
                "季-年" = "yearseason",
                "周-季" = 'weekdayseason',
                "年-月" = "yearmonth",
                "年-季" = "seasonyear",
                "季-周" = 'seasonweekday',
                "小时-周" = 'weekdayhour',
                "dst" = "dst",
                "日/夜" = "daylight"
              ),
              selected = 'all'
            ),
            prettyRadioButtons(
              ns("per_pDateRaoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )

          )
        ),
        jqui_resizable(plotOutput(ns("per_polOfDateRose"),height = 650))
      )
    ),
    fluidRow(
      box(
        title = "概率统计",
        closable = FALSE,
        width = 12,
        height = 600,
        solidHeader = T,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:270px',
            prettyRadioButtons(
              ns("per_cpfpol"),
              "统计指标",
              inline = T,
              status = "primary",
              choices = cfg_info$pol_wea,
              selected = 'PM25'
            ),
            numericInputIcon(
              inputId = ns("per_cpfValue"),
              label = "统计百分位数",
              value = 90,
              min = 5,
              max = 98,
              step = 5,
              icon = list(NULL, icon("percent"))
            ),

            prettyRadioButtons(
              ns("per_datePcfType"),
              "时间尺度",
              inline = T,
              choices = list(
                "全时段" = 'all',
                "年" = 'year',
                "季" = 'season',
                "月" = 'month',
                "周" = 'weekday',

                "小时" = "hour",
                "周内/周末" = "weekend"
                ,
                "季-日/夜"="daylightseason",
                "月-年" = "monthyear",
                "季-年" = "yearseason",
                "周-季" = 'weekdayseason',
                "年-月" = "yearmonth",
                "年-季" = "seasonyear",
                "季-周" = 'seasonweekday',
                "小时-周" = 'weekdayhour',
                "dst" = "dst",
                "日/夜" = "daylight"
              ),
              selected = 'all'
            ),

            prettyRadioButtons(
              ns("per_pPcfRaoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )

          )
        ),
        jqui_resizable(plotOutput(ns("per_polOfPcfRose"),height = 650))
      )
    )
  )
}

#' percentile_roseplot_module Server Functions
#'
#' @noRd
mod_percentile_roseplot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #---------------------Draw Plot function----------------------------
    getWindRose <- function(data) {
      p <- windRose(data,cols = input$per_wRoseCols,main=getTile(ImProxy,NULL))
    }

    getPerPolRose <- function(data) {
      p <-percentileRose(data,pollutant = input$per_pol,cols=input$per_pRoseCols,main=getTile(ImProxy,input$per_pol))
    }

    getPerPcfPolRose <- function(data) {
      # len <- length(input$per_cpfpol)
      # num <- ceiling(len/2)
      # if(len>=5){
      #   col <- num
      #   row <- 2
      # }else
      # {
      #   col <- len
      #   row <- 1
      # }
      ptype <- input$per_datePcfType
      if (ptype == 'monthyear')
        ptype = c("month", "year")
      else if (ptype == 'yearseason')
        ptype = c("season", "year")
      else if (ptype == 'weekdayseason')
        ptype = c("season", "weekday")
      else if (ptype == 'yearmonth')
        ptype = c("year", "month")
      else if (ptype == 'seasonyear')
        ptype = c("year", "season")
      else if (ptype == 'seasonweekday')
        ptype = c("weekday", "season")
      else if (ptype == 'weekdayhour')
        ptype = c("hour", "weekday")
      else if (ptype == 'daylightseason')
        ptype = c("season", "daylight")
      else if(ptype=='all')
        ptype='default'
      p <-percentileRose(data,pollutant = input$per_cpfpol,type=ptype,
                         cols=input$per_pPcfRaoseCols,percentile = input$per_cpfValue,
                         method = "cpf",main=getTile(ImProxy,input$per_cpfpol))
    }

    getPerPolOfDateRose <- function(data) {
      ptype <- input$per_dateType
      if (ptype == 'monthyear')
        ptype = c("month", "year")
      else if (ptype == 'yearseason')
        ptype = c("season", "year")
      else if (ptype == 'weekdayseason')
        ptype = c("season", "weekday")
      else if (ptype == 'yearmonth')
        ptype = c("year", "month")
      else if (ptype == 'seasonyear')
        ptype = c("year", "season")
      else if (ptype == 'seasonweekday')
        ptype = c("weekday", "season")
      else if (ptype == 'weekdayhour')
        ptype = c("hour", "weekday")
      else if (ptype == 'daylightseason')
        ptype = c("season", "daylight")
      else if(ptype=='all')
        ptype='default'
      p <-percentileRose(
        data,
        pollutant = input$per_datepol,
        type = ptype,
        cols = input$per_pDateRaoseCols,
        key.position = "bottom",
        main=getTile(ImProxy,NULL)
      )
    }


    #--------------------------output function----------------------------



      # waiter_hide() # hide the waiter
      output$per_windRose <- renderPlot({
        getWindRose(ImProxy$data)
      },res=96)

      output$per_polRose <- renderPlot({
        getPerPolRose(ImProxy$data)
      },res=96)

      output$per_polOfDateRose <- renderPlot({
        getPerPolOfDateRose(ImProxy$data)
      },res=96)

      output$per_polOfPcfRose <- renderPlot({
        getPerPcfPolRose(ImProxy$data)
      },res=96)

      print(as.character(now()))


      observe({
        color <- input$cols
        if(!is.null(color)){
          colsIds = c(
            "per_pDateRaoseCols",
            "per_wRoseCols",
            "per_pPcfRaoseCols",
            "per_pRoseCols"
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
            "per_pol",
            "per_cpfpol"
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
# mod_percentile_roseplot_module_ui("percentile_roseplot_module_1")

## To be copied in the server
# mod_percentile_roseplot_module_server("percentile_roseplot_module_1")
