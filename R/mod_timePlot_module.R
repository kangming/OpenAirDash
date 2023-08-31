#' timePlot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timePlot_module_ui <- function(id){
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
          prettyCheckboxGroup(ns("pol"),
                             "指标:",
                             inline = T,
                             choices = cfg_info$pol_wea,
                             selected=c("PM10","PM25"))),
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
        title = "时序变化图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("time_plot"),height = 650))

      ),
      box(
        title = "均值时序变化图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:115px',
            prettyRadioButtons(
              ns("avgtype"),
              "均值类型",
              inline = F,
              status = "primary",
              choices = list(
                "年度" = 'year',
                "季度" = 'quarter',
                "月度" = 'month',
                "周" = 'week',
                "日" = "day"
              ),
              selected = "month"
            )
          )
        ),
        jqui_resizable(plotOutput(ns("time_avgplot"),height = 650))

      ),
      box(
        title = "百分位数时序变化图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(

            numericInputIcon(
              inputId = ns("perValue"),
              label = "统计百分位数",
              value = 90,
              min = 5,
              max = 98,
              step = 5,
              icon = list(NULL, icon("percent"))
            )

          )
        ),
        jqui_resizable(plotOutput(ns("time_perplot"),height = 650))

      ),
      box(
        title = "数据标准化时序变化图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("time_norplot"),height = 650))

      )
    )
  )
}

#' timePlot_module Server Functions
#'
#' @noRd
mod_timePlot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    getTimePlot <- function(data) {
      p <- timePlot(data,cols = input$cols,pollutant=input$pol,main=getTile(ImProxy,NULL),
                    y.relation = "free",ylab="",stack=F)
    }
    getTimeAvgPlot <- function(data) {
      p <- timePlot(data,cols = input$cols,pollutant=input$pol,main=getTile(ImProxy,getAvgTitle()),
                    y.relation = "free" ,ylab="",avg.time = input$avgtype)
    }

    getTimePerPlot <- function(data) {
      p <- timePlot(data,cols = input$cols,pollutant=input$pol,main=getTile(ImProxy,paste0("月",input$perValue,'百分位浓度')),
                    y.relation = "free", avg.time = "month",ylab=""
                    ,statistic = "percentile", percentile = input$perValue)
    }

    getTimeNorPlot <- function(data) {
      p <- timePlot(data,
               pollutant = input$pol,
               avg.time = "year", normalise = getDateStr(),
               lwd = 4, lty = 1,
               main=getTile(ImProxy,"变化分析"),
               group = TRUE)
    }

    getAvgTitle <-function(){
      if(input$avgtype=="day")
        return("日均值")
      else if(input$avgtype=="week")
        return("周均值")
      else if(input$avgtype=="month")
         return("月均值")
      else if(input$avgtype=="year")
        return("年均值")
      else if(input$avgtype=="quarter")
        return("季度均值")
      else
        return('')
    }

    getDateStr <- function(){
      date <- ymd(ImProxy$sdate)
      return(paste(mday(date),month(date),year(date),sep="/"))
    }


    output$time_plot <- renderPlot(getTimePlot(ImProxy$data),res=96)
    output$time_avgplot <- renderPlot(getTimeAvgPlot(ImProxy$data),res=96)
    output$time_perplot <- renderPlot(getTimePerPlot(ImProxy$data),res=96)
    output$time_norplot <- renderPlot(getTimeNorPlot(ImProxy$data),res=96)
  })
}

## To be copied in the UI
# mod_timePlot_module_ui("timePlot_module_1")

## To be copied in the server
# mod_timePlot_module_server("timePlot_module_1")
