#' trendLevel_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trendLevel_module_ui <- function(id){
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
                             "指标:",
                             inline = T,
                             choices = cfg_info$pol,
                             selected="PM10")),
        column(
          width = 6,
          selectInput(
            inputId =  ns("curryear"),
            label = "选择年份",
            choices = 2014:as.numeric(format(Sys.Date(),"%Y"))
          )
        )

      )
    ),

    fluidRow(
      box(
        title = "月-小时趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("month_hour_plot"),height = 650))

      ),
      box(
        title = "月-风向趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("month_wd_plot"),height = 650))

      ),box(
            title = "月-风速趋势图",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            collapsible = T,
            style = "overflow-x: scroll",
            jqui_resizable(plotOutput(ns("month_ws_plot"),height = 650))

      ),
      box(
        title = "SO2/NO2比值趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("year_wd_plot"),height = 650))

      ),
      box(
        title = "PM25/PM10比值趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("year_wd_pm_plot"),height = 650))

      ),
      box(
        title = "季度-昼夜趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("season_daylight_plot"),height = 650))

      ),
      box(
        title = "季度-小时趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("season_hour_year"),height = 650))

      ),
      box(
        title = "NO2与O3-工作日趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("weekday_wd"),height = 650))

      ),
      box(
        title = "周-小时趋势图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("week_hour_plot"),height = 650))

      ),
      box(
        title = "NO2、SO2与O3相关性分析",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("so2_no2_o3_plot"),height = 650))

      )
    )
  )
}

#' trendLevel_module Server Functions
#'
#' @noRd
mod_trendLevel_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    getTrendLevelPlot <- function(data,pol,x,y,type) {
      p<-trendLevel(data, pollutant = pol,x=x,y=y,type=type,main=getTile(ImProxy,pol))
    }

    getSoNoO3TrendLevelPlot <- function(data) {
      p<-trendLevel(data, x = "SO2", y = "NO2", pollutant = "O3",
                    border = "white",
                    n.levels = 30, statistic = "max",
                    limits = c(0, 50),main=getTile(ImProxy,NULL))
    }


    getDateStr <- function(dtstr){
      date <- ymd(dtstr)
      return(paste(mday(date),month(date),year(date),sep="/"))
    }

    output$month_hour_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,'month','hour','year'),res=96)
    output$month_wd_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,"month",'wd','year'),res=96)
    output$month_ws_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,"month",'ws','year'),res=96)
    output$year_wd_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,'NO2',"year",'wd','SO2/NO2'),res=96)
    output$year_wd_pm_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,'PM10',"year",'wd','PM25/PM10'),res=96)
    output$season_daylight_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,"season",'daylight','year'),res=96)
    output$season_hour_year <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,"season",'hour','year'),res=96)
    output$weekday_wd <- renderPlot(getTrendLevelPlot(ImProxy$data,"O3","NO2",'weekday','wd'),res=96)
    output$week_hour_plot <- renderPlot(getTrendLevelPlot(ImProxy$data,input$pol,"week",'hour','year'),res=96)
    output$so2_no2_o3_plot <- renderPlot(getSoNoO3TrendLevelPlot(ImProxy$data),res=96)

  })
}

## To be copied in the UI
# mod_trendLevel_module_ui("trendLevel_module_1")

## To be copied in the server
# mod_trendLevel_module_server("trendLevel_module_1")
