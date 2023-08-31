#' calendarPlot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calendarPlot_module_ui <- function(id){
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
          width = 4,
          prettyRadioButtons(ns("pol"),
                              "指标:",
                              inline = T,
                              choices = cfg_info$pol,
                              selected="PM10")),
        column(
          width = 4,
          selectInput(
            inputId =  ns("curryear"),
            label = "选择日历年份",
            choices = 2014:as.numeric(format(Sys.Date(),"%Y"))
          )
        ),
        column(
          width = 4,
          prettyRadioButtons(
            inputId =  ns("showtype"),
            label = "日历显示设置",
            choices = c("日期"="date","值"="value","风"="ws"),
            selected = "value"

          )
        )

      )
    ),

    fluidRow(
      box(
        title = "PM10",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("calendar_pm10_plot"),height = 650))

      ),
      box(
        title = "PM25",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("calendar_pm25_plot"),height = 650))

      ),
      box(
        title = "O3-8h",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("calendar_o3_plot"),height = 650))

      ),
      box(
        title = "SO2",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("calendar_so2_plot"),height = 650))

      ),
      box(
        title = "CO",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("calendar_co_plot"),height = 650))

      ),
      box(
        title = "NO2",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("calendar_no2_plot"),height = 650))

      )
    )
  )
}

#' calendarPlot_module Server Functions
#'
#' @noRd
mod_calendarPlot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## the labels - same for all species
    labels <- c(
      "优", "良", "轻度", "中度", "重度",
      "严重"
    )

    cols <- c("green", "yellow", "orange", "red","purple","brown")

    breaks <- list("O3"=c(0, 100, 160, 215, 265, 800, 1000),
                   "NO2"= c(0, 40, 80, 180, 240, 565, 750),
                   "PM10"=c(0, 50, 150, 250, 350, 420, 500),
                   "PM25"=c(0, 35, 75, 115, 150, 250, 350),
                   "CO"=c(0, 2, 4, 14, 24, 36, 48),
                   "SO2"=c(0, 50, 150, 475, 800, 1600, 2100)
                   )

    getPolCalendarPlot <- function(dt,pol) {
      print(input$curryear)
      data <- selectByDate(dt, year = input$curryear)
      if(pol=="O3")
      {
        dat <- rollingMean(data, pollutant = "O3", hours = 8,new.name =
                             "rollingo3", )
        p <- calendarPlot(dat,pollutant="rollingo3",annotate = input$showtype,
                          labels = labels,breaks = breaks[[pol]],statistic = "max",
                          cols = cols,main = paste0(ImProxy$name,'-',input$curryear,'年','-','O3-8h日历图'))
      }else
      p <- calendarPlot(data,pollutant=pol,annotate = input$showtype,
                        labels = labels,breaks = breaks[[pol]],cols = cols,main = paste0(ImProxy$name,'-',input$curryear,'年','-',pol,'日历图'))
    }

    getDateStr <- function(dtstr){
      date <- ymd(dtstr)
      return(paste(mday(date),month(date),year(date),sep="/"))
    }

    output$calendar_pm25_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"PM25"),res=96)
    output$calendar_pm10_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"PM10"),res=96)
    output$calendar_so2_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"SO2"),res=96)
    output$calendar_no2_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"NO2"),res=96)
    output$calendar_co_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"CO"),res=96)
    output$calendar_o3_plot <- renderPlot(getPolCalendarPlot(ImProxy$data,"O3"),res=96)
    # output$calendar_wind_plot <- renderPlot(getPolWindCalendarPlot(ImProxy$data),res=96)
    # output$time_splitplot <- renderPlot(getTimeSplitPlot(ImProxy$data),res=96)
    # output$time_featureplot <- renderPlot(getTimeFeaturePlot(ImProxy$data),res=96)
    # output$time_mplot <- renderPlot(getTimeMPlot(ImProxy$data),res=96)

  })
}

## To be copied in the UI
# mod_calendarPlot_module_ui("calendarPlot_module_1")

## To be copied in the server
# mod_calendarPlot_module_server("calendarPlot_module_1")
