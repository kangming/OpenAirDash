#' timeProp_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timeProp_module_ui <- function(id){
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
        title = "PM10",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("timeProp_pm10_plot"),height = 650))

      ),
      box(
        title = "PM25",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("timeProp_pm25_plot"),height = 650))

      ),
      box(
        title = "O3-8h",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("timeProp_o3_plot"),height = 650))

      ),
      box(
        title = "SO2",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("timeProp_so2_plot"),height = 650))

      ),
      box(
        title = "CO",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",

        jqui_resizable(plotOutput(ns("timeProp_co_plot"),height = 650))

      ),
      box(
        title = "NO2",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        jqui_resizable(plotOutput(ns("timeProp_no2_plot"),height = 650))

      )
    )
  )
}

#' timeProp_module Server Functions
#'
#' @noRd
mod_timeProp_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    getTimePropPlot <- function(dt,pol) {
        data <- selectByDate(dt, year = input$curryear)
        wd <- timeProp(data,pollutant=pol,avg.time = "3 day",
                      proportion = "wd",
                      date.breaks = 10, key.position = "top",
                      key.columns = 8,main=paste0(ImProxy$name,'-',input$curryear,'年','-',pol,'风向比例图'))
        ws <- timeProp(data,pollutant=pol,avg.time = "3 day",
                       n.levels = 3,
                       cols = "viridis",
                       proportion = "ws", date.breaks = 10,
                       key.position = "top", key.columns = 3,main=paste0(ImProxy$name,'-',input$curryear,'年','-',pol,'风速比例图'))
        print(wd, split = c(1, 1, 1, 2))
        print(ws, split = c(1, 2, 1, 2), newpage = FALSE)
    }


    getDateStr <- function(dtstr){
      date <- ymd(dtstr)
      return(paste(mday(date),month(date),year(date),sep="/"))
    }


    output$timeProp_pm25_plot <- renderPlot(getTimePropPlot(ImProxy$data,"PM25"),res=96)
    output$timeProp_pm10_plot <- renderPlot(getTimePropPlot(ImProxy$data,"PM10"),res=96)
    output$timeProp_so2_plot <- renderPlot(getTimePropPlot(ImProxy$data,"SO2"),res=96)
    output$timeProp_no2_plot <- renderPlot(getTimePropPlot(ImProxy$data,"NO2"),res=96)
    output$timeProp_co_plot <- renderPlot(getTimePropPlot(ImProxy$data,"CO"),res=96)
    output$timeProp_o3_plot <- renderPlot(getTimePropPlot(ImProxy$data,"O3"),res=96)

  })
}

## To be copied in the UI
# mod_timeProp_module_ui("timeProp_module_1")

## To be copied in the server
# mod_timeProp_module_server("timeProp_module_1")
