#' timevariationPlot_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timevariationPlot_module_ui <- function(id){
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
        title = "时间变化图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
            prettyRadioButtons(
              ns("variapol"),
              "指标选择",
              inline = F,
              status = "primary",
              choices = cfg_info$pol,
              selected="PM10")
          )
        ),
        jqui_resizable(plotOutput(ns("time_variaplot"),height = 650))

      ),
      box(
        title = "多指标时间变化分析图",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:115px',
            prettyCheckboxGroup(
              ns("pols"),
              "指标选择",
              inline = F,
              status = "primary",
              choices = cfg_info$pol,
              selected=c("PM10","PM25","NO2","CO","SO2","O3"))
            )
          ),
        jqui_resizable(plotOutput(ns("time_morepolplot"),height = 650))

      ),
      box(
        title = "时段对比分析",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
           dateInput(ns("splitdate"), "分割时间",value='2018-01-01'),
           prettyRadioButtons(
             ns("splitpol"),
             "指标选择",
             inline = F,
             status = "primary",
             choices = cfg_info$pol,
             selected="PM10")
          )
        ),
        jqui_resizable(plotOutput(ns("time_splitplot"),height = 650))

      ),
      box(
        title = "特征变化分析",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
            prettyRadioButtons(
              ns("featurepol"),
              "指标选择",
              inline = F,
              status = "primary",
              choices = cfg_info$pol,
              selected="PM10")
          )
        ),
        jqui_resizable(plotOutput(ns("time_featureplot"),height = 650))

      ),
      box(
        title = "分位数统计",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        style = "overflow-x: scroll",
        dropdownMenu = boxDropdown(
          div(
            prettyRadioButtons(
              ns("mpol"),
              "指标选择",
              inline = F,
              status = "primary",
              choices = cfg_info$pol,
              selected="PM10")
          )
        ),
        jqui_resizable(plotOutput(ns("time_mplot"),height = 650))

      )
    )
  )
}

#' timevariationPlot_module Server Functions
#'
#' @noRd
mod_timevariationPlot_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    getTimeVariaPlot <- function(data) {
      p <- timeVariation(data,pollutant=input$variapol,main=getTile(ImProxy,input$variapol))
    }
    getTimeMorePolPlot <- function(data) {
      p <- timeVariation(data,pollutant=input$pols,main=getTile(ImProxy,NULL),normalise = TRUE)
    }

    getTimeSplitPlot <- function(data) {
      splitData <- splitByDate(data, dates= getDateStr(input$splitdate),
                               labels = c(paste0("Before ",getDateStr(input$splitdate)),paste0("After ",getDateStr(input$splitdate))))

      p <- timeVariation(splitData, pollutant = input$splitpol,
                    group = "split.by",
                    difference = TRUE,main=getTile(ImProxy,input$splitpol))
    }

    getTimeFeaturePlot <- function(data) {
      feadata <- mutate(data,
                       feature = ifelse(ws > 4 & wd > 0 & wd <= 180, "easterly", "other"))

      p <- timeVariation(feadata, pollutant =input$featurepol, group = "feature",
                    difference = TRUE)
    }

    getTimeMPlot <- function(data) {
      p <- timeVariation(data,pollutant=input$mpol,main=getTile(ImProxy,NULL),statistic = "median",
                         col = "firebrick")
    }

    getDateStr <- function(dtstr){
      date <- ymd(dtstr)
      return(paste(mday(date),month(date),year(date),sep="/"))
    }


    output$time_variaplot <- renderPlot(getTimeVariaPlot(ImProxy$data),res=96)
    output$time_morepolplot <- renderPlot(getTimeMorePolPlot(ImProxy$data),res=96)
    output$time_splitplot <- renderPlot(getTimeSplitPlot(ImProxy$data),res=96)
    output$time_featureplot <- renderPlot(getTimeFeaturePlot(ImProxy$data),res=96)
    output$time_mplot <- renderPlot(getTimeMPlot(ImProxy$data),res=96)



    # observe({
    #   data <- ImProxy
    #   print(data)
    #   if(!is.null(data$sdate)){
    #     diff = as.numeric(difftime(ymd(ImProxy$sdate),
    #                                ymd(ImProxy$edate), units = "days"))
    #     updateDateInput(session=session,inputId='splitdate',
    #                     value = ymd(ImProxy$sdate)+days(diff),
    #                     min=ImProxy$sdate,max=ImProxy$edate)
    #   }
    #
    # })


  })
}

## To be copied in the UI
# mod_timevariationPlot_module_ui("timevariationPlot_module_1")

## To be copied in the server
# mod_timevariationPlot_module_server("timevariationPlot_module_1")
