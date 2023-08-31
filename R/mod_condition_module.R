#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import tidyverse
#' @importFrom dplyr left_join mutate
mod_condition_module_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      box(
        title = "条件选择",
        width = 12,
        height = 300,
        closable = FALSE,
        status = "warning",
        solidHeader = FALSE,
        collapsible = T,
        column(
          width = 4,
          uiOutput(ns("citySelect")),

          selectInput(ns("metasta"), "气象站:",choices = NULL),
        ),
        column(
          width = 4,
          selectInput(ns("airsta"), "空气站:",choices = NULL),
          dateRangeInput(ns("mdate"), '时间范围', start = "2014-04-01" ,
                         language = "zh-CN",min="2014-04-01",
                         end = NULL)
        ),
        column(
          width = 4,
          prettyRadioButtons(
            ns("pollType"),
            "分析类型",
            inline = T,
            choices = list(
              "城市" = 0,
              "站点" = 1
            ),
            selected = 0
          ),
          actionBttn(
            ns("getData"),
            label = "统计分析",
            icon = icon("search"),
            color = 'primary',
            style = "bordered",
            size = 'xs'
          )
        )
      )
    )

  )
}

#' name_of_module1 Server Functions
#'
#' @noRd
mod_condition_module_server <- function(id,pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # 共享数据
    vals <- reactiveValues()

    cityList <- queryCityList(pool)

    output$citySelect <- renderUI({
      selectInput(ns("city"), "城市:",
                  cityList$name)
    })

    observe({
      x <- input$city
      if(!is.null(x)){
        air_sta_list <- queryAirSiteList(pool,x)
        updateSelectInput(session, "airsta",
                          choices = air_sta_list
        )
      }
    })

    observe({
      y <- strsplit(input$airsta,"_")[[1]][2]
      print(y)
      if(!is.null(y)){
        meta_sta_list <- as.list(queryMetaSiteList(pool,y))
        print(meta_sta_list)
        updateSelectInput(session, "metasta",
                          choices = meta_sta_list
        )
      }

    })


    observeEvent(input$metasta,{
      # waiter_show( # show the waiter
      #   html = spin_fading_circles() # use a spinner
      # )
      air <- strsplit(input$airsta,"_");
      meta <- strsplit(input$metasta,"_");
      name <- input$city
      acode<- air[[1]][2]
      mcode <-meta[[1]][2]
      ptype <- input$pollType
      code <- ifelse(ptype==0,name,acode)
      data <- queryAnalysisData(pool,code,mcode,ptype,input$mdate[1], input$mdate[2])
      data <- mutate(data, 'SO2/NO2' = SO2 / NO2)
      data <- mutate(data, 'PM25/PM10' = PM25 / PM10)
      vals$data <- data
      vals$angle <- input$angleGlobal
      vals$color <- input$colsGlobal
      vals$pol <- input$pol
      vals$aname<- air[[1]][1]
      vals$mname<- meta[[1]][1]
      vals$name<-ifelse(ptype==0,name,air[[1]][1])
      vals$sdate<-input$mdate[1]
      vals$edate<-input$mdate[2]
      # waiter_hide() # hide the waiter
    },once=TRUE,ignoreNULL = TRUE,ignoreInit = TRUE)

    observeEvent(input$getData,{
      # waiter_show( # show the waiter
      #   html = spin_fading_circles() # use a spinner
      # )
      air <- strsplit(input$airsta,"_");
      meta <- strsplit(input$metasta,"_");
      name <- input$city
      acode<- air[[1]][2]
      mcode <-meta[[1]][2]
      ptype <- input$pollType
      code <- ifelse(ptype==0,name,acode)
      data <- queryAnalysisData(pool,code,mcode,ptype,input$mdate[1], input$mdate[2])
      data <- mutate(data, 'SO2/NO2' = SO2 / NO2)
      data <- mutate(data, 'PM25/PM10' = PM25 / PM10)
      vals$data <- data
      vals$angle <- input$angleGlobal
      vals$color <- input$colsGlobal
      vals$pol <- input$pol
      vals$aname<- air[[1]][1]
      vals$mname<- meta[[1]][1]
      vals$sdate<-input$mdate[1]
      vals$edate<-input$mdate[2]
      vals$name<-ifelse(ptype==0,name,air[[1]][1])
      # print(vals$data)
      # waiter_hide() # hide the waiter
    },ignoreNULL = TRUE,ignoreInit = FALSE)

    return(vals)
  })
}


## To be copied in the UI
# mod_condition_module_ui("condition_module_1")

## To be copied in the server
# mod_condition_module_server("condition_module_1")
