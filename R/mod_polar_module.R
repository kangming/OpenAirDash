#' polar_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_polar_module_ui <- function(id){
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
        title = "风玫瑰",
        closable = FALSE,
        width = 6,
        solidHeader = T,
        collapsible = T,

        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:115px',
            prettyRadioButtons(
              ns("p_wtype"),
              "类型",
              inline = T,
              choices = list("桨状" = 0, "扇状" = 1),
              selected = 0
            ),
            prettyRadioButtons(
              ns("p_wRoseCols"),
              "配色",
              inline = F,
              status = "primary",
              choices = cfg_info$cols
            )
          )

        ),
        jqui_resizable(plotOutput(ns("plot"),height = 650))


      ),
      box(
        title = "污染物极坐标图",
        closable = FALSE,
        width = 6,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns( "p_pol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),
            prettyRadioButtons(
              ns("p_pRoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_polRose"),height = 650))
      )
    ),
    fluidRow(

      box(
        title = "不确定性",
        closable = FALSE,
        width = 12,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns( "p_unpol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),
            prettyRadioButtons(
              ns( "p_unRoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_unpolRose"),height = 650))
      )
    ),
    fluidRow(
      box(
        title = "指标时间变化统计",
        closable = FALSE,
        width = 12,
        solidHeader = T,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:270px',
            prettyCheckboxGroup(
              ns( "p_datepol"),
              "指标",
              inline = T,
              status = "primary",
              choices =cfg_info$pol_wea
            ),
            prettyRadioButtons(
              ns("p_dateType"),
              "时间尺度",
              inline = T,
              choices = cfg_info$dtype
            ),
            prettyRadioButtons(
              ns("p_pDateRaoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_polOfDateRose"),height = 650))
      )
    ),
    fluidRow(
      box(
        title = "NWR",
        closable = FALSE,
        width = 6,
        solidHeader = T,
        collapsible = T,

        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns("p_nwrpol"),
              "指标",
              inline = T,
              choices = cfg_info$pol_wea,
            ),
            prettyRadioButtons(
              ns("p_nwrCols"),
              "配色",
              inline = T,
              status = "primary",
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_nwrplot"),height = 650))
      ),
      box(
        title = "CPF",
        closable = FALSE,
        width = 6,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns("p_cpfpol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),
            numericInputIcon(
              inputId = ns("p_cpfValue"),
              label = "统计百分位数",
              value = 90,
              min = 5,
              max = 98,
              step = 5,
              icon = list(NULL, icon("percent"))
            ),
            prettyRadioButtons(
              ns("p_cpfCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )

          )
        ),
        jqui_resizable(plotOutput(ns("p_cpfplot"),height = 650))
      )
    ),
    fluidRow(
      box(
        title = "多个",
        closable = FALSE,
        width = 12,
        height = 600,
        solidHeader = T,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:270px',
            prettyCheckboxGroup(
              ns("p_morepcfpol"),
              "指标",
              inline = T,
              status = "primary",
              choices = cfg_info$pol_wea
            ),
            prettyRadioButtons(
              ns("p_moreCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )

          )
        ),
        jqui_resizable(plotOutput(ns("p_morecpfplot"),height = 650))
      )
    ),
    fluidRow(

      box(
        title = "极性图-百分比",
        closable = FALSE,
        width = 12,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns("p_perpol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),

            numericInputIcon(
              inputId = ns("p_percpfValue"),
              label = "统计百分位数",
              value = 90,
              min = 5,
              max = 98,
              step = 5,
              icon = list(NULL, icon("percent"))
            ),
            prettyRadioButtons(
              ns("p_perpRoseCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_perpolRose"),height = 650))
      )
    ),
    fluidRow(

      box(
        title = "关联性分析",
        closable = FALSE,
        width = 12,
        solidHeader = FALSE,
        collapsible = T,
        dropdownMenu = boxDropdown(
          div(
            style = 'padding:15px;width:250px',
            prettyRadioButtons(
              ns("p_relpol"),
              "指标",
              inline = T,
              choices = cfg_info$pol
            ),

            prettyRadioButtons(
              ns("p_relCols"),
              "配色",
              inline = T,
              choices = cfg_info$cols
            )
          )
        ),
        jqui_resizable(plotOutput(ns("p_relpolplot"),height = 650))
      )
    )
  )
}

#' polar_module Server Functions
#'
#' @noRd
mod_polar_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #---------------------Draw Plot function----------------------------
    getPloarPlotWindRose <- function(data) {
      p <- windRose(data,cols = input$p_wRoseCols, main=getTile(ImProxy,NULL))
    }

    getPloartPolPlot <- function(data) {
      p <-polarPlot(data,pollutant = input$p_pol,cols=input$p_pRoseCols, main=getTile(ImProxy,input$p_pol))
    }

    getPloartUnPolPlot <- function(data) {
      p <-polarPlot(data, pollutant = input$p_unpol,cols = input$p_unRoseCols, uncertainty = TRUE, main=getTile(ImProxy,input$p_unpol))
    }

    getPloartNWRPolPlot <- function(data) {
      p <-polarPlot(data, pollutant = input$p_nwrpol,cols = input$p_nwrCols,  statistic = 'nwr', main=getTile(ImProxy,input$p_nwrpol))
    }

    getPloartCPFPolPlot <- function(data) {
      p <-polarPlot(data, pollutant = input$p_cpfpol,cols = input$p_cpfCols, statistic = 'cpf',percentile = input$p_cpfValue, main=getTile(ImProxy,input$p_cpfpol))
    }

    getPolarMorCPFPlot <- function(data){
      l <- vector("list", 10)
      for (i in 1:10) {
        p<-polarPlot(data, pollutant = input$p_morepcfpol,cols = input$p_moreCols,
                     statistic = "cpf", percentile = c((i-1)*10, i*10), main=getTile(ImProxy,input$p_morepcfpol))
        l[[i]]<-p

      }

      multiplot(ls=l,rows=2)
    }

    getPolarPerPolPlot <-function(data){
      l <- vector("list", 2)
      l[[2]]<-polarPlot(data, pollutant = input$p_perpol,cols = input$p_perpRoseCols,
                        statistic = "cpf", percentile = input$p_percpfValue, main=getTile(ImProxy,input$p_perpol))
      l[[1]]<-polarPlot(data, pollutant = input$p_perpol,cols = input$p_perpRoseCols, main=getTile(ImProxy,input$p_perpol))

      multiplot(ls=l,rows=1)
    }


    getPolRelationPlot <- function(data){
      l <- vector("list", 2)
      l[[2]] <- polarPlot(data,
                poll = c("PM25", "PM10"),
                statistic = "robust_slope",
                col = "turbo",
                limits = c(0, 1),
                ws_spread = 1.5,
                wd_spread = 10
      )

      l[[1]] <- polarPlot(data,
                     poll = c("SO2", "NO2"),
                     statistic = "robust_slope",
                     col = "turbo",
                     limits = c(0, 1),
                     ws_spread = 1.5,
                     wd_spread = 10
      )
      multiplot(ls=l,rows=1)
    }


    getPPolOfDateRose <- function(data) {
      ptype <- input$p_dateType
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
      p <-polarPlot(
        data,
        pollutant = input$p_datepol,
        type = ptype,
        cols = input$p_pDateRaoseCols
        , main=getTile(ImProxy,input$p_datepol)
      )
    }


    #--------------------------output function----------------------------



    # createPolarRosePlot <- function(){
    #   # waiter_show( # show the waiter
    #   #   html = spin_wandering_cubes() # use a spinner
    #   # )
    #   print('perrose')
    #   print(as.character(now()))
    #   currPlotData_p <<- getPolarPlotData()
    #   print(as.character(now()))

      # waiter_hide() # hide the waiter
      output$plot <- renderPlot({
        getPloarPlotWindRose(ImProxy$data)
      },res=96)

      output$p_polRose <- renderPlot({
        getPloartPolPlot(ImProxy$data)
      },res=96)

      output$p_unpolRose <- renderPlot({
        getPloartUnPolPlot(ImProxy$data)
      },res=96)

      output$p_polOfDateRose <- renderPlot({
        getPPolOfDateRose(ImProxy$data)
      },res=96)

      output$p_nwrplot <- renderPlot({
        getPloartNWRPolPlot(ImProxy$data)
      },res=96)


      output$p_cpfplot <- renderPlot({
        getPloartCPFPolPlot(ImProxy$data)
      },res=96)


      output$p_morecpfplot <- renderPlot({
        getPolarMorCPFPlot(ImProxy$data)
      },res=96)

      output$p_perpolRose <- renderPlot({
        getPolarPerPolPlot(ImProxy$data)
      },res=96)

      output$p_relpolplot <- renderPlot({
        getPolRelationPlot(ImProxy$data)
      },res=96)



    #
    #   print(as.character(now()))
    # }


      observe({
        color <- input$cols
        if(!is.null(color)){
          colsIds = c(
            "p_wRoseCols",
            "p_pRoseCols",
            "p_unRoseCols",
            "p_pDateRaoseCols",
            "p_nwrCols",
            "p_cpfCols",
            "p_moreCols",
            "p_perpRoseCols",
            "p_relCols"
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
            "p_pol",
            "p_unpol",
            "p_morepcfpol",
            "p_perpol",
            "p_datepol",
            "p_nwrpol",
            "p_cpfpol",
            "p_relpol"
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
# mod_polar_module_ui("polar_module_1")

## To be copied in the server
# mod_polar_module_server("polar_module_1")
