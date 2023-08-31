#' rose_module UI Function
#'
#' @description Wind Rose and Pollutant Rose Plot Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import openair
#' @import shinyjqui
#' @import lubridate
mod_rose_module_ui <- function(id){
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
          prettyRadioButtons(ns("pol"),  inline = T,"指标:",choices = cfg_info$pol)
        ),
        column(
          width = 4,
          prettyRadioButtons(
            ns("angle"),
            "风向选择",
            inline = T,
            choices = list(
              "8风向" = 45,
              "12风向" = 30,
              "16风向" = 22.5,
              "36风向" = 10
            ),
            selected = 30
          )
        ),
        column(
          width = 4,
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
    jqui_sortable(div(

      fluidRow(
        box(

          title = "风玫瑰",
          closable = FALSE,
          width = 6,
          solidHeader = T,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:115px',
              prettyRadioButtons(
                ns("wtype"),
                "类型",
                inline = T,
                choices = list("桨状" = 0, "扇状" = 1),
                selected = 0
              ),
              prettyRadioButtons(
                ns("wRoseCols"),
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
          title = "污染物风玫瑰",
          closable = FALSE,
          width = 6,
          solidHeader = FALSE,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:250px',
              prettyRadioButtons(
                ns("pols"),
                "指标",
                inline = T,
                choices = cfg_info$pol
              ),
              prettyRadioButtons(
                ns("pRoseCols"),
                "配色",
                inline = T,
                choices = cfg_info$cols
              )
            )
          ),

          jqui_resizable(plotOutput(ns("pollutionRose"),height = 650))
        )
      ),
      fluidRow(
        box(
          title = "污染物风玫瑰-归一化",
          width = 12,
          closable = FALSE,
          solidHeader = FALSE,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:250px',
              prettyRadioButtons(
                ns("poln"),
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
          jqui_resizable(plotOutput(ns("pollutionRoseForNorm"),height = 500))
          # column(width = 6, plotOutput(ns("pollutionRoseForNorm"))),
          # column(width = 6,plotOutput(ns("polNormaliseRose")))
        )
      ),
      fluidRow(
        box(
          title = "浓度与气象关联性分析",
          closable = FALSE,
          width = 12,
          solidHeader = T,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:250px',
              prettyRadioButtons(
                ns( "polType"),
                "指标",
                inline = T,
                choices = cfg_info$pol
              )
              ,
              prettyRadioButtons(
                ns( "pDataRaoseCols"),
                "配色",
                inline = T,
                choices = cfg_info$cols
              ),
              prettyRadioButtons(
                ns( "ly"),
                "布局",
                inline = T,
                choices =cfg_info$ly
              )
            )
          ),
          jqui_resizable(plotOutput(ns("windRosePolType"),height = 500))
        )
      ),
      fluidRow(
        box(
          title = "指标关联系分析",
          closable = FALSE,
          width = 12,
          solidHeader = T,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:250px',
              prettyRadioButtons(
                ns( "apol"),
                "分析指标",
                inline = T,
                choices = cfg_info$pol
              ),
              prettyRadioButtons(
                ns("cpol"),
                "关联指标",
                inline = T,
                choices = cfg_info$pol_wea,
                selected = 'PM10'
              ),
              prettyRadioButtons(
                ns("pCRaoseCols"),
                "配色",
                inline = T,
                choices = cfg_info$cols
              ),
              prettyRadioButtons(
                ns("ly1"),
                "布局",
                inline = T,
                choices  =cfg_info$ly
              )
            )
          ),
          jqui_resizable(plotOutput(ns("polCorrelationRose"),height = 500))
        )
      ),
      fluidRow(
        box(
          title = "时间变化分析",
          closable = FALSE,
          width = 12,
          height = 900,
          solidHeader = T,
          collapsible = T,
          style = "overflow-x: scroll",
          dropdownMenu = boxDropdown(
            div(
              style = 'padding:15px;width:270px',
              prettyRadioButtons(
                ns("datepol"),
                "指标",
                inline = T,
                choices = cfg_info$pol_wea
              ),
              prettyRadioButtons(
                ns("dateType"),
                "时间尺度",
                inline = T,
                choices = cfg_info$dtype,
                selected = 'year'
              ),
              prettyRadioButtons(
                ns("pDateRaoseCols"),
                "配色",
                inline = T,
                choices = cfg_info$cols
              )

            )
          ),
          jqui_resizable(plotOutput(ns("polOfDateRose"),height = 750))
        )
      )
    ))

  )
}

#' rose_module Server Functions
#'
#' @noRd
mod_rose_module_server <- function(id,ImProxy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #--------------------------output function----------------------------
      output$plot <- renderPlot({
        ifelse(input$wtype == 0,
               windRose(ImProxy$data,cols = input$wRoseCols,angle = as.numeric(input$angle),main=getTile(ImProxy,NULL)),
               pollutionRose(ImProxy$data,pollutant = "ws", cols = input$wRoseCols,angle = as.numeric(input$angle),main=getTile(ImProxy,NULL))
               )
      },res=96)

      output$pollutionRose <- renderPlot({
        pollutionRose(ImProxy$data,pollutant = input$pols, cols = input$pRoseCols,angle = as.numeric(input$angle),main=getTile(ImProxy,input$pols))
      },res=96)

      output$pollutionRoseForNorm <- renderPlot({
        a <-pollutionRose(ImProxy$data,pollutant = input$poln, cols = input$pNRoseCols,angle = as.numeric(input$angle),main=getTile(ImProxy,input$poln))
        b <-  pollutionRose(
          ImProxy$data,
          pollutant = input$poln,
          normalise = T,
          seg = 1,
          cols = input$pNRoseCols,angle = as.numeric(input$angle),main=getTile(ImProxy,input$poln)
        )
        print(a, split = c(1, 1, 2, 1))
        print(b, split = c(2, 1, 2, 1), newpage = FALSE)

      },res=96)

      # output$polNormaliseRose <- renderPlot({
      #
      # })

      output$windRosePolType <- renderPlot({
        row <- 1
        col <- 4
        if (input$ly == 1) {
          row <- 2
          col <- 2
        }
        p <- windRose(
          ImProxy$data,
          type = input$polType,
          cols = input$pDataRaoseCols,
          main=getTile(ImProxy,input$polType),
          layout = c(col, row),angle =as.numeric(input$angle)
        )
      },res=96)

      output$polCorrelationRose <- renderPlot({
        row <- 1
        col <- 4
        if (input$ly1 == 1) {
          row <- 2
          col <- 2
        }

        p <- pollutionRose(
          ImProxy$data,
          pollutant = input$cpol,
          type = input$apol,
          cols = input$pCRaoseCols,
          layout = c(col, row),
          main=getTile(ImProxy,input$apol),
          key.position =  "bottom",angle = as.numeric(input$angle)
        )

      },res=96)

      output$polOfDateRose <- renderPlot({

        ptype <- input$dateType
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
        p <-pollutionRose(
          ImProxy$data,
          pollutant = input$datepol,
          type = ptype,
          cols = input$pDateRaoseCols,
          key.position = "bottom",
          main=getTile(ImProxy,input$datepol),
          angle = as.numeric(input$angle)
        )
      },res=96)



      observe({
        color <- input$cols
        if(!is.null(color)){
          colsIds = c(
            "wRoseCols",
            "pRoseCols",
            "pNRoseCols",
            "pDataRaoseCols",
            "pCRaoseCols",
            "pDateRaoseCols"
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
            "pols",
            "poln",
            "polType",
            "apol",
            "datepol"
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

