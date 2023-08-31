#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RPostgreSQL
#' @import pool
#' @import lubridate
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  drv <- dbDriver("PostgreSQL")
  pool <- dbPool(
    drv,
    host = "139.159.137.207",
    #主机名，默认localhost
    port = '5432',
    #端口号，默认5432
    dbname = 'openair',
    #数据库
    user = 'postgres',
    #用户名
    password = 'km619150'
  )#密码

  ImProxy <- mod_condition_module_server("condition_module",pool)
  mod_rose_module_server("rose_module_1",ImProxy)
  mod_polar_module_server("polar_module_1",ImProxy)
  mod_polarFreq_module_server("polarFreq_module_1",ImProxy)
  mod_summaryplot_module_server("summaryplot_module_1",ImProxy)
  mod_percentile_roseplot_module_server("percentile_roseplot_module_1",ImProxy)
  mod_polar_annuluslot_module_server("polar_annuluslot_module_1",ImProxy)
  mod_timePlot_module_server("timePlot_module_1",ImProxy)
  mod_timevariationPlot_module_server("timevariationPlot_module_1",ImProxy)
  mod_calendarPlot_module_server("calendarPlot_module_1",ImProxy)
  mod_timeProp_module_server("timeProp_module_1",ImProxy)
  mod_trendLevel_module_server("trendLevel_module_1",ImProxy)
}
