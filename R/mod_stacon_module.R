#' stacon_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stacon_module_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' stacon_module Server Functions
#'
#' @noRd 
mod_stacon_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_stacon_module_ui("stacon_module_1")
    
## To be copied in the server
# mod_stacon_module_server("stacon_module_1")
