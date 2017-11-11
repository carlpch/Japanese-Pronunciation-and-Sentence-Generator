# Application

require(shiny)
require(RMeCab)
require(tidyverse)

ui <- fluidPage(
  fluidRow(
    column(6, offset = 0, 
           titlePanel("言語例文、音声生成トスト"),"ファン・ビーチェン（ph9xn@virginia.edu）", tags$hr(),
           textInput(inputId = "text", label = "日本語の言葉をご入力ください。", value = "紆余曲折"),
           actionButton(inputId = "submit", label = "検索"), tags$br())),
  fluidRow(
    column(12, offset=0, 
           HTML('<hr/>'),
           sliderInput(inputId = "slider", label = "値をご入力ください。", value = 50, min=0, max=100))
    )
)

server <- function(input, output){
  
}

shinyApp(ui=ui, server=server)