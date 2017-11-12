# Application

require(shiny)
require(RMeCab)
require(tidyverse)
library(rvest)
library(tm)
library(stringr)
library(httr)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML('
       .shiny-input-container {
          width:100%;
       }
           ')
      
    )
  ),
  titlePanel("言語例文、音声生成トスト"),
  "ファン・ビーチェン（",
  tags$a(href="mailto: ph9xn@virginia.edu", "ph9xn@virginia.edu"),
  "）", tags$br(),tags$br(), 
  div(
  tabsetPanel(
    tabPanel("単語", tags$br(),
           textInput(inputId = "text", label = "日本語の言葉をご入力ください。", value = "紆余曲折"),
           actionButton(inputId = "submitWord", label = "検索"), tags$br(), 
           tableOutput(outputId = "table")),
    tabPanel("文の分析", 
             tags$br(), "日本語の文、あるいは文章をペーストしてください",
             textAreaInput(inputId = "article", label = "", 
                      rows = 5, width="600px",
                       value = "　新国立競技場が２０２０年東京五輪・パラリンピックの後、８万人規模の球技専用競技場に生まれ変わる見通しになった。収益を確保するためだが、年２４億円と見込まれる管理維持費をまかなえるのか、不透明な部分が多い。"),
             actionButton(inputId = "submitArticle", label = "分析"), 
             htmlOutput(outputId = "analyzed_article")
             ),
    tabPanel("CSVのアプロード", 
             "準備中です。")
    ))
  )



server <- function(input, output){
  
  goo_id <- "adf6e23c9cc3208fc01c85f45362c1eb6fdb068b6d3e883e87f18d57eefae1c9"
  
  # for TabPanel 1 (Word)
  data <- eventReactive(input$submitWord, {
    # Working on each element first, and then present the data in tibble at last:
    
    # 1/3 Example Sentence (contents extracted from Yourei.com)
    yourei <- read_html(paste("http://yourei.jp/", input$text, sep = "")) %>% 
      html_nodes(xpath = "//li[@id='sentence-1']/*") %>% as.String()
    
    # 2/3 Hiragana Conversion (sponsored by Goo! API)
    hiragana_api <- POST(url = "https://labs.goo.ne.jp/api/hiragana", 
                         body = list("app_id"=goo_id,
                                     "sentence"=input$text, 
                                     "output_type"="hiragana"), encode = "json")
    
    # 3/3 Audio file (purchased from Forvo)
    base_url <- "https://apifree.forvo.com/key/7bc97d43becc4bfd4280c37f90070d05/format/xml/action/standard-pronunciation/word/"
    forvo_url = paste(base_url, input$text, "/language/ja", sep = "")
    xml <- forvo_url %>% read_xml() %>% as_list()
    mp3url <-  xml$item$pathmp3 %>% unlist() %>% toString()
    themp3 <- paste("<audio controls>
                    <source src= ", mp3url, ", type='audio/mp3'>
                    </audio>", sep = "")
    
    tibble(
      "入力" = input$text,
      "平仮名" =  content(hiragana_api)$converted,
      "例文" = yourei,
      "音声" = themp3
      )
  })

  output$table <- renderTable({data()}, sanitize.text.function = function(x) x)
  # the ", sanitize.text.function = function(x) x)" above, powerfully renders htmls within tibble, yay.
  
  paragraph <- eventReactive(input$submitArticle, {
    
    # Goo API again for 形態素解析
    word_list <- 
      POST(url = "https://labs.goo.ne.jp/api/morph", 
           body = list("app_id"=goo_id, 
                       "sentence"=input$article, 
                       "info_filter"="form"), encode = "json") %>% 
      content() %>% 
      unlist()
    
    output_article <- paste(word_list[c(-1,-2)], collapse = " ")
  })
  output$analyzed_article <- renderText({paragraph()})
}

shinyApp(ui=ui, server=server)