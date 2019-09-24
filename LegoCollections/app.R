#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(corpus)
library(ggplot2)
library(DT)
library(dplyr)
library(tools)
LegoData <- read.csv('LEGO2019.csv', stringsAsFactors = FALSE)
all_themes <- unique(LegoData$Theme)
getTermMatrix <- memoise(function(sub_legodata) {
   sep_theme <- sub_legodata[, 5]
   theme_corpus = Corpus(VectorSource(sep_theme))
   theme_corpus = tm_map(theme_corpus, content_transformer(tolower))
   theme_corpus = tm_map(theme_corpus, removeNumbers)
   theme_corpus = tm_map(theme_corpus, removePunctuation)
   theme_corpus = tm_map(theme_corpus, removeWords, stopwords())
   name_cluster = TermDocumentMatrix(theme_corpus,
                                     control = list(minWordLength = 1))
   c = as.matrix(name_cluster)
   sort(rowSums(c), decreasing = TRUE)
})

# Define UI for application that draws a histogram
ui <- navbarPage('LEGO Roster',
                 tabPanel('Name Cluster',
                          titlePanel('Name Cluster for All Themes'),
                          sidebarLayout(
                             sidebarPanel(
                                checkboxGroupInput(inputId = 'year',
                                                   label = 'Select interested year(s):',
                                                   choices = c('2018', '2019'),
                                                   selected = '2019'),
                                selectInput(inputId = 'theme',
                                            label = 'Select a set theme',
                                            choices = all_themes),
                                actionButton(inputId = 'get_theme',
                                             label = 'Create Set Name Cluster'),
                                hr(),
                                sliderInput(inputId = "freq",
                                            "Minimum Frequency:",
                                            min = 1,  max = 20, value = 10),
                                sliderInput(inputId = "word_max",
                                            "Maximum Number of Words:",
                                            min = 1,  max = 50,  value = 20)
                             ),
                             mainPanel(
                                plotOutput("wcplot")
                             )
                          )
                     ),
                 tabPanel('Histogram',
                          title = 'Histogram',
                          sidebarLayout(
                             sidebarPanel(
                                checkboxGroupInput(inputId = 'year_his',
                                                   label = 'Select interested year(s):',
                                                   choices = c('2018', '2019'),
                                                   selected = '2019'),
                                selectInput(inputId = "bin_num",
                                            label = "Number of bins in histogram (approximate):",
                                            choices = c(10, 20, 35, 50),
                                            selected = 20),
                                sliderInput(inputId = "n_samp", 
                                            label = "Number of Samples:",  
                                            min = 1, max = 500, value = 200, step = 50)
                                 ),
                             mainPanel(
                                tabsetPanel(
                                   tabPanel('Pieces',plotOutput('piece_histogram')), 
                                   tabPanel('US Price', plotOutput('price_histogram'))
                                   )
                                )
                             )
                 ),
                 tabPanel('Own/Want Ratio',
                          title = 'Possessing Ratio',
                          sidebarLayout(
                             sidebarPanel(position = 'right',
                                sliderInput(inputId = "n_samp_ratio", 
                                            label = "Number of Samples:",  
                                            min = 1, max = 1000, value = 500, step = 100),
                                sliderInput(inputId = "size", 
                                            label = "Point Size:", 
                                            min = 0, max = 5, 
                                            value = 2)
                             ),
                          tabsetPanel(
                             tabPanel('Scatter Plot', plotOutput('scatter')),
                             tabPanel('Data Table', DT::dataTableOutput(outputId = "sample_table"))
                              )
                          )
                     )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   # nav1
   lego_sub_name <- reactive({
      req(input$year)
      req(input$theme)
      filter(LegoData, (Year %in% as.numeric(input$year)) & (Theme == input$theme))
   })
   # text <- reactive({
   #    input$get_theme
   #    isolate({
   #          getTermMatrix(lego_sub_name(), input$theme)
   #    })
   # })
   wordcloud_rep <- repeatable(wordcloud)
   output$wcplot <- renderPlot({
      v <- getTermMatrix(lego_sub_name())
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$word_max,
                    colors=brewer.pal(8, "Set3"))
   })
   
   # nav2
   lego_sub_his <- reactive({
      req(input$year_his)
      filter(LegoData, Year %in% as.numeric(input$year))
   })
   observe({
      val <- input$n_samp
      updateSliderInput(session, 
                         inputId = "n_samp",
                         value = val)
   })
   # Get new sample
   his_sample <- reactive({
      req(input$n_samp)
      sample_n(lego_sub_his(), input$n_samp) 
   })
   
   observe({
      x <- input$bin_num
      updateSelectInput(session, 
                        inputId = "bin_num",
                        choices = c(10, 20, 35, 50),
                        selected = x)
   })
   output$price_histogram <- renderPlot({
      hist(his_sample$USPrice,
           probability = TRUE,
           breaks = input$bin_num,  #??????
           xlab = "US Price",
           main = "Lego Set US Price Histogram")
   })
   output$piece_histogram <- renderPlot({
      hist(his_sample()$Pieces,
           probability = TRUE,
           breaks = input$bin_num,
           xlab = "Pieces",
           main = "Lego Set Pieces Histogram")
   })
   # nav3
   ratio_sample <- reactive({
      invalidateLater(millis = 8000)
      req(input$n_samp_ratio)
      sample_n(LegoData, input$n_samp_ratio) 
   })
   output$scatter <- renderPlot({ # Error in aes_string: object 'Theme' not found
      ggplot(data = ratio_sample(), aes_string(x = Wantedby, y = Ownedby, color = Theme)) +   
         geom_point(size = input$size) + 
         labs(x = 'Wanted by people (numbers)',
              y = 'Owned by people (numbers)',
              title = "Possessing Ratio")
   })
   output$sample_table <- DT::renderDataTable({
         DT::datatable(data = ratio_sample()[, 4:12], 
                       options = list(pageLength = 20), 
                       rownames = FALSE)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

