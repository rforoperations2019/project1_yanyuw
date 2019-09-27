#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(corpus)
library(ggplot2)
library(DT)
library(dplyr)
library(tools)
library(plotly)
library(shinydashboard)

# global data clean and processing for word cluster
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

# dashboard layout
header <- dashboardHeader(title = 'LEGO Roster')
sidebar <- dashboardSidebar(
   sidebarMenu(id = 'navbar',
               menuItem('Name Cluster', tabName = 'cluster', icon = icon('book')),
               menuItem('Histogram', tabName = 'histogram', icon = icon('chart-bar')),
               menuItem('Possessing Ratio', tabName = 'ratio', icon = icon('percent'))
               )
            )
body <- dashboardBody(
   tabItems(
      tabItem(
         tabName = 'cluster',
         fluidRow(
            box(
               checkboxGroupInput(inputId = 'year',
                                  label = 'Select interested year(s):',
                                  choices = c('2018', '2019'),
                                  selected = '2019'),
               selectInput(inputId = 'theme',
                           label = 'Select a set theme',
                           choices = all_themes),
               hr(),
               sliderInput(inputId = "freq",
                           "Minimum Frequency:",
                           min = 1,  max = 20, value = 10),
               sliderInput(inputId = "word_max",
                           "Maximum Number of Words:",
                           min = 1,  max = 50,  value = 20)
            ),
            box(plotOutput("wcplot"))
         )
      ),
      tabItem(
         tabName = 'histogram',
         fluidRow(
            infoBoxOutput("piece_ave"),
            valueBoxOutput("price_ave"),
            valueBoxOutput('count')
         ),
         fluidRow(
            box(
               checkboxGroupInput(inputId = 'year_his',
                                  label = 'Select interested year(s):',
                                  choices = c('2018', '2019'),
                                  selected = '2019'),
               sliderInput(inputId = "bin_num",
                           label = "Number of bins in histogram (approximate):",
                           min = 5, max = 50, value = 20),
               sliderInput(inputId = "n_samp", 
                           label = "Number of Samples:",  
                           min = 1, max = 500, value = 200, step = 50)
            )
         ),
         fluidRow(
            tabBox(title = 'Histograms',
                   tabPanel('Pieces',plotlyOutput('piece_histogram')), 
                   tabPanel('US Price', plotlyOutput('price_histogram'))
                   )
         )
         
      ),
      tabItem(
         tabName = 'ratio',
         box(
            sliderInput(inputId = "n_samp_ratio", 
                        label = "Number of Samples:",  
                        min = 1, max = 1000, value = 500, step = 100),
            sliderInput(inputId = "size", 
                        label = "Point Size:", 
                        min = 0, max = 5, 
                        value = 2)
         ),
         hr(),
         tabBox(title = 'Ratio plots and table',
                tabPanel('Scatter Plot', br(), plotlyOutput('scatter')),
                tabPanel('Data Table', DT::dataTableOutput(outputId = "sample_table"))
            
         )
      )
   )
)

ui <- dashboardPage(skin = 'black', header, sidebar, body)

server <- function(input, output, session) {
   # tab1
   lego_sub_name <- reactive({
      req(input$year)
      req(input$theme)
      filter(LegoData, (Year %in% as.numeric(input$year)) & (Theme == input$theme))
   })
   wordcloud_rep <- repeatable(wordcloud)
   output$wcplot <- renderPlot({
      v <- getTermMatrix(lego_sub_name())
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$word_max,
                    colors=brewer.pal(8, "Set3"))
   })
   # tab2
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
   # numeric boxes
   output$piece_ave <- renderInfoBox({
      num <- round(mean(his_sample()$Pieces, na.rm = T), 2)
      infoBox("Avg Pieces", value = num, subtitle = paste(nrow(his_sample()), "characters"), icon = icon("puzzle-piece"), color = "blue")
   })
   output$price_ave <- renderValueBox({
      num <- round(mean(his_sample()$USPrice, na.rm = T), 2)
      valueBox(subtitle = "Avg Price", value = num, icon = icon("dollar-sign"), color = "green")
   })
   output$count <- renderValueBox({
      num <- nrow(his_sample())
      valueBox(subtitle = "Sample counts", value = num, icon = icon("sort-numeric-asc"), color = "yellow")
   })
   # histogram plots
   output$price_histogram <- renderPlotly({
      ggplotly(
         ggplot(data = his_sample(), aes(x = USPrice), fill = 'darkgray') +
            geom_histogram(bins = input$bin_num) + 
            ggtitle('Price Histogram in US') + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            xlab('Price in US'), 
         tooltip = 'text')
   })
   output$piece_histogram <- renderPlotly({
      ggplotly(
         ggplot(data = his_sample(), aes(x = Pieces), fill = 'darkgray') +
            geom_histogram(bins = input$bin_num) + 
            ggtitle('Pieces Histogram') + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            xlab('Set Pieces'),  
         tooltip = 'text')
   })
   # tab3
   ratio_sample <- reactive({
      invalidateLater(millis = 5000)
      req(input$n_samp_ratio)
      sample_n(LegoData, input$n_samp_ratio) 
   })
   output$scatter <- renderPlotly({
      ggplotly(
      ggplot(data = ratio_sample(), aes(x = WantedBy, y = OwnedBy, color = Theme)) +   
         geom_point(size = input$size) + 
         labs(x = 'Wanted by people (numbers)',
              y = 'Owned by people (numbers)',
              title = "Possessing Ratio"),
      tooltip = c("x", "y")
      )
   })
   output$sample_table <- DT::renderDataTable({
         DT::datatable(data = ratio_sample()[, 4:12], 
                       options = list(pageLength = 20), 
                       rownames = FALSE)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

