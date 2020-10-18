library(RColorBrewer)
library(shiny)
library(wordcloud)
library(tidyverse)
library(shiny)
library(shinybusy)
library(mdsr)
library(tidytext)
library(stopwords)
library(shinythemes)
library(DT)

cleaned_book <- read.csv(file = "all_chapters.csv")

get_keywords <- function(chapter, num_freq, num_words) {
    cleaned_book <- read.csv(file = "all_chapters.csv")
    chapter <- cleaned_book[, grepl(chapter, names(cleaned_book))]
    chapter <- na.omit(chapter)
    chapter <- as.data.frame(chapter)
    names(chapter)[1] <- paste("text")
    
    word_count <- chapter %>%
        tidytext::unnest_tokens(word, text) %>%
        anti_join(get_stopwords(), by = "word") %>%
        count(word, sort = TRUE) %>%
        select(word, n) %>%
        filter(n > num_freq)
    head(word_count, num_words)
}

get_sentiment <- function(chapter, num_freq, num_words) {
    cleaned_book <- read.csv(file = "all_chapters.csv")
    chapter <- cleaned_book[, grepl(chapter, names(cleaned_book))]
    chapter <- na.omit(chapter)
    chapter <- as.data.frame(chapter)
    names(chapter)[1] <- paste("text")
    
    Chapter_Counts <- chapter %>%
        tidytext::unnest_tokens(word, text) %>%
        # exclude stop words (i.e. the, an, a, you)
        anti_join(tidytext::get_stopwords(), by = "word") %>%
        # count word frequencies for a given chapter
        count(word, sort = TRUE) %>%
        mutate(freq = n) %>%
        select(word, freq) %>%
        inner_join(get_sentiments("bing"), by = "word") %>%
        # count(word, sentiment) %>%
        ungroup() %>%
        filter(freq > num_freq) %>%
        group_by(sentiment) %>%
        head(num_words)
    # return(Chapter_Counts)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    titlePanel("History of Amherst"),
    
    sidebarLayout(
        
        # Sidebar with a slider and selection inputs
        sidebarPanel(
            selectInput("selection", "Choose a chapter:",
                        choices = colnames(cleaned_book)
            ),
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 500, value = 25
            ),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1, max = 200, value = 10
            )
        ),
        
        # Show Word Cloud
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Wordcloud", plotOutput("plot")),
                tabPanel("Sentiment Analysis", plotOutput("graph")),
                tabPanel(" Sentiment Analysis Summary",  DT::dataTableOutput("view"))#, tableOutput("chart"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Define a reactive expression for the document term matrix
    terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                # get_keywords(input$selection, input$freq, input$max)
                dataset <- get_keywords(input$selection, input$freq, input$max)
                validate(
                    need(nrow(dataset) > 0, "Input Not Valid: Enter New Inputs")
                )
                return(dataset)
            })
        })
    })
    
    sentiment <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                dataset <- get_sentiment(input$selection, input$freq, input$max)
                validate(
                    need(nrow(dataset) > 0, "Input Not Valid: Enter New Inputs")
                )
                return(dataset)
            })
        })
    })
    # Make the word cloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    #
    output$plot <- renderPlot({
        v <- terms()
        wordcloud_rep(
            words = v$word, freq = v$n,
            scale = c(4, 0.5), # min.freq = input$n,
            # max.words = input$max,
            colors = brewer.pal(7, "BuPu")
        )
    })
    
    output$graph <- renderPlot({
        v <- sentiment()
        
        ggplot(data = v, aes(reorder(word, freq), freq, fill = sentiment)) +
            geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(
                y = "Contribution to Sentiment", x = NULL,
                title = "Top Contributors of Each Sentiment"
            ) +
            coord_flip()
    })
    
    output$chart<-renderTable({
          v<-sentiment() %>%
              spread(sentiment, freq, fill = 0) %>%
               select(-word) %>%
              mutate(overall_negative_sentiments= sum(negative), overall_positive_sentiments= sum(positive)) %>%
              mutate(overall_sentiment = overall_positive_sentiments - overall_negative_sentiments) %>%
              select(overall_sentiment, overall_positive_sentiments, overall_negative_sentiments) %>%
              head(1)
        
        
    })
    
    output$view <- DT::renderDataTable({
        v<-sentiment()
        DT::datatable(data=v)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
