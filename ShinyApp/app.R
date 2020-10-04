library(RColorBrewer)
library(shiny)
library(wordcloud)
library(tidyverse)
library(shiny)
library(shinybusy)
library(mdsr)
library(tidytext)
library(stopwords)

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

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Amherst History Word Cloud"),
    
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
                        min = 1,  max = 500, value = 10
            ),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1, max = 50, value = 10
            )
        ),
        
        # Show Word Cloud
        mainPanel(
            plotOutput("plot")
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
                get_keywords(input$selection, input$freq, input$max)
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
            colors = brewer.pal(6, "Purples")
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
