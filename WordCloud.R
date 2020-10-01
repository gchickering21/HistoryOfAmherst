library(tidyverse)
library(mdsr)
library(tidytext)
library(readtext)
library(tm)
library(RColorBrewer)
library(shiny)

text <- readtext("chapter_files/*.txt")
n_chapters <- nrow(text)
for(i in 1:n_chapters){
    # single string of text that contains the entire chapter
    temp_chapter <- text[i,2]
    
    # rename string with appropriate chapter number
    assign(paste("chapter", i-1, "_raw", sep=""), temp_chapter)
}

chapter8 <- stringr::str_split(chapter8_raw, "\n")[[1]]
chapter8_df <- tibble(line = 1:469, text = chapter8 ) %>% select(text)
chapter8_df

key_words<- chapter8_df %>%
    tidytext::unnest_tokens(word, text) %>%
    anti_join(tidytext::get_stopwords(), by = "word") %>%
    count(word, sort = TRUE) %>% mutate(freq=n) %>% 
    select(word, freq)
key_words

chapters<<- list("Chapter 8 " = "chpt8")


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(" Amherst History Word Cloud"),
    
    sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
            selectInput("selection", "Choose a chapter:",
                        choices = chapters),
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 3,  max = 50, value = 5),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  max = 100,  value = 50)
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
                #get_words(input$selection)
                #going to have to change this line
                key_words
                #right now this just deals with chapter 8 
            })
        })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        wordcloud_rep(words=key_words$word,freq=key_words$freq,
                      scale=c(4,0.5), min.freq=input$freq,
                      max.words = input$max, colors = brewer.pal(6, "Purples"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
