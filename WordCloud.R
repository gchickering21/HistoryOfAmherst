library(tm)
library(RColorBrewer)
library(shiny)
library(wordcloud)
library(tidyverse)
library(mdsr)
library(readtext)
library(tidytext)
library(dplyr)
library(tidyr)
library(kableExtra)
library(tm)
library(RColorBrewer)
library(kableExtra)

cleaned_book <- read.csv(file="all_texts.csv")

n_chapters <- nrow(cleaned_book)
text <- c("")
book <- data.frame(text)

## This gets all the chapters into their own separate df's
for (i in 1:n_chapters) {
  # single string of text that contains the entire chapter
  temp_chapter <- cleaned_book[i, 3]
  string_chapter <- stringr::str_split(temp_chapter, "\n")[[1]]
  num_lines <- length(string_chapter)
  chapter_df <- tibble(line = 1:num_lines, text = string_chapter) %>% select(text)
  book <- rbind(book, chapter_df)
  # rename string with appropriate chapter number
  assign(paste("chapter", i - 1, "_df", sep = ""), chapter_df)
}

#chapters<-list("chapter1"=chapter1_df)

chapters <- list(
  "Book " = book,
  "Chapter 0 " = chapter0_df, "Chapter 1 " = chapter1_df, "Chapter 2 " = chapter2_df, "Chapter 3 " = chapter3_df, "Chapter 4 " = chapter4_df, "Chapter 5 " = chapter5_df, "Chapter 6 " = chapter6_df, "Chapter 7 " = chapter7_df, "Chapter 8 " = chapter8_df,
  "Chapter 9 " = chapter9_df, "Chapter 10 " = chapter10_df, "Chapter 11 " = chapter11_df, "Chapter 12 " = chapter12_df, "Chapter 13 " = chapter13_df,
  "Chapter 14 " = chapter14_df, "Chapter 15 " = chapter15_df, "Chapter 16 " = chapter16_df, "Chapter 17 " = chapter17_df,
  "Chapter 18 " = chapter18_df, "Chapter 19 " = chapter19_df, "Chapter 20 " = chapter20_df, "Chapter 21 " = chapter21_df, "Chapter 22 " = chapter22_df,
  "Chapter 23 " = chapter23_df, "Chapter 24 " = chapter24_df, "Chapter 25 " = chapter25_df, "Chapter 26 " = chapter26_df,
  "Chapter 27 " = chapter27_df, "Chapter 28 " = chapter28_df
)

get_keywords<-function(chapter){
  word_count <- chapter %>%
    tidytext::unnest_tokens(word, text) %>%
    anti_join(tidytext::get_stopwords(), by = "word") %>%
    count(word, sort = TRUE) %>%
    select(word, n)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(" Amherst History Word Cloud"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a chapter:",
        choices = chapters
      ),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
        "Minimum Frequency:",
        min = 3,  max = 50, value = 5
      ),
      sliderInput("max",
        "Maximum Number of Words:",
        min = 1,  max = 100,  value = 50
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
  keywords <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        get_keywords(input$selection)
      })
    })
  })
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    terms<-keywords()
    #v <- keywords()
    wordcloud_rep(
      words = terms$word, freq=terms$n,
      scale = c(4, 0.5), min.freq = input$n,
      max.words = input$max, colors = brewer.pal(6, "Purples")
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
