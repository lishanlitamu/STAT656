##################################
#                                #
# Title: STAT-656 Shiny App      #
# Authors: Kyle Dixon & Mia Li   #
# Date Created: November 27 2020 #
#                                #
##################################

library(shiny)

## Pull in news headlines from XLSX
tsla_news <- readxl::read_excel("tesla news headlines from reuter 09052019-09092020.xlsx")
goog_news <- readxl::read_excel("alphabet inc. news headlines from reuter 07112019-10302020.xlsx")

## Format date columns
tsla_news$date <- base::as.Date(tsla_news$date, "%B %d,%Y")
goog_news$date <- base::as.Date(goog_news$date, "%B %d, %Y")

## Rename headline columns to match previous code
base::names(tsla_news) <- c("Date", "Time", "webTitle", "Keywords")
base::names(goog_news) <- c("Date", "Time", "webTitle", "Keywords")

## Get positive and negative words from Loughran and McDonald
positive_words <- base::data.frame(
    readxl::read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",
                       sheet = "Positive", col_names = F))$...1

negative_words <- base::data.frame(
    readxl::read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",
                       sheet = "Negative", col_names = F))$...1

## Calculate sentiment for TSLA
tsla_news$Sentiment <- base::as.numeric(NA)
for (row in 1:base::nrow(tsla_news)) {
    tmp <- base::strsplit(tsla_news[row, ]$webTitle, "[[:space:]]|(?=[.!?])", perl = T)[[1]]
    
    sentiment <- 0
    for (word in tmp) {
        if (base::nchar(word) > 1 & !(base::grepl("\\(", word)) & !(base::grepl("\\)", word))) {
            if (base::any(stringr::str_detect(positive_words, stringr::regex(word, ignore_case = T)))) {
                sentiment <- sentiment + 1
            } else if (base::any(stringr::str_detect(negative_words, stringr::regex(word, ignore_case = T)))) {
                sentiment <- sentiment - 1
            }
        }
    }
    tsla_news[row, ]$Sentiment <- sentiment
}

## Calculate sentiment for GOOG
goog_news$Sentiment <- base::as.numeric(NA)
for (row in 1:base::nrow(goog_news)) {
    tmp <- base::strsplit(goog_news[row, ]$webTitle, "[[:space:]]|(?=[.!?])", perl = T)[[1]]
    
    sentiment <- 0
    for (word in tmp) {
        if (base::nchar(word) > 1 & !(base::grepl("\\(", word)) & !(base::grepl("\\)", word))) {
            if (base::any(stringr::str_detect(positive_words, stringr::regex(word, ignore_case = T)))) {
                sentiment <- sentiment + 1
            } else if (base::any(stringr::str_detect(negative_words, stringr::regex(word, ignore_case = T)))) {
                sentiment <- sentiment - 1
            }
        }
    }
    goog_news[row, ]$Sentiment <- sentiment
}

goog_news$Ticker <- "GOOG"
tsla_news$Ticker <- "TSLA"
plot_data <- dplyr::bind_rows(goog_news, tsla_news)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GOOG & TSLA News Headline Sentiment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates", "Date Range:", 
                           start = base::min(plot_data$Date), end = base::max(plot_data$Date),
                           min = base::min(plot_data$Date), max = base::max(plot_data$Date))),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        validate(need(input$dates[2] > input$dates[1], "End date is earlier than start date."))
        
        plot_data <- plot_data %>% dplyr::filter(Date > input$dates[1] & Date < input$dates[2])
        mu <- plyr::ddply(plot_data, "Ticker", summarise, grp.mean = mean(Sentiment))
        ggplot2::ggplot(data = plot_data, ggplot2::aes(x = Sentiment, fill = Ticker)) + 
            ggplot2::geom_density(alpha = 0.5, adjust = 1.5) + 
            ggplot2::geom_vline(data = mu, ggplot2::aes(xintercept = grp.mean, color = Ticker),
                                linetype = "dashed") +
            ggplot2::scale_color_manual(values = c("#5f506b", "#86bbbd")) + 
            ggplot2::scale_fill_manual(values = c("#5f506b", "#86bbbd"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

