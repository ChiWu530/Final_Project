library(shiny)
library(shinythemes)
library(shinyWidgets)

# Define UI (web interface)
ui <- fluidPage(theme = shinytheme("sandstone"),
    navbarPage("",
        tabPanel("HOME",
                 
                 mainPanel(
                     h5("──資料科學及分析導論 期末專題──", align = "center"),
                     h1("文學作品中的高頻字與主題標記", align = "center", style = "color:#456a8c"),
                     h5("指導老師：謝舒凱", align = "center"),
                     h5("組員：吳錤、周昕妤、黃彙茹、陳聯輝", align = "center"),
                     h1(""),
                     h1(""),
                     div(imageOutput("dfll"), style="text-align: center;")
                     
        )
                 ),
        tabPanel("tf-idf",
                 sidebarPanel(
                     selectInput("country1", "Choose a country", choices = c("UK","US","Germany","France")),
                     sliderInput("slider1", "Number of keywords: ",
                                 value = 10, min = 5, max = 15, step = 1)
                 ),
                 mainPanel(
                     div(textOutput("country_tfidf"), style = "text-align: center; font-size:20px;"),
                     verbatimTextOutput("bookOut"),
                     imageOutput("tfidf_rank")
                 )
        ),
        tabPanel("Topic Modeling",
                 sidebarPanel(
                    selectInput("country2", "Choose a country", choices = c("UK","US","Germany","France")),
                    awesomeRadio("num", "Select 3 or 5 topics", choices = c(3, 5), selected = "3", status = "warning")
                 ),
                 mainPanel(
                     div(textOutput("country_topic"), style = "text-align: center;font-size:20px;"),
                     verbatimTextOutput("countryOut"),
                     imageOutput("TOPIC"),
                     h1(""),
                     imageOutput("TOPIC2")
                 )
                 )
    )
)

# Define server logic (how R process user's inputs)
server <- function(input, output) {
    output$dfll <- renderImage({list(src = "meme.png",
                                     width = 500,
                                     height = 281)})
    output$bookOut <- renderText(input$bookname)
    output$countryOut <- renderText(input$country)
    output$country_tfidf <- renderText({paste0(input$country1,"  TF-IDF")})
    output$country_topic <- renderText({paste0(input$country2,"  Topic Modeling")})
    output$tfidf_rank <- renderImage({ 
        filename <- paste0(input$country1,"_",input$slider1, ".png")
        list(src = filename,
             width = 800,
             height = 450)
        })
    output$TOPIC <- renderImage({
        filename <- paste0(input$country2, "_TOPIC_", input$num, ".png")
        list(src = filename,
             width = 750,
             height = 400)
    })
    output$TOPIC2 <- renderImage({
        filename <- paste0(input$country2, "_TOPIC2_", input$num, ".png")
        list(src = filename,
             width = 750,
             height = 400)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)