# Martha Gizaw
# Homework #3
# Due Tuesday, March 3, 2020

library(shiny)
library(dplyr)
# (1) Load the hotels data set.
hotels <- readRDS(url("https://www.dropbox.com/s/88gmfovthfarfrq/data.rds?dl=1"))
# View(hotels)

# (2) Create a basic Shiny app with two columns.

ui<-function(req){
  #ui converts everything to HTML
  fluidPage(
    tags$style(), #stand in for <style></style>
    fluidRow(
      column(4,br(),
             wellPanel(
               # (3) On the left side, create a dropdown menu for any one of the variables.
               h4(strong("Criterion:")),
               # selectInput(input="hotel", label="Hotel Type: ", choices=c("All", levels(hotels$hotel))),
               selectInput(input="country", label="Country: ", choices=c("All", levels(hotels$country))),
             )
             ),
      column(8,br(),
             uiOutput("graph_container"),
             wellPanel(
               h4(strong("Hotel Listings")),
               tableOutput("table"),
               textOutput("text")
             )
            )
    )
  )
}

# the server is where you put the actual code to make things work
# the ui contains the "inputs" and the server creates the "outputs"
server <- function(input, output) {
  # (4) Next, create a reactive object called df that filters the data set according
  # to the selected input. Reminder: input exists in a named list!
  # reactive objects are objects that change according to the inputs / when comething updaes, hence "reactive"
  df <- reactive({
    df <- hotels
    df <- subset(df, country == input$country)
  })
  # NOTE: the "ALL" category does not present any listings, but choosing a specific country helps with
  # narrowing down on the resorts specific to that country
  
  output$table <- renderTable({
    df()
  })
}

shinyApp(ui, server)


# (5) Using df() as your data set, create an interactive graph with a varying
# x-axis and display it on the right side of the app.
#### (a) Use stat(count) for the tooltip.
#### (b) You will need to create an input to select the x-axis.

# NOTE: I was unable to answer this part due to techincal difficulties. Sorry for the inconvenience.