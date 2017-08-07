#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RMySQL)
library(DBI)


# Define UI for application that draws a histogram
ui <- fluidPage(theme ="bootstrap.css",


   # Application title
   titlePanel(h1("Beer Club App"),windowTitle ="BeerClub"),
   inverse = TRUE,

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        h3(uiOutput("userID")),
        #selectInput("uID", "User ID:",c("None" = "***","Hannes (HNS)" = "HNS","Ueli (UG)" = "UG","Arturo (ART)" = "ART","AFIF (AFF)" = "AFF","Oender (OK)" = "OK","Vimal (VIR)" = "VIR","Alex (ALX)" = "ALX","Dima (DIM)" = "DIM", "Nuno (NP)" = "NP"), selected=NULL),
        h3(dateInput("date", "Date:", value = NULL, min = NULL, max = NULL, startview = "month", weekstart = 0,language = "en", width = NULL)),
        h3(radioButtons("beers", "Beers:", c( "1" = 1,"2-3" = 3,">3" = 4), selected="1", inline = TRUE)),
        h3(actionButton("enterdata", "Enter", style='font-size:100%'))),
      # Show a plot of the generated distribution
       mainPanel(tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
        h3(textOutput("text1")), h3(tableOutput("tbl")), h3(tableOutput("tbl1"))
      )
   )
   )


# Define server logic required to draw a histogram

#con <- dbConnect(MySQL(), dbname="beerclub", username="root", password="abc123", host="localhost", port=13306)



############
server <- function(input, output, session) {


      data_sets  <- reactive({
      conn1 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "test",
      host = "localhost",
      username = "root",
      password = "abc123")
      on.exit(dbDisconnect(conn1), add = TRUE)
      return (dbGetQuery(conn1, paste0("SELECT  DISTINCT DisplayName  FROM test.beerclub_members ORDER BY DisplayName ASC;")))

     })
  # Drop-down selection box for which data set
    output$userID <- renderUI({
    selectInput("uID", "User ID", as.list(data_sets()))
  })

    output$data_table <- renderTable({
    # If missing input, return to avoid error later in function
    #if(is.null(input$uID))
    #return()

    # Get the data set
    get(input$uID)

   })

goButton1 <- eventReactive(input$enterdata,{
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "test",
      host = "localhost",
      username = "root",
      password = "abc123")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn, paste0("SELECT * FROM test.beerclub WHERE date >= CURDATE()  ORDER BY ID DESC LIMIT 10;"))
  })
})


   goButton2 <- eventReactive(input$enterdata,{

        conn1 <- dbConnect(
        drv = RMySQL::MySQL(),
        dbname = "test",
        host = "localhost",
        username = "root",
        password = "abc123")
        on.exit(dbDisconnect(conn1), add = TRUE)
        dbGetQuery(conn1, paste0("insert into test.beerclub ( UserID, date, beers) values ('",input$uID,"', '",input$date,"',",input$beers, ") ;"))

  })

output$text1 <- renderText({
            #paste0("Please varify", input$uID,", Today :", input$date, "You drank:", input$beers, " beer(s)")
            goButton1()
            goButton2()
            })



}
############

#server <- function(input, output,session) {
#
#    d <- eventReactive(input$enterdata, { input$uID })
#    observeEvent(input$button, {
#                                  paste("insert into beerclub.beerclub (UserID, data, beers) values (",input$uID,",",input$date,",",input$beers)
#                                  result<-reactive({dbSendQuery(con, sql())})
#                                  output$text1 <- renderText({
#                                                              paste("Please varify", input$uID,", Today :", input$date, "You drank:", input$beers, " beer(s)")
#                                                            })
#                           })
#
#}

# Run the application
shinyApp(ui = ui, server = server)

