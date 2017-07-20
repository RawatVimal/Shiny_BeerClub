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
ui <- fluidPage(
   
   # Application title
   titlePanel("Beer Club Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("uID", "User ID:",c("None" = "***","Hannes (HNS)" = "HNS","Ueli (UG)" = "UG","Arturo (ART)" = "ART","AFIF (AFF)" = "AFF","Oender (OK)" = "OK","Vimal (VIR)" = "VIR","Alex (ALX)" = "ALX","Dima (DIM)" = "DIM", "Nuno (NP)" = "NP"), selected=NULL),
        br(),
        dateInput("date", "Date:", value = NULL, min = NULL, max = NULL, startview = "month", weekstart = 0,language = "en", width = NULL),
        selectInput("beers", "Beers:", c("Zero" = "", "1" = 1,"2" = 2,"3" = 3, "10" = 10), selected=NULL),
        actionButton("enterdata", "Record")),
      # Show a plot of the generated distribution
      mainPanel(
        h4(textOutput("text1")), h5(tableOutput("tbl")), h5(tableOutput("tbl1"))
      )
   )
)


# Define server logic required to draw a histogram

#con <- dbConnect(MySQL(), dbname="beerclub", username="root", password="abc123", host="localhost", port=13306)

  

############
server <- function(input, output, session) {

goButton1 <- eventReactive(input$enterdata,{
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "test",
      host = "localhost",
      username = "root",
      password = "abc123")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn, paste0("SELECT * FROM test.beerclub WHERE date >= CURDATE();"))
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

