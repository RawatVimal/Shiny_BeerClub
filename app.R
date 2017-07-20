library(shiny)
library(googleVis)
library(ggplot2)
library(plotly)
library(DBI)
library(RMySQL)
ui <- fluidPage(
  titlePanel("This is dummy data for Beer club, Please Don't get offended !!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a Club:", choices = c("Beer Club", "Coffee Club")),
      numericInput("obs", "Number of observations to view:", 10),
      actionButton("showU","Show Data"), h4("Last 10 entries!!"), tableOutput("view2")),
    mainPanel( mainPanel(tableOutput("view")), plotOutput("plot"))
  )
)

server <- function(input, output,session){
  con <- dbConnect(RMySQL::MySQL(), dbname = "test", user='root', password = "abc123", host='localhost', port=3306)
  on.exit(dbDisconnect(con), add = TRUE)
  beerclub <- dbReadTable(con,  "beerclub")
  total <- dbGetQuery(con,"select UserID, sum(beers) as total_beers, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from beerclub GROUP  BY  UserID ;")
  rank<-dbGetQuery(con,"select UserID, MONTH(date) as Month, sum(beers) as glasses, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from beerclub  GROUP  BY  UserID, MONTH(date) ;")
  update<-dbGetQuery(con,"UPDATE beerclub SET cost = CASE WHEN beers < 2 THEN 0 WHEN beers >1 AND beers < 4 THEN 5 WHEN beers >3 THEN 10 END;")
  
  # Return the requested dataset
  
  
  output$view <- renderTable({
    #head(dbReadTable(con = con,  name = input$dataset), n = input$obs)
    total
    
  })
  
  output$view2 <- renderTable({
    
    last10 <- tail(beerclub)
    last10=last10[order(nrow(last10):1),]
    head(last10)
    
  })
  
  output$plot <- renderPlot({
    ggplot(rank , aes(x = rank$Month, y = rank$glasses, colour = UserID)) + geom_line(size=2) + xlab("Month in year 2017")+ ylab("Beer glasses") + ylim(low=0,high=200) + ggtitle("Beer glasses per month ") +  theme(plot.title = element_text(lineheight=1, face="bold"), axis.text=element_text(size=24), axis.title=element_text(size=18,face="bold"))
    #ggplotly(p)
    
  })
  
  
  
}

shinyApp(ui,server)