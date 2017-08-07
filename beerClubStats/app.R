library(shiny)
library(googleVis)
library(ggplot2)
library(plotly)
library(DBI)
library(ggthemes)
library(RMySQL)
ui <- fluidPage(theme ="bootstrap.css",
  titlePanel("This is dummy data for Beer club, Please Don't get offended !!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a Club:", choices = c("Beer Club", "Coffee Club")),
      numericInput("obs", "Number of observations to view:", 10),
      actionButton("showU","Show Data"), h4("Last 10 entries!!"), tableOutput("view2")),


     mainPanel(tabsetPanel( tabPanel("Current", tableOutput("view"), plotlyOutput("plot")),
                            tabPanel("New Users",
                                      textInput("name", "Name","Data Summary"),
                                      textInput("shortname", "Short Name","Data Summary"),
                                      textInput("displayname", "Display Name (Vimal-VIR)",""),
                                      textInput("dept", "Department",""),
                                      textInput("phone", "Phone Number","Data Summary"),
                                      textInput("age", "Age","")
                            )


                              ))
  )
)

server <- function(input, output,session){
  con <- dbConnect(RMySQL::MySQL(), dbname = "test", user='root', password = "abc123", host='localhost', port=3306)
  on.exit(dbDisconnect(con), add = TRUE)
  beerclub <- dbReadTable(con,  "beerclub")
  total <- dbGetQuery(con,"select UserID, sum(beers) as total_beers, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from beerclub GROUP  BY  UserID ;")
  rank<-dbGetQuery(con,"select UserID, MONTH(date) as Month, sum(beers) as glasses, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from beerclub  GROUP  BY  UserID, MONTH(date) ;")
  rank_month<-dbGetQuery(con,"select UserID, MONTH(date) as Month, sum(beers) as glasses  from beerclub  GROUP  BY  UserID, MONTH(date) ;")

  update<-dbGetQuery(con,"UPDATE beerclub SET cost = CASE WHEN beers < 2 THEN 0 WHEN beers >1 AND beers < 4 THEN 5 WHEN beers >3 THEN 10 END;")

  # Return the requested dataset


  output$view <- renderTable({
    #head(dbReadTable(con = con,  name = input$dataset), n = input$obs)
    total


  })

  output$view2 <- renderTable({

    last10=beerclub[order(10:1),]
    last10
    #rank_month
  })

  output$plot <- renderPlotly({
    print(ggplotly(ggplot(rank , aes(x = rank$Month, y = rank$glasses, colour = UserID)) + geom_line(size=0.5)+ geom_smooth() + xlab("Month in year 2017")+ ylab("Beer glasses") + ylim(low=0,high=200) + ggtitle("Beer glasses per month ") +  theme(plot.title = element_text(lineheight=1, face="bold"), axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold")), width=600,height=500)
    )
  })



}

shinyApp(ui,server)
