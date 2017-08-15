library(shiny)
library(googleVis)
library(ggplot2)
library(plotly)
library(DBI)
library(ggthemes)
library(RMySQL)
library(RGraphics)
library(gridExtra)
ui <- fluidPage(theme ="bootstrap.css",
  titlePanel("This is dummy data for Beer club, Please Don't get offended !!"),
  sidebarLayout(
    sidebarPanel(
                selectInput("dataset", "Choose a Club:", c("BeerClub"= "BeerClub", "CoffeeClub" = "CoffeeClub"), selected=NULL),
                numericInput("obs", "Number of observations to view:", 10),
                actionButton("showU","Show Data"), 
                br(),br(),
                actionButton("showBill","Show Bill"), 
                actionButton("clearBill","Clear Bill"),
                br(),br(),
                actionButton("showTrend","Show Trend"),
                actionButton("clearTrend","Clear Trend"),
                h4("Last 10 entries!!"), 
                textOutput("text10"),
                tableOutput("viewLast")
                ,width =4),

    mainPanel(tabsetPanel( tabPanel(h3("Current"), 
                                    h4(tableOutput("viewBill")), 
                                    textOutput("viewall1"),
                                    plotOutput("plot")),
                            tabPanel(h3("New Users"),
                                     h1(span(textOutput("errortext"), style="color:red")),
                                     h3(selectInput("club", "Choose Club:",c("none" = "none","beeclub" = "beerclub","coffeeclub" = "coffeeclub"), selected=NULL)),
                                     
                                     h4(textInput("name", "Name (Full name)","")),
                                     h4(textInput("shortname", "Short Name (eg. VIR)","")),
                                     h4(textInput("displayname", "Display Name (eg. Vimal-VIR)","")),
                                     h4(textInput("dept", "Department (eg. UG)","")),
                                     h4(textInput("phone", "Phone Number","+41 ")),
                                     h4(numericInput("age", "Age", 30, min = 18, max = 110)),
                                     h4(textInput("jobtitle", "Job title (eg. Postdoc)","")),
                                     h4(textInput("nationality", "Nationality (eg. German)","")),
                                     h4(dateInput("membersince", "MemberSince (eg. DATE)")),
                                     
                                     actionButton("addU","Add member")
                            )


                              ))
  )
)

server <- function(input, output,session){
  
  
  
  con1 <- dbConnect(RMySQL::MySQL(), dbname = "test", user='root', password = "abc123", host='localhost', port=3306)
  beerclub <- dbReadTable(con1,  "beerclub")
  #billBeer <- dbGetQuery(con1,"select UserID, sum(beers) as total_beers, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from test.beerclub GROUP  BY  UserID ;")
  on.exit(dbDisconnect(con1), add = TRUE)
  
  con2 <- dbConnect(RMySQL::MySQL(), dbname = "coffeeclub", user='root', password = "abc123", host='localhost', port=3306)
  coffeeclub <- dbReadTable(con2,  "coffeeclub")
  #billCoffee <- dbGetQuery(con2,"select UserID, sum(coffee) as total_coffee, sum(cost) as Cost, sum(cost)/sum(coffee) as cost_per_coffee  from coffeeclub.coffeeclub GROUP  BY  UserID ;")
  on.exit(dbDisconnect(con2), add = TRUE)
  
  updateBeerclub<-dbGetQuery(con1,"UPDATE test.beerclub SET cost = CASE WHEN beers < 2 THEN 0 WHEN beers >1 AND beers < 4 THEN 5 WHEN beers >3 THEN 10 END;")
  updateCoffeeclub<-dbGetQuery(con2,"UPDATE coffeeclub.coffeeclub SET cost = CASE WHEN coffee < 2 THEN 30 WHEN coffee >1  THEN 60  END;")
  
  dataBeer<-dbGetQuery(con1,"select UserID, MONTH(date) as Month, sum(beers) as glasses, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from beerclub  GROUP  BY  UserID, MONTH(date) ;")
  dataCoffee<-dbGetQuery(con2,"select UserID, MONTH(date) as Month, sum(coffee) as Cups, sum(cost) as Cost, sum(cost)/sum(coffee) as cost_per_coffee  from coffeeclub.coffeeclub  GROUP  BY  UserID, MONTH(date) ;")
  
  
  
  
  
  
  
  output$plot <- renderPlot({
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste0("Welcome to the Beer/Coffee club Admin Page\n",
                                  "Remember, changes made here will affect databases"), cex = 1.6, col = "black")
  })
  #rank_month<-dbGetQuery(con,"select UserID, MONTH(date) as Month, sum(beers) as glasses  from beerclub  GROUP  BY  UserID, MONTH(date) ;")

  # Return the requested dataset

  showLast <- eventReactive(input$showU,{
    output$viewLast <- renderTable({
      
      print (input$dataset)
      
      if(input$dataset == "BeerClub"){
      beerclub <- beerclub[,2:4]
      names(beerclub)[1]<-paste("User")
      names(beerclub)[2]<-paste("Date")
      names(beerclub)[3]<-paste("Beers")
      beerclubSub = tail(beerclub,input$obs)
      tail(beerclubSub[order(input$obs:1),],input$obs)
      }     
      
      else {
        coffeeclub <- coffeeclub[,2:4]
        names(coffeeclub)[1]<-paste("User")
        names(coffeeclub)[2]<-paste("Date")
        names(coffeeclub)[3]<-paste("Coffee")
        coffeeclubSub = tail(coffeeclub,input$obs)
        tail(coffeeclubSub[order(input$obs:1),],input$obs)
      } 
    #rank_month
  })
  })
  
  
  showBillInfo <- eventReactive(input$showBill,{
    output$viewBill <- renderTable({
      con3 <- dbConnect(RMySQL::MySQL(), dbname = "test", user='root', password = "abc123", host='localhost', port=3306)
      on.exit(dbDisconnect(con3), add = TRUE)
      if(input$dataset == "BeerClub"){
        dbGetQuery(con3,"select UserID, sum(beers) as total_beers, sum(cost) as Cost, sum(cost)/sum(beers) as cost_per_beer  from test.beerclub GROUP  BY  UserID ;")
      }
      else {
        dbGetQuery(con3,"select UserID as Member, sum(coffee) as Coffes, sum(cost)/100 as Payment, sum(cost)/(100 * sum(coffee)) as Per_Coffee  from coffeeclub.coffeeclub GROUP  BY  UserID ;")
        
        
      }
        
      })
  })
  
  
  clearBill <- eventReactive(input$clearBill,{
    output$viewBill <- renderTable({})
  })
  

  showTrend <- eventReactive(input$showTrend,{
               output$plot <- renderPlot({
                 if(input$dataset == "BeerClub"){
                   print(ggplot(dataBeer , aes(dataBeer$Month,dataBeer$glasses, colour = UserID)) + geom_line(size=0.5) + xlab("Month in year 2017")+ ylab("Beer glasses") + ylim(low=0,high=100) + ggtitle("Beer glasses per month ") +  theme(plot.title = element_text(lineheight=1, face="bold"), axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold")), width=600,height=500)
                 }
                 
                 else {
                   print(ggplot(dataCoffee , aes(dataCoffee$Month,dataCoffee$Cups, colour = UserID)) + geom_line(size=0.5) + xlab("Month in year 2017")+ ylab("Coffee cups") + ylim(low=0,high=100) + ggtitle("Coffee cups per month ") +  theme(plot.title = element_text(lineheight=1, face="bold"), axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold")), width=600,height=500)
                   
                   
                 }
                   })
            })
  
  clearTrend <- eventReactive(input$clearTrend,{
    par(mar = c(0,0,0,0))
    output$plot <- renderPlot({
     
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste0("Welcome to the Beer/Coffee club Admin Page\n",
                                   "Remember, changes made here will affect databases"), cex = 1.6, col = "black")
    })
  })
  
  

  
  output$viewall1 <- renderText({
    #head(dbReadTable(con = con,  name = input$dataset), n = input$obs)
    showLast()
    showBillInfo()
    clearBill()
    showTrend()
    clearTrend()
  })  
  
  
  
  error <- eventReactive(input$addU,{
    output$errortext <- renderText({
      paste0("Choose club first !!!!")
    })
  })
  
  addMember <- eventReactive(input$addU,{
      conn <- dbConnect(
        drv = RMySQL::MySQL(),
        dbname = "test",
        host = "localhost",
        username = "root",
        password = "abc123")
      on.exit(dbDisconnect(conn), add = TRUE)
      if(input$club == "beerclub") {
      dbGetQuery(conn, paste0("insert into test.beerclub_members ( Name, ShortName, DisplayName, Dept, Phone, Age, Jobtitle, Nationality, MemberSince) values ('",input$name,"', '", input$shortname,"', '", input$displayname,"', '", input$dept,"', '", input$phone,"', '", input$age,"', '", input$jobtitle,"', '",input$nationality,"',",input$membersince, ") ;"))
      }
      if(input$club == "coffeeclub") {
        dbGetQuery(conn, paste0("insert into coffeeclub.coffeeclub_members ( Name, ShortName, DisplayName, Dept, Phone, Age, Jobtitle, Nationality, MemberSince) values ('",input$name,"', '", input$shortname,"', '", input$displayname,"', '", input$dept,"', '", input$phone,"', '", input$age,"', '", input$jobtitle,"', '",input$nationality,"',",input$membersince, ") ;"))
      }
      if(input$club == "none")
      {
        error()
      }
  })
  

  
  output$text10 <- renderText({
    #paste0("Please varify", input$uID,", Today :", input$date, "You drank:", input$beers, " beer(s)")
    addMember()
  })
  





}

shinyApp(ui,server)
