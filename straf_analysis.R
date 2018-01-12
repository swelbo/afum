#### R shiny application ####
#### PopGen Tool for AFUM ####

# Load shiny
library(shiny)
library(shinydashboard)
library(adegenet)
library(DT)


#### Data ####
db <- read.csv(file = "~/Documents/Imperial/R_folder/Shiny/STRAF/straf_db_shiny_only_2.csv")
#db <- read.csv(file = "~/Desktop/R_folder/Shiny/STRAF/straf_db_shiny_only.csv")
db <- db[,3:14]
head(db)
straf <- c("X2A", "X2B", "X2C", "X3A", "X3B", "X3C", "X4A", "X4B", "X4C")

#### UI ####
ui = dashboardPage(
  dashboardHeader(title = "Afum"),
  dashboardSidebar(
    selectInput("source", "Choose a Source:",
                choices = c(levels(db$Source))),
    selectInput("country", "Choose a Country:",
                choices = c(levels(db$Country))),
    #selectInput("country", "Choose a Continent:",
                #choices = c(levels(db$Continent))),
    selectInput("pop", "Choose a Population:",
                choices = c(levels(db$Population), "C")),
    #selectInput("country", "Choose a Mutation:",
                #choices = c(levels(db$Mutation))),
    numericInput(inputId = "straf2a", label="Allele 2A", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf2b", label="Allele 2B", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf2c", label="Allele 2C", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf3a", label="Allele 3A", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf3b", label="Allele 3B", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf3c", label="Allele 3C", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf4a", label="Allele 4A", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf4b", label="Allele 4B", value="20", width = 180, min = 0, max = 400),
    numericInput(inputId = "straf4c", label="Allele 4C", value="20", width = 180, min = 0, max = 400),
    actionButton("goButton","Enter :)")),
  
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Table", dataTableOutput("table")),
                tabPanel("Genind", verbatimTextOutput("genind")),
                tabPanel("Print", verbatimTextOutput("print")),
                tabPanel("PCA", plotOutput("pca")),
                tabPanel("DAPC", plotOutput("dapc")),
                tabPanel("Result", verbatimTextOutput("result"))
    )
   )
)
#### Server ####
server <- function(input, output) {
  
  # The important part of reactiveValues()
  values <- reactiveValues()
  values$df <- db
  addData <- observe({
    
    # your action button condition
    if(input$goButton > 0) {
      # create the new line to be added from your inputs
      newLine <- isolate(c(input$source, input$country, input$straf2a, input$straf2b, 
                           input$straf2c, input$straf3a, input$straf3b, 
                           input$straf3c, input$straf4a, input$straf4b, input$straf4c, input$pop))
      
      # update your data
      # note the unlist of newLine, this prevents a bothersome warning message that the rbind will return regarding rownames because of using isolate.
      #isolate(values$df <- rbind(as.data.frame(values$df), unlist(newLine)))
      values$df <- isolate(rbind(values$df, newLine))
      #values$df <- data.frame(values$df, stringsAsFactors = FALSE)
    }
  })
  obj <- reactive({df2genind(values$df[,2:10], ploidy = 1, NA.char = "NA",  sep = "", pop = values$df$Population)})
  #obj()@pop <- reactive({values$df$Alelle})
  #obj@pop <- reactive({values$df})
  
  #reactive({pop(obj()) <- db$Allele})
  x <- reactive({scaleGen(obj(), NA.method="mean")})
  #pca <- reactive({dudi.pca(x(), cent = FALSE, scale = FALSE, scannf = FALSE, nf = 3)})
  pca <- reactive({prcomp(x())})
  dapc <- reactive({dapc.genind(obj(), n.pca = 3, n.da = nPop(obj()))})
  
  
  output$table <- renderDataTable({values$df})
  output$pca <- renderPlot({autoplot(pca(), x = 1, y = 3, frame = TRUE, frame.type = 'norm', data = values$df, colour = "Population")})
  output$dapc <- renderPlot({scatter(dapc(), cell = 1, mstree = T, lwd = 3, lty = 1, cex = 0.8, solid = 1, legend = T)})
  output$print <- renderPrint({print(dapc())})
  output$genind <- renderPrint({print(obj())})
  output$result <- renderPrint({print("if else thingy --- Your strain fits into Genetic cluster ?? and has a higher chance of being resistant to azole antifungals")})
}


#### ShinyApp ####
shinyApp(ui = ui, server = server)

#### Spares ####
lapply(straf, function(i) {numericInput(inputId = paste0(straf, i),
                                        label = paste0("Allele ", i),
                                        value = 0,
                                        min = 0,
                                        max = 300,
                                        width = 200)})




for(i in 1:9) values$df[,i] <- as.character(floor(as.numeric(as.character(values$df[,i]))))

values <- reactiveValues()
values$df <- db
tail(db)
newLine <- isolate(c(20,20,20,20,20,20,20,20,20))
newLine
view <- isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
tail(view)
obj <- df2g
enind(view, ploidy = 1, sep = "")
obj


obj <- df2genind(view, ploidy = 1)
obj

tail(db)

### addd to plots 

class(x)
dim(x)

s.class(pca1$li, pop(str.gen), xax = 3, yax = 1)
