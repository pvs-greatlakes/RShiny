library(shiny)
library(ggplot2)
library(gridExtra)
require(ggplot2)
require(caret)
require(scales)
require(gmodels)
require(pscl)
require(InformationValue)
require(plotROC)
require(ROCR)
require(Metrics)
require(pROC)
require(expss)
require(Rmisc)
require(mctest)
require(car)
require(data.table)

df            <-  read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"))
cname         <-  c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','AHD')
colnames(df)  <-  cname

data_clean                                                    <-   na.omit(df)
data_clean$AHD[data_clean$AHD %in% c('0')]                    <-   0
data_clean$AHD[data_clean$AHD %in% c('1', '2', '3', '4')]     <-   1

# Define UI for app that draws ROC plots

ui <- shinyUI(fluidPage(
  # App title ----
  titlePanel("Model performance measures"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",
    # Sidebar panel for inputs ----
      sidebarPanel( "Sidebar panel",
                    radioButtons("DataP", "Data partition type:",
                                 c("Training" = "train",
                                   "Test"     = "test")),
                    br(),
                    
                    sliderInput(inputId = "Ratio",  # Input: Slider for  Test ratio ----
                                label = "Test ratio:",
                                min = 0.10,
                                max = 0.50,
                                value = 30)
      ),

    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot"))
    )
     )))
)

# Define server logic required to draw a histogram ----
server <- shinyServer(function(input, output) {
		set.seed(123)
		test_data_ratio                                               <-   input$Ratio
		train_data_ratio                                              <-   1 - input$Ratio()
		
		ind    <- sample(c(TRUE, FALSE), nrow(data_clean), replace = T, prob = c(train_data_ratio(), test_data_ratio()))
		train  <- data_clean[ind, ]
		test   <- data_clean[!ind, ]

		log_model             <-    glm(AHD ~ ., data = train, family = binomial(link="logit"))

		modelfit              <-    pR2(log_model)["McFadden"]

		summ                  <-    summary(log_model)

		log_predict           <-    predict(log_model, newdata = train, type = "response")
		log_predicted         <-    ifelse(log_predict > 0.5,1,0)
		cm1                   <-    table(train$AHD,log_predicted)

		# Reactive expression to generate the requested graph
		# This is called whenever the inputs change. The output
		# functions defined below then all use the value computed from
		# this expression
		data <- reactive({
                     whichData            <-    input$DataP
		                 pr                   <-    prediction(log_predict, whichData$AHD)
		                 perf                 <-    performance(pr, measure = "tpr", x.measure = "fpr") 	                 
		                })    
		
		auroc_Val                             <-    round(auc(input$DataP$AHD, log_predicted),4)
		title                                 <-    paste(input$DataP, "ROC and Area under the ROC curve =", auroc_Val, sep = " ")			

		output$plot                          =      renderPlot ({
		                                            plot(data(), colorize = TRUE, text.adj = c(-0.2,1.7), main = title)
                                          })
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)