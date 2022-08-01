#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  
  headerPanel("HEALTH INSURANCE COST PREDICTOR"),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input Parameters</h4>"),
    sliderInput("age", label = "Age", value = 20,
                min = min(Data_train$age),
                max = max(Data_train$age)
    ),
    selectInput("sex", label = "Sex", 
                choices = sort(unique(Data_train$sex)), 
                selected = 'TRUE'),
    sliderInput("bmi", label = "BMI", value = 20,
                min = min(Data_train$bmi),
                max = max(Data_train$bmi)),
    sliderInput("children", label = "Children", value = 0,
                min = min(Data_train$children),
                max = max(Data_train$children)),
    selectInput("smoker", label = "Smoker", 
                choices = sort(unique(Data_train$smoker)), 
                selected = 'TRUE'),
    selectInput("region", label = "Region", 
                choices = sort(unique(Data_train$region)), 
                selected = 'TRUE'),
    actionButton('go',"Predict")
  ),
  
  mainPanel(
    sidebarPanel( width = 25,
                  
                  headerPanel("THE PREDICTED COST IS:- "),
                  
                  textOutput("value")
                  
    )
  )
  
)




server <- function(input, output) {
  
  
  
  data2 = reactiveValues()
  observeEvent(input$go,{
    
    Data_train$sex[Data_train$sex  == 0]<- 'F'
    Data_train$sex[Data_train$sex  == 1]<- 'M'
    
    inputdata = Data_train[,c("age","sex", "bmi", "children","smoker","region","charges")]
    
  str(inputdata)
  
    
    
    
    data2$myage <- as.numeric(input$age)
    data2$mysex <- as.character(input$sex)
    data2$mybmi <- as.numeric(input$bmi)
    data2$mychildren <- as.numeric(input$children)
    data2$mysmoker <- as.character(input$smoker)
    data2$myregion <- as.character(input$region)

    
    newPredict = data.frame(age =data2$myage, sex = data2$mysex,
                            bmi = data2$mybmi, children =  data2$mychildren, smoker = data2$mysmoker,
                            region = data2$myregion)
    
    model = lm(charges ~ age+sex+bmi+children+smoker+region,
               data = inputdata)
    
    data2$op = predict(model, newPredict)
  })
  
  output$value <- renderPrint({data2$op})
}

shinyApp(ui, server)