#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
patient <- readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
 
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

   # Application title
   titlePanel("BreastCancer Prediction"),
   navlistPanel(
     tabPanel(title = "Patient" ,
              fluidPage(
                titlePanel("Patient Details"),
                p("Enter the patient details below and select ADD for new patient"),
                  fluidRow(
                    column(4,
                           textInput(inputId = "name", 
                           label = "Name",
                           value = "enter the name")),
                    column(4,
                          numericInput(inputId = "age", 
                           label = "Age", 
                           value = "Enter the age")),
                    column(4,
                           textInput(inputId = "pid",
                           label = "Patient Id", 
                           value = "enter the id"))
                     ),
                    fluidRow(
                      column(4,actionButton("add",label = "ADD")),
                      column(4,actionButton("view", label = "View"))
                      
                    ),
                  hr(),
                titlePanel("Patient Diagnostic Detail"),
                br(),
                tableOutput("summary"),
                hr(),
                titlePanel("Result"),
                fluidRow(
                 
                  column(4,verbatimTextOutput("result"))
                
                )
                )
                  
               
     ),
     tabPanel(title = "Doctor",
              titlePanel("Doctor Details"),
              fluidPage(
                fluidRow(
                  column(4,
                    selectInput(
                    inputId = "dname" , 
                    label = "select the doctor" , 
                    choices = c("Dr.Wilson","Dr.suresh"), 
                    selectize = FALSE
                    )),
                  column(4,
                         textInput(
                           inputId = "pname",
                           label = "Type the patient name"
                         )
                           
                         )
                  )
                ),
                br(),
                hr(),
                titlePanel("Diagnostic details"),
                p("Enter the details of the diseases below"),
                fluidRow(
                  column(3,
                         selectInput(inputId = "cl_thickness",
                                     label = "Cell thickness",
                                     selectize = FALSE,
                                     choices = c(1,2,3,4,5,6,7,8,9,10)
                                     )),
                  column(3,
                         selectInput(inputId = "cell_size",
                                     label = "Cell size",
                                     choices = c(1,2,3,4,5,6,7,8,9,10),
                                     selectize = FALSE
                                     ))
                ),
                hr(),
                fluidRow(
                  column(3,
                         selectInput(inputId = "cell_shape",
                                     label = "Cell shape",
                                     selectize = FALSE,
                                     choices = c(1,2,3,4,5,6,7,8,9,10)
                         )),
                  column(3,
                         selectInput(inputId = "marg_adhesion",
                                     label = "Marginal adhesion",
                                     choices = c(1,2,3,4,5,6,7,8,9,10),
                                     selectize = FALSE
                         ))
                ),
                hr(),
                fluidRow(
                  column(3,
                         selectInput(inputId = "epith_c_size",
                                     label = "epithelial cell size",
                                     selectize = FALSE,
                                     choices = c(1,2,3,4,5,6,7,8,9,10)
                         )),
                  column(3,
                         selectInput(inputId = "bare_nuclei",
                                     label = "Bare nuclei",
                                     choices = c(1,2,3,4,5,6,7,8,9,10),
                                     selectize = FALSE
                         ))
                ),
                hr(),
                fluidRow(
                  column(3,
                         selectInput(inputId = "bl_cromatin",
                                     label = "Cromatin",
                                     selectize = FALSE,
                                     choices = c(1,2,3,4,5,6,7,8,9,10)
                         )),
                  column(3,
                         selectInput(inputId = "normal_nucleoli",
                                     label = "Normal nucleoli",
                                     choices = c(1,2,3,4,5,6,7,8,9,10),
                                     selectize = FALSE
                         ))
                ),
                hr(),
                fluidRow(
                  column(3,
                         selectInput(inputId = "mitoses",
                                     label = "Mtoses",
                                     selectize = FALSE,
                                     choices = c(1,2,3,4,5,6,7,8,9)
                         )),
                  column(4,
                         actionButton(inputId = "result" , label = "View results"))
                ),
                hr(),
                fluidRow(column(5, verbatimTextOutput("output")))
              )
      )
    ))



# Define server logic 
server <- shinyServer(function(input, output) {
 
 observeEvent(input$add,{
    new <- data.frame(
      P_name = input$name , 
      P_age = as.numeric(input$age) , 
      P_id = input$pid,
      P_class =""
            )
    rownames(new) <- new$P_name
    patient <- readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
    patient <- rbind.data.frame(patient , new)
    saveRDS(patient ,"C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds" , compress = FALSE)
    patient <- readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
    print(patient)
    })
 
   observeEvent(input$view,{
     patient <- readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
     
     output$summary <- isolate(renderTable(
     patient[input$name ,]
     ))
     output$result <- isolate(renderPrint(
       as.character(patient[input$name ,4])
     
    ))
  })
   inputToPrediction <- reactive(
     feature <- data.frame(Id = as.character(333), 
                           Cl.thickness = factor(input$cl_thickness , levels = c(1,2,3,4,5,6,7,8,9,10)),  
                           Cell.size = factor(input$cell_size , levels = c(1,2,3,4,5,6,7,8,9,10)) , 
                           Cell.shape = factor(input$cell_shape , levels = c(1,2,3,4,5,6,7,8,9,10)),
                           Marg.adhesion = factor(input$marg_adhesion , levels = c(1,2,3,4,5,6,7,8,9,10)),
                          Epith.c.size = factor(input$epith_c_size, levels = c(1,2,3,4,5,6,7,8,9,10)),
                          Bare.nuclei = factor(input$bare_nuclei , levels = c(1,2,3,4,5,6,7,8,9,10)), 
                          Bl.cromatin = factor(input$bl_cromatin , levels = c(1,2,3,4,5,6,7,8,9,10)),
                          Normal.nucleoli = factor(input$normal_nucleoli , levels = c(1,2,3,4,5,6,7,8,9,10)),
                          Mitoses = factor(input$mitoses ,levels = c(1,2,3,4,5,6,7,8,9)),stringsAsFactors = FALSE)
   )
   observeEvent(input$result,{
     patient <- readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
     
      mod <- readRDS("bay.rds")
     
     test <- inputToPrediction()
    
     final <- predict(mod , test)
     patient[input$pname ,4] <- as.character( final)
     print(patient[input$pname ,])
     saveRDS(patient ,"C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds" , compress = FALSE)
     
     output$output <- renderPrint(
       final
     )
  
     
     
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

