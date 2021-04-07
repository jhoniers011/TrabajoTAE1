library(shiny)
library(dplyr)
library(tidyverse)
library(class)
library(gmodels)
library(caret)
library(shinyWidgets)
library(shinydashboard)
library(bs4Dash)



ui <- dashboardPage(
    
    
        dashboardHeader(title = "Hijos"),
        dashboardSidebar(sidebarMenu(
        menuItem("Introducción", tabName = "Introduccion", icon = icon("book")),
        menuItem("Aplicación", icon = icon("th"), tabName = "Aplicacion"))),
        
        dashboardBody(
        
            tags$style(HTML("
    
    
            .box.box-solid.box-danger>.box-header {
            color:#fff;
            background:#5F5A58
                        }
    
            .box.box-solid.box-danger{
            border: 5 px;
            border-bottom-color:#5F5A58;
            border-left-color:#5F5A58;
            border-right-color:#5F5A58;
            border-top-color:#5F5A58;
            }
    
            .box.box-danger>.box-header {
            border: 5 px;
            color:#000000;
            background:#fff
                        }
    
            .box.box-danger{
            border: 5 px;
            border-bottom-color:#5F5A58;
            border-left-color:#5F5A58;
            border-right-color:#5F5A58;
            border-top-color:#5F5A58;
            }
    
                                        ")),
        
            tabItems(
                tabItem(tabName = "introduccion",
                        
                        
                        bs4Jumbotron(
                          title = "Welcome to the LEGO Mosaic Maker!",
                          lead = "This is a Shiny application that lets you convert any picture to a LEGO mosaic directly from the comfort of your web browser!  Once you upload a picture, you can customize many settings.  This app would not be possible without the innovative R scripts created by Ryan Timpe!  Here are links to his excellent blog posts detailing the workflow:"
                          ,
                          status = "primary",
                          btn_name = "App GitHub Repository",
                          href = "https://gitlab.com/rpodcast/shinylego"
                        )
                        
                        
                        
                        
                        
                        ),
            
                tabItem(tabName = "Aplicacion",
                    

                    fixedRow(
                        column(12,
        
                            box(title= "Por favor responda las siguientes preguntas",align = "center",width = 12,
            
                                fluidRow(
                                    column(8,
                                        box(title = "1/7", status = "info",solidHeader = TRUE,width = 6,
                                        numericInput("edad_conyuge", label = h3("Edad de la cónyuge"),value = 45),
                                        sliderInput("slider_edad_conyuge","",
                                                    min = 15, max = 100, value = 45
                                        ),
                                        ),
                                        box(title = "2/7", status = "info",solidHeader = TRUE,width = 6,
                                        numericInput("edad_jefe", label = h3("Edad del jefe del hogar"), value = 48),
                                        sliderInput("slider_edad_jefe","",
                                                    min = 15, max = 100, value = 48
                                        )),
                                        
                                        box(title = "3/7", status = "info",solidHeader = TRUE,width = 6,
                                        numericInput("cant_personas", label = h3("Cantidad de personas del hogar"), value = 4),
                                        sliderInput("slider_cant_personas","",
                                                    min = 2, max = 19, value = 4
                                        )),
                                        box(title = "4/7", status = "info",solidHeader = TRUE,width = 6,
                                        numericInput("num_cuartos", label = h3("Número de cuartos para dormir"), value = 2),
                                        sliderInput("slider_num_cuartos","",
                                                    min = 1, max = 7, value = 2
                                        )
                                        ),
                                        ),
                                    box(title = "5/7", status = "info",solidHeader = TRUE,width = 4,
                                    numericInput("per_capita", label = h3("Ingreso per cápita"), value = 700000),
                                    sliderInput("slider_per_capita","",
                                                min = 0, max = 95000000, value = 700000
                                    )),
                                    box(title = "6/7", status = "info",solidHeader = TRUE,width = 4,
                                        prettyRadioButtons(
                                            inputId = "vive_madre_jefe",
                                            label = "Vive la madre del jefe del hogar contigo", 
                                            choices = c("Si"=1, "No" = 2, "Fallecida" = 3),
                                            icon = icon("check"),
                                            inline = TRUE,
                                            bigger = TRUE, 
                                            status = "info",
                                            animation = "jelly"
                                                        )),
                                    box(title = "7/7", status = "info",solidHeader = TRUE,width = 4,
                                        prettyRadioButtons(
                                            inputId = "vive_madre_conyuge",
                                            label = "Vive la madre del conyuge contigo", 
                                            choices = c("Si" = 1, "No" = 2, "Fallecida" = 3),
                                            icon = icon("check"),
                                            bigger = TRUE,
                                            inline = TRUE,
                                            status = "info",
                                            animation = "jelly"
                                            ))
            
                                            ),
                                actionBttn(
                                    inputId = "button1",
                                    label = "Enviar", 
                                    style = "material-flat",
                                    color = "primary"
                                )
                                
                                
                                )))
                    
                )
            )
        
        
        
        
        )
    
    
    )

# Define server -----------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, ...) {
    
    
    cant_personas <- reactive(input$cant_personas)
    num_cuartos <- reactive(input$num_cuartos)
    edad_conyuge <- reactive(input$edad_conyuge)
    edad_jefe <- reactive(input$edad_jefe)
    per_capita <- reactive(input$per_capita)
    vive_madre_conyuge <- reactive(input$vive_madre_conyuge)
    vive_madre_jefe <- reactive(input$vive_madre_jefe)
    
    
    #Observers para la pregunta edad del conyuge
    
    
    observeEvent(input$slider_edad_conyuge,{
        
        updateNumericInput(session = getDefaultReactiveDomain(),"edad_conyuge",value = input$slider_edad_conyuge)
        
    })
    
    observeEvent(input$edad_conyuge, {
        updateSliderInput(session = getDefaultReactiveDomain(), "slider_edad_conyuge",
                          value = input$edad_conyuge)
    })
    
    #Observers para la pregunta edad del jefe
    observeEvent(input$slider_edad_jefe,{
        
        updateNumericInput(session = getDefaultReactiveDomain(),"edad_jefe",value = input$slider_edad_jefe)
        
    })
    
    observeEvent(input$edad_jefe, {
        updateSliderInput(session = getDefaultReactiveDomain(), "slider_edad_jefe",
                          value = input$edad_jefe)
    })
    
    #Observers para la pregunta cantidad de personas
    observeEvent(input$slider_cant_personas,{
        
        updateNumericInput(session = getDefaultReactiveDomain(),"cant_personas",value = input$slider_cant_personas)
        
    })
    
    observeEvent(input$cant_personas, {
        updateSliderInput(session = getDefaultReactiveDomain(), "slider_cant_personas",
                          value = input$cant_personas)
    })
    
    #Observers para la pregunta num_cuartos
    observeEvent(input$slider_num_cuartos,{
        
        updateNumericInput(session = getDefaultReactiveDomain(),"num_cuartos",value = input$slider_num_cuartos)
        
    })
    
    observeEvent(input$num_cuartos, {
        updateSliderInput(session = getDefaultReactiveDomain(), "slider_num_cuartos",
                          value = input$num_cuartos)
    })
    
    #Observers para la pregunta per_capita
    observeEvent(input$slider_per_capita,{
        
        updateNumericInput(session = getDefaultReactiveDomain(),"per_capita",value = input$slider_per_capita)
        
    })
    
    observeEvent(input$per_capita, {
        updateSliderInput(session = getDefaultReactiveDomain(), "slider_per_capita",
                          value = input$per_capita)
    })
    
    
    
    
    observeEvent(input$button1, {
        
        
        
        datos <- read.csv("Datos_final.csv", header = TRUE, sep = ",") ## Cargando los datos
        datos <- datos[, c(19,4, 9,29,31,32,49,54)] ## se obtienen solo las 7 variables que interesan, 19 es hijos,hijos ya es la primera columna
        
        normalize <- function(x){
            return((x - min(x)) / (max(x) - min(x)))
        }
        
        dat = data.frame(cantidad_personas = (as.numeric(cant_personas())-min(datos$cantidad_personas))/
                             (max(datos$cantidad_personas)-min(datos$cantidad_personas)),
                         Num_cuartos_para_dormir = (as.numeric(num_cuartos())-min(datos$Num_cuartos_para_dormir))/
                             (max(datos$Num_cuartos_para_dormir)-min(datos$Num_cuartos_para_dormir)),
                         Edad_Conyuge = (as.numeric(edad_conyuge())-min(datos$Edad_Conyuge))/
                             (max(datos$Edad_Conyuge)-min(datos$Edad_Conyuge)),
                         Edad_Jefe = (as.numeric(edad_jefe())-min(datos$Edad_Jefe))/
                             (max(datos$Edad_Jefe)-min(datos$Edad_Jefe)),
                         Ingreso_percapita = (as.numeric(per_capita())-min(datos$Ingreso_percapita))/
                             (max(datos$Ingreso_percapita)-min(datos$Ingreso_percapita)),
                         Madre_vive_Conyuge = (as.numeric(vive_madre_conyuge())-min(datos$Madre_vive_Conyuge))/
                             (max(datos$Madre_vive_Conyuge)-min(datos$Madre_vive_Conyuge)),   
                         Madre_vive_Jefe = (as.numeric(vive_madre_jefe())-min(datos$Madre_vive_Jefe))/
                             (max(datos$Madre_vive_Jefe)-min(datos$Madre_vive_Jefe)))
        
        
        
        
        
       
        
        entradas <- as.data.frame(lapply(datos[,2:8], normalize))
        set.seed(123)
        aleatorio <- sample(1:nrow(datos), size=nrow(datos)*0.9)
        entradas_entre <- entradas[aleatorio, ]
        entradas_prueba <- entradas[-aleatorio, ]
        Hijos = datos[aleatorio, 1]
        Hijos_prueba = datos[-aleatorio, 1]
        modelo3 <- knn(train = entradas_entre, test = dat, cl = Hijos, k = 15)
        result <- data.frame(Hijos=modelo3[1])
        
        
        
        #loadmodel <- load("modelo3.Rdata")
        
      
        
        showModal(modalDialog(title = "",valueBox(as.character(result), "Número  de hijos", icon = icon("child"))))
        print(input$vive_madre_conyuge)
        
    })
    
    
    
    

    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)