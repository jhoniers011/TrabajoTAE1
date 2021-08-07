library(shiny)
library(dplyr)
library(tidyverse)
library(class)
library(gmodels)
library(caret)
library(shinyWidgets)
library(shinydashboard)
library(shinyLP)



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
            
            
            a {
  background-color: #4B4746;
  box-shadow: 0 5px 0 darkwhite;
  color: white;
  padding: 1em 1.5em;
  position: relative;
  text-decoration: none;
  text-transform: uppercase;
}

a:hover {
  background-color: #777474;
  cursor: pointer;
}

a:active {
  box-shadow: none;
  top: 5px;
}
            
            #link1 {
            background-color: #9e6d27;
            color: #533203;}
            
          #link1::before {
          width: calc(100%);
          height: calc(100%-14px);
            position: absolute;
            left: 0px,
            top: 5 px,
            border-top: 2px dashed #5332013;
            border-bottom: 2px dashed #533203;
          
          }
            
            
    
                                        ")),
        
            tabItems(
                tabItem(tabName = "Introduccion",
                        
                        jumbotron("Introducción", "La presente aplicación busca predecir el número de hijos en los hogares de la población Colombiana, teniendo en cuenta los datos obtenidos de la encuesta de calidad de vida suministrada por el DANE para el año 2019. Para lograr un correcto funcionamiento de esta, es necesario que proporcione algunos datos personales sobre la situación actual en la que te encuentras dentro de tu hogar.", 
                                  button = FALSE),
                        h3("video promocional: "),
                        
                        
                        fixedRow(
                          column(6,
                        iframe(width = "560", height = "315",
                               url_link = "https://www.youtube.com/embed/n7jG4kVZ448")),
                        
                       
                         box(title = h3("Integrantes"),
                            
                            tags$div(tags$ul(
                              tags$li(tags$span(h4("Catherine Andrea Córdoba Espinosa"))),
                              tags$li(tags$span(h4("Santiago Ramírez Zapata"))),
                              tags$li(tags$span(h4("Carlos Mario Calle González"))),
                              tags$li(tags$span(h4("Allison Piedrahita García"))),
                              tags$li(tags$span(h4("Jhonier Santiago Serna Cardona")))
                              )) 
                            
                            ),
                        column(6,
                        infoBox("------------", a("github",href = "https://github.com/jhoniers011/TrabajoTAE1"), width = 6,icon = icon("github", lib = "font-awesome"),subtitle = "hola",color = "black"),
                  
                        
                               infoBox("------", a("Rpubs",href = "https://rpubs.com/allisonpg12/750813"), width = 6,icon = icon("r-project", lib = "font-awesome"),subtitle = "hola",color = "black"))
                            
                        
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
    
    
    num_cuartos <- reactive(input$num_cuartos)
    edad_conyuge <- reactive(input$edad_conyuge)
    edad_jefe <- reactive(input$edad_jefe)
    per_capita <- reactive(input$per_capita)
    vive_madre_conyuge <- reactive(input$vive_madre_conyuge)
    
    
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
        #Se elimina la 9 -> madre vive jefe, y cantidad de personas -> 32
        datos <- datos[, c(19,4,29,31,49,54)] ## se obtienen solo las 7 variables que interesan, 19 es hijos,hijos ya es la primera columna
        
        normalize <- function(x){
            return((x - min(x)) / (max(x) - min(x)))
        }
        
        dat = data.frame(Num_cuartos_para_dormir = (as.numeric(num_cuartos())-min(datos$Num_cuartos_para_dormir))/
                             (max(datos$Num_cuartos_para_dormir)-min(datos$Num_cuartos_para_dormir)),
                         Edad_Conyuge = (as.numeric(edad_conyuge())-min(datos$Edad_Conyuge))/
                             (max(datos$Edad_Conyuge)-min(datos$Edad_Conyuge)),
                         Edad_Jefe = (as.numeric(edad_jefe())-min(datos$Edad_Jefe))/
                             (max(datos$Edad_Jefe)-min(datos$Edad_Jefe)),
                         Ingreso_percapita = (as.numeric(per_capita())-min(datos$Ingreso_percapita))/
                             (max(datos$Ingreso_percapita)-min(datos$Ingreso_percapita)),
                         Madre_vive_Conyuge = (as.numeric(vive_madre_conyuge())-min(datos$Madre_vive_Conyuge))/
                             (max(datos$Madre_vive_Conyuge)-min(datos$Madre_vive_Conyuge)))
        
        
        
        
        
       
        
        entradas <- as.data.frame(lapply(datos[,2:8], normalize))
        set.seed(123)
        aleatorio <- sample(1:nrow(datos), size=nrow(datos)*0.9)
        entradas_entre <- entradas[aleatorio, ]
        entradas_prueba <- entradas[-aleatorio, ]
        Hijos = datos[aleatorio, 1]
        Hijos_prueba = datos[-aleatorio, 1]
        modelo3 <- knn(train = entradas_entre, test = dat, cl = Hijos, k = 15)
        result <- data.frame(Hijos=modelo3[1])
        
        
        
        
      
        
        showModal(modalDialog(title = "",valueBox(as.character(result), "Número  de hijos", icon = icon("child"))))
        print(input$vive_madre_conyuge)
        
    })
    
    
    
    

    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)