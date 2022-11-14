###########################################
# CARGAR LIBRERIAS NECESARIAS
###########################################

library(shiny)
library(gtrendsR)
library(fpp2)
library(tsbox)
library(xts)

###########################################
# DEFINIR LA UI DE LA APP
###########################################

ui <- fluidPage(

    # Titulos de la aplicacion
    titlePanel("TENDENCIA DE BUSQUEDAS GOOGLE"),

    # Slide
    sidebarLayout(
        sidebarPanel(
            textInput("txtWord", 
                      NULL, 
                      value = "", 
                      width = NULL, 
                      placeholder = "Palabrea clave"),
            actionButton("goButton", 
                         "Aceptar")
        ),

        # Donde se renderiza el grafico
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

###########################################
# DEFINIR EL LADO DEL SERVER DE LA APP
###########################################

server <- function(input, output) {
    
    p1 <- eventReactive(input$goButton,{
        
        if(is.null(input$txtWord)){
            return()
        }
        
        wordSerch <- input$txtWord
        listSerch <- gtrends(wordSerch, time = "all")
        
        dta <- listSerch$interest_over_time
        dta <- xts(dta[,2:7], order.by=as.Date(dta[,1], "%Y/%d/%m"))
        X <- na.interp(ts_ts(dta$hits))
        
        p1 <- autoplot(X) +
            ggtitle( toupper(input$txtWord) ) +
            autolayer(meanf(X, h=40),
                      series="Mean", PI=FALSE) +
            autolayer(rwf(X, h=40),
                      series="Naïve", PI=FALSE) +
            autolayer(rwf(X, drift=TRUE, h=40),
                      series="Drift", PI=FALSE)
        
        return(p1)
        
    })
    
    output$distPlot <- renderPlot({
        
        p1()
        
    })
    
}

###########################################
# CORRER LA APP
###########################################
shinyApp(ui = ui, server = server)