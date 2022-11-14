###########################################
# CARGAR LIBRERIAS NECESARIAS
###########################################

library(shiny)
library(quantmod)
library(fpp2)
library(tsbox)

###########################################
# CARGAR DATOS 
###########################################

if("simbolos" %in% ls()){
    rm(list = ls()[-(which.max(ls() %in% "simbolos"))])
}else{
    rm(list = ls())
    simbolos <- stockSymbols()
}

###########################################
# DEFINIR LA UI DE LA APP
###########################################

ui <- fluidPage(

    # Titulos de la aplicacion
    titlePanel("TENDENCIA DE ACCIONES"),

    # Slide
    sidebarLayout(
        sidebarPanel(
            textInput("txtStock", 
                      NULL, 
                      value = "MAR", 
                      width = NULL, 
                      placeholder = "Ticket de la activo"),
            actionButton("goButton", 
                         "Aceptar")
        ),

        # Donde se renderiza el grafico
        mainPanel(
           plotOutput("distPlot"),
           textOutput("textIntercep"),
           textOutput("textTrend")
        )
    )
)

###########################################
# DEFINIR EL LADO DEL SERVER DE LA APP
###########################################

server <- function(input, output) {
    
    mod <- eventReactive(input$goButton,{
        
        if(is.null(input$txtStock)){
            salida <- list(p1="", 
                           textoIntercepto="",
                           textoTrend="")
            return(salida)
        }
        
        company <- input$txtStock
        nameComp <- simbolos[which.max(simbolos$Symbol %in% company),2]
        getSymbols(company)
        
        psComp <- eval(as.name(company)) %>% ts_regular()
        Dates <- index(psComp)
        psComp <-psComp %>% ts_ts()
        psComp <- cbind(Dates,psComp) %>% ts_ts()
        rm(list = company)
        
        for (i in 1:ncol(psComp)) {
            psComp[,i] <-  na.interp(psComp[,i])
        }
        rm(i)
        
        reg_01 = tslm(psComp[,5]~trend)
        textoIntercepto <- paste("El intercepto es de: ", reg_01$coefficients[1])
        textoTrend <- paste("La tasa de cambio por unidad de tiempo (Tendencia) es de:", reg_01$coefficients[2])
        
        p1 <- autoplot(psComp[,5]) + 
            autolayer(fitted(reg_01), series = "Trend") +
            
            autolayer(forecast(reg_01, h=round(nrow(psComp)*0.20,0)), series = "Trend", PI=T) +
            
            ggtitle(paste(nameComp,"-", company)) +
            guides(colour=guide_legend(title="Metodos")) +
            ylab("Precios") +
            xlab("Dias")
        
        salida <- list(p1=p1, 
                       textoIntercepto=textoIntercepto,
                       textoTrend=textoTrend)
        
        return(salida)
        
    })
    
    output$distPlot <- renderPlot({
        
        resul <- mod()
        resul$p1
        
    })
    
    output$textIntercep <- renderText({
        resul <- mod()
        resul$textoIntercepto
        })
    
    output$textTrend <- renderText({
        resul <- mod()
        resul$textoTrend
        })
}

###########################################
# CORRER LA APP
###########################################
shinyApp(ui = ui, server = server)