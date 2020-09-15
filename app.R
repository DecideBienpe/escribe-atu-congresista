### ------------------------------------------------------------
# shiny app para preparar mail para congresistas
# Autor : Malena Maguina
# Fecha : 2020-09-11
### ------------------------------------------------------------

library(shiny)
library(ggplot2)
library(stringr)
library(scales)
library(shinyjs)

existeusuario <- function(xcorreo){
    if (file.exists("usuarios.RDS")){
        dfuser <- readRDS(file = "usuarios.RDS")
        xcontesto <- nrow(dfuser[dfuser$Correo == xcorreo,])>0    
    } else {
        xcontesto <- FALSE
    }
    return(xcontesto)
}
creadfuser <- function(){
    xdf <- data.frame(FechaHora = character(),
               Correo = character(),
               IndContesto = character(),
               IndVacanciaOK = character(),
               Departamento = character(),
               Provincia = character(),
               Distrito = character(),
               Congresistas = character())
    return(xdf)
}
creamensaje <- function(nombre, distrito, provincia, monto_maximo, partido){
    text <- character(0)
    text[1] <- stringr::str_c("Estimad@ ", nombre, sep=' ')
    text[2] <- ""
    text[3] <- stringr::str_c("Vivo en ", distrito, "-", provincia, ", ", "donde usted obtuvo ", monto_maximo, " votos,")
    text[4] <- ""
    text[5] <- stringr::str_c("la votación más alta",
                              ". Dado que usted es nuestro representatente le pedimos que, por favor, en la votación por la vacancia ")
    text[5] <- stringr::str_c(text[5], " vote en contra. De no ser así, haré público el hecho que usted no escucha a sus electores ",
                              "y en la próxima elección que se presente me esforzaré para que ",
                              partido , " y usted no obtenga la misma votación.")
    text[6] <- ""
    text[7] <- "Cordialmente"
    return(text)
}
#Tomado de https://www.r-bloggers.com/validating-email-adresses-in-r/
isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}
vdep <- function(){
    r <- unique(dfin$DEPARTAMENTO)  
    return(r)
} 
vprov <- function(x) {
    if (!is.null(x)){
        r <- unique(dfin[dfin$DEPARTAMENTO == x,]$PROVINCIA)    
    } else {
        r <- dfin$DEPARTAMENTO
    }
    return(r)
}
vdist <- function(x,y) {
    if (!is.null(x) & !is.null(y)){
        r <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y,]$DISTRITO)
    } else {
        r <- dfin$DISTRITO
    }
    return(r)
} 
vcong <- function(x,y,z) {
    if (!is.null(x) & !is.null(y) & !is.null(z)){
        r <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y & dfin$DISTRITO == z,]$strCandidato)
    } else {
        r <- dfin$strCandidato
    }
    return(r)
}
dfcong <- function(x,y,z,c){
    if (!is.null(x) & !is.null(y) & !is.null(z) & !is.null(c)) {
        dfr <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y & dfin$DISTRITO == z & dfin$strCandidato == c,])            
    } else {
        dfr <- dfin[FALSE,]
    }
    return(dfr)
} 
actualizaUsuarios <- function(txtcorreo, txtVacanciaOK, txtDepartamento, txtProvincia, txtDistrito, txtCongresista) {
    if (!file.exists("usuarios.RDS")){
        dfuser <- creadfuser()
        saveRDS(dfuser, "usuarios.RDS")
    } 
    dfuser <- readRDS(file = "usuarios.RDS")
    txtFechaHora <- Sys.time()
    icontesto <- existeusuario(xcorreo = txtcorreo)
    dfuser[nrow(dfuser)+1,] <- list(FechaHora = txtFechaHora,
                                    Correo = txtcorreo,
                                    IndContesto = icontesto,
                                    IndVacanciaOK = txtVacanciaOK,
                                    Departamento = txtDepartamento,
                                    Provincia = txtProvincia,
                                    Distrito = txtDistrito,
                                    Congresistas = txtCongresista)
    saveRDS(dfuser, "usuarios.RDS")    
    
}
getchart <- function() {
    
    dfuser <- readRDS("usuarios.RDS")
    dffreq <- data.frame(table(dfuser$IndVacanciaOK))
    names(dffreq) <- c("Opinion", "Freq")
    dffreq$h <- round(dffreq$Freq/sum(dffreq$Freq)*100,0)
    
    g <- ggplot(data = dffreq, aes(x=Opinion, y=h)) +
        geom_bar(stat="identity", width = 0.5, fill = "steelblue") +
        geom_text(aes(label=percent(h/100)), hjust=-0.3, size=3.5) +
        coord_flip()  
    return(g)
}

blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )
load("DataTotal.RData")
dfin <- x
nombres <- c("PROVINCIA", "ubigeo", "ORGPOL", "DEPARTAMENTO", "Candidato", "monto", "strSexo",
             "strUbigeoPostula", "intPosicion", "strCandidato", "ELECTO2020", "Email", "ranking", "tipo",
             "circun", "DISTRITO")
names(dfin) <- nombres



    
#mensaje <- "Este es el mensaje para el congresista"


# Define UI for application that draws a histogram
ui <-  shiny::fluidPage(
    useShinyjs(debug = TRUE),
    titlePanel("Escribele a tu representante en el Congreso"),
    sidebarLayout(
        sidebarPanel(
            shiny::textInput(inputId = "txtcorreo", label = "correo electronico", value = "", placeholder = "tucorreo@electronico.com"),
            shiny::htmlOutput("VacanciaOK"),
            shiny::htmlOutput("Departamento"),
            shiny::htmlOutput("Provincia"),
            shiny::htmlOutput("Distrito"),
            shiny::htmlOutput("Congresista"),
            shiny::htmlOutput("button"),
            shiny::htmlOutput("linkbutton")
        ),
        shiny::mainPanel(
            shiny::plotOutput(outputId = "pie", width = "200px", height = "200px" ),
            #shiny::h4("Mail "),
            shiny::textOutput("mail"),
            #shiny::h4("Mensaje"),
            shiny::textOutput("mensaje")
            
        )
    )
)

server <- function(input, output, session) {
    
    #text <- reactiveValues()
    
    shiny::observeEvent(input$txtcorreo, {
        incluido <- TRUE 
        if (!input$txtcorreo == "" & isValidEmail(x = input$txtcorreo)){
            if (file.exists("usuarios.RDS")){
                message(input$txtcorreo)
                incluido <- existeusuario(xcorreo = stringr::str_trim(input$txtcorreo))    
            } else {
                incluido <- FALSE
            }
        } else{
            incluido <- TRUE
        }
        output$VacanciaOK <- renderUI({
            if (!incluido == TRUE){
                texto <- "Esta de acuerdo con la vacancia del Presidente Martin Vizcarra"
                shiny::selectInput(inputId = "txtVacanciaOK", label = texto, choices = c("Si", "No", ''), selected = '')    
            }
        })
        
        output$Departamento <- renderUI({
            if (!incluido == TRUE){
                shiny::selectInput(inputId = "txtDepartamento", label = "Departamento", choices = vdep(),  selected = head(vdep(),1))
            }
        })
        
        output$Provincia <- renderUI({
            if (!incluido == TRUE){
                shiny::selectInput(inputId = "txtProvincia",    label = "Provincia",    choices = c(''), selected = "")
            }
        })
        
        output$Distrito <- renderUI({
            if (!incluido == TRUE){
                shiny::selectInput(inputId = "txtDistrito",     label = "Distrito",     choices = c(''), selected = "")
            }
        })
        
        output$Congresista <- renderUI({
            if (!incluido == TRUE){
                shiny::selectInput(inputId = "txtCongresista",  label = "Congresista",  choices = c(''), selected = "")
            }
        })
        
        output$button <- renderUI({
            if (!incluido == TRUE){
                shiny::actionButton(inputId = "OKbutton", label = "Continuar", icon = icon("OK"))
            }
        })
        
        output$linkbutton <- renderUI({
            if (!incluido == TRUE){
                shiny::tags$a(class="btn btn-default", href="https://mail.google.com", "mail")
            }
        })
        
        shiny::observe({
            if (!incluido == TRUE) {
                    shiny::updateSelectInput(session, "txtProvincia", label = "Provincia", choices = vprov(input$txtDepartamento), selected = head(vprov,1))
            }
        })
        
        shiny::observe({
            if (!incluido == TRUE) {
                    shiny::updateSelectInput(session, "txtDistrito", label = "Distrito", choices = vdist(input$txtDepartamento, input$txtProvincia), selected = head(vdist,1))                                    
            }
        })
        
        shiny::observe({
            if (!incluido == TRUE) {
                    shiny::updateSelectInput(session, "txtCongresista", label = "Congresista", 
                                             choices = vcong(input$txtDepartamento, input$txtProvincia, input$txtDistrito), selected = head(vcong, 1))
            }
        })
    })

    msg <- shiny::eventReactive(input$OKbutton, {
        actualizaUsuarios(txtcorreo = input$txtcorreo,
                          txtVacanciaOK = input$txtVacanciaOK, 
                          txtDepartamento = input$txtDepartamento, 
                          txtProvincia = input$txtProvincia, 
                          txtDistrito = input$txtDistrito, 
                          txtCongresista = input$txtCongresista)
        #output$pie <- getpie()
        dfcong <- dfcong(x = input$txtDepartamento, y = input$txtProvincia, z = input$txtDistrito, c = input$txtCongresista)
        mensaje <- creamensaje(nombre = unique(dfcong$strCandidato), 
                               distrito = unique(dfcong$DISTRITO), 
                               provincia = unique(dfcong$PROVINCIA), 
                               monto_maximo = unique(dfcong$monto), 
                               partido = unique(dfcong$ORGPOL))
    mensaje
})
    
    t <- shiny::eventReactive(input$OKbutton, {
        unique(dfin[dfin$DEPARTAMENTO == input$txtDepartamento & 
                        dfin$PROVINCIA == input$txtProvincia & 
                        dfin$DISTRITO == input$txtDistrito & 
                        dfin$strCandidato == input$txtCongresista,]$Email)
    })

    output$mail <- shiny::renderText(
        t()
    )
    
    
    g <- shiny::eventReactive(input$OKbutton, {
        
        # output$pie <- shiny::renderPlot({
        #     dfuser <- readRDS("usuarios.RDS")
        #     dffreq <- data.frame(table(plotData()$IndVacanciaOK))
        #     names(dffreq) <- c("Opinion", "Freq")
        #     dffreq$h <- round(dffreq$Freq/sum(dffreq$Freq),0)
        #     ggplot(dffreq, aes(x='', y=h, fill=Opinion)) +
        #         geom_bar(width = 1, stat="identity") +
        #         coord_polar("y", start=0) +
        #         blank_theme +
        #         theme(axis.text.x=element_blank()) +
        #         geom_text(aes(y = h/3 + c(0, cumsum(h)[-length(h)]), 
        #                       label = percent(h)), size=5)
        # })
        getchart()
    })
    

    output$mensaje <- shiny::renderText(
        msg()
    )
    
    output$pie <- shiny::renderPlot(
        g()  
    ) 

}

# Run the application 
shinyApp(ui = ui, server = server)
