#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(shinyjs)

source("Functions.R")

load("DataTotal.RData")
dfin <- x
#nombres <- c("PROVINCIA", "ubigeo", "ORGPOL", "DEPARTAMENTO", "Candidato", "monto", "strSexo",
#             "strUbigeoPostula", "intPosicion", "strCandidato", "ELECTO2020", "Email","Censura2020","ranking", "tipo",
#             "circun", "DISTRITO")
#names(dfin) <- nombres

dfcong <- function(x,y,z,c){
  if (!is.null(x) & !is.null(y) & !is.null(z) & !is.null(c)) {
    dfr <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y & dfin$DISTRITO == z & dfin$CONGRESISTA == c,])
  } else {
    dfr <- dfin[FALSE,]
  }
  return(dfr)
}

creamensaje <- function(nombre, usuario, dni, distrito, provincia, monto_maximo, partido){
  text <- character(0)
  text[1] <- stringr::str_c("Estimad@ ", nombre,sep=' ')
  text[2] <- "<br>"
  text[3] <- stringr::str_c("Soy un(a) elector(a) del distrito", distrito, "-", provincia, ", ", "donde usted obtuvo la votación más alta ","(", 
                            monto_maximo, " votos preferenciales.)")
  text[4] <- ""
  text[5] <- stringr::str_c("Dado que usted, en la práctica, es nuestro representante, le pedimos que por favor en la votación sobre la vacancia presidencial, vote en contra y urja a su bancada a hacer lo mismo.")
  text[6] <- stringr::str_c("La vacancia presidencial, en medio de la pandemia Covid-19, es tanto una afrenta contra la institucionalidad democrática de nuestro país, como un un riesgo de salud pública. Esto no quita que se investigue al presidente al terminar su mandato.")
  text[7] <- stringr::str_c("Tengo confianza de que usted, como representante del pueblo peruano, escuchará este pedido y responderá favorablemente. De no ser así, tenga la seguridad de que en la próxima elección, el electorado peruano recordará que usted no escucha a sus electores, yo no votaré por usted y convenceré a amig@s y familiares que tampoco.")
  text[8] <- "Cordialmente"
  text[9] <- stringr::str_c(usuario,dni, sep=' ')
  return(text)
}

encabezado<-function(nombre){
  enca=paste0("Estimad@ ",nombre)
  return(enca)
}
presentacion<-function(usuario, distrito,provincia,monto_maximo){
  presentacion=paste0("Soy un(a) elector(a) del distrito ", distrito, "-", provincia, ", ", "donde usted obtuvo la votación más alta ","(", 
                            monto_maximo, " votos preferenciales.)")
  return (presentacion)
}
parrafo1<-function(){
  p1=as.character(paste0("Dado que usted, en la práctica, es nuestro representante le pedimos que por favor en la votación sobre la vacancia presidencial, vote en contra y urja a su bancada a hacer lo mismo."," ",
    "La vacancia presidencial, en medio de la pandemia Covid-19, es tanto una afrenta contra la institucionalidad democrática de nuestro país, como un un riesgo de salud pública. Esto no quita que se investigue al presidente al terminar su mandato."))
}
firma<-function(usuario,dni){
  fi=paste(as.character(usuario),",DNI: " ,as.character(dni))
  return(fi)
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

server <- function(input, output, session){
  VacanciaOK <- reactive(input$txtVacanciaOK)
  carta <- reactive(input$txtcarta)
  #email<-reactive(input$txtcorreo)
  shiny::observe({
    x <- input$txtDepartamento
    #vprov <- unique(dfin[dfin$DEPARTAMENTO == x,]$PROVINCIA)
    shiny::updateSelectInput(session, "txtProvincia", label = "Provincia", choices = vprov(dfin, x), selected = head(vprov(dfin, x),1))
  })
  shiny::observe({
    x <- input$txtDepartamento
    y <- input$txtProvincia
    vdist <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y,]$DISTRITO)
    shiny::updateSelectInput(session, "txtDistrito", label = "Distrito", choices = vdist(dfin, x, y), selected = head(vdist(dfin, x, y),1))    
  })
  shiny::observe({
    message(paste0("VacanciaOK= ",VacanciaOK()))
    if(!is.null(VacanciaOK())){
      if(input$txtVacanciaOK=="Si"){
        output$txtcorreo<-renderUI({
          shiny::textInput(inputId = "txtcorreo", label = "correo electronico", value = " ", placeholder = "tucorreo@electronico.com")
        })
        output$Nombre<-renderUI({
          shiny::textInput(inputId = "txtNombreUsuario", label = "Nombre usuario", value = "", placeholder = "Tu nombre")
        })
        output$DNI<-renderUI({
          shiny::textInput(inputId = "txtDNI", label = "DNI", value = "", placeholder = "Tu DNI")
        })
        output$Congresista <- renderUI({
          r <- unique(dfin[dfin$DEPARTAMENTO == input$txtDepartamento & dfin$PROVINCIA == input$txtProvincia & dfin$DISTRITO == input$txtDistrito,]$CONGRESISTA)
          shiny::selectInput(inputId = "txtCongresista",  label = "Congresista",  choices = r, selected = head(r, 1))
        })
        output$button <- renderUI({
          shiny::actionButton(inputId = "OKbutton", label = "TextoEmail", icon = icon("OK"))
        })
        output$linkbutton <- renderUI({
          shiny::tags$a(class="btn btn-default", href="https://mail.google.com", "Gmail")
        })
        output$carta <- renderUI({
          texto <- "¿Quieres ser parte de la carta grupal que mandaremos al congreso? Si seleccionas sí! Guardamos tu información de lo contrario la eliminamos"
          shiny::selectInput(inputId = "txtcarta", label = texto, choices = c("Si", "No", ''), selected = '')
        })
        
      }
    }
  })
  shiny::observe(
    if (!is.null(carta())){
      message(paste0("carta = ", carta()))
      if (carta()=="Si"){
        actualizaUsuarios(txtcorreo = input$txtcorreo,
                          txtVacanciaOK = input$txtVacanciaOK, 
                          txtDepartamento = input$txtDepartamento, 
                          txtProvincia = input$txtProvincia, 
                          txtDistrito = input$txtDistrito, 
                          txtCongresista = input$txtCongresista)
        output$mensajeDataSave <- shiny::renderText(
          "<b>Se grabaron sus datos</b>"
        )
        
      } 
    }
  )
  # msg <- shiny::eventReactive(input$OKbutton, {
  #   #output$pie <- getpie()
  #   dfcong <- dfcong(x = input$txtDepartamento, y = input$txtProvincia, z = input$txtDistrito, c = input$txtCongresista)
  #   mensaje <- creamensaje(nombre = unique(dfcong$CONGRESISTA),
  #                          usuario = input$txtNombreUsuario,
  #                          dni = input$txtDNI,
  #                          distrito = unique(dfcong$DISTRITO), 
  #                          provincia = unique(dfcong$PROVINCIA), 
  #                          monto_maximo = unique(dfcong$VP), 
  #                          #partido = unique(dfcong$ORGPOL)
  #                          )
  #   mensaje
  # })
  encabezado1<-shiny::eventReactive(input$OKbutton,{
    dfcong <- dfcong(x = input$txtDepartamento, y = input$txtProvincia, z = input$txtDistrito, c = input$txtCongresista)
    mensajeE <- encabezado(nombre = unique(dfcong$CONGRESISTA))
    mensajeE
  })
  presentacion1<-shiny::eventReactive(input$OKbutton,{
    dfcong <- dfcong(x = input$txtDepartamento, y = input$txtProvincia, z = input$txtDistrito, c = input$txtCongresista)
    mensajeP <- presentacion(usuario = input$txNombreUsuario,distrito=input$txtDistrito, 
      provincia=input$txtProvincia,monto_maximo=unique(dfcong$VP))
    mensajeP
  })
  p1=shiny::eventReactive(input$OKbutton,{
    p12=parrafo1()
    p12
  })
  p2=shiny::eventReactive(input$OKbutton,{
    p22="Tengo confianza de que usted, como representante electo por mi distrito, escuchará este pedido y responderá favorablemente. De no ser así, tenga la seguridad de que en la próxima elección, el electorado peruano recordará que usted no escucha a sus electores, yo no votaré por usted y convenceré a amig@s y familiares que tampoco lo hagan."
    p22
  })
  p3=shiny::eventReactive(input$OKbutton,{
    p33="Atentamente,"
    p33
  })
  fir <- shiny::eventReactive(input$OKbutton,{
    firma(usuario = input$txtNombreUsuario,
          dni = input$txtDNI)
  })
  output$table1<-renderTable({
    dfin%>%filter(DEPARTAMENTO==input$txtDepartamento,
                  PROVINCIA==input$txtProvincia,
                  DISTRITO==input$txtDistrito)%>%
      dplyr::select(CONGRESISTA,EMAIL,VCENSURA)%>%
      unique()
  })
  # output$mensaje <- shiny::renderText(
  #   msg()
  # )
  output$encabezado2 <- shiny::renderText(
    encabezado1()
  )
  output$presentacion2 <- shiny::renderText(
    presentacion1()
  )
  output$parrafo1<-shiny::renderText(
    p1()
  )
  output$parrafo2<-shiny::renderText(
    p2()
  )
  output$parrafo3<-shiny::renderText(
    p3()
  )
  output$firma<-shiny::renderText(
    fir()
  )
  #output$parrafo2<-shiny::renderText(
  #  "La vacancia presidencial, en medio de la pandemia Covid-19, es tanto una afrenta contra la institucionalidad democrática de nuestro país, como un un riesgo de salud pública. Esto no quita que se investigue y se sancione al presidente al terminar su mandato."
  #  )
  #output$parrafo3<-shiny::renderText(
  #  "La vacancia presidencial, en medio de la pandemia Covid-19, es tanto una afrenta contra la institucionalidad democrática de nuestro país, como un un riesgo de salud pública. Esto no quita que se investigue al presidente al terminar su mandato."
  #  )
  #output$parrafo4<-shiny::renderText(
  #  "Tengo confianza de que usted, como representante del pueblo peruano, escuchará este pedido y responderá favorablemente. De no ser así, tenga la seguridad de que en la próxima elección, el electorado peruano recordará que usted no escucha a sus electores, yo no votaré por usted y convenceré a amig@s y familiares que tampoco."
  #  )
  t <- shiny::eventReactive(input$OKbutton, {
    unique(dfin[dfin$DEPARTAMENTO == input$txtDepartamento & 
                  dfin$PROVINCIA == input$txtProvincia & 
                  dfin$DISTRITO == input$txtDistrito & 
                  dfin$strCandidato == input$CONGRESISTA,]$Email)
  })
  output$mail <- shiny::renderText(
    t()
  )
}