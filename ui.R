
### ------------------------------------------------------------
# shiny app para preparar mail para congresistas
# Autor : varios
# Fecha : 2020-09-11
### ------------------------------------------------------------

library(shiny)
library(shiny)
library(ggplot2)
library(stringr)
library(scales)
library(shinyjs)


source("Functions.R")
load("DataTotal.RData")
dfin <- x
#url <- "https://twitter.com/intent/tweet?text=Esto%20Prueba&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"
url <- "https://twitter.com/intent/tweet?text=La%20Isla%20Bonita"
url1 <- "https://twitter.com/intent/tweet?text="
url2 <- "La Isla Bonita"
url <- paste0(url1, gsub(pattern = "\\s", replacement = "%20", x = url2, perl = TRUE))


gentabSalir <- function(){
  tp <- shiny::tabPanel(title = "Salir", value = "stop", icon = icon("circle-o-notch"))
  return(tp)
}
url1=textOutput("tweet")
tag_twitter <- function(){
  t <- tags$a(href=url, "Tweet", class="twitter-share-button", id = "twitterbutton")
  includeScript("http://platform.twitter.com/widgets.js")
  return(t)
}

sbp_input <- function(){
  sbp <- sidebarPanel(
    shiny::selectInput(inputId = "txtDepartamento", label = "Departamento", choices = vdep(dfin),  selected = ""),
    shiny::selectInput(inputId = "txtProvincia",    label = "Provincia",    choices = c(''), selected = ""),
    shiny::selectInput(inputId = "txtDistrito",     label = "Distrito",     choices = c(''), selected = ""),
    shiny::selectInput(inputId = "txtVacanciaOK",   label = "¿Desea escribir a su congresista para evitar la vacancia del Presidente?", 
                       choices = c("Si", "No",''), selected = ''),
    shiny::htmlOutput("txtcorreo"),
    shiny::htmlOutput("Nombre"),
    shiny::htmlOutput("DNI"),
    shiny::htmlOutput("Congresista"),
    #shiny::includeScript("http://platform.twitter.com/widgets.js"),
    fluidRow( 
      shiny::htmlOutput("button")
    ),
    p("Para enviar tu tweet, primero da click a TEXTOEMAIL y luego has click ", htmlOutput("link", inline = TRUE),
      class="alert2"),
    #shiny::htmlOutput("carta")
  )  
  return(sbp)
}

div_instructions <- function(){
  dv <- div(
    tags$ol(
      class = "textoInstrucciones",
      tags$li("Elige tu distrito"),
      tags$li("Si quieres escribir a tus congresistas para evitar la vacancia marca sí en la pregunta"),
      tags$li("Preparamos un correo por tí: llena tu DNI y Nombre (no guardamos la info)"),
      tags$li("Copia el texto y envíalo desde tu correo."),
      tags$li("Algunos congresistas tienen twitter. Para ellos te preparamos un tweet. Primero da click a TEXTOEMAIL y luego al link para enviar el tweet")
    ))
  return(dv)
}

div_explicacion <- function(){
  dv <- div(h5("El lunes 09 el Congreso votará la vacancia del presidente Martín Vizcarra por segunda vez. 
            Creemos que si bien los hechos ameritan una investigación y posible sanción estos no justifican la vacancia del presidente especialmente en plena pandemia y a poco meses de una nueva elección. 
            Con esta aplicación queremos acercar los congresistas a los ciudadanos.
            Hemos identificado que congresista obtuvo mayor votación en un distrito por lo que debería hacerte caso. Sino al menos le llenamos el buzón ;).
            Más información y nuesta metodología", a(href = "https://github.com/DecideBienpe/escribe-atu-congresista/blob/master/DecideBien.md", "aquí."))
  )
  return(dv)          
}

mp_salida <- function(){
  mp <- shiny::mainPanel(
    shiny::h4("Los congresistas de tu región"),
    tableOutput("table2"),
    shiny::h4("Más votado en tu distrito", class ="alert"),
    tableOutput("table1"),
    shiny::h4("Mensaje: copia el texto y mándalo desde tu correo"),
    tags$div(shiny::htmlOutput("encabezado2"),tags$br(),
             shiny::htmlOutput("presentacion2"),
             tags$br(),shiny::htmlOutput("parrafo1"),tags$br(),
             shiny::htmlOutput("parrafo2"),tags$br(),
             shiny::htmlOutput("parrafo3"),tags$br(),
             shiny::htmlOutput("firma"), tags$br()#,
             #HTML('<hr style="color: blue;">'),
             #shiny::h4("Copia el texto, pégalo en tu browser y manda el tweet"),
             #shiny::verbatimTextOutput("tweet")#,
             #shiny::uiOutput("url"),
             #actionButton("twitter_share",
             #label = "Share",
             #icon = icon("twitter"),
             #onclick = sprintf("window.open('%s')", htmlOutput("link", inline = TRUE)))
    ),
    shiny::htmlOutput("mensajeDataSave")
  )
  return(mp)
}

tp_email_congresista <- function(){
  tp <- tabPanel("Email Congresista",
    shinyjs::useShinyjs(), # to initialize shinyjs
    shiny::tags$head(includeScript("ganalytics.js")),
    includeCSS("styles.css"),
    h2("¡Decide bien! Escríbele a tu congresista", class = "centrado titulo"),
    div_explicacion(),
    div_instructions(),
    sbp_input(),
    mp_salida()
  )
  return(tp)
}

tp_creditos <- function(){
  tp <- tabPanel("Creditos",
                 p("Esta iniciativa es voluntaria y ciudadana.","Esta plataforma fue iniciada por",
                   a(href = "http://www.joseincio.com", "José Incio"),"Cualquier problema o error me escriben a jincio@gmail.com.","La programación se la debemos escencialmente a 
      la genia de Malena Maguina",a(href = "https://twitter.com/malena_maguina", "Twitter"))
  )
  return(tp)
}

shiny::navbarPage(
  title = "Decidebien",
  id = "navbar",
  tp_email_congresista(),
  tp_creditos(),
  gentabSalir()
)
