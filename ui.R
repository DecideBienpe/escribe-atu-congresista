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

navbarPage(
    title = "Decidebien",
    tabPanel(
        "Email Congresista",
        tags$head(includeScript("ganalytics.js")),
        includeCSS("styles.css"),
        h2("¡Decide bien! Escríbele a tu congresista", class =
         "centrado titulo"),
        div(h5("El viernes 18 el Congreso votará la vacancia del presidente Martín Vizcarra. 
            Creemos que si bien los hechos ameritan una investigación estos no justifican la vacancia del presidente especialmente en plena pandemia. 
            Con esta aplicación queremos acercar los congresistas a los ciudadanos.
            Hemos identificado que congresista obtuvo mayor votación en un distrito por lo que debería hacerte caso.
            Más información y nuesta metodología", a(href = "https://docs.google.com/document/d/1wgdEtIyFvAcddj84fUqu8yXNsZ63VYaOZb9WEDRVjBM/edit?usp=sharing", "aquí."),"Sigue estos pasos:")
        ),
        div(
        tags$ol(
            class = "textoInstrucciones",
            tags$li("Elige tu distrito, te mostramos tu representante(s) y cómo voto la semana pasada sobre la vacancia"),
            tags$li("Si quieres escribir a tus congresistas para evitar la vacancia marca SI en la pregunta"),
            tags$li("Preparamos un correo por tí, necesitamos tu DNI y Nombre (no lo guardaremos! ver último punto)."),
            tags$li("Si tienes más de un 'representante' elije uno de ell@s"),            
            tags$li("Dale click a 'TextoEmail',copia el texto generado y envíalo desde tu correo (pronto tendremos un boton para conectarse con tu email, más fácil aún)"),
            tags$li("La última pregunta es para saber si quieres ser parte de la carta grupal que mandaremos el día jueves (solo en este caso guardaremos tus datos).")
        )),
        sidebarLayout(
        sidebarPanel(
            shiny::selectInput(inputId = "txtDepartamento", label = "Departamento", choices = vdep(dfin),  selected = ""),
            shiny::selectInput(inputId = "txtProvincia",    label = "Provincia",    choices = c(''), selected = ""),
            shiny::selectInput(inputId = "txtDistrito",     label = "Distrito",     choices = c(''), selected = ""),
            shiny::selectInput(inputId = "txtVacanciaOK", label = "¿Desea escribir a su congresista para evitar la vacancia del Presidente?", choices = c("Si", "No",''), selected = ''),
            #shiny::textInput(inputId = "txtcorreo", label = "correo electronico", value = "", placeholder = "tucorreo@electronico.com"),
            #shiny::textInput(inputId = "txtcorreo", label = "correo electronico", value = "", placeholder = "tucorreo@electronico.com"),
            shiny::htmlOutput("txtcorreo"),
            shiny::htmlOutput("Nombre"),
            shiny::htmlOutput("DNI"),
            #shiny::selectInput(inputId = "txtCongresista",  label = "Congresista",  choices = c(''), selected = "")
            #shiny::htmlOutput("Departamento"),
            #shiny::htmlOutput("Provincia"),
            #shiny::htmlOutput("Distrito"),
            shiny::htmlOutput("Congresista"),
            fluidRow(
              shiny::htmlOutput("button"),
              #shiny::htmlOutput("linkbutton")
              ),
            shiny::htmlOutput("carta")
        ),
        shiny::mainPanel(
            tableOutput("table1"),
            #DT::dataTableOutput("table1"),
            #shiny::plotOutput(outputId = "pie", width = "200px", height = "200px" ),
            #shiny::h4("Mail "),
            #shiny::textOutput("mail"),
            shiny::h4("Mensaje: copia el texto y mándalo desde tu correo"),
            #shiny::textOutput("mensaje"),
            #shiny::htmlOutput("encabezado2"),
            tags$div(shiny::htmlOutput("encabezado2"),tags$br(),
                shiny::htmlOutput("presentacion2"),
                tags$br(),shiny::htmlOutput("parrafo1"),tags$br(),
                shiny::htmlOutput("parrafo2"),tags$br(),
                shiny::htmlOutput("parrafo3"),tags$br(),
                shiny::htmlOutput("firma")
                )
            #shiny::htmlOutput("presentacion2"),
            #shiny::htmlOutput("mensajeDataSave")
        )
    )),
    tabPanel(
        "Creditos",
        p("Esta iniciativa es voluntaria y ciudadana.","Esta plataforma fue iniciada por",
      a(href = "http://www.joseincio.com", "José Incio"),"Cualquier problema o error me escriben a jincio@gmail.com.","La programación se la debemos escencialmente a 
      la genia de Malena Maguina",a(href = "https://twitter.com/malena_maguina", "Twitter"))
            )
    )
