#functions
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
vdep <- function(dfin){
  r <- unique(dfin$DEPARTAMENTO)  
  return(r)
}
vprov <- function(dfin, x) {
  if (!is.null(x)){
    r <- unique(dfin[dfin$DEPARTAMENTO == x,]$PROVINCIA)
  } else {
    r <- dfin$DEPARTAMENTO
  }
  return(r)
}
vdist <- function(dfin, x, y) {
  if (!is.null(x) & !is.null(y)){
    r <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y,]$DISTRITO)
  } else {
    r <- dfin$DISTRITO
  }
  return(r)
} 
vcong <- function(dfin, x, y, z) {
  if (!is.null(x) & !is.null(y) & !is.null(z)){
    r <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y & dfin$DISTRITO == z,]$CONGRESISTA)
  } else {
    r <- dfin$strCandidato
  }
  return(r)
}
dfcong <- function(dfin, x, y, z, c){
  if (!is.null(x) & !is.null(y) & !is.null(z) & !is.null(c)) {
    dfr <- unique(dfin[dfin$DEPARTAMENTO == x & dfin$PROVINCIA == y & dfin$DISTRITO == z & dfin$CONGRESISTA == c,])            
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

