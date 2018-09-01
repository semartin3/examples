#sergiomartinez



list.of.packages <- c("rvest", "xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#formatea la fecha de fin de contrato
formatearFechaFinContrato <- function (fechaInput){
  
 fecha <- gsub(".", " ", fechaInput, fixed = TRUE) 
 fecha <- as.Date(fecha, "%d %m %Y")
 fecha <- format(fecha,format = "%d/%m/%Y")
 return (fecha)
}
#funcion que formatea la fecha de inicio del contrato
formatearFechaIniContrato <- function(fechaInput){

lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")
fecha <- gsub(",", "", fechaInput, fixed = TRUE) 
fecha <- as.Date(fecha, "%b %d %Y")
Sys.setlocale("LC_TIME", lct)
fecha <- format(fecha, format = "%d/%m/%Y")
return (fecha)

}





#funcion que formatea la fecha de nacimiento
FormatearFechaNacimiento <- function (fechaSinformatear){
  
  a<- fechaSinformatear
  mes <- substr(a, 0, 3)
  indice <- gregexpr(pattern =',',a)
  indice <- unlist(indice)
  dia <-substr(a, 4, indice-1)
  dia <- gsub(" ", "", dia, fixed = TRUE)
  indice2 <- gregexpr(pattern ='\\(',a)
  indice2 <- unlist(indice2)
  anyo <- substr(a, indice+1,indice2-1)
  anyo <- gsub(" ", "", anyo, fixed = TRUE)
  fechaaux <- paste(dia,mes, anyo, sep = "")
  lct <- Sys.getlocale("LC_TIME"); 
  Sys.setlocale("LC_TIME", "C")
  fecha <- as.Date(fechaaux, "%d%B%Y")  
  Sys.setlocale("LC_TIME", lct)
  fecha <- format(fecha, format="%d/%m/%Y")
  return (fecha)
}

#funcion que adapta los nombres
recomposeName <- function(a){
  
  
  indice2 <- unlist(gregexpr("[a-z][A-Z]", a))
  
  indice2 <- indice2+1
  
  a <- substr(a, indice2,nchar(a))
  indice <- gregexpr(pattern = '\\.', a)
  indice <- unlist(indice)
  a <- substr(a, indice-1,nchar(a))
    
  return (a)
}




dataTeam <- function(web,dfinput){
content <- read_html(web)
cls <- html_nodes(content, "td")
res <- numeric(length = length(cls))
for (i in seq_along(cls)) {
  
  res[i] <-html_text(cls[i])

}

indexInicioJugador = 4
indexFinJugador <- 4+13
#calculamos el numero de jugadores
numElementos <- length(res)-3
numJugadores <- numElementos/13

df <- data.frame(stringsAsFactors=FALSE) 

##recorremos el numero  de jugadores
for(j in 1:numJugadores) { 
  dorsal <- res[indexInicioJugador]
  nombre <- recomposeName(res[indexInicioJugador+3])
  posicion <- res[indexInicioJugador+4]
  fechaNacimiento <- res[indexInicioJugador+5]
  fechaNacimiento <- FormatearFechaNacimiento(fechaNacimiento)
  altura <- res[indexInicioJugador+7]
  altura <- substr(altura,1,4)
  altura <- as.numeric(sub(",", ".", altura, fixed = TRUE))
  piernaBuena <- res[indexInicioJugador+8]
  piernaBuena <- gsub("left", "Izquierda", piernaBuena, fixed = TRUE)
  piernaBuena <- gsub("right", "Derecha", piernaBuena, fixed = TRUE)
  piernaBuena <- gsub("both", "Ambas", piernaBuena, fixed = TRUE)
  fechaInicioContrato <- res[indexInicioJugador+9]
  fechaInicioContrato <- formatearFechaIniContrato(fechaInicioContrato)
  diff=Sys.Date()-as.Date(fechaInicioContrato,"%d/%m/%Y")
  diffNum=as.numeric(diff)
  diasContrato <- diffNum
  fechaFinContrato <- res[indexInicioJugador+11]
  fechaFinContrato <- formatearFechaFinContrato(fechaFinContrato)
  valor <- res[indexInicioJugador+12]
  valor <- substr(valor,2,nchar(valor))
  indice <- unlist(gregexpr("m", valor))
  valor <- substr(valor,1,indice-1)
  valor <- as.numeric(valor)
  
  #equipo
  indice <- gregexpr(pattern = '/',l)
  indice <- unlist(indice)
  indice <- indice[3]
  equipo <- substr(l,indice+1,nchar(l))
  indice <- gregexpr(pattern = '/kader',equipo) 
  indice <- unlist(indice)
  equipo <- substr(equipo, 1, indice-1)
  
  
  
  
  dfaux<-data.frame(dorsal,nombre, posicion, fechaNacimiento,altura,piernaBuena,equipo, fechaInicioContrato,diasContrato, fechaFinContrato, valor)
  indexInicioJugador<-indexInicioJugador+13
  indexFinJugador<-indexFinJugador+13
  df <- rbind(df,dfaux)
  
}
dfoutput <- rbind(dfinput,df)
return(dfoutput)
}




lista <- list("https://www.transfermarkt.co.uk/athletic-bilbao/kader/verein/621/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/atletico-madrid/kader/verein/13/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/cd-leganes/kader/verein/1244/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/celta-vigo/kader/verein/940/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/deportivo-alaves/kader/verein/1108/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/espanyol-barcelona/kader/verein/714/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-barcelona/kader/verein/131/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-getafe/kader/verein/3709/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-girona/kader/verein/12321/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-sevilla/kader/verein/368/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-valencia/kader/verein/1049/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-villarreal/kader/verein/1050/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/rayo-vallecano/kader/verein/367/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/real-betis-sevilla/kader/verein/150/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/real-madrid/kader/verein/418/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/real-sociedad-san-sebastian/kader/verein/681/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/real-valladolid/kader/verein/366/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/sd-eibar/kader/verein/1533/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/sd-huesca/kader/verein/5358/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/ud-levante/kader/verein/3368/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/afc-bournemouth/kader/verein/989/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/afc-bournemouth/kader/verein/989/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/brighton-amp-hove-albion/kader/verein/1237/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/cardiff-city/kader/verein/603/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/crystal-palace/kader/verein/873/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-arsenal/kader/verein/11/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-burnley/kader/verein/1132/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-chelsea/kader/verein/631/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-everton/kader/verein/29/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-fulham/kader/verein/931/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-liverpool/kader/verein/31/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-southampton/kader/verein/180/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/fc-watford/kader/verein/1010/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/huddersfield-town/kader/verein/1110/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/leicester-city/kader/verein/1003/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/manchester-city/kader/verein/281/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/manchester-united/kader/verein/985/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/newcastle-united/kader/verein/762/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/tottenham-hotspur/kader/verein/148/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/west-ham-united/kader/verein/379/saison_id/2018/plus/1",
              "https://www.transfermarkt.co.uk/wolverhampton-wanderers/kader/verein/543/saison_id/2018/plus/1")


#to extract links https://hackertarget.com/extract-links/

#ejecutamos y recogemos resultados
dfEnd<- data.frame()
for(l in lista){
  
  dfEnd<-dataTeam(l,dfEnd)
  Sys.sleep(0.9)
}


#ordenamos
dfEnd <- dfEnd[order(-dfEnd$valor),] 

#escribimos a disco

write.xlsx(dfEnd, "C:/Users/casaP/Desktop/listaJugadoresFutbol.xlsx")