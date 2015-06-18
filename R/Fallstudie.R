##############################
#### Preliminary settings ####
##############################

setwd("./")
library(plot3D)

##############################
#### Get and Prepare Data ####
##############################

Ergebnisse <- read.csv("~/Uni/Master_Arbeit/R/Ergebnisse.csv", sep=";", dec=",", stringsAsFactors=FALSE)
Datenblatt <- read.csv("~/Uni/Master_Arbeit/R/Datenblatt.csv", sep=";", dec=",", stringsAsFactors=FALSE)

# zunächst entfernen wir die Ende-Zeilen:
Ergebnisse <- Ergebnisse[Ergebnisse$Programm != "Ende",]

source("./Data_Selections.R") # define data selections

###########################################
#### Plots for exploration of the Data ####
###########################################

# general settings:
selNotNull <- Ergebnisse$Gewicht != 0

col <- c("blue", "red")
xlim <- c(0, max(c(Ergebnisse$Gewicht, Datenblatt$Gewicht)))
ylim <- c(0, max(c(Ergebnisse$Strom, Datenblatt$Strom)))

# Plots seperated after temperature, programm and moment
nrFields <- with(selections, length(Temperatur) * (length(Drehzahl) * length(Programm)))
l <- matrix(1:nrFields, ncol=length(selections$Temperatur), byrow = TRUE)
layout(l)
par(mar=c(2,3,1,1), mgp=c(3,1,0), mex=1, cex.axis = 0.3, cex.lab = 0.3, cex.main = 0.3)

for(i in 1:length(selections$Programm)){
  vertical <- selections$Programm[i]
  verticalD <- selectionsD$Programm[i]
    for(j in 1:length(selections$Drehzahl)){
      dreh <- selections$Drehzahl[j]
      drehD <- selectionsD$Drehzahl[j]
        for(k in 1:length(selections$Temperatur)){
          horizontal <- selections$Temperatur[k]
          horizontalD <- selectionsD$Temperatur[k]

            sel = selNotNull & vertical[[1]] & dreh[[1]] & horizontal[[1]]
            selD = verticalD[[1]] & drehD[[1]] & horizontalD[[1]]
            # sel = selNotNull
            title = paste(names(vertical), names(dreh), names(horizontal))
            # title = "alles"

            # print(sel)
            if(any(sel)){
                with(Ergebnisse[sel,], text2D(Gewicht, Strom, 
                                              colvar = (Maschine == "links"), 
                                              col = col, colkey = FALSE,
                                              labels = rownames(Ergebnisse[sel,]), 
                                              xlab = "Gewicht [kg]", ylab = "Auslastung",
                                              main = title,
                                              cex = 0.3,
                                              xlim=xlim, ylim=ylim
                                              ))
                if(any(sel & selExtra)){
                    with(Ergebnisse[sel & selExtra,], scatter2D(Gewicht, Strom, 
                                                                colvar = (Maschine == "links"), 
                                                                col = col, colkey = FALSE,
                                                                type="p",
                                                                cex=1,
                                                                add=TRUE))
                }
                if(any(selD)){
                    with(Datenblatt[selD,], scatter2D(Gewicht, Strom, 
                                                                colvar = (Maschine == "links"), 
                                                                col = col, colkey = FALSE,
                                                                type="p",
                                                                pch=4,
                                                                cex=0.3,
                                                                add=TRUE))
                }
                if(any(selD & selExtraD)){
                    with(Datenblatt[selD & selExtraD,], scatter2D(Gewicht, Strom, 
                                                                colvar = (Maschine == "links"), 
                                                                col = col, colkey = FALSE,
                                                                type="p",
                                                                cex=1,
                                                                add=TRUE))
                }
            }
            else {
                plot(xlim, ylim, type="l", xlim=xlim, ylim=ylim, main=title)
                points(xlim, rev(ylim), type="l", xlim=xlim, ylim=ylim, main=title)
            }
        }
    }
}
# View(Ergebnisse[sel,])

# plot with all datapoints
l <- matrix(1)
layout(l)
par(mar=c(2,3,1,1), mgp=c(3,1,0), mex=1, cex.axis = 1, cex.lab = 1, cex.main = 1)

sel = selNotNull 
title = "alles"

with(Ergebnisse[sel,], text2D(Gewicht, Strom, 
                            colvar = (Maschine == "links"), 
                            col = col, colkey = FALSE,
                            labels = rownames(Ergebnisse[sel,]), 
                            xlab = "Gewicht [kg]", ylab = "Auslastung",
                            main = title,
                            cex = 1,
                            xlim=xlim, ylim=ylim
                            ))
with(Ergebnisse[sel & selExtra,], scatter2D(Gewicht, Strom, 
                                            colvar = (Maschine == "links"), 
                                            col = col, colkey = FALSE,
                                            type="p",
                                            cex=3,
                                            add=TRUE))
with(Datenblatt, scatter2D(Gewicht, Strom, 
                                            colvar = (Maschine == "links"), 
                                            col = col, colkey = FALSE,
                                            type="p",
                                            pch=4,
                                            cex=1,
                                            add=TRUE))
with(Datenblatt[selExtraD,], scatter2D(Gewicht, Strom, 
                                            colvar = (Maschine == "links"), 
                                            col = col, colkey = FALSE,
                                            type="p",
                                            cex=3,
                                            add=TRUE))


############################
#### Create latex table ####
############################
Sweave("Fallstudie_Datentabelle.Rnw", encoding="utf-8", output="../tex/Tabellen/Fallstudie_Datentabelle.tex")
