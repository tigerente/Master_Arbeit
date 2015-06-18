##########################################
#### Define Selections for Ergebnisse ####
##########################################
sel20 <- Ergebnisse$Temperatur == 20
sel30 <- Ergebnisse$Temperatur == 30
sel40 <- Ergebnisse$Temperatur == 40
sel60 <- Ergebnisse$Temperatur == 60

selBaumwolle <- Ergebnisse$Programm == "Baumwolle"
selPflegeleicht <- Ergebnisse$Programm == "Pflegeleicht"
selOther <- ! (selBaumwolle | selPflegeleicht)

sel1200 <- Ergebnisse$Drehzahl == 1200
sel1400 <- Ergebnisse$Drehzahl == 1400

selKurz <- Ergebnisse$Kurz == 1
selVorwaesche <- Ergebnisse$Vorwaesche == 1
selEinweichen <- Ergebnisse$Einweichen == 1
selWasserPlus <- Ergebnisse$WasserPlus == 1

selLinks <- Ergebnisse$Maschine == "links"
selRechts <- Ergebnisse$Maschine == "rechts"

selExtra <- selKurz | selVorwaesche | selEinweichen | selWasserPlus
selNoExtra <- ! selExtra

selections <- list(Temperatur=list(sel20=sel20,sel30=sel30,sel40=sel40,sel60=sel60), 
                         Drehzahl = list(sel1200=sel1200, sel1400=sel1400), 
                         Programm = list(selBaumwolle=selBaumwolle, selPflegeleicht=selPflegeleicht, selOther=selOther), 
                         Extras=list(selKurz=selKurz, selVorwaesche=selVorwaesche, selEinweichen=selEinweichen, selWasserPlus=selWasserPlus), 
                         Maschine=list(selLinks=selLinks, selRechts=selRechts))

##########################################
#### Define Selections for Datenblatt ####
##########################################

sel20 <- Datenblatt$Temperatur == 20
sel30 <- Datenblatt$Temperatur == 30
sel40 <- Datenblatt$Temperatur == 40
sel60 <- Datenblatt$Temperatur == 60

selBaumwolle <- Datenblatt$Programm == "Baumwolle"
selPflegeleicht <- Datenblatt$Programm == "Pflegeleicht"
selOther <- ! (selBaumwolle | selPflegeleicht)

sel1200 <- Datenblatt$Drehzahl == 1200
sel1400 <- Datenblatt$Drehzahl == 1400

selKurz <- Datenblatt$Kurz == 1
selVorwaesche <- Datenblatt$Vorwaesche == 1
selEinweichen <- Datenblatt$Einweichen == 1
selWasserPlus <- Datenblatt$WasserPlus == 1
selSparprogramm <- Datenblatt$Sonstiges == "Sparprogramm"

selLinks <- Datenblatt$Maschine == "links"
selRechts <- Datenblatt$Maschine == "rechts"

selExtraD <- selKurz | selVorwaesche | selEinweichen | selWasserPlus | selSparprogramm
selNoExtraD <- ! selExtra

selectionsD <- list(Temperatur=list(sel20=sel20,sel30=sel30,sel40=sel40,sel60=sel60), 
                         Drehzahl = list(sel1200=sel1200, sel1400=sel1400), 
                         Programm = list(selBaumwolle=selBaumwolle, selPflegeleicht=selPflegeleicht, selOther=selOther), 
                         Extras=list(selKurz=selKurz, selVorwaesche=selVorwaesche, selEinweichen=selEinweichen, selWasserPlus=selWasserPlus), 
                         Maschine=list(selLinks=selLinks, selRechts=selRechts))
