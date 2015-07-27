##############################
#### Preliminary settings ####
##############################


##############################
#### Get and Prepare Data ####
##############################

Ergebnisse <- read.csv("Ergebnisse.csv", sep=";", dec=",", stringsAsFactors=FALSE)
Datenblatt <- read.csv("Datenblatt.csv", sep=";", dec=",", stringsAsFactors=FALSE)
MIPS_Daten <- read.csv("MIPS_Daten.csv", sep=";", dec=",", stringsAsFactors=FALSE, row.names=1)

# zunächst entfernen wir die Ende-Zeilen:
Ergebnisse <- Ergebnisse[Ergebnisse$Sonstiges != "Ende",]

source("./Data_Selections.R") # define data selections

# jetzt erstellen wir noch einen Vektor, der alle Zeilen mit Gewicht angibt
selNotNull <- Ergebnisse$Gewicht != 0

##############################
#### Calculate parameters ####
##############################

mean_A = mean(Ergebnisse[selNotNull,]$Gewicht)
delta_N = 22 # Schätzwert für die nicht angegebenen Waschgänge
N = nrow(Ergebnisse) + delta_N
S = N * mean_A
Time = 7/365 # Betrachtungszeitraum in Jahren

# Strom:
I_N_Strom = sum(Ergebnisse$Strom)
i_N_Strom = I_N_Strom / N

# Wasser:
wasser = data.frame()
# first, get the frequency of the programs per machine:
wasser["links_Baumwolle_60","H"]        = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selLinks & 
                                                                 Programm$selBaumwolle &
                                                                 Temperatur$sel60),])

wasser["links_Baumwolle_40","H"]        = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selLinks & 
                                                                 Programm$selBaumwolle &
                                                                 Temperatur$sel40),])

wasser["links_Pflegeleicht_40","H"]     = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selLinks & 
                                                                 Programm$selPflegeleicht &
                                                                 Temperatur$sel40),])

wasser["links_Other_40","H"]            = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selLinks & 
                                                                 Programm$selOther &
                                                                 Temperatur$sel40),])

wasser["rechts_Baumwolle_60","H"]       = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selRechts & 
                                                                 Programm$selBaumwolle &
                                                                 Temperatur$sel60),])

wasser["rechts_Baumwolle_40","H"]       = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selRechts & 
                                                                 Programm$selBaumwolle &
                                                                 Temperatur$sel40),])

wasser["rechts_Pflegeleicht_40","H"]    = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selRechts & 
                                                                 Programm$selPflegeleicht &
                                                                 Temperatur$sel40),])

wasser["rechts_Other_40","H"]           = nrow(Ergebnisse[with(selections, 
                                                               Maschine$selRechts & 
                                                                 Programm$selOther &
                                                                 Temperatur$sel40),])

# now, calculate linear regressions for the programs, where two or more datapoints are given
# and use the linear model to get the value for the mean Auslastung
sel1 <- with(selectionsD, Maschine$selLinks & Programm$selBaumwolle &
               Temperatur$sel40)
Gewicht <- Datenblatt[sel1,]$Gewicht
Wasser <- Datenblatt[sel1,]$Wasser
fit1 <- lm(Wasser ~ Gewicht)
wasser["links_Baumwolle_40","W"] <- predict(fit1, 
                                            newdata = data.frame(Gewicht =
                                                                   mean_A))

sel2 <- with(selectionsD, Maschine$selLinks & Programm$selBaumwolle &
               Temperatur$sel60)
Gewicht <- Datenblatt[sel2,]$Gewicht
Wasser <- Datenblatt[sel2,]$Wasser
fit2 <- lm(Wasser ~ Gewicht)
wasser["links_Baumwolle_60","W"] <- predict(fit2, 
                                            newdata = data.frame(Gewicht =
                                                                   mean_A))

sel3 <- with(selectionsD, Maschine$selRechts & Programm$selBaumwolle &
               Temperatur$sel40)
Gewicht <- Datenblatt[sel3,]$Gewicht
Wasser <- Datenblatt[sel3,]$Wasser
fit3 <- lm(Wasser ~ Gewicht)
wasser["rechts_Baumwolle_40","W"] <- predict(fit3, 
                                             newdata = data.frame(Gewicht =
                                                                    mean_A))

sel4 <- with(selectionsD, Maschine$selRechts & Programm$selBaumwolle &
               Temperatur$sel60)
Gewicht <- Datenblatt[sel4,]$Gewicht
Wasser <- Datenblatt[sel4,]$Wasser
fit4 <- lm(Wasser ~ Gewicht)
wasser["rechts_Baumwolle_60","W"] <- predict(fit4, 
                                             newdata = data.frame(Gewicht =
                                                                    mean_A))

# now, calculate the missing values, for the programs, where only one datapoint is given
# using this point and the slope per machine
mean_slope_left = mean(c(fit1$coefficients[2], fit2$coefficients[2]))
mean_slope_right = mean(c(fit3$coefficients[2], fit4$coefficients[2]))

sel = with(selectionsD, Maschine$selLinks & 
             Programm$selPflegeleicht &
             Temperatur$sel40)
y = Datenblatt[sel, ]$Wasser[[1]]
x = Datenblatt[sel, ]$Gewicht[[1]]
wasser["links_Pflegeleicht_40","W"] <- y - mean_slope_left * (x - mean_A)

sel = with(selectionsD, Maschine$selLinks & 
             Programm$selOther &
             Temperatur$sel40)
y = Datenblatt[sel, ]$Wasser[[1]]
x = Datenblatt[sel, ]$Gewicht[[1]]
wasser["links_Other_40","W"] <- y - mean_slope_left * (x - mean_A)

sel = with(selectionsD, Maschine$selRechts & 
             Programm$selPflegeleicht &
             Temperatur$sel40)
y = Datenblatt[sel, ]$Wasser[[1]]
x = Datenblatt[sel, ]$Gewicht[[1]]
wasser["rechts_Pflegeleicht_40","W"] <- y - mean_slope_right * (x - mean_A)

sel = with(selectionsD, Maschine$selRechts & 
             Programm$selOther &
             Temperatur$sel40)
y = Datenblatt[sel, ]$Wasser[[1]]
x = Datenblatt[sel, ]$Gewicht[[1]]
wasser["rechts_Other_40","W"] <- y - mean_slope_right * (x - mean_A)

i_N_Wasser = weighted.mean(wasser[,"W"], wasser[,"H"])


##########################################
#### Save the results to a data.frame ####
##########################################

Szenario.Ind <- data.frame(
  values = c(p = 17, d = 0),
  sig = c(0, sig = 0)
)

Szenario.Luh <- data.frame(
  values = c(p = 2),
  sig = c(0)
)
Szenario.Luh["d",] <- Szenario.Ind["d",]

Szenario.Tra <- data.frame(values=vector(), sig=vector())
Szenario.Tra["p",] <- Szenario.Luh["p",]
Szenario.Tra["d",] <- c(2, sig = 0)# Einheit: Kilometer

# Material-Inputs:
i_N <- data.frame(
  values = c(MF = i_N_Strom * MIPS_Daten["Strom", "MF"] + MIPS_Daten["Waschmittel", "MF"], 
             CF = i_N_Strom * MIPS_Daten["Strom", "CF"] + MIPS_Daten["Waschmittel", "CF"]),
  sig = c(2, 2)
)
i_P <- data.frame(
  values = c(MF = MIPS_Daten["Waschmaschine","MF"], CF = MIPS_Daten["Waschmaschine","CF"]),
  sig = c(1, 1)
)
i_d <- data.frame(
  values = c(MF = MIPS_Daten["PKW","MF"], CF = MIPS_Daten["PKW","CF"]),
  sig = c(3, 3)
)
i_d_M <- data.frame(
  values = c(MF = MIPS_Daten["Transporter","MF"]/1000, CF = MIPS_Daten["Transporter","CF"]/1000),
  sig = c(3, 3)
)


# alle weiteren Parameter:
S_D   = c(          S, sig = 0)
N     = c(          N, sig = 0)
n_max_low = c(       2000, sig = 0) # Nutzungseinheiten (unterer Wert)
n_max_mid = c(       2500, sig = 0) # Nutzungseinheiten (mittlerer Wert)
n_max_hig = c(       3000, sig = 0) # Nutzungseinheiten (oberer Wert)
theta = c(        1/2, sig = 1) # Anzahl Transporte je Nutzungseinheit
m_S   = c(          1, sig = 0) # Kilogramm (je Serviceeinheit zu transportierende Menge)
K     = c(          7, sig = 0) # Kilogramm
t_max_low = c(          8, sig = 0) # Maximalnutzungsdauer in Jahren  (unterer Wert)
t_max_mid = c(          11, sig = 0) # Maximalnutzungsdauer in Jahren  (mittlerer Wert)
t_max_hig = c(          14, sig = 0) # Maximalnutzungsdauer in Jahren  (oberer Wert)
A     = c(     mean_A, sig = 1)
a     = c(  A[1]/K[1], sig = 0)
alpha = c( 0.75, sig=2) # Korrekturfaktor für Transportinputs


results <- data.frame( # use for tex-printout
  i_P   = t(i_P),
  i_N   = t(i_N),
  i_d   = t(i_d),
  i_d_M = t(i_d_M),

  # I_fix_h = I_fix_h,
  # I_fix_d = I_fix_d,
  # I_fix_k = I_fix_k,

  S_D   = S_D,
  N     = N,
  n_max = n_max_mid,
  theta = theta,
  m_S   = m_S,
  K     = K,
  t_max = t_max_mid,
  A     = A,
  a     = a,
  
  Ind = t(Szenario.Ind),
  Luh = t(Szenario.Luh),
  Tra = t(Szenario.Tra)
)

