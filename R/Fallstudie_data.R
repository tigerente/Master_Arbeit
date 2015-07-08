##############################
#### Preliminary settings ####
##############################


##############################
#### Get and Prepare Data ####
##############################

Ergebnisse <- read.csv("~/Uni/Master_Arbeit/R/Ergebnisse.csv", sep=";", dec=",", stringsAsFactors=FALSE)
Datenblatt <- read.csv("~/Uni/Master_Arbeit/R/Datenblatt.csv", sep=";", dec=",", stringsAsFactors=FALSE)

# zunächst entfernen wir die Ende-Zeilen:
Ergebnisse <- Ergebnisse[Ergebnisse$Sonstiges != "Ende",]

source("./Data_Selections.R") # define data selections

# jetzt erstellen wir noch einen Vektor, der alle Zeilen mit Gewicht angibt
selNotNull <- Ergebnisse$Gewicht != 0

# MIPS-Data from the database:
MIPS_Strom = 1
i_P_db = c(1,1)
i_d_db = c(1,1)

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
  p = c(10, sig = 0),
  d = c(0, sig = 0)
)
Szenario.Luh <- data.frame(
  p = c(2, sig = 0),
  d = Szenario.Ind$d
)
Szenario.Tra <- data.frame(
  p = Szenario.Luh$p,
  d = c(1.5, sig = 0) # Einheit: Kilometer
)

# Material-Inputs:
i_N <- data.frame(
  dim1 = c(I_N_Strom * MIPS_Strom, sig = 0),
  dim2 = c(i_N_Wasser, sig = 0)
)
i_P <- data.frame(
  dim1 = c(i_P_db[1], sig = 0),
  dim2 = c(i_P_db[2], sig = 0)
)
i_d <- data.frame(
  dim1 = c(i_d_db[1], sig = 0),
  dim2 = c(i_d_db[2], sig = 0)
)

I_N     <- i_N * N
I_Theta <- i_d * Szenario.Ind$d
I_P <- 

I_fix_h <- ( I_N  + I_d ) 
I_fix_h["sig",] <- c(0,0)

I_fix_d  <- data.frame(
  dim1 = c(-1, sig = 0),
  dim2 = c(-1, sig = 0)
)
I_fix_k  <- data.frame(
  dim1 = c(0, sig = 0),
  dim2 = c(0, sig = 0)
)

# alle weiteren Parameter:
S_D   = c(          S, sig = 0)
n_max = c(         -1, sig = 0)
theta = c(         -1, sig = 0)
m_S   = c(         -1, sig = 0)
K     = c(          7, sig = 0)       # Einheit: Kilogramm
t_max = c(         -1, sig = 0)
A     = c(     mean_A, sig = 0)
a     = c(  A[1]/K[1], sig = 0)

# results <- list( # use list for calculations
results <- data.frame( # use for tex-printout
  i_P = i_P,
  i_N = i_N,
  i_d = i_d,

  I_fix_h = I_fix_h,
  I_fix_d = I_fix_d,
  I_fix_k = I_fix_k,

  S_D   = S_D,
  n_max = n_max,
  theta = theta,
  m_S   = m_S,
  K     = K,
  t_max = t_max,
  A     = A,
  a     = a,
  
  Ind = Szenario.Ind,
  Luh = Szenario.Luh,
  Tra = Szenario.Tra
)

