#############
#### I_R ####
#############

# I_R in Abhängigkeit von der gewünschten Nutzungsdauer 
# t = Vektor
I_R <- function(t, i_R, t_tech, case){
  if(case == 1){
    i_R * (ceiling(t/t_tech) - 1)
  } else if(case == 2){
    i_R * (t/t_tech)
  } else if(case == 3){
    i_R * (t/t_tech) - i_R
  } else {
    stop("choosen case is not defined")
  }
}
################
#### MIPS_R ####
################

# MIPS in Abhängigkeit von der gewünschten Nutzungsdauer 
# t_max = Vektor
# const = Dataframe (
#           A     = absolute Auslastung (K*a)
#           I_fix = alle nicht variablen Inputs
#           i_P   = Input für die Bereitstellung eines Produkts
#           h     = Nutzungshäufigkeit
#           S_D   = Nachfrage
# I_R = Vektor
MIPS_R <- function(t_max, I_R, const){
  attach(const)
  MIPS <- (i_P + I_R)/(A*h*t_max) + I_fix/S_D
  detach(const)
  return(MIPS)
}
##################
#### MIPS_I_R ####
##################

# MIPS Anteil, der auf I_R entfällt in Abhängigkeit von der gewünschten
# Nutzungsdauer 
# t_max = Vektor
# const = Dataframe (
#           A     = absolute Auslastung (K*a)
#           I_fix = alle nicht variablen Inputs
#           i_P   = Input für die Bereitstellung eines Produkts
#           h     = Nutzungshäufigkeit
#           S_D   = Nachfrage
# I_R = Vektor
MIPS_I_R <- function(t_max, I_R, const){
  attach(const)
  MIPS <- I_R/(A*h*t_max)
  detach(const)
  return(MIPS)
}
##################
#### MIPS_i_P ####
##################

# MIPS Anteil, der auf i_P entfällt in Abhängigkeit von der gewünschten
# Nutzungsdauer 
# t_max = Vektor
# const = Dataframe (
#           A     = absolute Auslastung (K*a)
#           I_fix = alle nicht variablen Inputs
#           i_P   = Input für die Bereitstellung eines Produkts
#           h     = Nutzungshäufigkeit
#           S_D   = Nachfrage
MIPS_i_P <- function(t_max, const){
  attach(const)
  MIPS <- i_P/(A*h*t_max)
  detach(const)
  return(MIPS)
}
