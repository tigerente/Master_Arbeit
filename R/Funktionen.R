###############
#### n_max ####
###############

# Nutzungsvorrat eines Produkts, abhängig von der relativen Auslastung:
n_max <- function(a, n_max_max = 50, n_max_min = 10, n_max_exp = 1){
  n_max_max + (n_max_min - n_max_max) * a^n_max_exp
}

#############
#### i_N ####
#############

# Input pro Nutzungseinheit, abhängig von der relativen Auslastung:
# i_N(a) = (i_N_max - i_N_min) * a^i_N_exp + i_N_min

i_N <- function(a, i_N_max = 1, i_N_min = 0.5, i_N_exp = 1){
    (i_N_max - i_N_min) * a^i_N_exp + i_N_min
}

###############
#### i_N_t ####
###############

# Input pro Nutzungseinheit, abhängig von der Zeit, bei gegebener
# Wartungshäufigkeit:
# t...Zeit
# w...Wartungshäufigkeit (hier: Anzahl Wartungen über den gesamten Zeitraum)
# i_N_min...i_N zum Zeitpunkt der Wartung
# i_N_max...i_N kurz vor der Wartung
# i_N_delta...um diesen Wert erhöht sich i_N_min und i_N_max nach jedem
#             Wartungszyklus
# i_N_t <- function(t, w, i_N_min, i_N_max, i_N_delta){
#   result <- vector()
#
# }

###########
#### P ####
###########

# Anzahl Geräte, abhängig von der relativen Auslastung:
# verschiedene Varianten sind möglich
# case == 1: p(a) = konst. = ceiling(S/(a_min*K*n_max(a_min)))
# case == 2: p(a) = Mindestanzahl notwendiger Produkte  (kontinuierlich) =
#                   S/(a*K*n_max(a))
# case == 3: p(a) = Mindestanzahl notwendiger Produkte  (diskret) =
#                   ceiling(S/(a*K*n_max(a)))
# a_min = Punkt, an dem Produktzahl der MIPS-Kurve mit konstanten
#         Produktzahl optimal ist (also die MIPS-Kurve mit optimaler
#         Produktzahl schneidet)
# const = Dataframe (
#           K     = Kapazität
#           S_D   = Nachfrage
# ... = Parameter für n_max

P <- function(a, case, a_min=0.2, const, ...){
  attach(const)
  if(case == 1){
    eval <- rep(ceiling(S_D/(a_min*K*n_max(a_min, ...))), times=length(a))
    detach(const)
    return(eval)
  }
  else if(case == 2){
    eval <- S_D/(a*K*n_max(a,...))
    detach(const)
    return(eval)
  }
  else if(case == 3){
    eval <- ceiling(S_D/(a*K*n_max(a, ...)))
    detach(const)
    return(eval)
  }
  else{
    detach(const)
    stop("undefined p-function, currently only case 1 till 3 are defined.")
  }
  detach(const)
}


################
#### MIPS_a ####
################

# MIPS in Abhängigkeit von der Auslastung 
# a = Vektor
# const = Dataframe (
#           K     = Kapazität
#           I_fix = alle nicht variablen Inputs
#           i_P   = Input für die Bereitstellung eines Produkts
#           S_D   = Nachfrage
# ... = Parameter, die an die Funktion i_N, P und n_max weitergegeben werden
MIPS_a <- function(a, const,...){
  # folgende Zeilen dienen dazu, die richtigen Parameter an die Funktionen i_N
  # und P weiterzugeben:
  i_N_names <- names(formals(i_N))
  P_names <- names(formals(P))
  n_max_names <- names(formals(n_max))
  dots <- list(...)
  eval_i_N <- do.call('i_N', c(list(a=a), dots[names(dots) %in%
                                    i_N_names]))
  eval_P <- do.call('P', c(list(a=a, const=const), dots[names(dots) %in%
                                c(P_names, n_max_names)]))

  # jetzt, nach dem i_N und P ausgewertet wurden, kann das MIPS berechnet
  # werden:
  attach(const)
  MIPS <- eval_i_N/(a*K) + I_fix/S_D +eval_P*i_P/S_D
  detach(const)
  return(MIPS)
}
