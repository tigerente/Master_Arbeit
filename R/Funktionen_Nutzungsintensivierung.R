###############
#### h(p) ####
###############

# Nutzungshäufigkeit eines einzelnen Produkts, abhängig von der parallelen Produktanzahl in einem Nutzungssystem:
# Größen:     h:      Nutzungshäufigkeit        [h] = 1 / Jahr
#             p:      parallele Produktanzahl   [P] = Anzahl
#             T:      Betrachtungszeitraum      [T] = Jahre
#             N:      Gesamtnutzungsmenge       [N] = Nutzungseinheiten
h <- function(p, N = 10000, T = 10){
  return (N / (p * T))
}


#################
#### time(h) ####
#################

# Nutzungsdauer eines Produkts, abhängig von der Nutzungshäufigkeit:
# Größen:     h:      Nutzungshäufigkeit   [h] = 1 / Jahr
#             t_max:  Nutzungsdauer        [t] = Jahre
#             n_max:  Nutzungsvorrat       [n_max] = Nutzungseinheiten
time <- function(h, t_max = 15, n_max = 4000){
  return (ifelse(h < n_max / t_max, t_max, n_max / h))
}

###############
#### n(h) ####
###############

# Nutzungsmenge eines Produkts, abhängig von der Nutzungshäufigkeit:
# Größen:     h:      Nutzungshäufigkeit  [h] = 1 / Jahr
#             n:      Nutzungsmenge       [n] = Nutzungseinheiten
#             n_max:  Nutzungsvorrat      [n_max] = Nutzungseinheiten
#             t_max:  Nutzungsdauer       [t_max] = Jahre
n <- function(h, t_max = 15, n_max = 4000){
  return (ifelse (h < n_max / t_max, h*t_max, n_max))
}

###############
#### P(h) ####
###############

# Produktanzahl eines Nutzungssystems, abhängig von der Nutzungshäufigkeit:
# Größen:     P:      Produktanzahl       [P] = 1 (rationale Zahl)
#             h:      Nutzungshäufigkeit  [h] = 1 / Jahr
#             N:      Gesamtnutzungsmenge [N] = Nutzungseinheiten
#             n_max:  Nutzungsvorrat      [n_max] = Nutzungseinheiten
#             t_max:  Nutzungsdauer       [t_max] = Jahre
P <- function(h, t_max = 15, n_max = 4000, N = 10000){
  return (ifelse(h < n_max / t_max, N/(h*t_max), N/n_max))
}

###############
#### MIPS(h) ####
###############

# MIPS eines Nutzungssystems, abhängig von der Nutzungshäufigkeit:
# Größen:     h:      Nutzungshäufigkeit  [h] = 1 / Jahr
#             I_fix:  konstante Inputs    [I_fix] = kg
#             S_D:    Service-Nachfrage   [S_D] = Service-Einheiten
#             i_P:    Inputs je Produkt   [i_P] = kg
#             t_max:  Nutzungsdauer       [t_max] = Jahre
#             A:      absolute Auslastung [A] = Service-Einheiten
#             n_max:  Nutzungsvorrat      [n_max] = Nutzungseinheiten

MIPS <- function(h, I_fix, S_D, i_P, t_max, A, n_max){
  return (I_fix/S_D + ifelse(h < n_max / t_max, i_P/(h * t_max * A), i_P/(n_max * A)))
}
