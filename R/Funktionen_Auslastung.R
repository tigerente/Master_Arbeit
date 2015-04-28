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

i_N <- function(a, i_N_max, i_N_min = 0.5, i_N_exp = 1){
    (i_N_max - i_N_min) * a^i_N_exp + i_N_min
}


#############
### p(a) ####
#############

# Parallele Produktzahl. Zwei Varianten:
# - konstant (p_is_const=TRUE)
# - variabel (p_is_const=FALSE): Für gegebenes a, wähle p = min {p | h(a, p) <= h_max}
p <- function(a, Konstanten){
  attach(Konstanten)  
  eval_p <- if(p_is_const==TRUE) rep(p_const, times=length(a)) else ceiling(S_D/(a*K*h_max*T))
  detach(Konstanten)
  return(eval_p)
}


################
##### P(a) #####
################

# Effektive Produktanzahl in Abhängigkeit von der Auslastung
P <- function(a, n_max_Pars, Konstanten){
  # n_max(a) auswerten:
  attach(n_max_Pars)
  eval_n_max <- n_max(a = a, n_max_max = n_max_max, n_max_min = n_max_min, n_max_exp = n_max_exp)
  detach(n_max_Pars)
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # P(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_P <- ifelse(eval_n_max > S_D*t_max/(a*K*p*T), p*T/(t_max), S_D/(a*K*eval_n_max))
  detach(Konstanten)
  return(eval_P)
}


#############
### q(a) ####
#############

# Sequentielle Produktanzahl: q=P/p
q <- function(a, n_max_Pars, Konstanten){
  # P(a) auswerten:
  P <- P(a, n_max_Pars, Konstanten)  
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  eval_q <- P/p
  return(eval_q)
}


################
##### h(a) #####
################

h <- function(a, Konstanten){
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # h(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_h <- S_D/(a*K*p*T)
  detach(Konstanten)
  return(eval_h)
}


################
##### n(a) #####
################

n <- function(a, n_max_Pars, Konstanten){
  # n_max(a) auswerten:
  attach(n_max_Pars)
  eval_n_max <- n_max(a = a, n_max_max = n_max_max, n_max_min = n_max_min, n_max_exp = n_max_exp)
  detach(n_max_Pars)
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # n(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_n <- ifelse(eval_n_max > S_D*t_max/(a*K*p*T), S_D*t_max/(a*K*p*T), eval_n_max)
  detach(Konstanten)
  return(eval_n)
}


################
##### t(a) #####
################

t <- function(a, n_max_Pars, Konstanten){
  # n_max(a) auswerten:
  attach(n_max_Pars)
  eval_n_max <- n_max(a = a, n_max_max = n_max_max, n_max_min = n_max_min, n_max_exp = n_max_exp)
  detach(n_max_Pars)
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # t(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_t <- ifelse(eval_n_max > S_D*t_max/(a*K*p*T), t_max, eval_n_max*a*K*p*T/S_D)
  detach(Konstanten)
  return(eval_t)
}


################
## t_tech(a) ###
################

t_tech <- function(a, n_max_Pars, Konstanten){
  # n_max(a) auswerten:
  attach(n_max_Pars)
  eval_n_max <- n_max(a = a, n_max_max = n_max_max, n_max_min = n_max_min, n_max_exp = n_max_exp)
  detach(n_max_Pars)
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # t_tech(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_t_tech <- eval_n_max*a*K*p*T / S_D
  detach(Konstanten)
  return(eval_t_tech)
}


################
#### MIPS(a) ###
################

# MIPS in Abhängigkeit von der Auslastung 
#
# a = Vektor der Auslastungs-Werte
# i_N_Pars = Dataframe (
#                       i_N_max = Maximaler Wert von i_N(a)
#                       i_N_min = Minimaler Wert von i_N(a)
#                       i_N_exp = Exponent (a^i_N_exp)
#                      )
# n_max_Pars = Dataframe (
#                       n_max_max = Maximaler Wert von n_max(a)
#                       n_max_min = Minimaler Wert von n_max(a)
#                       n_max_exp = Exponent (a^n_max_exp)
#                      )
# Konstanten = Dataframe (
#                        K     = Kapazität
#                        I_fix = alle nicht variablen Inputs
#                        i_P   = Inputs für die Bereitstellung eines Produkts
#                        S_D   = Nachfrage
#                        p     = parallele Produktanzahl
#                        T     = Betrachtungszeitraum
#                        t_max = maximale Nutzungsdauer
#                        )
MIPS <- function(a,
                 i_N_Pars,
                 n_max_Pars,
                 Konstanten
                 ){
  # i_N(a) auswerten:
  attach(i_N_Pars)
  eval_i_N <- i_N(a = a, i_N_max = i_N_max, i_N_min = i_N_min, i_N_exp = i_N_exp)
  detach(i_N_Pars)
  
  # n_max(a) auswerten:
  attach(n_max_Pars)
  eval_n_max <- n_max(a = a, n_max_max = n_max_max, n_max_min = n_max_min, n_max_exp = n_max_exp)
  detach(n_max_Pars)
  
  # p(a) auswerten:
  p <- p(a, Konstanten)
  
  # MIPS(a) auswerten und zurückgeben:
  attach(Konstanten)
  eval_MIPS <- eval_i_N/(a*K) + I_fix/S_D + ifelse(eval_n_max > S_D*t_max/(a*K*p*T), i_P*p*T/(t_max*S_D), i_P/(a*K*eval_n_max))
  detach(Konstanten)
  return(eval_MIPS)
}
