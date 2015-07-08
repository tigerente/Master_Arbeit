###################
#### MIPS_a(d) ####
###################
# Variante für i_d = konst. oder M_\Theta = konst.
# Parameter:
# d             = Transportweg
# theta         = Transporthäufigkeit
# i_d           = Inputs je Transportweg
# a             = relative Auslastung
# K             = Kapazität der Produkte
# I_fix         = fixe Inputs
# S_D           = Servicenachfrage
MIPS_a <- function(d, theta, i_d, a, K, I_fix, S_D ){
    return(d * (theta * i_d)/(a * K) + I_fix/S_D)
}
                   
###################
#### MIPS_b(d) ####
###################
# Variante für i_d ~ M_\Theta ~ 1/\Theta
# Parameter:
# d             = Transportweg
# i_d_M         = Inputs je Transportweg und Transportmenge
# m_S           = Transportmenge je Serviceeinheit
# I_fix         = fixe Inputs
# S_D           = Servicenachfrage
MIPS_b <- function(d, i_d_M, m_S,  I_fix, S_D)  {
    return(d * i_d_M * m_S + I_fix/S_D)
}

