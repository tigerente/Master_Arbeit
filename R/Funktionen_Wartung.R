#############
#### i_N ####
#############

# i_N in Abhängigkeit von der Wartungshäufigkeit
# w = Vektor
i_N <- function(w, i_N_max = 0.5, i_N_min = 1, i_N_exp = 1){
    i_N_min + (i_N_max - i_N_min) * w^i_N_exp 
}

###########
#### t ####
###########

# t in Abhängigkeit von der Wartungshäufigkeit
# w = Vektor
t <- function(w, h, t_max,...){
    pmin(n_max(w,...)/h , t_max)
}


###############
#### n_max ####
###############

# n_max in Abhängigkeit von der Wartungshäufigkeit
# w = Vektor
n_max <- function(w, n_max_min = 40, n_max_max = 50, n_max_exp = 1){
  n_max_min + (n_max_max - n_max_min) * w^n_max_exp
}

################
#### MIPS_W ####
################

# MIPS in Abhängigkeit von der Wartungshäufigkeit 
# w = Vektor
# const = Dataframe (
#           A     = absolute Auslastung (K*a)
#           I_fix = alle nicht variablen Inputs
#           S_D   = Nachfrage
#           i_W   = Input für eine Wartungstätigkeit
#           i_P   = Input für die Bereitstellung eines Produkts
#           h     = Nutzungshäufigkeit
#           t_max = gewünschte Nutzungsdauer
#           i_N_max = i_N(1)
#           i_N_min = i_N(0)
#           i_N_exp = Exponent für die Kurve i_N
#           n_max_max = n_max(1)
#           n_max_min = n_max(0)
#           n_max_exp = Exponent für die Kurve n_max
#       )
# part = which part of the sum is to be evaluated
# part = 0: everything
# part = 1: i_N-Inputs
# part = 2: i_W-Inputs
# part = 3: i_P-Inputs
# part = 4: fixe Inputs

MIPS_W <- function(w, const, part = 0){
  attach(const)
    eval_i_N <- i_N(w=w, 
                    i_N_max = i_N_max,
                    i_N_min = i_N_min,
                    i_N_exp = i_N_exp
                    )
    eval_t <- t(w=w, h=h, t_max = t_max, 
                    n_max_max = n_max_max,
                    n_max_min = n_max_min,
                    n_max_exp = n_max_exp
                    )
    if(part==0){
        MIPS <- 1/A * (eval_i_N + w*i_W + i_P/(h*eval_t)) + I_fix/S_D
    } 
    else if(part==1){
        MIPS <- 1/A * (eval_i_N)
    } 
    else if(part==2){
        MIPS <- 1/A * (w*i_W)
    } 
    else if(part==3){
        MIPS <- 1/A * (i_P/(h*eval_t))
    } 
    else if(part==4){
        MIPS <-  rep(I_fix/S_D, times=length(w))
    } 
  detach(const)
  return(MIPS)
}
