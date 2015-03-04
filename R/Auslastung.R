require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 

# Einstellungen für den Export der Graphik
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8

export <- FALSE # if figures should be exported, or just visualized
# export <- TRUE # if figures should be exported, or just visualized 

# Konstanten Definition
Konstanten <- data.frame(
                        K     = 1, # Kapazität
                        I_fix = 1, # alle nicht variablen Inputs
                        i_P   = 20, # Input für die Bereitstellung eines Produkts
                        S_D   = 100, # Nachfrage
                        a_min = 0.4 # vorgegeben, um Konsistenz der Parameter zu
                                    # erreichen (müsste eigentlich ermittelt
                                    # werden)
                        )

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
# verschiedene Varianten sind möglich
# case == 1: i_N(a) = (i_N_max - i_N_min) * sqrt(a) + i_N_min
# case == 2: i_N(a) = (2*i_N_max - i_N_min) * a^3 + i_N_min
# case == 3: i_N(a) = (i_N_max - i_N_min) * a^x + i_N_min

i_N_max = 1
i_N_min = 0.5
i_N <- function(a, case, exponent = 1, i_N_max = 1, i_N_min = 0.5){
  if(case == 1)
    (i_N_max - i_N_min) * sqrt(a) + i_N_min
  else if(case == 2)
    (2*i_N_max - i_N_min) * a^3 + i_N_min
  else if(case == 3)
    (i_N_max - i_N_min) * a^exponent + i_N_min
  else 
    stop("undefined i_N-function, currently only case 1 and 2 are defined.")
}

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

P <- function(a, case, ...){
  if(case == 1)
    ceiling(S_D/(a_min*K*n_max(a_min, ...)))
  else if(case == 2)
    S_D/(a*K*n_max(a,...))
  else if(case == 3)
    ceiling(S_D/(a*K*n_max(a, ...)))
  else 
    stop("undefined p-function, currently only case 1 and 2 are defined.")
}


############################
#### MIPS_aK und MIPS_a ####
############################

# MIPS in Abhängigkeit von der Auslastung (a = Vektor, const = Dataframe)
MIPS_aK <- function(a, case_i_N, case_p, const,...){
  attach(const)
  i_N_names <- names(formals(i_N))
  P_names <- names(formals(P))
  dots <- list(...)
  eval_i_N <- do.call('i_N', c(list(a=a, case = case_i_N), dots[names(dots) %in%
                                    i_N_names]))
  eval_P <- do.call('P', c(list(a=a, case = case_p), dots[names(dots) %in%
                                P_names]))
  MIPS <- eval_i_N/(a*K) + I_fix/S_D +eval_P*i_P/S_D
  detach(const)
  return(MIPS)
}
MIPS_a <- function(a, ...){
  MIPS_aK(a, const=Konstanten, ...)
}

###############
#### Plots ####
###############

# a <- seq(length = 100, from = 0.1, to=1) # der Definitionsbereich für MIPS
# a_i <- seq(length = 100, from = 0, to=1)
#
# # Plot mit vier Teilplots: Spalten sind unterschiedliche Fälle
# # (unterschiedliche i_N-Funktionen), erste Spalte ist die i_N-Funktion
# # dargestellt, zweite Spalte MIPS in Abhängigkeit von a
#
# if(export) tikz( '../tex/Abbildungen/Auslastung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)
#
# par(mfcol=c(2,2))
# # i_N(a), Fall 1
# plot(a_i, i_N(a_i, case=1), type="l", xlim=c(0,1), ylim=c(0,i_N(1,1)),
#      xlab = "Relative Produktauslastung $a$",
#      ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
#      cex = fntsize
#      )
# title( main = paste("$i_N(a) = (i_{N_\\text{max}} - i_{N_\\text{min}})",
#                    "* \\sqrt{a} + i_{N_\\text{min}}$"),
#       cex.main = 0.8)
#
# # MIPS(a), Fall 1
# vec_a_1 = MIPS_a(a, case_i_N = 1, case_p = 1)
# vec_a_2 = MIPS_a(a, case_i_N = 1, case_p = 2)
# vec_a_3 = MIPS_a(a, case_i_N = 1, case_p = 3)
# plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)),
#      xlab = "Relative Produktauslastung $a$",
#      ylab = "$\\text{MIPS}_a$",
#      cex = fntsize,
#      panel.first = polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_3),
#                                            rev(pmin(vec_a_1, vec_a_3))),
#                            col="lightgray", border = NA)
#      ) 
# points(a, vec_a_2, type="l", lty="dashed", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
# points(a, vec_a_3, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_3)))
#
# # i_N(a), Fall 2
# plot(a_i, i_N(a_i, case=2), type="l", xlim=c(0,1), ylim=c(0,i_N(1,2)),
#      xlab = "Relative Produktauslastung $a$",
#      ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
#      cex = fntsize) 
# title(main = paste("$(2*i_{N_\\text{max}} - i_{N_\\text{min}}",
#                   "* a^3 +i_{N_\\text{min}}$"),
#       cex.main = 0.8)
# # MIPS(a), Fall 2
# vec_a_1 = MIPS_a(a, case_i_N = 2, case_p = 1)
# vec_a_2 = MIPS_a(a, case_i_N = 2, case_p = 2)
# vec_a_3 = MIPS_a(a, case_i_N = 2, case_p = 3)
#
# plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)), 
#      xlab = "Relative Produktauslastung $a$", 
#      ylab = "$\\text{MIPS}_a$", 
#      cex = fntsize,
#      panel.first = polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_3),
#                                            rev(pmin(vec_a_1, vec_a_3))),
#                            col="lightgray", border = NA)) 
# points(a, vec_a_2, type="l", lty="dashed", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
# points(a, vec_a_3, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_3)))
#
# if(export) dev.off()
#
#### Manipulation Plot ####
plotMIPS <- function(i_N_max, i_N_min, exponent, K, I_fix, i_P, S_D,
                     n_max_max, n_max_min, n_max_exp, a_min){
a <- seq(length = 100, from = 0.1, to=1) # der Definitionsbereich für MIPS
a_i <- seq(length = 100, from = 0, to=1)

Konstanten <- data.frame(
                        K     = K, # Kapazität
                        I_fix = I_fix, # alle nicht variablen Inputs
                        i_P   = i_P, # Input für die Bereitstellung eines Produkts
                        S_D   = S_D, # Nachfrage
                        n_max_max = n_max_max,# falls N_max unabhängig von a ist
                        n_max_min = n_max_min,
                        n_max_exp = n_max_exp,
                        a_min = a_min # vorgegeben, um Konsistenz der Parameter zu
                                    # erreichen (müsste eigentlich ermittelt
                                    # werden)
                        )
par(mfcol=c(2,2))

#MIPS(a), Fall 1
vec_a_1 = MIPS_aK(a, case_p = 1, case_i_N = 3, exponent=exponent, i_N_max =
                 i_N_max, i_N_min = i_N_min, const=Konstanten,
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp)
vec_a_2 = MIPS_aK(a, case_p = 2, case_i_N = 3, exponent=exponent, i_N_max =
                 i_N_max, i_N_min = i_N_min, const=Konstanten,
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp)
vec_a_3 = MIPS_aK(a, case_p = 3, case_i_N = 3, exponent=exponent, i_N_max =
                 i_N_max, i_N_min = i_N_min, const=Konstanten,
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp)

plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize,
     panel.first = polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_3),
                                           rev(pmin(vec_a_1, vec_a_3))),
                           col="lightgray", border = NA)
     ) 
points(a, vec_a_2, type="l", lty="dashed", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
points(a, vec_a_3, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_3)))

#i_N(a), Fall 1
plot(a_i, i_N(a_i, case=3, exponent = exponent, i_N_min = i_N_min, i_N_max =
              i_N_max), 
     type="l", xlim=c(0,1), 
     # ylim=c(0,i_N(1,1,exponent=exponent)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize
     )
title( main = paste("$i_N(a) = (i_{N_\\text{max}} - i_{N_\\text{min}})",
                   "* \\sqrt{a} + i_{N_\\text{min}}$"),
      cex.main = 0.8)

attach(Konstanten)
plot(a, P(a,3), type="l", lty="dashed")
points(a, P(a,1), type="l")

plot(a, S_D/(a*K), type="l")
points(a, P(a,1)*n_max(a, n_max_max, n_max_min, n_max_exp), type="l")
points(a, P(a,3)*n_max(a, n_max_max, n_max_min, n_max_exp), type="l")
detach(Konstanten)
}

manipulate(
           plotMIPS(i_N_max, i_N_min, exponent, K, I_fix, i_P, S_D, n_max_max,
                    n_max_min, n_max_exp,
                    a_min),
           i_N_max = slider(0.5, 2, step=0.5),
           i_N_min = slider(0,2, step = 0.5),
           exponent = slider(0.5,5, step = 0.5),
           a_min = slider(0.1, 1, step = 0.05), # vorgegeben, um Konsistenz der Parameter zu
           K     = slider(1,10, step = 1), # Kapazität
           i_P   = slider(0,50), # Input für die Bereitstellung eines Produkts
           S_D   = slider(100,1000,step=50), # Nachfrage
           n_max_exp = slider(0,3, step = 0.5),
           n_max_max = slider(10, 1000, step=50),# falls N_max unabhängig von a ist
           n_max_min = slider(10,1000, step = 50),
           I_fix = slider(0, 5) # alle nicht variablen Inputs
                                    # erreichen (müsste eigentlich ermittelt
                                    # werden)
          ) 

