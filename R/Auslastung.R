require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
source('./Funktionen.R')

# Einstellungen für den Export der Graphik
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8

# export <- FALSE # if figures should be exported, or just visualized
export <- TRUE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 1 # if set to 1, Plots are drawn, which will eventually
                         # be exported, if set to 2, a plot with manipulate is
                         # drawn in order to analyse the model

if(Plots_or_manipulate == 1){

###############
#### Plots ####
###############

# Konstanten Definition
Konstanten <- data.frame(
                        K     = 1, # Kapazität
                        I_fix = 1, # alle nicht variablen Inputs
                        i_P   = 50, # Input für die Bereitstellung eines Produkts
                        S_D   = 100 # Nachfrage
                        )
a_min = 0.4

#Spezifikation für den Verlauf von n_max:
n_max_max = 10
n_max_min = 10
n_max_exp = 0

#Spezifikation für den Verlauf von i_N im ersten Fall
i_N_max_1 = 2
i_N_min_1 = 1
i_N_exp_1 = 0.5

#Spezifikation für den Verlauf von i_N im zweiten Fall
i_N_max_2 = 4 
i_N_min_2 = 1
i_N_exp_2 = 3

a <- seq(length = 100, from = 0.1, to=1) # der Definitionsbereich für MIPS
a_i <- seq(length = 100, from = 0, to=1)

# Plot mit vier Teilplots: Spalten sind unterschiedliche Fälle
# (unterschiedliche i_N-Funktionen), erste Spalte ist die i_N-Funktion
# dargestellt, zweite Spalte MIPS in Abhängigkeit von a

if(export) tikz( '../tex/Abbildungen/Auslastung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

par(mfcol=c(2,2))
# i_N(a), Fall 1
eval_i_N <- i_N(a_i, i_N_max = i_N_max_1, i_N_min = i_N_min_1, i_N_exp =
                i_N_exp_1)
plot(a_i, eval_i_N, type="l", xlim=c(0,1), ylim=c(0,max(eval_i_N)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize
     )
title( main = paste("$i_N(a) = ", i_N_max_1 - i_N_min_1,
                   "* a^{", i_N_exp_1, "}+", i_N_min_1,"$", sep=""),
      cex.main = 0.8)

# MIPS(a), Fall 1
#MIPS(a), P=Konstant, i_N=allgemein
vec_a_1 = MIPS_a(a, case = 1,
                 i_N_exp=i_N_exp_1, i_N_max = i_N_max_1, i_N_min = i_N_min_1, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, diskret, i_N=allgemein
vec_a_2 = MIPS_a(a, case = 2,
                 i_N_exp=i_N_exp_1, i_N_max = i_N_max_1, i_N_min = i_N_min_1, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, kontinuierlich, i_N=allgemein
vec_a_3 = MIPS_a(a, case = 3,
                 i_N_exp=i_N_exp_1, i_N_max = i_N_max_1, i_N_min = i_N_min_1, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)

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

# i_N(a), Fall 2
eval_i_N <- i_N(a_i, i_N_max = i_N_max_2, i_N_min = i_N_min_2, i_N_exp =
                i_N_exp_2)
plot(a_i, eval_i_N, type="l", xlim=c(0,1), ylim=c(0,max(eval_i_N)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize
     )
title( main = paste("$i_N(a) = ", i_N_max_2 - i_N_min_2,
                   "* a^{", i_N_exp_2, "}+", i_N_min_2,"$", sep=""),
      cex.main = 0.8)

# MIPS(a), Fall 2
vec_a_1 = MIPS_a(a, case = 1,
                 i_N_exp=i_N_exp_2, i_N_max = i_N_max_2, i_N_min = i_N_min_2, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, diskret, i_N=allgemein
vec_a_2 = MIPS_a(a, case = 2,
                 i_N_exp=i_N_exp_2, i_N_max = i_N_max_2, i_N_min = i_N_min_2, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, kontinuierlich, i_N=allgemein
vec_a_3 = MIPS_a(a, case = 3,
                 i_N_exp=i_N_exp_2, i_N_max = i_N_max_2, i_N_min = i_N_min_2, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)

plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)), 
     xlab = "Relative Produktauslastung $a$", 
     ylab = "$\\text{MIPS}_a$", 
     cex = fntsize,
     panel.first = polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_3),
                                           rev(pmin(vec_a_1, vec_a_3))),
                           col="lightgray", border = NA)) 
points(a, vec_a_2, type="l", lty="dashed", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
points(a, vec_a_3, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_3)))

if(export) dev.off()

}

if(Plots_or_manipulate == 2){

###########################
#### Manipulation Plot ####
###########################
  # first, we define a function, which draws the desired plots. In the second
  # step we call that function in a manipulate routine
plotMIPS <- function(i_N_max, i_N_min, i_N_exp, # i_N Parameter
                  
                     n_max_max, n_max_min, n_max_exp, # n_max Parameter
                     K, I_fix, i_P, S_D, a_min # Konstanten
                     ){
a <- seq(length = 100, from = 0.1, to=1) # der Definitionsbereich fürs MIPS
a_i <- seq(length = 100, from = 0, to=1) # der Definitionsbereich für die
                                         # anderen Funktionen
Konstanten <- data.frame(
                  K     = K, # Kapazität
                  I_fix = I_fix, # alle nicht variablen Inputs
                  i_P   = i_P, # Input für die Bereitstellung eines Produkts
                  S_D   = S_D # Nachfrage
                  )

#MIPS(a), P=Konstant, i_N=allgemein
vec_a_1 = MIPS_a(a, case = 1, 
                 i_N_exp=i_N_exp, i_N_max = i_N_max, i_N_min = i_N_min, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, diskret, i_N=allgemein
vec_a_2 = MIPS_a(a, case = 2,
                 i_N_exp=i_N_exp, i_N_max = i_N_max, i_N_min = i_N_min, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)
#MIPS(a), P=Optimal, kontinuierlich, i_N=allgemein
vec_a_3 = MIPS_a(a, case = 3,
                 i_N_exp=i_N_exp, i_N_max = i_N_max, i_N_min = i_N_min, 
                 n_max_max = n_max_max,
                 n_max_min = n_max_min,
                 n_max_exp = n_max_exp,
                 a_min = a_min,
                 const=Konstanten)

# now the Plotting begins:
par(mfcol=c(2,2))

# first Plot is MIPS over a, with different P-functions
plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize,
     panel.first = polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_3),
                                           rev(pmin(vec_a_1, vec_a_3))),
                           col="lightgray", border = NA)
     ) 
points(a, vec_a_2, type="l", lty="dashed")
points(a, vec_a_3, type="l")

# second Plot is i_N(a) 
plot(a_i, i_N(a_i, i_N_exp = i_N_exp, i_N_min = i_N_min, i_N_max =
              i_N_max), 
     type="l", xlim=c(0,1), 
     # ylim=c(0,i_N(1,1,i_N_exp=i_N_exp)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize
     )

# third Plot is P(a)
plot(a, P(a, case=3, a_min=a_min, const=Konstanten, n_max_max=n_max_max,
          n_max_min=n_max_min, n_max_exp=n_max_exp), 
     type="l", lty="dashed")
points(a, P(a, case=1, a_min = a_min, const=Konstanten, n_max_max=n_max_max,
            n_max_min=n_max_min,
            n_max_exp=n_max_exp), 
       type="l")

# fourth Plot is the Plot for the auxiliary condition   
attach(Konstanten)
plot(a, S_D/(a*K), type="l")
detach(Konstanten)
points(a, P(a,case=1, a_min=a_min, const=Konstanten, n_max_max=n_max_max,
            n_max_min=n_max_min,
            n_max_exp=n_max_exp)*n_max(a, n_max_max, n_max_min, n_max_exp), type="l")
points(a, P(a,case=3, a_min=a_min, const=Konstanten, n_max_max=n_max_max,
            n_max_min=n_max_min,
            n_max_exp=n_max_exp)*n_max(a, n_max_max, n_max_min, n_max_exp), type="l")
}

manipulate(
           plotMIPS(i_N_max=i_N_max, i_N_min=i_N_min, i_N_exp=i_N_exp, 
                    n_max_max=n_max_max, n_max_min=n_max_min, 
                    n_max_exp=n_max_exp,
                    K=K, I_fix=I_fix, i_P=i_P, S_D=S_D, 
                    a_min=a_min),
           i_N_max = slider(0.5, 2, step=0.5),
           i_N_min = slider(0,2, step = 0.5),
           i_N_exp = slider(0.5,5, step = 0.5),
           a_min = slider(0.1, 1, step = 0.05), 
           K     = slider(1,10, step = 1), # Kapazität
           i_P   = slider(0,50), # Input für die Bereitstellung eines Produkts
           S_D   = slider(100,1000,step=50), # Nachfrage
           n_max_exp = slider(0,3, step = 0.5),
           n_max_max = slider(10, 1000, step=50),# falls N_max unabhängig von a ist
           n_max_min = slider(10,1000, step = 50),
           I_fix = slider(0, 5) # alle nicht variablen Inputs
          ) 
}
