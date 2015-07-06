setwd("C:/Users/janos/Studium/Master_Arbeit/R")
require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
source('./Funktionen_Kopplung.R')

# Einstellungen für den Export der Graphik
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8

export <- FALSE # if figures should be exported, or just visualized
#export <- TRUE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 2 # if set to 1, Plots are drawn, which will eventually
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
  plotMIPS <- function(R, h_ind, h_gem, n_max, t_max, T, A, i_P, alpha, I_fix, S_D)
  {
    
    ### Vorbereitungen ###
    
    # Definitionsbereich von r:
    r <- seq(length = 1000, from = 0, to=R)
    
    ### Funktionen auswerten ###
    
    # q(h)
    q_ind = q(h_ind, n_max, t_max, T)
    q_gem = q(h_gem, n_max, t_max, T)
    
    # MIPS(a):
    eval_MIPS = MIPS(r,
                     R,
                     h_ind, h_gem, q_ind, q_gem,
                     T, A, i_P, alpha, I_fix, S_D)
    
    # r_opt:
    r_opt = i_P / (A * T * alpha) * (q_ind/h_ind - q_gem/h_gem)
    
    ### Plots erstellen ###
    
    par(mfrow=c(1,1))
    
    plot (r, eval_MIPS, type='l')
    print(paste("h*: ", n_max/t_max))
    print(paste("r_opt: ", r_opt))
    
  }
  
  ### Parameter als Schieberegler ###
  manipulate(
    plotMIPS(R,
             h_ind, h_gem,
             n_max, t_max,
             T, A, i_P, alpha, I_fix, S_D
            ),
    R = slider (1, 20, step = 1, initial = 10),
    h_ind = slider(1, 5, step = 0.5, initial = 2),
    h_gem = slider(1, 5, step = 0.5, initial = 4),
    n_max = slider(1, 20, step = 1, initial = 15),
    t_max = slider (1, 20, step = 1, initial = 5),    
    T = slider(1, 20, step = 1, initial = 1),
    A = slider(1, 20, step = 1, initial = 1),
    i_P   = slider(0, 200, initial = 100), # Input für die Bereitstellung eines Produkts
    alpha = slider (1, 100, initial = 1),
    I_fix = slider(0, 10, initial = 5), # alle nicht variablen Inputs
    S_D   = slider(50, 200, step = 10, initial = 100) # Nachfrage
  ) 
}
