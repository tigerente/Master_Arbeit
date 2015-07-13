setwd("C:/Users/janos/Studium/Master_Arbeit/R")
require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
source('./Funktionen_Kopplung.R')

# Einstellungen für den Export der Graphik
tikzheight <- 1.6
tikzwidth <- 4.4
fntsize <- 0.75

export <- TRUE # if figures should be exported, or just visualized
#export <- FALSE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 1 # if set to 1, Plots are drawn, which will eventually
# be exported, if set to 2, a plot with manipulate is
# drawn in order to analyse the model

if(Plots_or_manipulate == 1){
  
  ###############
  #### Plots ####
  ###############
  
  ### Vorbereitungen ###
  
  # Parameter:
  T = 1
  n_max = 15
  t_max = 5    
  A = 1
  i_P = 100
  I_fix = 0
  S_D = 100
  R = 20
  alpha = 1
  
  h_ind_2 = 3.5
  h_ind_3 = 1.2
  h_gem = 4
  
  # Definitionsbereich von r:
  r <- seq(length = 1000, from = 0, to=R)

  # Funktionen auswerten
  
  # q(h)
  q_ind_2 = q(h_ind_2, n_max, t_max, T)
  q_ind_3 = q(h_ind_3, n_max, t_max, T)
  q_gem = q(h_gem, n_max, t_max, T)

  
  # MIPS(a):
  eval_MIPS_2 = MIPS(r,
                   R,
                   h_ind_2, h_gem, q_ind_2, q_gem,
                   T, A, i_P, alpha, I_fix, S_D)
  eval_MIPS_3 = MIPS(r,
                     R,
                     h_ind_3, h_gem, q_ind_3, q_gem,
                     T, A, i_P, alpha, I_fix, S_D)
  
  # r_opt:
  r_opt = i_P / (A * T * alpha) * (q_ind_3/h_ind_3 - q_gem/h_gem)
  r_max = 3/2 * r_opt
  MIPS_r_max = MIPS(r_max,
                    R,
                    h_ind_3, h_gem, q_ind_3, q_gem,
                    T, A, i_P, alpha, I_fix, S_D)

  ### Plots erzeugen###
  
  if(export) tikz( '../Vortrag_USF/Abbildungen/Kopplung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)
  
  par(mfcol=c(1,2), mar=c(3,1.6,2,1), mgp=c(3,0.5,0), cex=fntsize)
  
  # Fall 2
  plot(r, eval_MIPS_2, type="l", xlim=c(0,R), ylim=c(0,20),
       xlab = "",
       ylab = "",
       main = 'Fall 2',
       xaxt = 'n', yaxt = 'n'
  )
  mtext("Radius $r$", side=1, line=1.7, cex=fntsize)
  mtext("$\\text{MIPS}(r)$", side=2, line=0.7, cex=fntsize)
  
  
  # Fall 3:
  plot(r, eval_MIPS_3, type="l", xlim=c(0,R), ylim=c(0,20),
       xlab = "",
       ylab = "",
       main = 'Fall 3',
       xaxt = 'n', yaxt = 'n'
  )
  abline(v = c(r_opt, r_max), lty = 3)
  abline(h = MIPS_r_max, lty = 3)
  axis (side = 1, at = c(r_opt, r_max), labels = c('$r_\\text{opt}$', '$\\hat{r}$'))
  mtext("Radius $r$", side=1, line=1.7, cex=fntsize)
  mtext("$\\text{MIPS}(r)$", side=2, line=0.7, cex=fntsize)
  
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
    
    # feste Parameter:
    T = 1
    n_max = 15
    t_max = 5    
    A = 1
    i_P = 100
    I_fix = 0
    S_D = 100
    
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
    
    plot (r, eval_MIPS, type='l', ylim = c(5,20))
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
    R = slider (5, 30, step = 5, initial = 10),
    h_ind = slider(1, 5, step = 0.1, initial = 2),
    h_gem = slider(1, 5, step = 0.1, initial = 4),
    #n_max = slider(1, 20, step = 1, initial = 15),
    #t_max = slider (1, 20, step = 1, initial = 5),    
    #T = slider(1, 20, step = 1, initial = 1),
    #A = slider(1, 20, step = 1, initial = 1),
    #i_P   = slider(0, 200, initial = 100), # Input für die Bereitstellung eines Produkts
    alpha = slider (0.1, 4, step = 0.05, initial = 1)
    #I_fix = slider(0, 10, initial = 5), # alle nicht variablen Inputs
    #S_D   = slider(50, 200, step = 10, initial = 100) # Nachfrage
  ) 
}
