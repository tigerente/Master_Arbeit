require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
source('./Funktionen_Auslastung.R')

# Einstellungen für den Export der Graphik
tikzheight <- 4.0
tikzwidth <- 5.8
fntsize <- 0.8

#export <- FALSE # if figures should be exported, or just visualized
export <- TRUE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 1 # if set to 1, Plots are drawn, which will eventually
                         # be exported, if set to 2, a plot with manipulate is
                         # drawn in order to analyse the model

if(Plots_or_manipulate == 1){

  ###############
  #### Plots ####
  ###############
  
  i_N_max = 10 # Maximaler Wert von i_N(a)
  i_N_min = 1 # Minimaler Wert von i_N(a)
  i_N_exp_1 = 1 # Exponent (a^i_N_exp)
  i_N_exp_2 = 2 # Exponent (a^i_N_exp)
  
  # Parameter für i_N zusammenfassen: Fall 1
  i_N_Pars_1 <- data.frame(
    i_N_max = i_N_max, # Maximaler Wert von i_N(a)
    i_N_min = i_N_min, # Minimaler Wert von i_N(a)
    i_N_exp = i_N_exp_1 # Exponent (a^i_N_exp)
  )
  
  # Parameter für i_N zusammenfassen: Fall 2
  i_N_Pars_2 <- data.frame(
    i_N_max = i_N_max, # Maximaler Wert von i_N(a)
    i_N_min = i_N_min, # Minimaler Wert von i_N(a)
    i_N_exp = i_N_exp_2 # Exponent (a^i_N_exp)
  )
  
  # Parameter für n_max zusammenfassen:
  n_max_const = 5000
  n_max_Pars <- data.frame(
    n_max_max = 5000, # Maximaler Wert von n_max(a)
    n_max_min = 5000, # Minimaler Wert von n_max(a)
    n_max_exp = 1 # Exponent (a^n_max_exp)
  )
  
  # Konstanten Definition
  K     = 10 # Kapazität
  I_fix = 0 # alle nicht variablen Inputs
  i_P   = 2000 # Input für die Bereitstellung eines Produkts
  S_D   = 40000 # Nachfrage,
  T = 10
  p_is_const = TRUE # konstante parallele Produktanzahl? (Boolean)
  p_const     = 3 # parallele Produktanzahl (relevant nur bei p_is_const=TRUE)
  h_max     = 2 # Nutzungshäufigkeit (relevant nur bei p_is_const=FALSE)
  t_max = 15 # maximale Nutzungsdauer
  Konstanten <- data.frame(
                          K     = K, # Kapazität
                          I_fix = I_fix , # alle nicht variablen Inputs
                          i_P   = i_P, # Input für die Bereitstellung eines Produkts
                          S_D   = S_D, # Nachfrage,
                          T = T,
                          p_is_const = p_is_const, # konstante parallele Produktanzahl? (Boolean)
                          p_const     = p_const, # parallele Produktanzahl (relevant nur bei p_is_const=TRUE)
                          h_max     = h_max, # Nutzungshäufigkeit (relevant nur bei p_is_const=FALSE)
                          t_max = t_max # maximale Nutzungsdauer
                          )
  # Definitionsbereich:
  a_min = 0.15
  a <- seq(length = 1000, from = a_min, to=1) # der Definitionsbereich für MIPS
  
  # Plot mit vier Teilplots: Spalten sind unterschiedliche Fälle
  # (unterschiedliche i_N-Funktionen), erste Spalte ist die i_N-Funktion
  # dargestellt, zweite Spalte MIPS in Abhängigkeit von a
  
  if(export) tikz( '../tex/Abbildungen/Auslastung_2.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)
  
  #Funktionen auswerten:
  # MIPS(a): Fall 1
  eval_MIPS_1 = MIPS(a,
                   i_N_Pars = i_N_Pars_1,
                   n_max_Pars = n_max_Pars,
                   Konstanten = Konstanten
  )
  
  # MIPS(a): Fall 2
  eval_MIPS_2 = MIPS(a,
                     i_N_Pars = i_N_Pars_2,
                     n_max_Pars = n_max_Pars,
                     Konstanten = Konstanten
  )
  
  # i_N(a): Fall 1
  eval_i_N_1 = i_N(a, i_N_max, i_N_min, i_N_exp_1)
  eval_i_N_2 = i_N(a, i_N_max, i_N_min, i_N_exp_2)
  
  # Plots erstellen:
  par(mfcol=c(2,2), mar = c(4.1,4.1,4.5,2.1))
  
  plot(a, eval_i_N_1, type='l', xlab = 'relative Produktauslastung $a$', ylab = '$i_N$ [kg/NE]')
  title('$i_N(a)$', line=1)
  mtext ('(a) $i_N(a)$ unelastisch', side = 3, line = 3)
  
  plot(a, eval_MIPS_1, type='l', ylim=c(0,2), xaxt="n", xlab = 'relative Produktauslastung $a$', ylab = 'MIPS [kg/SE]')
  abline(v = S_D * t_max / (K * p_const * T * n_max_const), lty = 3)
  axis (side = 1, at = c(0.2, 0.6, 0.8, 1.0, S_D * t_max / (K * p_const * T * n_max_const)), labels = c(0.2, 0.6, 0.8, 1.0, '$a^*$'))
  title('MIPS($a$)', line=1)
  
  plot(a, eval_i_N_2, type='l', xlab = 'relative Produktauslastung $a$', ylab = '$i_N$ [kg/NE]')
  title('$i_N(a)$', line=1)
  mtext ('(b) $i_N(a)$ elastisch', side = 3, line = 3)
  
  plot(a, eval_MIPS_2, type='l', ylim=c(0,2), xaxt="n", xlab = 'relative Produktauslastung $a$', ylab = 'MIPS [kg/SE]')
  abline(v = S_D * t_max / (K * p_const * T * n_max_const), lty = 3)
  axis (side = 1, at = c(0.2, 0.6, 0.8, 1.0, S_D * t_max / (K * p_const * T * n_max_const)), labels = c(0.2, 0.6, 0.8, 1.0, '$a^*$'))
  title('MIPS($a$)', line=1)
  
  if(export) dev.off()
}

if(Plots_or_manipulate == 2){

  ###########################
  #### Manipulation Plot ####
  ###########################
    # first, we define a function, which draws the desired plots. In the second
    # step we call that function in a manipulate routine
  plotMIPS <- function(a_min, # Kleinster Wert von a für Plots
                       K, I_fix, i_P, S_D, p_is_const, p_const, h_max, T, t_max, # Konstanten
                       n_max_max, n_max_min, n_max_exp, # n_max Parameter
                       i_N_max, i_N_min, i_N_exp # i_N Parameter
                      )
  {
    
    ### Vorbereitungen ###
    
    # Definitionsbereich von a:
    a <- seq(length = 1000, from = a_min, to=1)

    # Parameter für i_N zusammenfassen:
    i_N_Pars <- data.frame(
                          i_N_max = i_N_max, # Maximaler Wert von i_N(a)
                          i_N_min = i_N_min, # Minimaler Wert von i_N(a)
                          i_N_exp = i_N_exp # Exponent (a^i_N_exp)
                          )

    # Parameter für n_max zusammenfassen:
    n_max_Pars <- data.frame(
                      n_max_max = n_max_max, # Maximaler Wert von n_max(a)
                      n_max_min = n_max_min, # Minimaler Wert von n_max(a)
                      n_max_exp = n_max_exp # Exponent (a^n_max_exp)
                      )
    
    # Alle Konstanten zusammenfassen:
    Konstanten <- data.frame(
                            K     = K, # Kapazität
                            I_fix = I_fix, # alle nicht variablen Inputs
                            i_P   = i_P, # Input für die Bereitstellung eines Produkts
                            S_D   = S_D, # Nachfrage
                            p_is_const = p_is_const, # konstante parallele Produktanzahl? (Boolean)
                            p_const     = p_const, # parallele Produktanzahl (relevant nur bei p_is_const=TRUE)
                            h_max     = h_max, # Nutzungshäufigkeit (relevant nur bei p_is_const=FALSE)
                            T     = T, # Betrachtungszeitraum
                            t_max = t_max # maximale Nutzungsdauer
                          )
    
    ### Funktionen auswerten ###
    
    # p(a):
    eval_p = p(a, Konstanten)
    
    # MIPS(a):
    eval_MIPS = MIPS(a,
                     i_N_Pars = i_N_Pars,
                     n_max_Pars = n_max_Pars,
                     Konstanten = Konstanten
                     )
    
    # i_N(a):
    eval_i_N = i_N(a, i_N_max, i_N_min, i_N_exp)
    
    # n_max(a):
    eval_n_max = n_max(a, n_max_max, n_max_min, n_max_exp)
    
    # n(a):
    eval_n = n(a, n_max_Pars, Konstanten)
    
    # h(a)
    eval_h = h(a, Konstanten)
    
    # q(a)
    eval_q = q(a, n_max_Pars, Konstanten)
    
    # P(a)
    eval_P = P(a, n_max_Pars, Konstanten)
    
    # t(a)
    #eval_t = t(a, n_max_Pars, Konstanten)
    
    # t_tech(a)
    #eval_t_tech = t_tech(a, n_max_Pars, Konstanten)
    
    ### Plots erstellen ###
    
    par(mfrow=c(2,3))
    
    plot (a, eval_p, type='l')
    
    y_min = min(eval_h*t_max, eval_n_max)
    y_max = max(eval_h*t_max, eval_n_max)
    plot(a, eval_h * t_max, type='l', lty='dotted', col='grey', ylim=c(y_min, y_max))
    lines(a, eval_n_max, col='grey')
    lines(a, eval_n)
    legend(x='topright', legend=c("n_max", "h*t_max", "n"), col=c('grey', 'grey', 'black'), lty=c('solid', 'dotted', 'solid'))
    
    plot(a, eval_h, type='l')
    
    plot(a, eval_q, type='l')
    
    #plot(a, eval_t_tech, type='l', col='grey')
    #lines(a, rep(t_max, times=length(a)), lty='dotted', col='grey')
    #lines(a, eval_t)
    #legend(x='topright', legend=c("t_tech", "t_max", "t"), col=c('grey', 'grey', 'black'), lty=c('solid', 'dotted', 'solid'))
    
    plot(a, eval_P, type='l')
    
    #plot(a, eval_i_N, type='l')
    plot(a, eval_MIPS, type='l')
  }
    
    ### Parameter als Schieberegler ###
    manipulate(
             plotMIPS(a_min=0.1,
                      K=10, I_fix=10000, i_P=2000, S_D=40000, 
                      p_is_const = p_is_const, p_const = p_const, h_max=h_max, T=10, t_max=t_max,
                      n_max_max=n_max_max, n_max_min=n_max_min, n_max_exp=n_max_exp,
                      i_N_max=i_N_max, i_N_min=i_N_min, i_N_exp=i_N_exp
                      ),
             p_is_const = checkbox (initial = TRUE, label = "p(a)=p (konstant)"),
             p_const = slider(1, 15, step=1, initial=2, label = "p (wirksam nur bei 'p(a)=p'=TRUE)"),
             h_max = slider(1, 5, step=0.5, initial=2.5, label ="h_max (wirksam nur bei 'p(a)=p'=FALSE)"),
             #T = slider(1, 20, step=1, initial=10),
             t_max = slider (1, 20, step=1, initial=15),
             i_N_min = slider(1,15, step = 1, initial=1),
             i_N_max = slider(1, 15, step=1, initial=1),
             i_N_exp = slider(0.1, 3, step = 0.1, initial=0.8),
             #a_min = slider(0.1, 1, step = 0.05), 
             #K     = slider(1, 10, step = 1, initial=5), # Kapazität
             #i_P   = slider(0, 50, initial = 10), # Input für die Bereitstellung eines Produkts
             #S_D   = slider(50,200,step=10, initial=100), # Nachfrage
             n_max_min = slider(0, 10000, step = 100, initial = 5000),
             n_max_max = slider(0, 10000, step=100, initial = 5000),
             n_max_exp = slider(0.1, 3, step = 0.1, initial=1)
             #I_fix = slider(0, 10, initial=5) # alle nicht variablen Inputs
             ) 
}
