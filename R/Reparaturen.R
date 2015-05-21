require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
require(plot3D)
source('./Funktionen_Reparatur.R')


export <- FALSE # if figures should be exported, or just visualized
# export <- TRUE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 2 # if set to 1, Plots are drawn, which will eventually
                         # be exported, if set to 2, a plot with manipulate is
                         # drawn in order to analyse the model

if(Plots_or_manipulate == 1){
  const <- data.frame(
    A     = 1,
    I_fix = 1,                    
    h     = 1,
    S_D   = 100
  )

###############
#### Plots ####
###############
param_1 <- data.frame(
  i_R = 3,
  i_P = 5,
  t_tech= 2,
  t_to = 10
    )
param_2 <- data.frame(
  i_R = 7,
  i_P = 5,
  t_tech= 2,
  t_to = 10
    )
  
# Einstellungen für den Export der Graphik
tikzheight <- 3.2
tikzwidth <- 4.5
fntsize <- 0.8
if(export) tikz( '../tex/Abbildungen/Reparaturen_I_R.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

  attach(param_1)
  t <- seq(length = 1000, from = 0.1, to=t_to)
  vec_1 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 1) 
  vec_2 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 2) 
  vec_3 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 3) 

  plot(t, vec_1, type="l", xlab="$t_\\text{max}$", ylab="$I_R$")
  points(t, vec_2, type="l", lty="dashed")
  points(t, vec_3, type="l", lty="dashed")

if(export) dev.off()

# Einstellungen für den Export der Graphik
tikzheight <- 7.5
tikzwidth <- 5.5
fntsize <- 0.8
if(export) tikz( '../tex/Abbildungen/Reparaturen_MIPS.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

  orig_par <- par(mfcol =c(2,1), mgp=c(2,1,0))
  on.exit(par(orig_par), add=TRUE)

  MIPS_1 <- MIPS_R(t_max = t, I_R = vec_1, const = const)
  MIPS_2 <- MIPS_R(t_max = t, I_R = vec_2, const = const)
  MIPS_3 <- MIPS_R(t_max = t, I_R = vec_3, const = const)
  attach(const)
  limes <- I_fix/S_D + i_R/(h*t_tech*A)
  detach(const)

  plot(t, MIPS_1, type="l", ylim=c(0,limes*4), ylab="MIPS",
       xlab="$t_\\text{max}$", main="$\\tilde{i}_P > i_R$")
  points(t, MIPS_2, type="l", lty="dashed")
  points(t, MIPS_3, type="l", lty="dashed")
  abline(h=limes, col="grey")
  legend(x="topright", legend=c("MIPS", 
                           "", 
                           "$\\text{MIPS}^\\text{max}\\text{ und }\\text{MIPS}^\\text{min}$",
                           "",
                           "$\\lim\\limits_{t_\\text{max}\\rightarrow\\infty} \\text{MIPS}$"),
col=c("black","white", "black","white", "grey"),
         lty=c(1,0, 2,0, 1), bty="n", inset=0.05, cex=0.6)

  detach(param_1)

  attach(param_2)
  vec_1 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 1) 
  vec_2 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 2) 
  vec_3 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 3) 

  MIPS_1 <- MIPS_R(t_max = t, I_R = vec_1, const = const)
  MIPS_2 <- MIPS_R(t_max = t, I_R = vec_2, const = const)
  MIPS_3 <- MIPS_R(t_max = t, I_R = vec_3, const = const)
  attach(const)
  limes <- I_fix/S_D + i_R/(h*t_tech*A)
  detach(const)

  plot(t, MIPS_1, type="l", ylim=c(0,limes*4), ylab="MIPS",
       xlab="$t_\\text{max}$", main="$\\tilde{i}_P < i_R$",
       panel.first=abline(h=limes, col="grey"))
  points(t, MIPS_2, type="l", lty="dashed")
  points(t, MIPS_3, type="l", lty="dashed")
  abline(h=limes, col="grey")
  legend(x="topright", legend=c("MIPS", 
                           "", 
                           "$\\text{MIPS}^\\text{max}\\text{ und }\\text{MIPS}^\\text{min}$",
                           "",
                           "$\\lim\\limits_{t_\\text{max}\\rightarrow\\infty} \\text{MIPS}$"),
col=c("black","white", "black","white", "grey"),
         lty=c(1,0, 2,0, 1), bty="n", inset=0.05, cex=0.6)

  detach(param_2)

if(export) dev.off()
}

if(Plots_or_manipulate == 2){

###########################
#### Manipulation Plot ####
###########################

  # first, we define a function, which draws the desired plots. In the second
  # step we call that function in a manipulate routine
plot_I_R <- function(i_R, t_tech, t_to, i_P){
  const <- data.frame(
                      A     = 1,
                      I_fix = 1,
                      i_P   = i_P,                     
                      h     = 1,
                      S_D   = 100
                      )

  t <- seq(length = 1000, from = 0.001, to=t_to)

  vec_1 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 1) 
  vec_2 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 2) 
  vec_3 <- I_R(t=t, i_R=i_R, t_tech=t_tech, case = 3) 
  fm <- lm(vec_1 ~ t)
  
  MIPS_1 <- MIPS_R(t_max = t, I_R = vec_1, const = const)
  MIPS_2 <- MIPS_R(t_max = t, I_R = vec_2, const = const)
  MIPS_3 <- MIPS_R(t_max = t, I_R = vec_3, const = const)
  
  orig_par <- par(mfcol=c(2,2))

  plot(t, vec_1, type="l")
  points(t, vec_2, type="l", lty="dashed")
  points(t, vec_3, type="l", lty="dashed")
  abline(fm, col="grey")
  attach(const)
  limes <- I_fix/S_D + i_R/(h*t_tech*A)
  detach(const)

  plot(t, MIPS_1, type="l", ylim=c(0,limes*5))
  points(t, MIPS_2, type="l", lty="dashed")
  points(t, MIPS_3, type="l", lty="dashed")
  abline(h=limes)


  vec_i_P <- MIPS_i_P(t_max = t, const = const)
  vec_I_R <- MIPS_I_R(t_max = t, I_R = vec_1, const = const)
  plot(t, vec_i_P, type="l", ylim = c(0,max(vec_i_P[-1])/10))

  points(t, vec_I_R, type="l")




  par(orig_par)
}

i=0
manipulate(
  {
    if(saveValues){
      i <- i+1
      manipulatorSetState(paste("param",i, sep="_"), 
                          data.frame(i_R=i_R, i_P=i_P, t_tech=t_tech, t_to =
                                     t_to))
    }
           plot_I_R(i_R, t_tech, t_to, i_P)
  },
  i_R = slider(0.25,10, step=0.25),
  i_P = slider(1,10, step=1),
  t_tech = slider(1,21, step=2),
  t_to = slider(10,100, step=10),
  saveValues = button("Save parameter values")
  ) 
}
