require(tikzDevice) # für den Export von Graphiken in tex-Files
require(manipulate) 
source('./Funktionen_Wartung.R')


# export <- FALSE # if figures should be exported, or just visualized
export <- TRUE # if figures should be exported, or just visualized 
Plots_or_manipulate <- 1 # if set to 1, Plots are drawn, which will eventually
                         # be exported, if set to 2, a plot with manipulate is
                         # drawn in order to analyse the model

if(Plots_or_manipulate == 1){

###############
#### Plots ####
###############
  
# Einstellungen für den Export der Graphik
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8
if(export) tikz( '../tex/Abbildungen/Wartungen.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

  # orig_par <- par(mfcol =c(2,2), mgp=c(2,1,0))
  # on.exit(par(orig_par), add=TRUE)
layout(matrix(c(1,1,2,2,0,3,3,0), ncol=4,nrow=2, byrow=TRUE), width=c(1,3,3,1),
       height=c(3,4))

  const <- data.frame(
                      A     = 1,
                      I_fix = 50,
                      i_P   = 8,                     
                      h     = 1,
                      S_D   = 100,
                      i_W   = 0.6, 
                      t_max = 34,
                      i_N_max = 1,
                      i_N_min = 0.4,
                      i_N_exp = 1,
                      n_max_max = 20,
                      n_max_min = 10,
                      n_max_exp = 1
                      )

  w <- seq(length = 100, from = 0.0, to=1)

  

  vec_t <- with(const, t(w = w, h = h, t_max = t_max,
              n_max_max = n_max_max, 
              n_max_min = n_max_min,
              n_max_exp = n_max_exp))
  vec_i_N <- with(const, i_N(w=w, 
                 i_N_max = i_N_max,
                 i_N_min = i_N_min,
                 i_N_exp = i_N_exp))
  vec_n_max <- with(const, (n_max(w=w, 
                     n_max_min = n_max_min, 
                     n_max_max = n_max_max,
                     n_max_exp = n_max_exp)))
  
  vec_MIPS_total <- MIPS_W(w=w, const=const, part=0)
  vec_MIPS_part4 <- MIPS_W(w=w, const=const, part=4)
  vec_MIPS_part1 <- MIPS_W(w=w, const=const, part=1) + vec_MIPS_part4 
  vec_MIPS_part2 <- MIPS_W(w=w, const=const, part=2) + vec_MIPS_part1 
  vec_MIPS_part3 <- MIPS_W(w=w, const=const, part=3) + vec_MIPS_part2 
  
  # Plot von n_max
  plot(w, vec_n_max, type="l", main="Nutzungsvorrat $n_\\text{max}(w)$", ylab="$n_\\text{max}(w)$ in [NE]", xlab="Wartungsh\"aufigkeit $w$")

  # Plot von i_N
  plot(w, vec_i_N, type="l", main="Material-Inputs je Nutzung $i_N(w)$", ylab="$i_N(w)$ in [kg/NE]", xlab="Wartungsh\"aufigkeit $w$")

  # Plot von t
  # plot(w, vec_t, type="l", main="$t(w)$", ylab="")

  # Plot von MIPS
  plot(w, vec_MIPS_total, type="l", main="$\\text{MIPS}$ in Abh\"angigkeit von der Wartungsh\"aufigkeit $w$",
       ylim=c(0,1.5*max(vec_MIPS_total)), ylab="$\\text{MIPS}$ in [kg/SE]", xlab="Wartungsh\"aufigkeit $w$", axes=FALSE)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, 0),
                          rev(pmin(vec_MIPS_part4, 0))),
          col=grey(0.95), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, vec_MIPS_part1),
                          rev(pmin(vec_MIPS_part4, vec_MIPS_part1))),
          col=grey(0.85), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part1, vec_MIPS_part2),
                          rev(pmin(vec_MIPS_part1, vec_MIPS_part2))),
          col=grey(0.75), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part2, vec_MIPS_part3),
                          rev(pmin(vec_MIPS_part2, vec_MIPS_part3))),
          col=grey(0.65), border = NA)
  points(w, vec_MIPS_part4, type="l", col="grey60")
  points(w, vec_MIPS_part1, type="l", col="grey60")
  points(w, vec_MIPS_part2, type="l", col="grey60")
  points(w, vec_MIPS_part3, type="l")
  pos_min <- which.min(vec_MIPS_total)
  

  legend(x="topright", legend=c("produktbezogene Inputs",  
                                "wartungsbedingte Inputs",
                                "nutzungsbedingte Inputs", 
                                "fixer Anteil",
                                "MIPS$(w)$"),
         # fill=c(grey(0.80),grey(0.85),grey(0.90),grey(0.95), NA),
         col="black",
         bty="n", inset=0.05, cex=1, lty=c(0,0,0,0,1), lwd=c(0,0,0,0,1),
         pch=c(22,22,22,22,NA),
         pt.bg=c(grey(0.65),grey(0.75),grey(0.85),grey(0.95), "black"),
         pt.cex=2)
 # Vergleich herkömmliche und gemeinschaftliche Nutzung:
 w_her <- w[11]
 pos_max <- which(vec_MIPS_total[] > vec_MIPS_total[11])[11]  
 w_max <- w[pos_max]
 points(c(w_her,w_max), rep(vec_MIPS_total[11], times=2), type="l", lty="dotted")
points(rep(w[pos_min],times=2), c(-0.13, vec_MIPS_total[pos_min]), type="l", lty="dashed")
points(rep(w_her,times=2), c(-0.13, vec_MIPS_total[11]), type="l", lty="dotted")
points(rep(w_max,times=2), c(-0.13, vec_MIPS_total[pos_max]), type="l", lty="dotted")
axis(side=1, at = c(0,w_her, w[pos_min], 0.5, w_max,1), labels = c(0.0, "$w^\\text{herk}$", "$w^*$", 0.5, "$w^\\text{max}$",1.0)) 
axis(side=2)
box()
  #par(orig_par)

if(export) dev.off()

}

if(Plots_or_manipulate == 2){

###########################
#### Manipulation Plot ####
###########################

  # first, we define a function, which draws the desired plots. In the second
  # step we call that function in a manipulate routine
plot_W <- function(i_P, i_W, t_max, i_N_max, i_N_min, i_N_exp,
                   n_max_max, n_max_min, n_max_exp){
  const <- data.frame(
                      A     = 1,
                      I_fix = 50,
                      i_P   = i_P,                     
                      h     = 1,
                      S_D   = 100,
                      i_W   = i_W, 
                      t_max = t_max,
                      i_N_max = i_N_max,
                      i_N_min = i_N_min,
                      i_N_exp = i_N_exp,
                      n_max_max = n_max_max,
                      n_max_min = n_max_min,
                      n_max_exp = n_max_exp
                      )

  w <- seq(length = 100, from = 0.0, to=1)

  vec_t <- with(const, t(w = w, h = h, t_max = t_max,
              n_max_max = n_max_max, 
              n_max_min = n_max_min,
              n_max_exp = n_max_exp))
  vec_i_N <- with(const, i_N(w=w, 
                 i_N_max = i_N_max,
                 i_N_min = i_N_min,
                 i_N_exp = i_N_exp)  )
  vec_n_max <- with(const, n_max(w=w, 
                     n_max_min = n_max_min, 
                     n_max_max = n_max_max,
                     n_max_exp = n_max_exp))
  
  vec_MIPS_total <- MIPS_W(w=w, const=const, part=0)
  vec_MIPS_part4 <- MIPS_W(w=w, const=const, part=4)
  vec_MIPS_part1 <- MIPS_W(w=w, const=const, part=1) + vec_MIPS_part4 
  vec_MIPS_part2 <- MIPS_W(w=w, const=const, part=2) + vec_MIPS_part1 
  vec_MIPS_part3 <- MIPS_W(w=w, const=const, part=3) + vec_MIPS_part2 
  
  orig_par <- par(mfcol=c(2,2))
  on.exit(par(orig_par), add=TRUE)

  # Plot von n_max
  plot(w, vec_n_max, type="l", main="n_max(w)")

  # Plot von i_N
  plot(w, vec_i_N, type="l", main="i_N(w)")

  # Plot von t
  plot(w, vec_t, type="l", main="t(w)")

  # Plot von MIPS
  plot(w, vec_MIPS_total, type="l", main="MIPS(w)",
       ylim=c(0,1.5*max(vec_MIPS_total)))
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, 0),
                          rev(pmin(vec_MIPS_part4, 0))),
          col=grey(0.95), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, vec_MIPS_part1),
                          rev(pmin(vec_MIPS_part4, vec_MIPS_part1))),
          col=grey(0.9), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part1, vec_MIPS_part2),
                          rev(pmin(vec_MIPS_part1, vec_MIPS_part2))),
          col=grey(0.85), border = NA)
  polygon(c(w, rev(w)), c(pmax(vec_MIPS_part2, vec_MIPS_part3),
                          rev(pmin(vec_MIPS_part2, vec_MIPS_part3))),
          col=grey(0.8), border = NA)
  # polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, 0),
  #                         rev(pmin(vec_MIPS_part4, 0))),
  #         density=10, angle=30, border = NA)
  # polygon(c(w, rev(w)), c(pmax(vec_MIPS_part4, vec_MIPS_part1),
  #                         rev(pmin(vec_MIPS_part4, vec_MIPS_part1))),
  #         density=15, angle=-60, border = NA)
  # polygon(c(w, rev(w)), c(pmax(vec_MIPS_part1, vec_MIPS_part2),
  #                         rev(pmin(vec_MIPS_part1, vec_MIPS_part2))),
  #         density=10, angle=60, border = NA)
  # polygon(c(w, rev(w)), c(pmax(vec_MIPS_part2, vec_MIPS_part3),
  #                         rev(pmin(vec_MIPS_part2, vec_MIPS_part3))),
  #         density=15, angle=-30, border = NA)
  points(w, vec_MIPS_part4, type="l", lty="dashed")
  points(w, vec_MIPS_part1, type="l", lty="dashed")
  points(w, vec_MIPS_part2, type="l", lty="dashed")
  points(w, vec_MIPS_part3, type="l")
  legend(x="topright", legend=c("produktbezogene Inputs",  
                                "wartungsbedingte Inputs",
                                "nutzungsbedingte Inputs", 
                                "fixer Anteil"),
         # angle=c(30,-60,60,-30),
         # density=c(25,40,25,40),
         fill=c(grey(0.80),grey(0.85),grey(0.90),grey(0.95)),
         bty="n", inset=0.05, cex=0.8)


  par(orig_par)
}

manipulate(
  {
      plot_W(i_P = i_P, i_W = i_W, t_max = t_max, 
             i_N_max = i_N_max, 
             i_N_min = i_N_min, 
             i_N_exp = i_N_exp,
             n_max_max = n_max_max,
             n_max_min = n_max_min,
             n_max_exp = n_max_exp)  
  },
  i_P   = slider(0,20,step=1),                     
  i_W   = slider(0,2,step=0.02), 
  t_max = slider(1,100,step=1),
  i_N_max = slider(0.1,10,step=0.1),
  i_N_min = slider(0,10,step=0.1),
  i_N_exp = slider(0,2, step=0.1),
  n_max_max = slider(10,100, step=10),
  n_max_min = slider(10,100, step=10),
  n_max_exp =slider(0,2, step=0.1)
  ) 
}
