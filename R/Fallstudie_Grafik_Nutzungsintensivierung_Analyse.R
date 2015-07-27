# Funktionsdefinition einbinden:
source('./Funktionen_Nutzungsintensivierung.R')
source('./Funktionen_Transporte.R')
source('./Fallstudie_data.R')

# für den Export von Graphiken in tex-Files
require(tikzDevice)
require(manipulate)

# Einstellungen für den Export der Graphik
tikzheight <- 4.6

tikzwidth <- 5.8
fntsize <- 0.8

# Export aktivieren
 useTikz <- TRUE
# useTikz <- FALSE

#### Konstanten ####
var = c(0.01, 0.05, 0.1)      # Relative Abweichung für Sensititvitäts-Analyse
low = 1 - var
mid = 1
hig = 1 + var

#### Parametersätze ####
Avg <- data.frame(
  # n_max = n_max_mid[1],
  # t_max = t_max_mid[1],
  n_max = n_max_mid[1]*mid,
  t_max = t_max_mid[1]*mid,
  A     = A[1]*mid,
  N     = N[1]*mid,
  i_P   = i_P["MF","values"]*mid,
  i_d   = i_d["MF","values"]*mid,
  i_N   = i_N["MF","values"]*mid,
  theta = theta[1]*mid,
  alpha = alpha[1]*mid
  )
Wor <- data.frame(
  # n_max = n_max_low[1],
  # t_max = t_max_hig[1],
  n_max = n_max_mid[1]*low,
  t_max = t_max_mid[1]*hig,
  A     = A[1]*hig,
  N     = N[1]*hig,
  i_P   = i_P["MF","values"]*low,
  i_d   = i_d["MF","values"]*hig,
  i_N   = i_N["MF","values"]*hig,
  theta = theta[1]*hig,
  alpha = alpha[1]*hig
  )
Bes <- data.frame(
  # n_max = n_max_hig[1],
  # t_max = t_max_low[1],
  n_max = n_max_mid[1]*hig,
  t_max = t_max_mid[1]*low,
  A     = A[1]*low,
  N     = N[1]*low,
  i_P   = i_P["MF","values"]*hig,
  i_d   = i_d["MF","values"]*low,
  i_N   = i_N["MF","values"]*low,
  theta = theta[1]*low,
  alpha = alpha[1]*low
  )

################################
#### Nutzungsintensivierung ####
################################

#### Teil 1: Kriterien überprüfen ####
# h* berechnen #
h_stern_Wor = with(Wor, n_max/t_max)
h_stern_Avg = with(Avg, n_max/t_max)
h_stern_Bes = with(Bes, n_max/t_max)

p_stern_Wor = with(Wor, floor(N[1] / (h_stern_Wor * Time)))
p_stern_Avg = with(Avg, floor(N[1] / (h_stern_Avg * Time)))
p_stern_Bes = with(Bes, floor(N[1] / (h_stern_Bes * Time)))

p_Ind = Szenario.Ind["p","values"]
p_Gem = Szenario.Luh["p","values"]
h_Ind_Wor = with(Wor, h(p_Ind, N[1], Time))
h_Ind_Avg = with(Avg, h(p_Ind, N[1], Time))
h_Ind_Bes = with(Bes, h(p_Ind, N[1], Time))

p_Gem = Szenario.Luh["p","values"]

# Ausgabe #
overview <- data.frame(h_Ind    = c(h_Ind_Wor[3], h_Ind_Wor[2],h_Ind_Wor[1], 
                                    h_Ind_Avg,
                                    h_Ind_Bes[1], h_Ind_Bes[2], h_Ind_Bes[3]),
                       h_stern  = c(h_stern_Wor[3], h_stern_Wor[2],h_stern_Wor[1], 
                                    h_stern_Avg,
                                    h_stern_Bes[1], h_stern_Bes[2], h_stern_Bes[3]),
                       p_Ind    = rep(p_Ind, times=7),
                       p_stern  = c(p_stern_Wor[3], p_stern_Wor[2],p_stern_Wor[1], 
                                    p_stern_Avg,
                                    p_stern_Bes[1], p_stern_Bes[2], p_stern_Bes[3]))
overview$check <- overview$h_Ind < overview$h_stern
row.names(overview) <- c("worst 10%", "worst 5%", "worst 1%", "average", "best 1%", "best 5%", "best 10%")

print("Teil 1: Kriterienüberprüfung:")
print(overview, digits=0)

# export to latex:
require(xtable)
output.table <- xtable(overview, digits = 0)
print(output.table, type="latex", file="../tex/Tabellen/NI_Kriterien.tex", booktabs=TRUE, floating=FALSE)

#### Teil 2: Delta MIPS (p, daten) ####
delta_MIPS <- function(p, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  return(MIPS_Ind-MIPS_Gem)
}


p_min = 2
p_dis <- seq(from=p_Ind, to=p_min, by=-1) # ganzzahlige p-Werte
p_con <- seq(from=p_Ind, to=p_min-0.08, length.out=200) # "kontinuierliche" p-Werte

if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_DeltaMIPS_p.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

par(mar=c(3,3,5,0), mgp=c(2,0.6,0)) # mgp: for arranging axes and labels spatially, first value: distance axes label, second value: distance tick labe, third value: distance tick mark from axes
plot(p_con, delta_MIPS(p_con, Bes[3,]), type = 'l', lty = 1, col='grey90', ylim=c(0,0.3), xlim=rev(range(p_dis)),  xlab = "Parallele Produktanzahl $p^\\text{gem}$ (Ziel-Szenario)", ylab = "$\\Delta$MIPS [kg/kg]",
     panel.first = polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[3,]),
                                           rev(delta_MIPS(p_con, Bes[3,]))),
                           col="gray90", border = "gray90"),
     axes = FALSE
     )

# Unsicherheitsbereiche:
polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[2,]),
                                                   rev(delta_MIPS(p_con, Bes[2,]))),
                           col="gray80", border = "gray80")

polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[1,]),
                                                     rev(delta_MIPS(p_con, Bes[1,]))),
                             col="gray70", border = "gray70")

points(p_dis, delta_MIPS(p_dis, Avg), xlim=rev(range(p_dis)), type = 'p', pch = 22, bg = 'gray70', col="gray20",
     panel.first = lines(p_con, delta_MIPS(p_con, Avg), type = 'l', lty = 1, lwd = 1.5, col='gray20'))

# Maintitle:
title("MIPS-Einsparung im Vergleich zu Szenario I", line=3.5, cex.main=1.2)
# Subtitle:
title('Erh\"ohung der Nutzungsh\"aufigkeit', line=1.5, cex.main=1.0)

# Achsen:
axis(side=1, at = c(p_Ind, p_Gem, p_stern_Avg, 15, 10, 5), 
     labels = c(paste("$p_\\text{I}=", p_Ind, "$", sep=""),
                paste("$p_\\text{II}=", p_Gem,"$", sep=""), 
                paste("$p^*=", p_stern_Avg,"$", sep=""),
                "", 10, 5),
     cex.axis=0.8) 
axis(side=2, cex.axis = 0.8, at = seq(0,0.3,by=0.05), labels = c("0.0", "", "0.1", "", "0.2", "", "0.3"))
box()

# vertikale Linie für p*:
lines(rep(p_stern_Avg, times=2), c(-0.1,0.05), lty="dashed")

# Legende:
legend("topleft", inset = 0.02,
       legend = c("Sch\"atzwert", "+/- \\ 1 \\% Parameterabweichung", "+/- \\ 5 \\% Parameterabweichung", "+/- 10 \\% Parameterabweichung"),
       bty = "n",
       col = c("gray20","gray70", "gray80", "gray90"),
       lty = c(1, 0, 0, 0),
       lwd = c(1, 0, 0, 0),
       pch = c(22, 22, 22, 22),
       pt.bg = c(NA,"gray70", "gray80", "gray90"),
       pt.cex = c(1, 2.5, 2.5, 2.5)*0.8,
       cex = 0.8
)

if(useTikz) dev.off()


##################
#### Kopplung ####
##################
#------------------------------------------------------------------------------#
d_stern <- function(p_Ind, p_Gem, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                               t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p_Gem, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                                t_max = t_max, A = A, n_max = n_max))
  return(with(daten, (MIPS_Ind - MIPS_Gem)*A/(theta * i_d * alpha)))
}

#------------------------------------------------------------------------------#
delta_MIPS <- function(d, p, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                               t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p, N[1], Time), I_fix = i_N*N[1], S_D = N[1]*A, i_P = i_P,
                                t_max = t_max, A = A, n_max = n_max))
  MIPS_Tra <- with(daten, MIPS_Gem + MIPS_a(d = d, theta = theta, i_d = i_d * alpha, 
                                            a = A, K = 1, I_fix = 0, S_D = N[1]*A))
  return((MIPS_Ind - MIPS_Tra))
}

#### Teil 1: delta_MIPS in Abhängigkeit von d ####

if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_DeltaMIPS_d.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

p_Tra <- Szenario.Tra["p", "values"]
d_seq <- seq(from=0, to=5.5, length.out=200) 
d_stern_Wor <- d_stern(p_Ind = p_Ind, p_Gem = p_Tra, Wor)
d_stern_Avg <- d_stern(p_Ind = p_Ind, p_Gem = p_Tra, Avg)
d_stern_Bes <- d_stern(p_Ind = p_Ind, p_Gem = p_Tra, Bes)

par(mar=c(3,3,5,0), mgp=c(2,0.6,0)) # mgp: for arranging axes and labels spatially, first value: distance axes label, second value: distance tick labe, third value: distance tick mark from axes
vec_plot <- delta_MIPS(d = d_seq, p = p_Tra, daten = Bes[3,])
plot(d_seq, vec_plot, 
     type = 'l', lty = 1, col='grey90', 
     ylim=c(-0.35,0.35), 
     xlab = "Mittlere Transportdistanz $d^\\text{gem}$ [km] (Ziel-Szenario)", ylab = "$\\Delta$MIPS [kg/kg]",
     panel.first = polygon(c(d_seq, rev(d_seq)), 
                           c(delta_MIPS(d = d_seq, p = p_Tra, daten = Wor[3,]),
                             rev(delta_MIPS(d = d_seq, p = p_Tra, daten = Bes[3,]))),
                           col="gray90", border = "gray90"),
     axes = FALSE
     )

# Unsicherheitsbereiche:
polygon(c(d_seq, rev(d_seq)), 
        c(delta_MIPS(d_seq, p = p_Tra, daten = Wor[2,]), 
          rev(delta_MIPS(d_seq, p = p_Tra, daten = Bes[2,]))),
        col="gray80", border = "gray80")

polygon(c(d_seq, rev(d_seq)), 
        c(delta_MIPS(d_seq, p = p_Tra, daten = Wor[1,]),
          rev(delta_MIPS(d_seq, p = p_Tra, daten = Bes[1,]))),
        col="gray70", border = "gray70")


# Maintitle:
title("MIPS-Einsparung im Vergleich zu Szenario I", line=3.5, cex.main=1.2)
# Subtitle:
title("Erh\"ohung der Nutzungsh\"aufigkeit und zus\"atzliche Transporte", line=1.5, cex.main=1.0)

# Achsen:
axis(side=1, 
     at = c(0, d_stern_Avg, Szenario.Tra["d","values"]), 
     labels = c(paste("$d_\\text{I}=", 0, "$", sep=""),
                paste("$d^*=", round(d_stern_Avg,1),"$", sep=""), 
                paste("$d_\\text{III}=", Szenario.Tra["d","values"],"$", sep="") 
                ),
     cex.axis=0.8
     ) 
axis(side=1, 
     at = c(1, 2, 3, 4, 5), 
     labels = c("", "", 3, 4, 5),
     cex.axis=0.8
     ) 

axis(side=2, cex.axis = 0.8
#      pos = 0
)
box()

# abline(h=0, lty="dashed")
# Unsicherheitsbereiche für d_stern:
lines(c(d_stern_Wor[3], d_stern_Bes[3]), c(0,0), lwd=2, col="grey80", lend="butt")
lines(c(d_stern_Wor[2], d_stern_Bes[2]), c(0,0), lwd=3, col="grey70", lend="butt")
lines(c(d_stern_Wor[1], d_stern_Bes[1]), c(0,0), lwd=4, col="grey60", lend="butt")

axis(side=1, 
     at = c(d_stern_Wor[3], d_stern_Bes[3]), 
     labels = rep("", times=2),
     pos = 0.006,
     lwd = 0,
     lwd.ticks= 2,
     lend="square",
     col = "grey80"
) 
axis(side=1, 
     at = c(d_stern_Wor[2], d_stern_Bes[2]), 
     labels = rep("", times=2),
     pos = 0.006,
     lwd = 0,
     lwd.ticks= 2,
     lend="square",
     col = "grey70"
) 
axis(side=1, 
     at = c(d_stern_Wor[1], d_stern_Bes[1]), 
     labels = rep("", times=2),
     pos = 0.006,
     lwd = 0,
     lwd.ticks= 2,
     lend="square",
     col = "grey60"
) 
# vertikale Linie für d*:
lines(rep(d_stern_Avg, times=2), c(-0.4,0.0), lty="dashed")

# Avg Szenrio:
lines(d_seq, delta_MIPS(d_seq, p = p_Tra, daten = Avg), 
                           type = 'l', lty = 1, lwd = 1.5, col='gray20')
# Legende:
legend("topright", inset = 0.04,
       legend = c("Sch\"atzwert", "+/- \\ 1 \\% Parameterabweichung", "+/- \\ 5 \\% Parameterabweichung", "+/- 10 \\% Parameterabweichung"),
       bty = "n",
       col = c("gray20","gray70", "gray80", "gray90"),
       lty = c(1, 0, 0, 0),
       lwd = c(1, 0, 0, 0),
       pch = c(NA, 22, 22, 22),
       pt.bg = c(NA,"gray70", "gray80", "gray90"),
       pt.cex = c(1, 2.5, 2.5, 2.5)*0.8,
       cex = 0.8
)

if(useTikz) dev.off()


#### Teil 2: Überprüfung von d < d* ####

# Ausgabe #
overview_d <- data.frame(d_Tra      = rep(Szenario.Tra["d","values"], times=3),
                         d_stern10  = c(d_stern_Wor[3],d_stern_Avg,d_stern_Bes[3])
                         )
overview_d$check10 <- overview_d$d_Tra < overview_d$d_stern10
overview_d$d_stern05 <- c(d_stern_Wor[2],d_stern_Avg,d_stern_Bes[2])
overview_d$check05 <- overview_d$d_Tra < overview_d$d_stern05
overview_d$d_stern01 <- c(d_stern_Wor[1],d_stern_Avg,d_stern_Bes[1])
overview_d$check01 <- overview_d$d_Tra < overview_d$d_stern01
row.names(overview_d) <- c("worst", "average", "best")

print("Teil 2: Kriterienüberprüfung:")
print(overview_d, digits=0)

# export to latex:
require(xtable)
output.table <- xtable(overview_d)
print.xtable(output.table, type="latex", file="../tex/Tabellen/Tra_Kriterien.tex", booktabs=TRUE, floating=FALSE)


#### Teil 3: Szenarien vergleichen ####
MIPS_N_I <- with(Bes[3,], i_N/A)
MIPS_N_II <- with(Bes[3,], i_N/A)
MIPS_P_I <- with(Bes[3,], MIPS(h = h(p_Ind, N[1], Time), I_fix = 0, S_D = N[1]*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_P_II <- with(Bes[3,], MIPS(h = h(p_Gem, N[1], Time), I_fix = 0, S_D = N[1]*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_T <- with(Bes[3,], MIPS_a(d = Szenario.Tra["d", "values"], theta = theta, i_d = i_d * alpha, 
                    a = A, K = 1, I_fix = 0, S_D = N[1]*A))

MIPS_Vergleich <- matrix(c(MIPS_N_II, MIPS_P_II, MIPS_T,
                           MIPS_N_II, MIPS_P_II, 0, 
                           MIPS_N_I, MIPS_P_I, 0), nrow=3, ncol=3)
dimnames(MIPS_Vergleich) <- list(c("N","P", "T"), c("Szenario III", "Szenario II", "Szenario I"))

# PLOT:
if(useTikz) tikz( '../tex/Abbildungen/Vergleich_Szenarien.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

par(mar=c(3.2,3,8,0.2), mgp=c(2,0.6,0))
barplot(MIPS_Vergleich, col=c("gray70", "gray80","gray90"), xlim = c(0,4), ylim=c(0,2.2), horiz = TRUE, axes = FALSE, width=0.4, cex.names=0.7, space=0.4)

# obere Achse
axis (side = 3, at = c(0,0.25,0.5,0.75,1)*(MIPS_N_I+MIPS_P_I), labels = paste(c(0,25,50,75,100), "\\%"))
mtext ('MIPS relativ zu Szenario I', side = 3, line = 2, cex = 1)

# untere Achse
axis(side = 1)
mtext ("MIPS [kg/kg]", side = 1, line = 2, cex = 1)

# Hilfslinien
abline(v=0, lwd=1)
abline(v=MIPS_N_I+MIPS_P_I, lty="dashed")

# Legende
legend("top", inset = 0.05, horiz = TRUE, 
       legend = c("MIPS Produktanteil\\ \\ \\ \\ \\ \\ \\ \\ \\ ", 
                  "MIPS Nutzungsanteil", 
                  "MIPS Transportanteil"),
       bty = "n",
       col = c("gray70", "gray80", "gray90"),
       pch = c(22, 22, 22, 22),
       pt.bg = c("gray70", "gray80", "gray90"),
       pt.cex = c(2.5, 2.5, 2.5)*0.8,
       cex = 0.8
)
# Maintitle:
title("MIPS-Bestandteile", line=6, cex.main=1.2)
# Subtitle:
title("Erh\"ohung der Nutzungsh\"aufigkeit und zus\"atzliche Transporte", line=4, cex.main=1.0)

# evtl. eine Angabe der Reduktion: (noch nicht fertig)
text(3.98,0.30, "3\\%", cex=0.6, adj=1) # Reduktion Transportszenario
arrows(x0 = 3.86, x1 = 3.99, y0 = 0.36, y1 = 0.36, code=1, length=0.04)
text(3.95,0.86, "5\\%", cex=0.6, adj=1) # Reduktion Luhrmannhof
arrows(x0 = 3.79, x1 = 3.99, y0 = 0.92, y1 = 0.92, code=1, length=0.04)

if(useTikz) dev.off()

#### Teil 4: Plot d~p ####
require(plot3D)
d_seq <- seq(from=0, to=4, length.out=200) 
p_con <- seq(from=p_Ind, to=p_min+1, length.out=200)

if(useTikz) tikz( '../tex/Abbildungen/p-d-Diagramm.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)
# background color:
par(mar=c(3,3,4,0), mgp=c(1,0.6,0))
vec <- d_stern(p_Ind=p_Ind, p_Gem = rev(p_con), daten=Bes[2,])
plot(p_con, vec, xlim=c(3,17), ylim=c(0,4), type="l", axes=FALSE, 
     xlab="parallele Produktanzahl $p$ (Zielszenario)", 
     ylab="mittlere Transportdistanz $d$ [km] (Zielszenario)",  
     panel.first = polygon(c(p_con, rev(p_con)), 
                           c(vec, rep(4,times=length(vec))), 
                           col="gray90", border = "gray90")
     )
# image2D(x=p_con, y=d_seq, z=outer(rev(p_con), d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes[2,])), col=c("grey70", "white" ), zlim=c(-0.3,0.3), alpha=0.6, xlim=c(2,17), axes=FALSE, colkey=FALSE, xlab="parallele Produktanzahl $p$ (Zielszenario)", ylab="mittlere Transportdistanz $d$ [km] (Zielszenario)")

# Unsicherheitsbereiche:
# color_hig <- adjustcolor("gray90",alpha.f=0.4)
# color_mid <- adjustcolor("gray80",alpha.f=0.4)
# color_sma <- adjustcolor("gray70",alpha.f=0.4)

# # -- highest variation
# polygon(c(p_con, rev(p_con)), 
#         c(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Wor[3,]),
#           rev(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Bes[3,]))),
#         col=color_hig, border = color_hig)

# # -- mid variation
# polygon(c(p_con, rev(p_con)), 
#         c(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Wor[2,]),
#           rev(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Bes[2,]))),
#         col=color_mid, border = color_mid)

# # -- smallest variation
# polygon(c(p_con, rev(p_con)), 
#         c(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Wor[1,]),
#           rev(d_stern(p_Ind = p_Ind, p_Gem = rev(p_con), daten = Bes[1,]))),
#         col=color_sma, border = color_sma)

# -- Avg Case
# contour2D(x=p_con, y=d_seq, z=outer(rev(p_con), d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes[2,])), add=TRUE, col="black", alpha = 1, drawlabels = FALSE, nlevels=1, lwd=3, labcex=1, xlab="test")

# contour lines:
# contour2D(x=p_con, y=d_seq, z=outer(rev(p_con), d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Avg)), add=TRUE, col="black", alpha = 0.4, drawlabels = TRUE, nlevels=15, labcex= 1)
axis(side=1, pos=0, at=seq(3,17, by=2), labels=seq(17, 2, by=-2))
axis(side=2, pos=3)
# box()
 
text(10,3.5,"$-$", cex=4, col="grey30")
text(10,1.25,"$+$", cex=4, col="grey30")

# Maintitle:
title("MIPS-Einsparung allgemeine Kopplung", line=2.5, cex.main=1.2)
# Subtitle:
title("Erh\"ohung der Nutzungsh\"aufigkeit und zus\"atzliche Transporte", line=0.5, cex.main=1.0)

if(useTikz) dev.off()
