# Funktionsdefinition einbinden:
source('./Funktionen_Nutzungsintensivierung.R')
source('./Funktionen_Transporte.R')
source('./Fallstudie_data.R')

# für den Export von Graphiken in tex-Files
require(tikzDevice)
require(manipulate)

# Einstellungen für den Export der Graphik
tikzheight <- 4.0

tikzwidth <- 5.8
fntsize <- 0.8

# Export aktivieren
useTikz <- TRUE
# useTikz <- FALSE
if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_DeltaMIPS_p.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

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
  theta = theta[1]*mid
  )
Wor <- data.frame(
  # n_max = n_max_low[1],
  # t_max = t_max_hig[1],
  n_max = n_max_mid[1]*low,
  t_max = t_max_mid[1]*hig,
  A     = A[1]*hig,
  N     = N*hig,
  i_P   = i_P["MF","values"]*low,
  i_d   = i_d["MF","values"]*hig,
  i_N   = i_N["MF","values"]*hig,
  theta = theta[1]*hig
  )
Bes <- data.frame(
  # n_max = n_max_hig[1],
  # t_max = t_max_low[1],
  n_max = n_max_mid[1]*hig,
  t_max = t_max_mid[1]*low,
  A     = A[1]*low,
  N     = N*low,
  i_P   = i_P["MF","values"]*hig,
  i_d   = i_d["MF","values"]*low,
  i_N   = i_N["MF","values"]*low,
  theta = theta[1]*low
  )

#### Teil 1: Kriterien überprüfen ####
# h* berechnen #
h_stern_Wor = with(Wor, n_max/t_max)
h_stern_Avg = with(Avg, n_max/t_max)
h_stern_Bes = with(Bes, n_max/t_max)

p_stern_Wor = with(Wor, floor(N / (h_stern_Wor * Time)))
p_stern_Avg = with(Avg, floor(N / (h_stern_Avg * Time)))
p_stern_Bes = with(Bes, floor(N / (h_stern_Bes * Time)))

p_Ind = Szenario.Ind["p","values"]
p_Gem = Szenario.Luh["p","values"]
h_Ind_Wor = with(Wor, h(p_Ind, N, Time))
h_Ind_Avg = with(Avg, h(p_Ind, N, Time))
h_Ind_Bes = with(Bes, h(p_Ind, N, Time))

p_Gem = Szenario.Luh["p","values"]

# Ausgabe #
overview <- data.frame(h_Ind    = c(h_Ind_Wor[3],h_Ind_Avg,h_Ind_Bes[3]),
                       h_stern  = c(h_stern_Wor[3],h_stern_Avg,h_stern_Bes[3]),
                       p_Ind    = rep(p_Ind, times=3),
                       p_stern  = c(p_stern_Wor[3],p_stern_Avg,p_stern_Bes[3]))
overview$check <- overview$h_Ind < overview$h_stern
row.names(overview) <- c("worst", "average", "best")

print("Teil 1: Kriterienüberprüfung:")
print(overview, digits=0)

#### Teil 2: Delta MIPS (p, daten) ####
delta_MIPS <- function(p, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  return(MIPS_Ind-MIPS_Gem)
}


p_min = 2
p_dis <- seq(from=p_Ind, to=p_min, by=-1) # ganzzahlige p-Werte
p_con <- seq(from=p_Ind, to=p_min-0.08, length.out=200) # "kontinuierliche" p-Werte

par(mar=c(5,3,6,3), mgp=c(2,0.6,0)) # mgp: for arranging axes and labels spatially, first value: distance axes label, second value: distance tick labe, third value: distance tick mark from axes
plot(p_con, delta_MIPS(p_con, Bes[3,]), type = 'l', lty = 1, col='grey90', ylim=c(0,0.3), xlim=rev(range(p_dis)),  xlab = "$p$ (Ziel-Szenario)", ylab = "$\\Delta$MIPS [kg/kg]",
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
title("MIPS-Einsparung im Vergleich zu Szenario I", line=4, cex.main=1.4)
# Subtitle:
title("Nutzungsintensivierung", line=2, cex.main=1.2)

# Achsen:
axis(side=1, at = c(p_Ind, p_Gem, p_stern_Avg, 15, 10, 5), 
     labels = c(paste("$p^\\text{I}=", p_Ind, "$", sep=""),
                paste("$p^\\text{II}=", p_Gem,"$", sep=""), 
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

#### Teil 1: delta_MIPS in Abhängigkeit von d ####

# Export aktivieren
useTikz <- TRUE
# useTikz <- FALSE
if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_DeltaMIPS_d.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

delta_MIPS <- function(d, p, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  MIPS_Tra <- with(daten, MIPS_Gem + MIPS_a(d = d, theta = theta, i_d = i_d, 
                    a = A, K = 1, I_fix = 0, S_D = N*A))
  return((MIPS_Ind - MIPS_Tra))
}

p_Tra <- Szenario.Tra["p", "values"]
d_seq <- seq(from=0, to=4, length.out=200) 
d_stern_Avg <- 0.8040201 # das ist numerisch gewonnen, nicht analytisch

par(mar=c(5,3,6,3), mgp=c(2,0.6,0)) # mgp: for arranging axes and labels spatially, first value: distance axes label, second value: distance tick labe, third value: distance tick mark from axes
vec_plot <- delta_MIPS(d = d_seq, p = p_Tra, daten = Bes[3,])
plot(d_seq, vec_plot, 
     type = 'l', lty = 1, col='grey90', 
     ylim=range(vec_plot) + c(-0.25,0.03), 
     xlab = "$d$ (Ziel-Szenario)", ylab = "$\\Delta$MIPS [kg/kg]",
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

lines(d_seq, delta_MIPS(d_seq, p = p_Tra, daten = Avg), 
                           type = 'l', lty = 1, lwd = 1.5, col='gray20')

# Maintitle:
title("MIPS-Einsparung im Vergleich zu Szenario I", line=4, cex.main=1.4)
# Subtitle:
title("Nutzungsintensivierung und zusätzliche Transporte", line=2, cex.main=1.2)

# Achsen:
axis(side=1, 
     at = c(0, 1, 3, 4, Szenario.Tra["d","values"], d_stern_Avg), 
     labels = c(paste("$d^\\text{I}=", 0, "$", sep=""),
                "", 3, 4,
                paste("$p^\\text{II}=", Szenario.Tra["d","values"],"$", sep=""), 
                paste("$d^*=", round(d_stern_Avg,1),"$", sep="")
                ),
     cex.axis=0.8
#      pos = 0
     ) 
axis(side=1, 
     at = c(0, 1, 2, 3, 4), 
     labels = rep("", times=5),
     cex.axis=0.8,
          pos = 0,
     lwd = 0.5
) 
axis(side=2, cex.axis = 0.8
#      pos = 0
)
box()
abline(h=0, lwd=0.5)

# vertikale Linie für d*:
lines(rep(d_stern_Avg, times=2), c(-0.3,0.0), lty="dashed")

# Legende:
legend("topright", inset = 0.02,
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


#### Teil 2: Überprüfung von d < d* ####


#### Teil 3: Szenarien vergleichen ####
MIPS_N_I <- with(Bes[3,], i_N/A)
MIPS_N_II <- with(Bes[3,], i_N/A)
MIPS_P_I <- with(Bes[3,], MIPS(h = h(p_Ind, N, Time), I_fix = 0, S_D = N*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_P_II <- with(Bes[3,], MIPS(h = h(p_Gem, N, Time), I_fix = 0, S_D = N*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_T <- with(Bes[3,], MIPS_a(d = Szenario.Tra["d", "values"], theta = theta, i_d = i_d, 
                    a = A, K = 1, I_fix = 0, S_D = N*A))

MIPS_Vergleich <- matrix(c(MIPS_N_II, MIPS_P_II, MIPS_T,
                           MIPS_N_II, MIPS_P_II, 0, 
                           MIPS_N_I, MIPS_P_I, 0), nrow=3, ncol=3)
dimnames(MIPS_Vergleich) <- list(c("N","P", "T"), c("Szenario I", "Szenario II", "Szenario III"))

# PLOT:
par(mar=c(5,3,8,3), mgp=c(2,0.6,0))
barplot(MIPS_Vergleich, col=c("gray70", "gray80","gray90"), xlim = c(0,4), ylim=c(0,1.9), horiz = TRUE, axes = FALSE, width=0.5)

# obere Achse
axis (side = 3, at = c(0,0.25,0.5,0.75,1)*(MIPS_N_I+MIPS_P_I), labels = paste(c(0,25,50,75,100), "%"))
mtext ('MIPS relativ zu Szenario I', side = 3, line = 2, cex = 1)

# untere Achse
axis(side = 1)
mtext ("MIPS [kg/kg]", side = 1, line = 2, cex = 1)

# Hilfslinien
abline(v=0, lwd=1)
abline(v=MIPS_N_I+MIPS_P_I, lty="dashed")

# Legende
legend("top", inset = 0.02, horiz = TRUE,
       legend = c("MIPS Produktanteil      ", "MIPS Nutzungsanteil      ", 
                  "MIPS Transportanteil"),
       bty = "n",
       col = c("gray70", "gray80", "gray90"),
       pch = c(22, 22, 22, 22),
       pt.bg = c("gray70", "gray80", "gray90"),
       pt.cex = c(2.5, 2.5, 2.5)*0.8,
       cex = 0.8
)
# Maintitle:
title("MIPS-Bestandteile", line=6, cex.main=1.4)
# Subtitle:
title("Nutzungsintensivierung und zusätzliche Transporte", line=4, cex.main=1.2)

# evtl. eine Angabe der Reduktion: (noch nicht fertig)
text(3.9,0.35, "1%", adj=0, cex=0.8) # Reduktion Transportszenario
arrows(x0 = 3.9, x1 = 3.99, y0 = 0.4, y1 = 0.4, code=1, length=0.1)
text(3.9,0.95, "5%", cex=0.8) # Reduktion Luhrmannhof
arrows(x0 = 3.8, x1 = 3.99, y0 = 1, y1 = 1, code=1, length=0.1)

#### Teil 4: Plot d~p ####
require(plot3D)
require(colorspace)
image2D(x=p_con, y=d_seq, z=outer(p_con, d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes[3,])), col=my_color_palette(200), zlim=c(-0.23,0.23))
contour2D(x=p_con, y=d_seq, z=outer(p_con, d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes[3,])), add=TRUE, col="black", alpha = 0.5, drawlabels = FALSE, nlevels=1, lwd=2)
contour2D(x=p_con, y=d_seq, z=outer(p_con, d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes[3,])), add=TRUE, col="black", alpha = 0.1, drawlabels = FALSE, nlevels=30)
# the following palette is created using function choose_palette from package colorspace:
my_color_palette <- function (n, h = c(365, 120), c = 120, l = c(40, 100), power = 0.979838709677419, 
                              fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- c[1L]
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, -1, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                       C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                               h[2L])), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}