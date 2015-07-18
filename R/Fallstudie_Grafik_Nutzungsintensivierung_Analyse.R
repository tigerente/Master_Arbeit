# Funktionsdefinition einbinden:
source('./Funktionen_Nutzungsintensivierung.R')
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
#useTikz <- FALSE
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

plot(p_con, delta_MIPS(p_con, Bes[3,]), type = 'l', lty = 1, col='grey90', ylim=c(0,0.3), xlim=rev(range(p_dis)), main = "MIPS-Einsparung im Vergleich zu Szenario I", xlab = "$p$ (Ziel-Szenario)", ylab = "$\\Delta$MIPS(kg/kg)",
     panel.first = polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[3,]),
                                           rev(delta_MIPS(p_con, Bes[3,]))),
                           col="gray90", border = "gray90")
     )

polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[2,]),
                                                   rev(delta_MIPS(p_con, Bes[2,]))),
                           col="gray80", border = "gray80")

polygon(c(p_con, rev(p_con)), c(delta_MIPS(p_con, Wor[1,]),
                                                     rev(delta_MIPS(p_con, Bes[1,]))),
                             col="gray70", border = "gray70")

points(p_dis, delta_MIPS(p_dis, Avg), xlim=rev(range(p_dis)), type = 'p', pch = 22, bg = 'gray70', col="gray20",
     panel.first = lines(p_con, delta_MIPS(p_con, Avg), type = 'l', lty = 1, lwd = 1.5, col='gray20'))

axis(side=1, at = c(p_Ind, p_Gem), labels = c(paste("$p_I=", p_Ind, "$", sep=""), paste("$p_{II}=", p_Gem,"$", sep="")))

legend("topleft", inset = 0.02,
       legend = c("Sch\"atzwert", "+/- 1 \\% Parameterabweichung", "+/- 5 \\% Parameterabweichung", "+/- 10 \\% Parameterabweichung"),
       bty = "n",
       col = c("gray20","gray70", "gray80", "gray90"),
       lty = c(1, 0, 0, 0),
       lwd = c(1, 0, 0, 0),
       pch = c(22, 22, 22, 22),
       pt.bg = c(NA,"gray70", "gray80", "gray90"),
       pt.cex = c(1, 2.5, 2.5, 2.5)
)

if(useTikz) dev.off()