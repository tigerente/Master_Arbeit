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


##################
#### Kopplung ####
##################

#### Teil 1: delta_MIPS in Abhängigkeit von d ####
delta_MIPS <- function(d, p, daten){
  MIPS_Ind <- with(daten, MIPS(h = h(p_Ind, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  
  MIPS_Gem <-  with(daten, MIPS(h = h(p, N, Time), I_fix = i_N*N, S_D = N*A, i_P = i_P,
                   t_max = t_max, A = A, n_max = n_max))
  MIPS_Tra <- with(daten, MIPS_Gem + MIPS_a(d = d, theta = theta, i_d = i_d, 
                    a = A, K = 1, I_fix = 0, S_D = N*A))
  return((MIPS_Tra - MIPS_Ind)/(MIPS_Ind))
}

d_seq <- seq(from=0, to=5, length.out=200) 

plot(d_seq, -1 * delta_MIPS(d = d_seq, p = Szenario.Tra["p", "values"], daten = Bes), type = 'l') 
abline(h=0, col="grey")




#### Teil 2: Überprüfung von d < d* ####


#### Teil 3: Szenarien vergleichen ####
MIPS_N_I <- with(Bes, i_N/A)
MIPS_N_II <- with(Bes, i_N/A)
MIPS_P_I <- with(Bes, MIPS(h = h(p_Ind, N, Time), I_fix = 0, S_D = N*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_P_II <- with(Bes, MIPS(h = h(p_Gem, N, Time), I_fix = 0, S_D = N*A, i_P = i_P,
                                                t_max = t_max, A = A, n_max = n_max))
MIPS_T <- with(Bes, MIPS_a(d = Szenario.Tra["d", "values"], theta = theta, i_d = i_d, 
                    a = A, K = 1, I_fix = 0, S_D = N*A))

MIPS_Vergleich <- matrix(c(MIPS_N_I, MIPS_P_I, 0, MIPS_N_II, MIPS_P_II, 0, MIPS_N_II, MIPS_P_II, MIPS_T), nrow=3, ncol=3)
dimnames(MIPS_Vergleich) <- list(c("N","P", "T"), c("Szenario I", "Szenario II", "Szenario III"))

barplot(MIPS_Vergleich, col=c("gray75", "gray85","gray95"), ylim=c(0,ceiling(MIPS_N_I+MIPS_P_I + MIPS_T)))
axis (side = 4, at = c(0,0.25,0.5,0.75,1)*(MIPS_N_I+MIPS_P_I), labels = paste(c(0,25,50,75,100), "%"))
legend("topright",c("N","P", "T"))

#### Teil 4: Plot d~p ####
require(plot3D)
image2D(x=p_con, y=d_seq, z=outer(p_con, d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes)), col=grey(c(seq(0.7,1,length.out=147), seq(1,0.7
                                                                                                                                    ,length.out=200))))
contour2D(x=p_con, y=d_seq, z=outer(p_con, d_seq, FUN = function(p,d) delta_MIPS(p=p, d=d, daten=Bes)), add=TRUE, col="black")
I_N     <- i_N["MF","values"] * N

# spezifische MIPS-Funktion:
MIPS_spez <- function(h){
  I_N/S_D[1] +
    MIPS(h = h, I_fix = 0, S_D = S_D[1], i_P = i_P["MF","values"],
         t_max = t_max[1], A = A[1], n_max = n_max[1])  
}
