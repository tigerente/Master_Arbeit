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
# useTikz <- TRUE
useTikz <- FALSE
if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_Nutzungsintensivierung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

#### Konstanten ####
var = 0.1      # Relative Abweichung für Sensititvitäts-Analyse
manipulate.function <- function(var){
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

# Ausgabe #
overview <- data.frame(h_Ind    = c(h_Ind_Wor,h_Ind_Avg,h_Ind_Bes),
                       h_stern  = c(h_stern_Wor,h_stern_Avg,h_stern_Bes),
                       p_Ind    = rep(p_Ind, times=3),
                       p_stern  = c(p_stern_Wor,p_stern_Avg,p_stern_Bes))
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
  return((MIPS_Gem - MIPS_Ind)/(MIPS_Ind))
}


p_min = 2
p_dis <- seq(from=p_Ind, to=p_min, by=-1) # ganzzahlige p-Werte
p_con <- seq(from=p_Ind, to=p_min, length.out=200) # "kontinuierliche" p-Werte

plot(p_dis, delta_MIPS(p_dis, Bes), xlim=rev(range(p_dis)), type = 'p', pch = 22, bg = 'white',
     panel.first = lines(p_con, delta_MIPS(p_con, Bes), type = 'l', lty = 1, col='grey'), ylim=c(-0.2,0))

points(p_dis, delta_MIPS(p_dis, Avg), xlim=rev(range(p_dis)), type = 'p', pch = 22, bg = 'white',
     panel.first = lines(p_con, delta_MIPS(p_con, Avg), type = 'l', lty = 1, col='grey'))

points(p_dis, delta_MIPS(p_dis, Wor), xlim=rev(range(p_dis)), type = 'p', pch = 22, bg = 'white',
     panel.first = lines(p_con, delta_MIPS(p_con, Wor), type = 'l', lty = 1, col='grey'))
}
manipulate(manipulate.function(var),
           var=slider(0,0.5,0.01))



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

# Definition von Konstanten:
p_min = 2

# Parameter für Plots:
p_low = 1
p_high = 50
h_low = h(p_high, N, Time)
h_Ind = h(p_Ind, N, Time)
#h_Ind = h(50, N, Time)
h_Luh = h(Szenario.Luh["p","values"], N, Time)
h_high = h(p_low, N, Time)
MIPS_low = 0
MIPS_high = MIPS_spez(h_low)
MIPS_Ind = MIPS_spez(h_Ind)
MIPS_Luh = MIPS_spez(h_Luh)

h_len = 200 # Anzahl der Stützpunkte fuer Linien-Plots

# Daten generieren:

h_max = S_D[1] / (A[1] * p_min * Time)
p_points = p_low:p_high
h_points = h(p_points, N, Time)
MIPS_points = MIPS_spez(h_points) 

p_lines = seq(length = h_len, from = p_low, to = p_high)
h_lines = h (p_lines, N, Time)
MIPS_lines = MIPS_spez(h_lines)

# Plots erzeugen:
par(mar = c(4.1,3.9,5.6,0.1))
plot(h_points, MIPS_points, xlab = 'Nutzungsh\"aufigkeit $h$ [Nutzungseinheiten/Jahr]', ylab = 'MIPS [kg/Service-Einheit]', axes = FALSE, type = 'p', pch = 22, bg = 'white', xlim = c(0, h_high), ylim = c(MIPS_low, MIPS_high), panel.first = points(h_lines, MIPS_lines, type = 'l', lty = 1, col='grey'))
abline(v = c(h_Ind, n_max[1]/t_max[1], h_max), lty = 3)
abline(h = c(MIPS_Ind, MIPS_Luh), lty = 3)
points(rep(h_Luh, times=2), c(MIPS_Ind, MIPS_Luh+0.15), pch=c(NA_integer_,25), lty=1, type="b", bg="black", lwd=3)
text(x=h_max+50, y = ( MIPS_Ind + MIPS_Luh )/ 2, labels = paste("Reduktion um", 100*round(1-MIPS_Luh/MIPS_Ind, 2), "%"), adj=0)
title('Materialintensit\"at pro Service-Einheit MIPS$(h)$', line = 4.5)
axis (side = 1, at = c(n_max[1]/t_max[1], h_max, 0, 500, 1000, 1500, 2000, 2500, 3000, 3500), labels = c('$h^*$', '$h_\\text{max}$', 0, 500, 1000, 1500, 2000, 2500, 3000, 3500))
axis (side = 2, at = c(0, 1, MIPS_points), labels = c(0, 1, round(MIPS_points, 1)))
axis (side = 3, at = h_points, labels = p_points)
box()
mtext ('parallele Produktanzahl $p$', side = 3, line = 2.5, cex = 1)

if (useTikz) dev.off()


# sens_t <- 0.5
# sens_n <- 0.5
# sens.freq <- 3
# t_max_sens <- seq(from = t_max[1]*(1-sens_t), to = t_max[1]*(1+sens_t), length.out = sens.freq)
# n_max_sens <- seq(from = n_max[1]*(1-sens_n), to = n_max[1]*(1+sens_n), length.out = sens.freq)
