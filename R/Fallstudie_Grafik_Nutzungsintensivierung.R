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
# useTikz <- TRUE
useTikz <- FALSE
if(useTikz) tikz( '../tex/Abbildungen/Fallstudie_Nutzungsintensivierung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

manipulate.function <- function(t_max=t_max[1], n_max=n_max[1]){
# spezifische MIPS-Funktion:
# MIPS_spez <- function(h){
#     I_N/S_D[1] +
#     MIPS(h = h, I_fix = 0, S_D = S_D[1], i_P = i_P["MF","values"],
#          t_max = t_max[1], A = A[1], n_max = n_max[1])  
# }
MIPS_spez <- function(h){
    I_N/S_D[1] +
    MIPS(h = h, I_fix = 0, S_D = S_D[1], i_P = i_P["MF","values"],
         t_max = t_max, A = A[1], n_max = n_max)  
}

# Definition von Konstanten:
p_min = 2

# Parameter für Plots:
p_low = 1
p_high = 50
h_low = h(p_high, N, Time)
#h_Ind = h(Szenario.Ind["p","values"], N, Time)
h_Ind = h(50, N, Time)
h_Luh = h(Szenario.Luh["p","values"], N, Time)
h_high = h(p_low, N, Time)
MIPS_low = 0
MIPS_high = MIPS_spez(h_low)
MIPS_Ind = MIPS_spez(h_Ind)
MIPS_Luh = MIPS_spez(h_Luh)

h_len = 200 # Anzahl der Stützpunkte fuer Linien-Plots

# Daten generieren:
I_N     <- i_N["MF","values"] * N
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
}
manipulate(manipulate.function(t_max=t_max, n_max = n_max), 
           n_max = slider(500, 6000, step=250),
           t_max = slider(1, 20, step=1)
           )
# sens_t <- 0.5
# sens_n <- 0.5
# sens.freq <- 5
# t_max_sens <- seq(from = t_max[1]*(1-sens_t), to = t_max[1]*(1+sens_t), length.out = sens.freq)
# n_max_sens <- seq(from = n_max[1]*(1-sens_n), to = n_max[1]*(1+sens_n), length.out = sens.freq)
# MIPS_sens <- function(t_max, n_max){
#     MIPS_Ind <- I_N/S_D[1] +
#                 MIPS(h = h_Ind, I_fix = 0, S_D = S_D[1], i_P = i_P["MF","values"],
#                     t_max = t_max, A = A[1], n_max = n_max)  
#     MIPS_Luh <- I_N/S_D[1] +
#                 MIPS(h = h_Luh, I_fix = 0, S_D = S_D[1], i_P = i_P["MF","values"],
#                     t_max = t_max, A = A[1], n_max = n_max)  
#
#     return(100*round(1-MIPS_Luh/MIPS_Ind, 4))
# }
# sens.mat <- outer(t_max_sens, n_max_sens, MIPS_sens) # vertikal wird t_max variiert
