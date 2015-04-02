# Funktionsdefinition einbinden:
source('./Funktionen_Nutzungsintensivierung.R')

# für den Export von Graphiken in tex-Files
require(tikzDevice)

# Einstellungen für den Export der Graphik
tikzheight <- 2.5
tikzwidth <- 6
fntsize <- 0.8

# Export aktivieren
useTikz <- FALSE
if(useTikz) tikz( '../tex/Abbildungen/Nutzungsintensivierung_1.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

# Definition von Konstanten:
I_fix = 10000
S_D = 10000
i_P = 5000
A = 1
T = 10
t_max = 15
n_max = 5000
p_min = 2

# Parameter für Plots:
p_low = 1
p_high = 15
h_low = h(p_high, N, T)
h_high = h(p_low, N, T)
MIPS_low = 0
MIPS_high = MIPS (h_low, I_fix, S_D, i_P, t_max, A, n_max)
h_len = 200 # Anzahl der Stützpunkte fuer Linien-Plots

# Daten generieren:
N = S_D / A
h_max = S_D / (A * p_min * T)
p_points = p_low:p_high
h_points = h (p_points, N, T)
MIPS_points = MIPS (h_points, I_fix, S_D, i_P, t_max, A, n_max)

p_lines = seq(length = h_len, from = p_low, to = p_high)
h_lines = h (p_lines, N, T)
MIPS_lines = MIPS (h_lines, I_fix, S_D, i_P, t_max, A, n_max)

# Plots erzeugen:
par(mar = c(5,4,5.5,2) + 0.1)
plot(h_points, MIPS_points, xlab = 'Nutzungsh\"aufigkeit $h$ [1/Jahr]', ylab = 'MIPS [kg/Service-Einheit]', axes = FALSE, type = 'p', pch = 22, bg = 'white', xlim = c(0, h_high), ylim = c(MIPS_low, MIPS_high), panel.first = points(h_lines, MIPS_lines, type = 'l', lty = 1, col='grey'))
abline(v = c(n_max/t_max, h_max), lty = 3)
title('Materialintensit\"at pro Service-Einheit MIPS$(h)$', line = 4.5)
axis (side = 1, at = c(0, 200, 400, 600, 800, 1000, n_max/t_max, h_max), labels = c(0, 200, 400, 600, 800, 1000, '$h^*$', '$h_\\text{max}$'))
axis (side = 2)
axis (side = 3, at = h_points, labels = p_points)
box()
mtext ('parallele Produktanzahl $p$', side = 3, line = 2.5, cex = 1)

if (useTikz) dev.off()
