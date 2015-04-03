# Funktionsdefinition einbinden:
source('./Funktionen_Nutzungsintensivierung.R')

# für den Export von Graphiken in tex-Files
require(tikzDevice)

# Einstellungen für den Export der Graphik
tikzheight <- 2.5
tikzwidth <- 6
fntsize <- 0.8

# Export aktivieren
tikz( '../tex/Abbildungen/Nutzungsintensivierung_1.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

# Definition von Konstanten:
N = 10000
T = 10
t_max = 15
n_max = 5000

# Parameter für Plots:
p_low = 1
p_high = 10
h_low = h(p_high, N, T)
h_high = h(p_low, N, T)
t_low = 0
t_high = t(h_low, t_max, n_max)
n_low = 0
n_high = n(h_high, t_max, n_max)
P_low = 0
P_high = P(h_low, t_max, n_max, N)
h_len = 200 # Anzahl der Stützpunkte fuer Linien-Plots


# Daten generieren:
p_points = p_low:p_high
h_points = h (p_points, N, T)
t_points = t(h_points, t_max, n_max)
n_points = n(h_points, t_max, n_max)
P_points = P(h_points, t_max, n_max, N)

p_lines = seq(length = h_len, from = p_low, to = p_high)
h_lines = h (p_lines, N, T)
t_lines = t(h_lines, t_max, n_max)
n_lines = n(h_lines, t_max, n_max)
P_lines = P(h_lines, t_max, n_max, N)

# Plots erzeugen:
par(mfcol=c(1,3), mar = c(5.1,4.1,6.5,2.1))

plot(h_points, t_points, xlab = 'Nutzungsh\"aufigkeit $h$ [NE/Jahr]', ylab = 'Nutzungsdauer $t$ [Jahre]', axes = FALSE, type = 'p', pch = 22, bg = 'white', xlim = c(0, h_high), ylim = c(t_low, t_high), panel.first = points(h_lines, t_lines, type = 'l', lty = 1, col='grey'))
abline(v = n_max/t_max, lty = 3)
title('Nutzungsdauer $t(h)$', line = 4.5)
axis (side = 1, at = c(0, 500, 1000, n_max/t_max), labels = c(0, 500, 1000, '$h^*$'))
axis (side = 2)
axis (side = 3, at = h_points, labels = p_points)
box()
mtext ('parallele Produktanzahl $p$', side = 3, line = 2.5, cex = 0.66)

plot(h_points, n_points, xlab = 'Nutzungsh\"aufigkeit $h$ [NE/Jahr]', ylab = 'Nutzungsmenge $n$ [NE]', axes = FALSE, type = 'p', pch = 22, bg = 'white', xlim = c(0, h_high), ylim = c(n_low, n_high), panel.first = points(h_lines, n_lines, type = 'l', lty = 1, col='grey'))
abline(v = n_max/t_max, lty = 3)
title('Nutzungsmenge $n(h)$', line = 4.5)
axis (side = 1, at = c(0, 500, 1000, n_max/t_max), labels = c(0, 500, 1000, '$h^*$'))
axis (side = 2)
axis (side = 3, at = h_points, labels = p_points)
box()
mtext ('parallele Produktanzahl $p$', side = 3, line = 2.5, cex = 0.66)

plot(h_points, P_points, xlab = 'Nutzungsh\"aufigkeit $h$ [NE/Jahr]', ylab = 'Effektive Produktanzahl $P$', axes = FALSE, type = 'p', pch = 22, bg = 'white', xlim = c(0, h_high), ylim = c(P_low, P_high), panel.first = points(h_lines, P_lines, type = 'l', lty = 1, col='grey'))
abline(v = n_max/t_max, lty = 3)
title('Produktanzahl $P(h)$', line = 4.5)
axis (side = 1, at = c(0, 500, 1000, n_max/t_max), labels = c(0, 500, 1000, '$h^*$'))
axis (side = 2)
axis (side = 3, at = h_points, labels = p_points)
box()
mtext ('parallele Produktanzahl $p$', side = 3, line = 2.5, cex = 0.66)

dev.off()
