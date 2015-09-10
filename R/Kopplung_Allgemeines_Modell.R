# für den Export von Graphiken in tex-Files
require(tikzDevice)
require(manipulate)

# Einstellungen für den Export der Graphik
tikzheight <- 3.6
tikzwidth <- 5.8
fntsize <- 0.8

# Export aktivieren
useTikz <- TRUE
#useTikz <- FALSE

# Konstanten
i_P <- 5000
t_max <- 15
n_max <- 5000
A <- 1
h_herk <- 200
T_ <- 0.5
  
# Zwischengrößen
h_stern <- n_max / t_max

# Zielgröße: kritische Distanz
d = function (h_gem){
  d_eval = ifelse(h_gem < h_stern, (i_P/(T_ * t_max * A)) * (1/h_herk - 1/h_gem), (i_P/(T_ * A)) * (1/(h_herk*t_max) - 1/n_max))
  return(d_eval)
}


#### Plot d~p ####
h_max <- 500
h_gem <- seq(from = h_herk, to = h_max, length.out = 200)
d_dach <- d(h_gem)

if(useTikz) tikz( '../tex/Abbildungen/Kopplung_Allgemein.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

par(mar=c(2.2,2,3.6,1.2), mgp=c(1,0.6,0))

plot(h_gem, d_dach, ylim=c(0,2.5), type="l", axes=FALSE, 
     xlab='Nutzungsh\"aufigkeit $h^\\text{gem}$ [NE/Jahr]', 
     ylab="zus\"atzliche Transportdistanz $\\Delta d$ [km]",  
     panel.first = polygon(c(h_gem, rev(h_gem)), 
                           c(d_dach, rep(2.5,times=length(d_dach))), 
                           col="gray90", border = "gray90")
     )
lines(x = c(h_stern, h_stern), y=c(0,2.5), lty = 3)
axis(side=1, pos=0, at=c(seq(h_herk,h_max, by=100), h_stern), labels=c('$h^\\text{herk}$', seq(h_herk + 100, h_max, by=100),'$h^*$'))
axis(side=2, pos=h_herk)
lines(x=c(200,500,500), y=c(2.5,2.5,0))

text(490,1.4, "$\\hat{d}$")
text(350,1.95,"$-$", cex=4, col="grey30")
text(350,0.65,"$+$", cex=4, col="grey30")

# Maintitle:
title("MIPS-Reduktion -- Kopplungsmodell", line=2.5, cex.main=1.2)
# Subtitle:
title("Erh\"ohung der Nutzungsh\"aufigkeit und zus\"atzliche Transporte", line=0.5, cex.main=1.0)

if(useTikz) dev.off()
