require(tikzDevice) # für den Export von Graphiken in tex-Files
#options(tikzLatexPackages = '')
tikzheight <- 4.2
tikzwidth <- 5.5
fntsize <- 0.8

# Konstanten Definition
Konstanten <- data.frame(
                        K     = 1, # Kapazität
                        I_fix = 1, # alle nicht variablen Inputs
                        D     = 1, # Nachfrage
                        T     = 1, # Zeitperiode des Nutzungssystems
                        P     = 1  # Anzahl Personen im Nutzungssystem
                        )

# Input pro Nutzungseinheit, abhängig von der relativen Auslastung:
i_N_max = 1
i_N_min = 0.5
i_N <- function(a){
  (i_N_max - i_N_min) * sqrt(a) + i_N_min
}

# MIPS in Abhängigkeit von der Auslastung (a = Vektor, const = Dataframe)
MIPS_aK <- function(a, const){
  attach(const)
  MIPS <- i_N(a)/(a*K) + I_fix/(D*T*P)
  detach(const)
  return(MIPS)
}
MIPS_a <- function(a){
  MIPS_aK(a,Konstanten)
}

#### Plots ####
a_min <- 0.1 # damit die y-Achse nicht zu groß skaliert ist
a <- seq(length = 100, from = a_min, to=1) # der Definitionsbereich

# 1) MIPS in Abhängigkeit von a

tikz( '../tex/Abbildungen/Auslastung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)
plot(a, MIPS_a(a), type="l", xlim=c(0,1), ylim=c(0,MIPS_a(a_min)),
     xlab = "Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     main = "Teilmodell: Erhöhung der Produktauslastung",
     cex = fntsize) 
#plot(a, MIPS_a(a)) 
dev.off()

