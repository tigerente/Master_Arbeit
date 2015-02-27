require(tikzDevice) # für den Export von Graphiken in tex-Files
#options(tikzLatexPackages = '')
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8

export <- TRUE # if figures should be exported, or just visualized

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
i_N <- function(a, case){
  if(case == 1)
    (i_N_max - i_N_min) * sqrt(a) + i_N_min
  else if(case == 2)
    (2*i_N_max - i_N_min) * a^3 + i_N_min
  else 
    stop("undefined i_N-Funktion, currently only case 1 and 2 are defined.")
}

# Anzahl Geräte, abhängig von der relativen Auslastung:

# MIPS in Abhängigkeit von der Auslastung (a = Vektor, const = Dataframe)
MIPS_aK <- function(a, case, const){
  attach(const)
  MIPS <- i_N(a, case)/(a*K) + I_fix/(D*T*P)
  detach(const)
  return(MIPS)
}
MIPS_a <- function(a, case){
  MIPS_aK(a, case=case, const=Konstanten)
}

#### Plots ####
a_min <- 0.1 # damit die y-Achse nicht zu groß skaliert ist
a <- seq(length = 100, from = a_min, to=1) # der Definitionsbereich

# Plot mit vier Teilplots: Spalten sind unterschiedliche Fälle
# (unterschiedliche i_N-Funktionen), erste Spalte ist die i_N-Funktion
# dargestellt, zweite Spalte MIPS in Abhängigkeit von a

if(export) tikz( '../tex/Abbildungen/Auslastung.tex', packages=c('\\usepackage{tikz}','\\usepackage{amsmath}'), width=tikzwidth, height=tikzheight)

par(mfcol=c(2,2))
# i_N(a), Fall 1
plot(a, i_N(a, case=1), type="l", xlim=c(0,1), ylim=c(0,i_N(1,1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize) 

# MIPS(a), Fall 1
plot(a, MIPS_a(a, case=1), type="l", xlim=c(0,1), ylim=c(0,MIPS_a(a_min,1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize) 

# i_N(a), Fall 2
plot(a, i_N(a, case=2), type="l", xlim=c(0,1), ylim=c(0,i_N(1,2)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize) 

# MIPS(a), Fall 2
plot(a, MIPS_a(a, case=2), type="l", xlim=c(0,1), ylim=c(0,MIPS_a(a_min,2)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize) 

if(export) dev.off()

