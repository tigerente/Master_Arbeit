require(tikzDevice) # für den Export von Graphiken in tex-Files
#options(tikzLatexPackages = '')
tikzheight <- 6.2
tikzwidth <- 5.5
fntsize <- 0.8

export <- FALSE # if figures should be exported, or just visualized

# Konstanten Definition
Konstanten <- data.frame(
                        K     = 1, # Kapazität
                        I_fix = 1, # alle nicht variablen Inputs
                        i_P   = 10, # Input für die Bereitstellung eines Produkts
                        S_D   = 1, # Nachfrage
                        n_max_const = 50,# falls N_max unabhängig von a ist
                        a_min = 0.1 # vorgegeben, um Konsistenz der Parameter zu
                                    # erreichen (müsste eigentlich ermittelt
                                    # werden)
                        )

# Nutzungsvorrat eines Produkts, abhängig von der relativen Auslastung:
n_max <- function(a){
  n_max_const
}

# Input pro Nutzungseinheit, abhängig von der relativen Auslastung:
# verschiedene Varianten sind möglich
# case == 1: i_N(a) = (i_N_max - i_N_min) * sqrt(a) + i_N_min
# case == 2: i_N(a) = (2*i_N_max - i_N_min) * a^3 + i_N_min
i_N_max = 1
i_N_min = 0.5
i_N <- function(a, case){
  if(case == 1)
    (i_N_max - i_N_min) * sqrt(a) + i_N_min
  else if(case == 2)
    (2*i_N_max - i_N_min) * a^3 + i_N_min
  else 
    stop("undefined i_N-function, currently only case 1 and 2 are defined.")
}

# Anzahl Geräte, abhängig von der relativen Auslastung:
# verschiedene Varianten sind möglich
# case == 1: p(a) = konst. = S/(a_min*K*n_max(a_min))
# case == 2: p(a) = Mindestanzahl notwendiger Produkte = S/(a*K*n_max(a))
P <- function(a, case){
  if(case == 1)
    S_D/(a_min*K*n_max(a_min))
  else if(case == 2)
    S_D/(a*K*n_max(a))
  else 
    stop("undefined p-function, currently only case 1 and 2 are defined.")
}


# MIPS in Abhängigkeit von der Auslastung (a = Vektor, const = Dataframe)
MIPS_aK <- function(a, case_i_N, case_p, const){
  attach(const)
  MIPS <- i_N(a, case_i_N)/(a*K) + I_fix/S_D + P(a, case = case_p)*i_P/S_D
  detach(const)
  return(MIPS)
}
MIPS_a <- function(a, ...){
  MIPS_aK(a, const=Konstanten, ...)
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
vec_a_1 = MIPS_a(a, case_i_N = 1, case_p = 1)
plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize) 
vec_a_2 = MIPS_a(a, case_i_N = 1, case_p = 2)
points(a, vec_a_2, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_2),rev(pmin(vec_a_1, vec_a_2))),
        col="lightgray")

# i_N(a), Fall 2
plot(a, i_N(a, case=2), type="l", xlim=c(0,1), ylim=c(0,i_N(1,2)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "Material-Inputs je Produktnutzung $i_N(a)$",
     cex = fntsize) 

# MIPS(a), Fall 2
vec_a_1 = MIPS_a(a, case_i_N = 2, case_p = 1)
plot(a, vec_a_1, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_1)),
     xlab = "Relative Produktauslastung $a$",
     ylab = "$\\text{MIPS}_a$",
     cex = fntsize) 
vec_a_2 = MIPS_a(a, case_i_N = 2, case_p = 2)
points(a, vec_a_2, type="l", xlim=c(0,1), ylim=c(0,max(vec_a_2)))
polygon(c(a, rev(a)), c(pmax(vec_a_1, vec_a_2),rev(pmin(vec_a_1, vec_a_2))),
        col="lightgray")

if(export) dev.off()

