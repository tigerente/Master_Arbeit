source("./Fallstudie_data.R")

#######################
#### Analyseteil 1 ####
#######################

#### Szenario Ind & Szenario Luhrmannhof ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Nutzungsintensivierung)
source("./Funktionen_Nutzungsintensivierung.R")

h_Ind = h(p = Szenario.Ind$p[1], N = N, T = Time)
h_Luh = h(p = Szenario.Luh$p[1], N = N, T = Time)

MIPS_Ind <- MIPS(h = h_Ind, I_fix = I_fix_h[1,], S_D = S_D[1], i_P = i_P[1,],
                 t_max = t_max[1], A = A[1], n_max = n_max[1])
MIPS_Luh <- MIPS(h = h_Luh, I_fix = I_fix_h[1,], S_D = S_D[1], i_P = i_P[1,],
                 t_max = t_max[1], A = A[1], n_max = n_max[1])

#### Szenario Luhrmannhof & Szenario Transport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Transport)
source("./Funktionen_Transporte.R")

MIPS_Luh <- MIPS_a(d = Szenario.Luh$d[1], theta = theta[1], i_d = i_d[1,], 
                   a = a[1], K = K[1], I_fix = I_fix_d[1,], S_D = S_D[1])
MIPS_Tra <- MIPS_a(d = Szenario.Tra$d[1], theta = theta[1], i_d = i_d[1,], 
                   a = a[1], K = K[1], I_fix = I_fix_d[1,], S_D = S_D[1])


#### Szenario Ind & Szenario Transport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Kopplung)


#######################
#### Analyseteil 2 ####
#######################
