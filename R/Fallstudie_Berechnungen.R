source("./Fallstudie_data.R")

#######################
#### Analyseteil 1 ####
#######################
source("./Funktionen_Nutzungsintensivierung.R")
source("./Funktionen_Transporte.R")

I_N     <- i_N[["values"]] * N

h_Ind = h(p = Szenario.Ind["p", "values"], N = N, T = Time)
h_Luh = h(p = Szenario.Luh["p", "values"], N = N, T = Time)
h_Tra = h_Luh

#### Szenario Ind ####
#~~~~~~~~~~~~~~~~~~~~~
MIPS_Ind <- I_N/S_D[1] +
          MIPS(h = rep(h_Ind, times=2), I_fix = 0, S_D = S_D[1], i_P = i_P[["values"]],
                 t_max = t_max[1], A = A[1], n_max = n_max[1]) +
          MIPS_a(d = Szenario.Ind["d", "values"], theta = theta[1], i_d = i_d[["values"]], 
                   a = a[1], K = K[1], I_fix = 0, S_D = S_D[1])

#### Szenario Luhrmannhof ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MIPS_Luh <- I_N/S_D[1] +
          MIPS(h = rep(h_Luh, times = 2), I_fix = 0, S_D = S_D[1], i_P = i_P[["values"]],
                 t_max = t_max[1], A = A[1], n_max = n_max[1]) +
          MIPS_a(d = Szenario.Luh["d", "values"], theta = theta[1], i_d = i_d[["values"]], 
                   a = a[1], K = K[1], I_fix = 0, S_D = S_D[1])

#### Szenario Transport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
MIPS_Tra_a <- I_N/S_D[1] +
            MIPS(h = rep(h_Tra, times = 2), I_fix = 0, S_D = S_D[1], i_P = i_P[["values"]],
                    t_max = t_max[1], A = A[1], n_max = n_max[1]) +
            MIPS_a(d = Szenario.Tra["d", "values"], theta = theta[1], i_d = i_d[["values"]], 
                    a = a[1], K = K[1], I_fix = 0, S_D = S_D[1])
MIPS_Tra_b <- I_N/S_D[1] +
            MIPS(h = rep(h_Tra, times = 2), I_fix = 0, S_D = S_D[1], i_P = i_P[["values"]],
                    t_max = t_max[1], A = A[1], n_max = n_max[1]) +
            MIPS_b(d = Szenario.Tra["d", "values"], i_d_M = i_d_M[["values"]], m_S = m_S[1], I_fix = 0, S_D = S_D[1])

print(MIPS_Ind)
print(MIPS_Luh)
print(MIPS_Tra_a)
print(MIPS_Tra_b)
#######################
#### Analyseteil 2 ####
#######################
