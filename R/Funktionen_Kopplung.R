###############
### MIPS(r) ###
###############

# MIPS in Abhängigkeit vom Radius des Einzugsgebietes
MIPS <- function(r, R, h_ind, h_gem, q_ind, q_gem, T, A, i_P, alpha, I_fix, S_D){
  return (q_ind*i_P / (A * h_ind * T) - q_ind*i_P * r^2 / (A * h_ind * T * R^2) + q_gem * i_P * r^2 / (A * h_gem * T * R^2) + alpha * 2/3 * r^3 / R^2 + I_fix / S_D)
}

###############
#### q(h) #####
###############

# Sequentielle Produktanzahl in Abhängigkeit von der Nutzungshäufigkeit
q <- function(h, n_max, t_max, T){
  return (ifelse (h < n_max / t_max, T/t_max, T*h/n_max))
}
