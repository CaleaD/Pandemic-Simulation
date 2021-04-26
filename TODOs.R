#TODOS from different files
#Check to see if they have already been implemented

#from Models.Types(1).R
### TODO:
transmit = function(id, m, it, p) {
  # a more efficient implementation
  isI = (m[,id] >= it);
  isS = (m[,id] == 0);
  canI = (isS & c(FALSE, head(isI, -1)));
  canI = (c(FALSE, tail(isS, -1)) & isI);
}

#from SIR_DEATH_COMPARISON
### TODO:
# - comparisons in Mortality & Hospitalization
#   vs different Vaccination rates;
#from SIR_Hosp_Comparison
### TODO:
# - comparisons in Mortality & Hospitalization
#   vs different Vaccination rates;
