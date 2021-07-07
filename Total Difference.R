# The function that calculates the differences for the data set as a whole
# and for subsets of the data
totaal_verschil = function(name_of_function){
  verschillen_total <- vector(mode = "list", length = 0)
  verschillen_groot <- vector(mode = "list", length = 0)
  verschillen_klein <- vector(mode = "list", length = 0)
  verschillen_dorp <- vector(mode = "list", length = 0)
  verschillen_stad <- vector(mode = "list", length = 0)
  verschillen_old <- vector(mode = "list", length = 0)
  verschillen_new <- vector(mode = "list", length = 0)
  for(i in 1:nrow(adressen)) {
    if (adressen[i,'Floor.Surface..GFA.'] > 110) {
      difference_g = name_of_function(i)
      verschillen_groot = append(verschillen_groot, difference_g)
    }
    else {
      difference_k = name_of_function(i)
      verschillen_klein = append(verschillen_klein, difference_k)
    }
    if (adressen[i,'Urbanity'] < 2.5) {
      difference_s = name_of_function(i)
      verschillen_stad = append(verschillen_groot, difference_s)
    }
    else {
      difference_d = name_of_function(i)
      verschillen_dorp = append(verschillen_dorp, difference_d)
    }
    if (adressen[i,'Construction.Year'] > 1960) {
      difference_n = name_of_function(i)
      verschillen_new = append(verschillen_new, difference_n)
    }
    else {
      difference_o = name_of_function(i)
      verschillen_old = append(verschillen_old, difference_o)
    }
    difference_t = name_of_function(i)
    verschillen_total = append(verschillen_total, difference_t)
  }
  gemiddeld_verschil_t = mean(unlist(verschillen_total))
  gemiddeld_verschil_g = mean(unlist(verschillen_groot))
  gemiddeld_verschil_k = mean(unlist(verschillen_klein))
  gemiddeld_verschil_s = mean(unlist(verschillen_stad))
  gemiddeld_verschil_d = mean(unlist(verschillen_dorp))
  gemiddeld_verschil_n = mean(unlist(verschillen_new))
  gemiddeld_verschil_o = mean(unlist(verschillen_old))
  print(paste0("Mean Difference of All REO's: ", gemiddeld_verschil_t))
  print(paste0("Mean Difference of Large REO's: ", gemiddeld_verschil_g))
  print(paste0("Mean Difference of Small REO's: ", gemiddeld_verschil_k))
  print(paste0("Mean Difference of Urban REO's: ", gemiddeld_verschil_s))
  print(paste0("Mean Difference of Rural REO's: ", gemiddeld_verschil_d))
  print(paste0("Mean Difference of New REO's: ", gemiddeld_verschil_n))
  print(paste0("Mean Difference of Old REO's: ", gemiddeld_verschil_o))
}

# For all objects in the data set, calculate the 5 most similar objects 
# and then calculate the price differences between them and take the average.
totaal_verschil(gower_prices_f)
totaal_verschil(huang_prices_f)
totaal_verschil(ahmad_prices_f)
totaal_verschil(random_prices_f)
totaal_verschil(gower_loc_prices_f)
totaal_verschil(gower_struc_prices_f)
totaal_verschil(huang_loc_prices_f)
totaal_verschil(huang_struc_prices_f)
totaal_verschil(ahmad_loc_prices_f)
totaal_verschil(ahmad_struc_prices_f)
totaal_verschil(gower_w_prices_f)
totaal_verschil(huang_w_prices_f)
totaal_verschil(ahmad_w_prices_f)

