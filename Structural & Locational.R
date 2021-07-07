Structural & Locational variables only

# Get two dataframes: one with and one without the price
adressen = read.csv('adressen_price_final.csv')
adressen <- adressen[-1]

# Without transaction price (not needed to calculate distance matrices)
adressen_z <- adressen[-14]

# Only the structural variables:
adressen_struct <- adressen_z
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]
adressen_struct <- adressen_struct[-5]

# Only the locational variables:
adressen_locat <- adressen_z[-3]
adressen_locat <- adressen_locat[-2]
adressen_locat <- adressen_locat[-1]
adressen_locat <- adressen_locat[-1]

#__________________________________________________________

### Gower Structural
gower_struc = distmix(adressen_struct, method = "gower", idnum = c(2,3), idbin = NULL,
                      idcat = c(1, 4))
colnames(gower_struc) <- paste0("a", 1:ncol(gower_struc))
gower_struc = as.data.frame(gower_struc)

### Gower Locational
gower_loc = distmix(adressen_locat, method = "gower", idnum = c(3, 4, 5, 7, 8, 9), idbin = NULL,
                    idcat = c(4))
colnames(gower_loc) <- paste0("a", 1:ncol(gower_loc))
gower_loc = as.data.frame(gower_loc)

### Huang Structural
huang_struc = distmix(adressen_struct, method = "huang", idnum = c(2,3), idbin = NULL,
                      idcat = c(1, 4))
colnames(huang_struc) <- paste0("a", 1:ncol(huang_struc))
huang_struc = as.data.frame(huang_struc)

### Huang Locational
huang_loc = distmix(adressen_locat, method = "huang", idnum = c(3, 4, 5, 7, 8, 9), idbin = NULL,
                    idcat = c(4))
colnames(huang_loc) <- paste0("a", 1:ncol(huang_loc))
huang_loc = as.data.frame(huang_loc)

### Ahmad Structural
ahmad_struc = distmix(adressen_struct, method = "ahmad", idnum = c(2,3), idbin = NULL,
                      idcat = c(1, 4))
colnames(ahmad_struc) <- paste0("a", 1:ncol(ahmad_struc))
ahmad_struc = as.data.frame(ahmad_struc)

### Ahmad Locational
ahmad_loc = distmix(adressen_locat, method = "ahmad", idnum = c(3, 4, 5, 7, 8, 9), idbin = NULL,
                    idcat = c(4))
colnames(ahmad_loc) <- paste0("a", 1:ncol(ahmad_loc))
ahmad_loc = as.data.frame(ahmad_loc)

#___________________________________________________________________


gower_loc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(gower_loc[order(gower_loc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}

gower_struc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(gower_struc[order(gower_struc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}

huang_loc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(huang_loc[order(huang_loc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}

huang_struc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(huang_struc[order(huang_struc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}

ahmad_loc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(ahmad_loc[order(ahmad_loc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}

ahmad_struc_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(ahmad_struc[order(ahmad_struc[ , object])][2:6])
  similar_indices = gsub("[a-zA-Z ]", "", most_similar)
  similar_indices_num = as.numeric(similar_indices)
  verschillen <- vector(mode = "list", length = 0)
  for (i in similar_indices_num){
    price = adressen[i, 14]
    diff_price = (ref_price - price)^2
    diff_price2 = sqrt(diff_price)
    verschillen = append(verschillen, diff_price2)
  }
  mean_diff = mean(unlist(verschillen))
  return(mean_diff)
}
