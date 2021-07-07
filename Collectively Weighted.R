# After experimenting with many different sets of variables, 
# the following model was chosen.
adressen = read.csv('adressen_price_final.csv')
adressen <- adressen[-1]

adressen_weighted = adressen
adressen_weighted$Floor.Surface..GFA.2 <- adressen_weighted$Floor.Surface..GFA.
adressen_weighted$Floor.Surface..GFA.3 <- adressen_weighted$Floor.Surface..GFA.
adressen_weighted_z <- adressen_weighted[-14]


### Gower Weighted
gower_weigthed = distmix(adressen_weighted_z, method = "gower", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13, 14, 15), idbin = NULL,
                         idcat = c(1, 4, 8))
colnames(gower_weigthed) <- paste0("a", 1:ncol(gower_weigthed))
gower_weigthed = as.data.frame(gower_weigthed)

### Huang Weighted
huang_weigthed = distmix(adressen_weighted_z, method = "huang", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13, 14, 15), idbin = NULL,
                 idcat = c(1, 4, 8))
colnames(huang_weigthed) <- paste0("a", 1:ncol(huang_weigthed))
huang_weigthed = as.data.frame(huang_weigthed)

### Ahmad Weighted
ahmad_weigthed = distmix(adressen_weighted_z, method = "ahmad", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13, 14, 15), idbin = NULL,
                idcat = c(1, 4, 8))
colnames(ahmad_weigthed) <- paste0("a", 1:ncol(ahmad_weigthed))
ahmad_weigthed = as.data.frame(ahmad_weigthed)

#___________________________________________

gower_w_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(gower_weigthed[order(gower_weigthed[ , object])][2:6])
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

huang_w_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(huang_weigthed[order(huang_weigthed[ , object])][2:6])
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

ahmad_w_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(ahmad_weigthed[order(ahmad_weigthed[ , object])][2:6])
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

