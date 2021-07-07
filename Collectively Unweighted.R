library(UBL)
library(kmed)
library(Rfast)
library(tidyverse)
library(dplyr)

# Get two dataframes: one with and one without the price
adressen = read.csv('adressen_price_final.csv')
adressen <- adressen[-1]

# Without transaction price (not needed to calculate distance matrices)
adressen_z <- adressen[-14]

### Gower
gower = distmix(adressen_z, method = "gower", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13), idbin = NULL,
        idcat = c(1, 4, 8))
colnames(gower) <- paste0("a", 1:ncol(gower))
gower = as.data.frame(gower)

### Huang
huang = distmix(adressen_z, method = "huang", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13), idbin = NULL,
                idcat = c(1, 4, 8))
colnames(huang) <- paste0("a", 1:ncol(huang))
huang = as.data.frame(huang)

### Ahmad & Dey
ahmad = distmix(adressen_z, method = "ahmad", idnum = c(2,3,5, 6, 7, 9, 10, 11, 12, 13), idbin = NULL,
                idcat = c(1, 4, 8))
colnames(ahmad) <- paste0("a", 1:ncol(ahmad))
ahmad = as.data.frame(ahmad)

## The Functions for finding the average price difference 
## for the 5 most similar objects.
gower_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(gower[order(gower[ , object])][2:6])
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

ahmad_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(ahmad[order(ahmad[ , object])][2:6])
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

huang_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  most_similar = colnames(huang[order(huang[ , object])][2:6])
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

random_prices_f <- function(object) {
  ref_price = adressen[object, 14]
  similar_indices_num = sample(1:nrow(adressen), 5, replace=F)
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

totaal_verschil = function(name_of_function){
  verschillen_total <- vector(mode = "list", length = 0)
  for(i in 1:nrow(adressen)) {       
    difference = name_of_function(i)
    verschillen_total = append(verschillen_total, difference)
  }
  gemiddeld_verschil = mean(unlist(verschillen_total))
  return(gemiddeld_verschil)
}

