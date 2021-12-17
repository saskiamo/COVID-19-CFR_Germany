# COVID-19 fatality in Germany: 
# Demographic determinants of variation in case-fatality rates across 
# and within German federal states during the first and second waves

# Last updated: 2021-10-07 

# Contact:
# morwinsky@demogr.mpg.de
# acosta@demogr.mpg.de

# Note
# ~~~~
# Some of these functions come from the code of: 
# Dudel et al. (2020) "Monitoring trends and differences in COVID-19 
# case fatality  rates using decomposition methods: A demographic perspective" 


### Load packages 
library(tidyverse)
library(data.table)
library(writexl)
library(httr)
library(readxl)
library(ISOweek)


# Case fatality rate 
# ~~~~~~~~~~~~~~~~~~
# cc = case-age distribution
# rr = age-specific case fatality rates
cfr <- function(cc,rr){
  sum(cc * rr)
}


# Kitagawa decomposition 
# ~~~~~~~~~~~~~~~~~~~~~~

# c1 = Age distribution population 1
# r1 = Case fatality rates population 1
# c2 = Age distribution population 2
# r2 = Case fatality rates population 2

kitagawa_cfr <- function(c1, r1, c2, r2){
  
  # Calculate age-distribution of cases
  c1  <- c1 / sum(c1)
  c2  <- c2 / sum(c2)
  
  # Total difference
  Tot <- cfr(c1, r1) - cfr(c2, r2)
  
  # Age component
  Aa  <- sum((c1 - c2) * (r1 + r2) / 2)
  
  # Case fatality component
  Bb  <- sum((r1 - r2) * (c1 + c2) / 2)
  
  # Output
  list(Diff = Tot, 
       AgeComp = Aa,
       RateComp = Bb, 
       CFR1 = weighted.mean(r1,c1), 
       CFR2 = weighted.mean(r2,c2))
}




# ~~~~~~~~~~~~~~~~~
# Horiuchi functions
# ~~~~~~~~~~~~~~~~~

CFR3 <- function(Pcx, cx, cfrx){
  sum(Pcx * cx * cfrx) / 
    sum(Pcx * cx)
}

CFR3_vec <- function(pars_vec){
  PARS <- matrix(pars_vec, ncol = 3)
  CFR3(PARS[,1],PARS[,2],PARS[,3])
}

apply_horiuchi_comp <- function(cas1, dea1, pop1, cas2, dea2, pop2){
  
  # age-distribution of population, incidence and CFRs
  Pcx1  <- pop1 / sum(pop1)
  cx1   <- cas1 / pop1
  cfrx1 <- dea1 / cas1
  
  Pcx2  <- pop2 / sum(pop2)
  cx2   <- cas2 / pop2
  cfrx2 <- dea2 / cas2
  
  dec <- horiuchi(CFR3_vec, 
                  pars1 = c(Pcx1, cx1, cfrx1),
                  pars2 = c(Pcx2, cx2, cfrx2),
                  N = 50)
  
  dim(dec) <- c(6, 3)
  result_h <- tibble(h_dCFR = sum(colSums(dec)), 
                     h_pop_c = colSums(dec)[1], 
                     h_alpha = colSums(dec)[2], 
                     h_beta = colSums(dec)[3])
  
  return(result_h)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Horiuchi function from Tim Riffe's package DemoTools
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
horiuchi <- function (func, pars1, pars2, N, ...) {
  y1 <- func(pars1, ...)
  y2 <- func(pars2, ...)
  d <- pars2 - pars1
  n <- length(pars1)
  delta <- d/N
  x <- pars1 + d * matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, 
                          ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  zeros <- rep(0, n)
  for (j in 1:N) {
    DD <- diag(delta/2)
    for (i in 1:n) {
      cc[i, j] <- func((x[, j] + DD[, i]), ...) - func((x[, 
                                                          j] - DD[, i]), ...)
    }
  }
  return(rowSums(cc))
}

# ~~~~~~~~~~~~~~~~~
# Kitagawa function
# ~~~~~~~~~~~~~~~~~

apply_kitagawa_comp <- function(cas1, dea1, cas2, dea2){
  
  # age distribution of cases
  cx1 <- cas1 / sum(cas1)
  cx2 <- cas2 / sum(cas2)
  # age-specific case fatality rates
  cfrx1 <- dea1 / cas1
  cfrx2 <- dea2 / cas2
  # differences and averages
  d_C = cx2 - cx1
  d_CFR = cfrx2 - cfrx1
  # averages of age-specific case distributions and CFRs
  a_C = (cx2 + cx1) * 0.5
  a_CFR = (cfrx2 + cfrx1) * 0.5
  # components
  alpha = sum(d_C * a_CFR)
  beta = sum(a_C * d_CFR)
  
  result_k <- tibble(k_dCFR = alpha + beta, 
                     k_pop_c = 0, 
                     k_alpha = alpha, 
                     k_beta = beta)
  
  return(result_k)
}


# Daily population interpolation function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interpop <- function(db){
  ys <- db %>% drop_na() %>% pull(t)
  ps <- db %>% drop_na() %>% pull(Pop)
  ds <- 1:n_dates
  # cubic interpolation
  md2 <- splinefun(x = ys, y = ps, method="fmm",  ties = mean)
  inter_pop <- tibble(t = ds,
                      Pop2 = md2(ds))
  return(inter_pop)
}



