##############################################
###### COVID-19 in EU/EEA 2020 - 2022 ########
##############################################


### MANUSCRIPT
### CAPTURING THE SPATIOTEMPORAL SPREAD OF COVID-19 IN 30 EUROPEAN COUNTRIES DURING 2020 - 2022

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium



# ====== GENERAL INFO ==========
### Document: FUNCTIONS
### Author: trangngpmd
### Date: 2024-10-08


# ====== Function for waning immunity of vaccination ==========
# These two functions are adapted from the paper:
# Bekker-Nielsen Dunbar M, Held L.
# The COVID-19 vaccination campaign in Switzerland and its impact on disease spread. Epidemics. 2024;47:100745.
# Paper link: https://www.sciencedirect.com/science/article/pii/S1755436524000069
# The codes: https://github.com/mariabnd/ee-vax/blob/main/lib/function-waning.R


## waning function
waning <- function(t, val){
  
  # Calculate downwards slope for waning time
  slope <- coef(lm(c(1, 0) ~ c(25, 38)))[[2]]
  adj <- ifelse(t <= 0, 0,
                ifelse(t == 1, 0.5,
                       ifelse(t > 1 & t <= 25, 1,
                              ifelse(t > 1 & t > 25 & t < 39, - 1 * (38 - t) * slope,
                                     ifelse(t > 1 & t >= 39, 0, 0)))))
  adj * val
}


## apply waning to data
apply_waning <- function(data_set, FUN,
                         name = c("csum_waned", "cov_waned")){
  
  # Create empty variable for populating
  data_set$tmp_csum_waned <- NA
  
  # Calculate the waned sum for each country
  for (c in unique(data_set$country)){
    
    # Extract the data for the group
    df <- data_set[data_set$country == c, ]
    dm <- dim(df)
    
    # Create an empty (triangle) matrix to be populated with the waned values of entries
    mat <- matrix(rep(NA, times = dm[1] * dm[1]), ncol = dm[1])
    colnames(mat) <- df$date_week
    
    for (i in 1 : dm[1]){
      # extract the values at each date, calculate the waning at that date
      mat[i, ] <- FUN(1 : dm[1] - i, df$newdose[i])
    }
    
    # sum across the date to obtain the cumulative sum of waned
    data_set[data_set$country == c, ]$tmp_csum_waned <- colSums(mat)
  }
  
  data_set$tmp_cov_waned <- data_set$tmp_csum_waned / data_set$population
  names(data_set)[which(names(data_set) == "tmp_csum_waned")] <- name[1]
  names(data_set)[which(names(data_set) == "tmp_cov_waned")] <- name[2]
  
  return(data_set)
}
