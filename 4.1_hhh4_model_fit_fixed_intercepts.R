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
### Document: HHH4 MODEL FIT (from the original observation)
### Author: trangngpmd
### Date: 2024-10-09


# ---------------------------------------------------------#
# sts OBJECT (surveillance-time-series STS class) -----
# ---------------------------------------------------------#

sts_covid <- sts(observed = cases_sts, #from week 13-2020 to week 50-2022 (143 weeks)
                 start = c(2020, 13), frequency = 52,
                 neighbourhood = neighbor_adjusted,  #use the adjusuted neighbourhood matrix
                 map = map_sts, 
                 population = pop_country_mat)

# plot(sts_covid, type = observed ~ time)


# ---------------------------------------------------------#
# DEFINE COVARIATES -----
# ---------------------------------------------------------#

offset <- population(sts_covid)         # in END component
log_pop <- log(population(sts_covid))   # in AR, NE component

log_susceptible <- log(1 - vaccine_mat)
stringency <- str_index_lag2
variant_alpha <- variant_alpha_mat
variant_delta <- variant_delta_mat
variant_omicron <- variant_omicron_mat



# ---------------------------------------------------------#
# MODEL FORMULA -----
# ---------------------------------------------------------#

f_end <- ~ 1  +  log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ar <- ~ 1 + log_pop + stringency + log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ne <- ~ 1 + log_pop + stringency + variant_alpha + variant_delta + variant_omicron


# hhh4 data list
hhh4_datalist <- list(log_pop = log_pop,
                      log_susceptible = log_susceptible,
                      stringency = stringency, 
                      variant_alpha = variant_alpha,
                      variant_delta = variant_delta,
                      variant_omicron = variant_omicron
                      )

# Model set up 
lag_optimal <- 2
fit_start <- 3   
fit_end <- 143 


# -------------------------------------------------------------------#
# MODEL FIT WITH ORIGINAL DATA (COMPONENT-SPECIFIC FIXED INTERCEPTS) -----
# -------------------------------------------------------------------#

# Control list
control_fit <- list(
  end = list(f = f_end, offset = offset),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE, initial = c("logd" = log(2)))),
  optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
  start= list(fixed=c("ar.1"=-0.038, "ar.log_pop"=0.002,"ar.stringency"=0.001,
                      "ar.log_susceptible"=-0.053,"ar.variant_alpha"=-0.129,
                      "ar.variant_delta"=0.077, "ar.variant_omicron"=-0.180,
                      "ne.1"=-13, "ne.log_pop"=0.707, "ne.stringency"=-0.087,
                      "ne.variant_alpha"=-3.492, "ne.variant_delta"=-1.968,
                      "ne.variant_omicron"=-8.539, "end.1"=-19.107, "end.log_susceptible"=-0.172,
                      "end.variant_alpha"=-2.307, "end.variant_delta"=8.444,
                      "end.variant_omicron"=10.882, "neweights.logd"=0.018,'overdisp'=0.2)),
  family = "NegBin1",
  data = hhh4_datalist,
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, #continue finding the time-lag weights, which will be estimated from simulated datasets
  max_lag = lag_optimal)

fit_original <- profile_par_lag(sts_covid, control = control_fit)
fit_original$convergence #check convergence
summary(fit_original)

# save(fit_original, file = paste0(path_model_fit,"fit_original.rda"))


# ---------------------------------------------------------#
# EXPLORE THE ORIGINAL MODEL FIT -----
# ---------------------------------------------------------#
# fit_original <- get(load(paste0(path_model_fit,"fit_original.rda")))
fit <- fit_original

plot(fit, total =TRUE) #plot the fitted total cases by time
plot(fit, type = "fitted", 
     units = which(colSums(observed(sts_covid), na.rm = T) > 0), hide0s = TRUE)

plot(fit, type = "neweights") #normalized spatial weights


# Extract parameters
nterms <- terms(fit)$nGroups + 2 #add neweights decay & overdisp parameters
coefs <- coef(fit)[1:nterms]
CIs <- confint(fit)[1:nterms, ]
tab_original <- cbind(coefs, CIs)
tab_original <- as.data.frame(tab_original) %>% mutate(param = rownames(tab_original))
round(tab_original[,1:3],3)

# write_xlsx(tab_original, paste0(path_model_fit,"point_estimates_original.xlsx"))

# ---------------------------------------------------------#
# CALCULATE THE COMPONENT CONTRIBUTION -----
# ---------------------------------------------------------#
# fit_original <- get(load(paste0(path_model_fit,"fit_original.rda")))
fit <- fit_original

# Extract fitted mean by component
model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
mu_coefs <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)

# Percent component
sum(mu_coefs$mean)/sum(fitted.values(fit)) #check if it is the same
sum(mu_coefs$epi.own)/sum(mu_coefs$mean)*100
sum(mu_coefs$epi.neighbours)/sum(mu_coefs$mean)*100
sum(mu_coefs$endemic)/sum(mu_coefs$mean)*100
