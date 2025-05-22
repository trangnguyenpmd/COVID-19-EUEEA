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
### Document: HHH4 MODEL FIT (from the original observation) WITH RANDOM INTERCEPTS
### Author: trangngpmd
### Date: 2024-04-29


# ---------------------------------------------------------#
# sts OBJECT (surveillance-time-series STS class) -----
# ---------------------------------------------------------#

sts_covid <- sts(observed = cases_sts, #from week 13-2020 to week 50-2022 (143 weeks)
                 start = c(2020, 13), frequency = 52,
                 neighbourhood = neighbor_adjusted,  #use the adjusuted neighbourhood matrix
                 map = map_sts, population = pop_country_mat)

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
# MODEL FORMULA FROM THE FIT_ORIGINAL -----
# (component-specific fixed intercepts)
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


# Model set up & control list
lag_optimal <- 2
fit_start <- 3   
fit_end <- 143 


# ---------------------------------------------------------#
# MODEL FIT WITH ORIGINAL DATA, UPDATE WITH RANDOM INTERCEPTS-----
# ---------------------------------------------------------#
fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) #original fitted model with component-specific fixed intercepts

# Random intercepts are included in all 3 components
fit <- update(fit_original,
              end = list(f = update(formula(fit_original)$end, ~. + ri() - 1)),
              ar = list(f = update(formula(fit_original)$ar, ~. + ri() - 1)),
              ne = list(f = update(formula(fit_original)$ne, ~. + ri() - 1)),
              use.estimates = FALSE,
              start = list(fixed=c("ar.ri(iid)"=-0.1657, "ar.log_pop"=0.0011,"ar.stringency"=0.0013,
                                   "ar.log_susceptible"=-0.0598,"ar.variant_alpha"=-0.0133,
                                   "ar.variant_delta"= 0.1462, "ar.variant_omicron"=-0.0831,
                                   "ne.ri(iid)"=-18.2257, "ne.log_pop"=1.0733, "ne.stringency"=-0.0897,
                                   "ne.variant_alpha"=-4.5731, "ne.variant_delta"=-2.3926,
                                   "ne.variant_omicron"=-7.4414, "end.ri(iid)"=-21.3839, "end.log_susceptible"=0.1603,
                                   "end.variant_alpha"=5.5582, "end.variant_delta"=10.6406,
                                   "end.variant_omicron"=13.2520, "neweights.logd"=-0.6271,'overdisp'=0.2)))


fit$convergence #check convergence
summary(fit)

# save(fit, file = paste0(path_model_fit,"fit_original_ri_ARNEEND.rda"))


# ---------------------------------------------------------#
# EXPLORE THE ORIGINAL MODEL FIT WITH RANDOM INTERCEPTS -----
# ---------------------------------------------------------#
fit <- get(load(paste0(path_model_fit,"fit_original_ri_ARNEEND.rda"))) 

# Plot check
# ---------------------------------#
plot(fit, total =TRUE) #plot the fitted total cases by time
plot(fit, type = "fitted", 
     units = which(colSums(observed(sts_covid), na.rm = T) > 0), hide0s = TRUE)

plot(fit, type = "neweights") #normalized spatial weights


# Extract parameters
# ---------------------------------#
nterms <- terms(fit)$nGroups + 2 #add neweights decay & overdisp parameters
coefs <- coef(fit)[1:nterms]
CIs <- confint(fit)[1:nterms, ]
tab_original <- cbind(coefs, CIs)
tab_original <- as.data.frame(tab_original) %>% mutate(param = rownames(tab_original))
round(tab_original[,1:3],4)

# write_xlsx(tab_original, paste0(path_model_fit,"point_estimates_original_ri_ARNEEND.xlsx"))


# PLOT RANDOM INTERCEPTS
# ---------------------------------#
# random_intercepts <- as.data.frame(ranef(fit, tomatrix = TRUE))
# random_intercepts$country <- rownames(random_intercepts)
# colnames(random_intercepts) <- c("ar_ri", "ne_ri", "end_ri", "country")
# summary(random_intercepts)
# 
# # write_xlsx(random_intercepts, paste0(path_model_fit,"random intercepts_ARNEEND.xlsx"))
# 
# 
# map_europe %>%
#   left_join(random_intercepts, by="country") %>%
#   ggplot() +
#   geom_sf(aes(fill = ne_ri),     # remmember adjust the column for each component
#           size = 0.15, color = "black") +
#   geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
#   scale_fill_gradient2(low = "#0081a7", mid="#ffffff", high = "#fb5607", limits = c(-3,3)) +
#   labs(x="", y="", fill = "value", title = "Neighbourhood random intercepts") +
#   theme_bw() 
# 
# ggsave(filename = "map_random intercepts_ARNEEND_ne.png",   # remmember adjust the name for each component
#        path = paste0(path_plot, "model_fit_plot/"),
#        units = "in",  width = 7, height = 7, 
#        device="png", dpi=600)

# ---------------------------------------------------------#
# CALCULATE THE COMPONENT CONTRIBUTION -----
# ---------------------------------------------------------#
# fit <- get(load(paste0(path_model_fit,"fit_original_ri_ARNEEND.rda"))) 

# Extract fitted mean by component
model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
mu_coefs <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)

# Percent component
sum(mu_coefs$mean)/sum(fitted.values(fit)) #check if it is the same
sum(mu_coefs$epi.own)/sum(mu_coefs$mean)*100
sum(mu_coefs$epi.neighbours)/sum(mu_coefs$mean)*100
sum(mu_coefs$endemic)/sum(mu_coefs$mean)*100



