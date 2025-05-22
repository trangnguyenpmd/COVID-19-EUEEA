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
### Document: PLOT SIMULATION (SCENARIO_1-7)
### Author: trangngpmd
### Date: 2024-05-28


## Set up dates
date_start_plot <- as.Date("2020-02-01")
date_end_plot <- as.Date("2023-02-01")

for (i in 1:7){
# Import data
scenario <- i

fit_list <- readRDS(paste0(path_simulation,"fit_list_scenario_",scenario,".rds"))
fit_summary <- readRDS(paste0(path_simulation,"fit_summary_scenario_",scenario,".rds"))
param_array <- readRDS(paste0(path_simulation,"param_array_scenario_",scenario,".rds"))

# Import data
component_extracted <- readRDS(paste0(path_simulation_results,"component_extracted_scenario_",scenario,".rds"))
component_extracted_paramtrue <- readRDS(paste0(path_simulation_results,"component_extracted_paramtrue_scenario_",scenario,".rds"))
component_bycountry_array <- readRDS(paste0(path_simulation_results,"component_extracted_bycountry_scenario_",scenario,".rds"))
component_bycountry_array_true <- readRDS(paste0(path_simulation_results,"component_extracted_bycountry_paramtrue_scenario_",scenario,".rds"))


# ============== COMPONENT CONTRIBUTION ==========================#
# ================================================================#
# -------------------------------------#
# ALL COUNTRIES -----
# -------------------------------------#

# 100 simulated datasets
aa <- component_extracted %>%  filter(!model=="fit_original") %>%
  select(date_week, model, obs) %>%
  group_by(date_week) %>%
  mutate(median = median(obs),
         upper = quantile(obs,probs = 0.975),
         lower = quantile(obs,probs = 0.025)) %>% ungroup()

plot1 <- ggplot() +
  geom_line(data=aa,aes(x=date_week, y=obs,group=model, color="simulation"), linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median, group=model, color="simulation"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper, group=model, color="simulation"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower, group=model,color="simulation"), linewidth=0.6, linetype = "dashed") +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle(paste0("(A) 100 simulated epicurves - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position = "top") +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,9000000)) +
  scale_color_manual(name="",
                     values=c("simulation"="red"),
                     labels = c("Simulated data"))
plot1

# Percentage (all countries)
aa <- component_extracted %>% filter(!model=="fit_original") %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100)  %>%
  group_by(date_week) %>%
  mutate(median_ar = median(percent_ar, na.rm = T),
         upper_ar = quantile(percent_ar,probs = 0.975,na.rm = T),
         lower_ar = quantile(percent_ar,probs = 0.025,na.rm = T),
         median_ne = median(percent_ne,na.rm = T),
         upper_ne = quantile(percent_ne,probs = 0.975,na.rm = T),
         lower_ne = quantile(percent_ne,probs = 0.025,na.rm = T),
         median_end = median(percent_end,na.rm = T),
         upper_end = quantile(percent_end,probs = 0.975,na.rm = T),
         lower_end = quantile(percent_end,probs = 0.025,na.rm = T)) %>% ungroup()


plot2 <- ggplot() +
  #Autoregressive
  geom_line(data=aa, aes(x=date_week, y=percent_ar, group=model), color="#0077b6", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ar, group=model, color="Sim_AR"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") +
  #Endemic
  geom_line(data=aa, aes(x=date_week, y=percent_end, group=model), color="#00f5d4", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_end, group=model, color="Sim_END"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") +
  #Neighbourhood
  geom_line(data=aa, aes(x=date_week, y=percent_ne, group=model),color="#f15bb5", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ne, group=model, color="Sim_NE"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") +
  
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("(B) Contribution (%) from fitted models") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  scale_color_manual(name="",
                     breaks=c("Sim_AR", "Sim_END", "Sim_NE"),
                     values=c("Sim_AR"="#0077b6", 
                              "Sim_END"="#00f5d4", 
                              "Sim_NE"="#f15bb5"),
                     labels = c("Autoregressive",
                                "Endemic", 
                                "Neighbourhood")) + 
  theme(legend.position = "top")
plot2

# Percentage FROM TRUE (all countries)
aa <- component_extracted_paramtrue %>% filter(!model=="fit_original") %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100)  %>%
  group_by(date_week) %>%
  mutate(median_ar = median(percent_ar, na.rm = T),
         upper_ar = quantile(percent_ar,probs = 0.975,na.rm = T),
         lower_ar = quantile(percent_ar,probs = 0.025,na.rm = T),
         median_ne = median(percent_ne,na.rm = T),
         upper_ne = quantile(percent_ne,probs = 0.975,na.rm = T),
         lower_ne = quantile(percent_ne,probs = 0.025,na.rm = T),
         median_end = median(percent_end,na.rm = T),
         upper_end = quantile(percent_end,probs = 0.975,na.rm = T),
         lower_end = quantile(percent_end,probs = 0.025,na.rm = T)) %>% ungroup()


plot3 <- ggplot() +
  #Autoregressive
  geom_line(data=aa, aes(x=date_week, y=percent_ar, group=model), color="#0077b6", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ar, group=model, color="Sim_AR"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ar, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") +
  #Endemic
  geom_line(data=aa, aes(x=date_week, y=percent_end, group=model), color="#00f5d4", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_end, group=model, color="Sim_END"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_end, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") +
  #Neighbourhood
  geom_line(data=aa, aes(x=date_week, y=percent_ne, group=model),color="#f15bb5", linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median_ne, group=model, color="Sim_NE"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower_ne, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") +
  
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("(C) Contribution (%) from the true parameter estimates") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  scale_color_manual(name="",
                     breaks=c("Sim_AR", "Sim_END", "Sim_NE"),
                     values=c("Sim_AR"="#0077b6", 
                              "Sim_END"="#00f5d4", 
                              "Sim_NE"="#f15bb5"),
                     labels = c("Autoregressive",
                                "Endemic", 
                                "Neighbourhood")) + 
  theme(legend.position = "top")
plot3

# Autoregressive (fitted cases)
aa <- component_extracted %>%  filter(!model=="fit_original") %>%
  select(date_week, model, ar) %>%
  group_by(date_week) %>%
  mutate(median = median(ar, na.rm = T),
         upper = quantile(ar,probs = 0.975, na.rm = T),
         lower = quantile(ar,probs = 0.025, na.rm = T)) %>% ungroup()

plot4 <- ggplot() +
  geom_line(data=aa, aes(x=date_week, y=ar, group=model, color="Sim_AR"), linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median, group=model, color="Sim_AR"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower, group=model, color="Sim_AR"), linewidth=0.6, linetype = "dashed") +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle("(D) The autoregressive component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name="Estimate from",
                     breaks=c("Sim_AR"),
                     values=c("Sim_AR"="#0077b6"),
                     labels = c("AR - Simulated data")) + 
  theme(legend.position = "none")
plot4

# Neighbourhood (fitted cases)
aa <- component_extracted %>%  filter(!model=="fit_original") %>%
  select(date_week, model, ne) %>%
  group_by(date_week) %>%
  mutate(median = median(ne, na.rm = T),
         upper = quantile(ne,probs = 0.975, na.rm = T),
         lower = quantile(ne,probs = 0.025, na.rm = T)) %>% ungroup()

plot5 <- ggplot() +
  geom_line(data=aa, aes(x=date_week, y=ne, group=model, color="Sim_NE"), linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median, group=model, color="Sim_NE"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower, group=model, color="Sim_NE"), linewidth=0.6, linetype = "dashed") +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle("(E) The neighbourhood component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name="Estimate from",
                     breaks=c("Sim_NE"),
                     values=c("Sim_NE"="#f15bb5"),
                     labels = c("NE - Simulated data")) + 
  theme(legend.position = "none")
plot5

# Endemic (fitted cases)
aa <- component_extracted %>%  filter(!model=="fit_original") %>%
  select(date_week, model, end) %>%
  group_by(date_week) %>%
  mutate(median = median(end, na.rm = T),
         upper = quantile(end,probs = 0.975, na.rm = T),
         lower = quantile(end,probs = 0.025, na.rm = T)) %>% ungroup()

plot6 <- ggplot() +
  geom_line(data=aa, aes(x=date_week, y=end, group=model, color="Sim_END"), linewidth=0.2, alpha=0.2) +
  geom_line(data=aa, aes(x=date_week, y=median, group=model, color="Sim_END"), linewidth=0.6) + 
  geom_line(data=aa, aes(x=date_week, y=upper, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") + 
  geom_line(data=aa, aes(x=date_week, y=lower, group=model, color="Sim_END"), linewidth=0.6, linetype = "dashed") +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle("(F) The endemic component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="6 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name="Estimate from",
                     breaks=c("Sim_END"),
                     values=c("Sim_END"="#00f5d4"),
                     labels = c("END - Simulated data")) + 
  theme(legend.position = "none")



png(paste0(path_plot,"simulation_plot/fit_model_scenario_",scenario,"_median.png"), units = "in",
    width = 15, height = 8, res = 600)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             layout_matrix = matrix(c(1, 1, 2, 2, 3,3, 
                                      4,4, 5,5, 6,6), nrow = 2, byrow = TRUE))
dev.off()



# -------------------------------------#
# BY COUNTRY -----
# -------------------------------------#

# All three components by country (fitted cases by country)
aa <- as.data.frame.table(component_bycountry_array[,,"obs",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, obs=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa,
            aes(x=date_week, y=obs, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle(paste0("100 simulated epicurves - Scenario ",scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="red"),
                     labels = c("Simulated data"))


ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_epicurves.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)



# Autoregressive (fitted cases by country)
aa <- as.data.frame.table(component_bycountry_array[,,"ar",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ar=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa,
            aes(x=date_week, y=ar, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle(paste0("The autoregressive component - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#0077b6"),
                     labels = c("AR - Simulated data"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_ar.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)



# Neighbourhood (fitted cases by country)
aa <- as.data.frame.table(component_bycountry_array[,,"ne",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ne=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa, 
            aes(x=date_week, y=ne, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle(paste0("The neighbourhood component - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#f15bb5"),
                     labels = c("NE - Simulated data"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_ne.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)



# Endemic (fitted cases by country)
aa <- as.data.frame.table(component_bycountry_array[,,"end",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, end=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa , 
            aes(x=date_week, y=end, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle(paste0("The endemic component - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#00f5d4"),
                     labels = c("END - Simulated data"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_end.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)


# Percentage (by country, 3 components)
fitted_sim <- as.data.frame.table(component_bycountry_array[,,"fitted_mean",]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, fitted_mean=Freq) %>%
  mutate(date_week = as.Date(date_week))

ar <- as.data.frame.table(component_bycountry_array[,,"ar",]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ar=Freq) %>%
  mutate(date_week = as.Date(date_week))
ne <- as.data.frame.table(component_bycountry_array[,,"ne",]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ne=Freq) %>%
  mutate(date_week = as.Date(date_week))
end <- as.data.frame.table(component_bycountry_array[,,"end",]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, end=Freq) %>%
  mutate(date_week = as.Date(date_week))

aa <- Reduce(function(x, y) merge(x, y, all=TRUE), 
             list(fitted_sim, ar, ne, end)) 

aa <- aa %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, country, model, percent_ar, percent_ne, percent_end) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') 


ggplot() + 
  geom_line(data=aa, 
            aes(x=date_week, y=percent, color=Component), linewidth=0.15, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle(paste0("Contribution in percentage - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name = "",   guide = "legend",
                     values = c("percent_ar" = "#0077b6" ,
                                "percent_end" = "#00f5d4",
                                "percent_ne" = "#f15bb5"), 
                     labels = c("Autoregressive"  ,
                                "Endemic", 
                                "Neighbourhood" ))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_percent.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)

# ===================================#
# PREDICTOR ONLY (SIMULATION) -----
# ===================================#

# Autoregressive (predictor by country)
aa <- as.data.frame.table(component_bycountry_array[,,"ar.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ar=Freq) %>%
  mutate(date_week = as.Date(date_week))

aa_true <- as.data.frame.table(component_bycountry_array_true[,,"ar.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ar=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa,
            aes(x=date_week, y=ar, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  geom_line(data=aa_true,
            aes(x=date_week, y=ar, group=model, color="true"), linewidth=0.5, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free_x") +
  xlab("Week of report") + 
  ylab("Effect size") +
  ggtitle(paste0("The autoregressive predictor - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#0077b6", "true" = "#231942"),
                     labels = c("Simulation", "True"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_predictor_ar.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)



# Neighbourhood (predictor by country)
aa <- as.data.frame.table(component_bycountry_array[,,"ne.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ne=Freq) %>%
  mutate(date_week = as.Date(date_week))

aa_true <- as.data.frame.table(component_bycountry_array_true[,,"ne.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, ne=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa, 
            aes(x=date_week, y=ne, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  geom_line(data=aa_true, 
            aes(x=date_week, y=ne, group=model, color="true"), linewidth=0.5, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free_x") +
  xlab("Week of report") + 
  ylab("Effect size") +
  ggtitle(paste0("The neighbourhood predictor - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#f15bb5", "true" = "#8f2d56"),
                     labels = c("Simulation", "True"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_predictor_ne.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)




# Endemic (predictor by country)
aa <- as.data.frame.table(component_bycountry_array[,,"end.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, end=Freq) %>%
  mutate(date_week = as.Date(date_week))

aa_true <- as.data.frame.table(component_bycountry_array_true[,,"end.exppred",2:101]) %>%
  rename(date_week=Var1, country=Var2, model=Var3, end=Freq) %>%
  mutate(date_week = as.Date(date_week))

ggplot() + 
  geom_line(data=aa , 
            aes(x=date_week, y=end, group=model, color="simulation"), linewidth=0.15, alpha=0.2) +
  geom_line(data=aa_true , 
            aes(x=date_week, y=end, group=model, color="true"), linewidth=0.5, alpha=0.2) +
  facet_wrap(~country, ncol=5, scales = "free_x") +
  xlab("Week of report") + 
  ylab("Effect size") +
  ggtitle(paste0("The endemic predictor - Scenario ", scenario)) + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name="",
                     values=c("simulation"="#00f5d4", "true" = "#588157"),
                     labels = c("Simulation", "True"))

ggsave(filename = paste0("fit_model_scenario_",scenario,"_bycountry_predictor_end.png"),
       path = paste0(path_plot,"simulation_plot/"),  
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)
}
