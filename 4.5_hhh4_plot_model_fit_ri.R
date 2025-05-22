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
### Document: PLOT MODEL FIT
### Author: trangngpmd
### Date: 2024-05-27


## Set up dates
date_start_plot <- as.Date("2020-01-01")
date_end_plot <- as.Date("2023-01-01")

## Import fitted models
# fit_original <- get(load(paste0(path_model_fit,"fit_original.rda"))) 
fit_ri_ARENEND <- get(load(paste0(path_model_fit,"fit_original_ri_ARNEEND.rda"))) 

fit <- fit_ri_ARENEND

# ---------------------------------------------------------#
# CALCULATE THE COMPONENT CONTRIBUTION -----
# ---------------------------------------------------------#
# Extract fitted mean by component
model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
mu_coefs <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)

# Percent component
sum(mu_coefs$mean)/sum(fitted.values(fit)) #check if it is the same
sum(mu_coefs$epi.own)/sum(mu_coefs$mean)*100
sum(mu_coefs$epi.neighbours)/sum(mu_coefs$mean)*100
sum(mu_coefs$endemic)/sum(mu_coefs$mean)*100

# Component extract by time (summing all countries)
component_extracted <- data.frame(obs = rowSums(fit$stsObj@observed),
                                  date_week = seq(dateweek_start, dateweek_end, by=7),
                                  fitted_mean = c(rep(NA,2),rowSums(mu_coefs$mean)), # adding 2 weeks with NA fitted values => total 143 weeks
                                  #fitted_values_check = c(rep(NA,2),rowSums(fitted.values(fit))),
                                  ar = c(rep(NA,2),rowSums(mu_coefs$epi.own)),
                                  ne = c(rep(NA,2),rowSums(mu_coefs$epi.neighbours)),
                                  end = c(rep(NA,2),rowSums(mu_coefs$endemic)),
                                  ar.exppred.mean = c(rep(NA,2),rowMeans(mu_coefs$ar.exppred)),
                                  ne.exppred.mean = c(rep(NA,2),rowMeans(mu_coefs$ne.exppred)),
                                  end.exppred.mean = c(rep(NA,2),rowMeans(mu_coefs$end.exppred))
)


# Component extract by time and country
component_bycountry_array <- array(data = NA, dim = c(143,30,8), 
                                   dimnames = list(as.character(seq(dateweek_start, dateweek_end, by=7)), 
                                                   EU30_countries, 
                                                   c("obs","fitted_mean","ar", "ne", "end","ar.exppred", "ne.exppred", "end.exppred")))

#fill the array
component_bycountry_array[,,"obs"] <- fit$stsObj@observed
component_bycountry_array[,,"fitted_mean"] <- rbind(NA, NA, mu_coefs$mean)
component_bycountry_array[,,"ar"] <- rbind(NA, NA, mu_coefs$epi.own)
component_bycountry_array[,,"ne"] <- rbind(NA, NA, mu_coefs$epi.neighbours)
component_bycountry_array[,,"end"] <- rbind(NA, NA, mu_coefs$endemic)

component_bycountry_array[,,"ar.exppred"] <- rbind(NA, NA, mu_coefs$ar.exppred)
component_bycountry_array[,,"ne.exppred"] <- rbind(NA, NA, mu_coefs$ne.exppred)
component_bycountry_array[,,"end.exppred"] <- rbind(NA, NA, mu_coefs$end.exppred)

#Extract data for plotting
tmp <- component_bycountry_array
obs <- as.data.frame(tmp[,,"obs"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
fitted_mean <- as.data.frame(tmp[,,"fitted_mean"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ar <- as.data.frame(tmp[,,"ar"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne <- as.data.frame(tmp[,,"ne"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end <- as.data.frame(tmp[,,"end"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))

ar_exppred <- as.data.frame(tmp[,,"ar.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne_exppred <- as.data.frame(tmp[,,"ne.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end_exppred <- as.data.frame(tmp[,,"end.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))


# ------------------------------------------------------------------------------------#
# CALCULATE THE CONTRIBUTION (PERCENTAGE) BY COUNTRY FROM THE RANDOM EFFECTS MODEL FIT -----
# ------------------------------------------------------------------------------------#
tmp <- lst(ar, ne, end, fitted_mean) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>%
  group_by(country) %>%
  summarise(total_ar = sum(ar, na.rm = T),
            total_ne =sum(ne, na.rm = T), 
            total_end = sum(end, na.rm = T), 
            total_fitted = sum(fitted_mean, na.rm = T)) %>%
  mutate(percent_ar = total_ar/total_fitted,
         percent_ne = total_ne/total_fitted,
         percent_end = total_end/total_fitted)

# write_xlsx(tmp, paste0(path_model_fit,"percent_bycountry_riARNEEND.xlsx" ))

# Plot barchart
plot1 <- ggplot(data = tmp, 
                aes(x=reorder(country, percent_ar*100), y=percent_ar*100)) +    
  geom_bar(stat="identity", color='black',fill='lightgrey', width = 0.8) +
  geom_text(aes(label = round(percent_ar*100,2)), vjust = 0.5, hjust=-0.2, size=3) + 
  labs(x="", y="Percentage",
       title = "(A) Autoregressive component") + 
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face = "bold")) +
  coord_flip(ylim = c(65,101))
plot1

plot2 <- ggplot(data = tmp, aes(x=reorder(country, percent_ne*100), y=percent_ne*100)) +    
  geom_bar(stat="identity", color='black',fill='lightgrey', width = 0.8) +
  geom_text(aes(label = round(percent_ne*100,2)), vjust = 0.5, hjust=-0.2, size=3) + 
  labs(x="", y="Percentage",
       title = "(B) Neighbourhood component") + 
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face = "bold")) +
  coord_flip(ylim = c(0,5.5))
plot2

plot3 <- ggplot(data = tmp, 
                aes(x=reorder(country, percent_end*100), y=percent_end*100)) +    
  geom_bar(stat="identity", color='black',fill='lightgrey', width = 0.8) +
  geom_text(aes(label = round(percent_end*100,2)), vjust = 0.5, hjust=-0.2, size=3) + 
  labs(x="", y="Percentage",
       title = "(C) Endemic component") + 
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face = "bold")) +
  coord_flip(ylim = c(0,35))
plot3

## combine plots
#library("gridExtra")
plotcombine <- arrangeGrob(plot1, plot2, plot3, ncol=2, nrow=2)
ggsave(file="fit_ri_component_comtribution.png", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), 
       height=10, width=10, units="in")



# -----------------------------------------------------------------#
# =========== COMPONENT CONTRIBUTION FROM THE RANDOM EFFECTS MODEL FIT =======
# -----------------------------------------------------------------#

# -------------------------------------#
#  ALL COUNTRIES -----
# -------------------------------------#

# Overall fit
tmp <- component_extracted %>%
  select(date_week, ar, ne, end) %>%
  pivot_longer(cols= c("ar", "ne", "end"),
               names_to='Component',
               values_to='estimated')

plot1 <- ggplot() +
  geom_area(data = tmp, aes(x=date_week, y=estimated, fill=Component)) +
  geom_point(data = component_extracted, aes(x=date_week, y=obs), 
             stat = "identity", color="black", fill="black", size=0.8) + 
  # geom_line(data = component_extracted, aes(x=date_week, y=obs)) +
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle("(A) The overall fitted model") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position=c(0.15,0.6)) + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,10000000)) +
  scale_fill_manual(name = "Component",   guide = "legend",
                    values = c("ar" = "#0077b6" ,
                               "end" = "#00f5d4",
                               "ne" = "#f15bb5"), 
                    labels = c("Autoregressive"  ,
                               "Endemic", 
                               "Neighbourhood"))
plot1

# Percentage contribution
tmp <- component_extracted %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, percent_ar, percent_ne, percent_end) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') 

plot2 <- ggplot() + 
  geom_line(data=tmp, aes(x=date_week, y=percent, color=Component), 
            linewidth=1) +
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("(B) The overall component contribution (%)") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position=c(0.12,0.5)) + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name = "Component",   guide = "legend",
                     values = c("percent_ar" = "#0077b6" ,
                                "percent_end" = "#00f5d4",
                                "percent_ne" = "#f15bb5"), 
                     labels = c("Autoregressive"  ,
                                "Endemic", 
                                "Neighbourhood" ))
plot2

# ---------------------------------------------------------#
# PERCENT - COMBINATION OF ALL 30 COUNTRIES ------
# ---------------------------------------------------------#

# Each component
tmp <- lst(fitted_mean, ar, ne, end) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>% 
  left_join(list_countries, by="country") %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, country, EU_region, percent_ar, percent_ne, percent_end) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') %>%
  group_by(date_week, Component) %>%
  mutate(#mean = mean(percent, na.rm = T),
    median = median(percent, na.rm = T),
    upper = quantile(percent,probs = 0.975, na.rm = T),
    lower = quantile(percent,probs = 0.025, na.rm = T)) %>% ungroup() 

plot3 <- ggplot() + 
  #geom_line(data=tmp, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=tmp, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=tmp, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = tmp %>% filter(Component == "percent_ar"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#0077b6", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_ar"), 
            aes(x=date_week, y=median, group=Component), color="#0077b6", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(C) The autoregressive component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))
plot3

plot4 <- ggplot() + 
  #geom_line(data=tmp, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=tmp, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=tmp, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = tmp %>% filter(Component == "percent_ne"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#f15bb5", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_ne"), 
            aes(x=date_week, y=median, group=Component), color="#f15bb5", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(D) The neighbourhood component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))
plot4

plot5 <- ggplot() + 
  #geom_line(data=tmp, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=tmp, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=tmp, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = tmp %>% filter(Component == "percent_end"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#00f5d4", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_end"), 
            aes(x=date_week, y=median, group=Component), color="#00f5d4", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(E) The endemic component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))

plot5


## combine plots
# library("cowplot")
plotcombine <- plot_grid(plot1, plot2, plot3,plot4,plot5, ncol = 1, align = "v", axis = "b")
ggsave(file="fit_ri.png", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), 
       height=12, width=8, units="in")


# -------------------------------------# 
#  BY COUNTRY -----
# -------------------------------------#
tmp_point <- lst(obs) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable)

tmp_area <- lst(ar, ne, end) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>%
  pivot_longer(cols= c("ar", "ne", "end"),
               names_to='Component', values_to='fitted_comp')

# All three components by country
ggplot() + 
  geom_area(data = tmp_area, aes(x=date_week, y=fitted_comp, fill=Component)) +
  geom_point(data = tmp_point, aes(x=date_week, y=obs), 
             stat = "identity", color="black", fill="black", size=0.5) + 
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle("Fitted component by country") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_fill_manual(name = "",   guide = "legend",
                    values = c("ar" = "#0077b6" ,
                               "end" = "#00f5d4",
                               "ne" = "#f15bb5"), 
                    labels = c("Autoregressive"  ,
                               "Endemic", 
                               "Neighbourhood" ))

ggsave(filename = "fit_ri_by_country.png",
       path = paste0(path_plot,"model_fit_plot/"), 
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)


# Autoregressive
ggplot() + 
  #geom_area(data = tmp_area %>% filter(Component=="ar"), 
  #          aes(x=date_week, y=fitted_comp, fill=Component)) +
  geom_line(data = tmp_area %>% filter(Component=="ar"), 
            aes(x=date_week, y=fitted_comp, color=Component)) +
  geom_point(data = tmp_point, aes(x=date_week, y=obs), 
             stat = "identity", color="black", fill="black", size=0.5) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle("The autoregressive component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name = "",   guide = "legend",
                     values = c("ar" = "#0077b6"), 
                     labels = c("Autoregressive"))

ggsave(filename = "fit_ri_by_country_ar.png",
       path = paste0(path_plot,"model_fit_plot/"), 
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)

# Neighbourhood
ggplot() + 
  #geom_area(data = tmp_area %>% filter(Component=="ne"), 
  #          aes(x=date_week, y=fitted_comp, fill=Component)) +
  geom_line(data = tmp_area %>% filter(Component=="ne"), 
            aes(x=date_week, y=fitted_comp, color=Component)) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle("The neighbourhood component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name = "",   guide = "legend",
                     values = c("ne" = "#f15bb5"), 
                     labels = c("Neighbourhood"))

ggsave(filename = "fit_ri_by_country_ne.png",
       path = paste0(path_plot,"model_fit_plot/"), 
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)


# Endemic
ggplot() + 
  #geom_area(data = tmp_area %>% filter(Component=="end"), 
  #          aes(x=date_week, y=fitted_comp, fill=Component)) +
  geom_line(data = tmp_area %>% filter(Component=="end"), 
            aes(x=date_week, y=fitted_comp, color=Component)) +
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("No. cases") +
  ggtitle("The endemic component") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_color_manual(name = "",   guide = "legend",
                     values = c("end" = "#00f5d4"), 
                     labels = c("Endemic"))

ggsave(filename = "fit_ri_by_country_end.png",
       path = paste0(path_plot,"model_fit_plot/"), 
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)


# Percentage
tmp <- lst(fitted_mean, ar, ne, end) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, country, percent_ar, percent_ne, percent_end) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') 

ggplot() + 
  geom_line(data=tmp, aes(x=date_week, y=percent, color=Component), linewidth=1) + 
  facet_wrap(~country, ncol=5, scales = "free") +
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("Contribution in percentage") + 
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

ggsave(filename = "fit_ri_by_country_percent.png",
       path = paste0(path_plot,"model_fit_plot/"), 
       units = "in",  width = 12, height = 10, 
       device="png", dpi=600)


# ---------------------------------#
# PLOT RANDOM INTERCEPTS -----
# ---------------------------------#
random_intercepts <- as.data.frame(ranef(fit, tomatrix = TRUE))
random_intercepts$country <- rownames(random_intercepts)
colnames(random_intercepts) <- c("Autoregressive random intercepts",  
                                 "Neighbourhood random intercepts", 
                                 "Endemic random intercepts", "country")

tmp <- random_intercepts %>%
  pivot_longer(cols = c("Autoregressive random intercepts", 
                        "Neighbourhood random intercepts", 
                        "Endemic random intercepts"),
               names_to = "ri_comp") %>%
  left_join(map_europe, by="country") %>%
  st_as_sf()

ggplot(data = tmp) +
  geom_sf(aes(fill = value), size = 0.15, color = "black") +
  facet_wrap(.~ri_comp, ncol=3) +
  geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
  scale_fill_gradient2(low = "#0081a7", mid="#ffffff", high = "#fb5607", limits = c(-3,3)) +
  labs(x="", y="", fill = "value") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"))

ggsave(filename = "map_random intercepts_ARNEEND.png",
       path = paste0(path_plot, "model_fit_plot/"),
       units = "in",  width = 15, height = 7, 
       device="png", dpi=600)


# ------------------------------------ #
# PREDICTORS (nu, lambda, phi) only ------
# ------------------------------------#
ne_exppred %>%
  pivot_longer(cols=EU30_countries,
               names_to='Country',
               values_to='est') %>%
  ggplot() +
  #geom_line(aes(x = date_week, y = mean_stringency), color="darkblue") +
  #geom_line(aes(x = date_week, y = est, color=Country)) +
  geom_line(aes(x = date_week, y = est)) +
  facet_wrap(~Country, scales = "free", ncol=5) +
  xlab("Week of report") +
  ylab("Effect size") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_color_manual(values = colors30) +
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "fit_ri_by_country_predictor_ne.png",
       path = paste0(path_plot,"model_fit_plot/"),
       units = "in",  width = 12, height = 10,
       device="png", dpi=600)


# ---------------------------------------------------------#
# THE DECAY PARAMETER ------
# --------------------------------------------------------#
decay <- exp(coef(fit)[19])
decay

decay_lower <- exp(confint(fit)[19,1])
decay_upper <- exp(confint(fit)[19,2])

## Non-normalized weight
tmp <- data.frame(order = c(1:10)) %>%
  mutate(decay_est = order^(-decay),
         upper = order^(-decay_lower),
         lower = order^(-decay_upper))

plot1 <- ggplot(data=tmp) + 
  geom_line(aes(x=order, y=decay_est), color = "steelblue") +
  geom_point(aes(x=order, y=decay_est), color = "steelblue") + 
  geom_line(aes(x=order, y=upper), color = "steelblue", linetype = "dashed") +
  geom_point(aes(x=order, y=upper), color = "steelblue") + 
  geom_line(aes(x=order, y=lower), color = "steelblue", linetype = "dashed") +
  geom_point(aes(x=order, y=lower), color = "steelblue") + 
  theme_bw() +
  labs(x="Neighbourhood order", y="Non-normalized weight",
       title = "(A) Non-normalized weights (95% CIs)" ) +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1:10))


## Normalized weight
tmp <- neighbor_adjusted^(-decay)
diag(tmp) <- 0
tmp <- tmp/rowSums(tmp)

mat_melt <- melt(tmp) %>%
  rename(origin = Var1, destination = Var2, normalized_decay = value) %>%
  full_join(neighbor_adjusted_long, by=c("origin", "destination")) %>%
  mutate_at(c("ne_order"), as.factor)


plot2 <- ggplot(data = mat_melt %>% filter(!ne_order==0), aes(x=ne_order, y = normalized_decay)) +
  geom_boxplot() +
  labs(x="Neighbourhood order", y="Normalized weight",
       title = "(B) Normalized weights" ) +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold"),
        panel.grid.minor = element_blank())

## combine plots
plotcombine <- plot_grid(plot1, plot2, ncol = 2, align = "v", axis = "b")
plotcombine
ggsave(file="powerlaw weight.png", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), 
       height=3, width=8, units="in")





