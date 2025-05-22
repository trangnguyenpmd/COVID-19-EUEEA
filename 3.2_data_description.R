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
### Document: DATA DESCRIPTION & PLOTTING
### Author: trangngpmd
### Date: 2024-10-09


# --------------------------------------------------------------------#
# ==================== DATA COVID-19 CASES ===========================
# --------------------------------------------------------------------#

# -------------------------------------------#
## BY TIME -----
# -------------------------------------------#

## Total cases
cases_owid_week_long %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(cases_wk = sum(cases_wk)) %>%
  mutate(caseswk_per_million = cases_wk/455993375*1000000) %>%
  ggplot() +
  geom_area(aes(x = date_week, y = cases_wk), fill="#003049") +
  xlab("Week of report") +
  ylab("New cases") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y")  +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "epicurves_all_cases.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 8, height = 5, 
       device="png", dpi=300)


## Cases per country
cases_owid_week_long %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  left_join(list_dateweek, by="date_week") %>%
  left_join(pop_country_year, by=c("country", "year_by_week" = "year")) %>%
  mutate(caseswk_per_million = cases_wk/population*1000000) %>%
  ggplot() +
  geom_area(aes(x = date_week, y = cases_wk), fill="#003049") +
  #geom_area(aes(x = date_week, y = caseswk_per_million), fill="#003049") +
  facet_wrap(~country, scales = "free", ncol=5) +
  xlab("Week of report") +
  ylab("New cases") +
  #ylab("New cases per million") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "epicurves_cases per country.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 10, height = 8, 
       device="png", dpi=600)


# -------------------------------------------#
## BY SPACE -----
# -------------------------------------------#

# Cases by YEAR
tmp <- cases_owid_date_long %>%
  filter(between(date, dateweek_start, dateweek_end)) %>%
  group_by(year_by_week, country) %>%
  summarise(cases_by_year = sum(new_cases, na.rm = T)) %>%
  left_join(map_europe, by="country") %>%
  left_join(pop_country_year, by=c("country", "year_by_week" = "year")) %>%
  mutate(cases_per_million_year = cases_by_year/population*1000) %>%
  st_as_sf()

ggplot(data = tmp) +
  #geom_sf(aes(fill = log(cases_by_year)), size = 0.15, color = "black") +
  geom_sf(aes(fill = cases_per_million_year), size = 0.15, color = "black") +
  facet_wrap(.~year_by_week, ncol=3) +
  geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(5, 500), breaks = c(5, 100,200,300, 400,500)) +
  #labs(x="", y="", fill = "No.cases (log)") +
  labs(x="", y="", fill = "Cases per 1000") +
  theme_bw() 

ggsave(filename = "map_cases by year per 1000.png",
       path = paste0(path_plot,"data_description/"),
       units = "in",  width = 15, height = 7, 
       device="png", dpi=600)


rm(tmp)

# --------------------------------------------------------------------#
# ================== DATA STRINGENCY INDEX ===========================#
# --------------------------------------------------------------------#

str_index_week_long %>%
  ggplot() +
  geom_line(aes(x = date_week, y = mean_stringency), color = "darkblue") +
  facet_wrap(~country, scales = "fixed", ncol=5) +
  xlab("Week of report") +
  ylab("Mean of Stringency Index") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_color_manual(values = colors30) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "Stringency Index by country.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 10, height = 8, 
       device="png", dpi=600)



# --------------------------------------------------------------------#
# ==================== DATA VACCINATION ==============================
# --------------------------------------------------------------------#

# -------------------------------------------#
## NO WANING (raw coverage only) -----
# -------------------------------------------#

tmp <- vaccination_clean %>%
  select(date_week, Country = country, 
         primarycourse = percent_dose2nd, 
         booster1 = percent_booster1, booster2 = percent_booster2, booster3 = percent_booster3) %>%
  filter(between(date_week, dateweek_start, dateweek_end))
tmp <- melt(tmp, id.vars=c("date_week", "Country"))

ggplot(data = tmp) +
  geom_line(aes(x = date_week, y=value, group=variable, color=variable)) +
  facet_wrap(~Country, scales = "free_x", ncol=5) +
  xlab ("Week of report") + 
  ylab("Vaccination coverage") +
  theme_bw()+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.border = element_blank()) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10),
        legend.key.size = unit(0.25, 'in')) +
  theme(axis.text.x=element_text(colour="black", size = 10, 
                                 angle=0, vjust = 0.5, hjust=0.5),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title=element_text(colour="black", size = 10), 
        panel.grid=element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_line(color = "gray90", size = 0.2))+
  
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "Dose waning",
                     guide = "legend",
                     values = c("primarycourse" = "#ef7b45",
                                "booster1" = "#0077b6" ,
                                "booster2" = "#f15bb5",
                                "booster3" = "#00f5d4"), 
                     labels = c('Primary course',
                                "Booster 1"  ,
                                "Booster 2", 
                                "Booster 3" ))

ggsave(filename = "vaccination_coverage (no waning).png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 10, height = 9,
       device="png", dpi=600)


rm(tmp)

# -------------------------------------------#
## WITH WANING -----
# -------------------------------------------#

tmp <- vaccination_waned
colnames(tmp) <- c("date_week","Country", "waned_primarycourse", "waned_booster1","waned_booster2", "waned_booster3", "waned_overall")
tmp <- melt(tmp, id.vars=c("date_week", "Country"))

ggplot(data = tmp) +
  geom_line(aes(x = date_week, y=value, group=variable, color=variable)) +
  facet_wrap(~Country, scales = "free_x", ncol=5) +
  xlab ("Week of report") + 
  ylab("Vaccination coverage (waning)") +
  theme_bw()+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.border = element_blank()) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10),
        legend.key.size = unit(0.25, 'in')) +
  theme(axis.text.x=element_text(colour="black", size = 10, 
                                 angle=0, vjust = 0.5, hjust=0.5),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title=element_text(colour="black", size = 10), 
        panel.grid=element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_line(color = "gray90", size = 0.2))+
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "Dose waning",
                     guide = "legend",
                     values = c("waned_primarycourse" = "#ef7b45",
                                "waned_booster1" = "#0077b6" ,
                                "waned_booster2" = "#f15bb5",
                                "waned_booster3" = "#00f5d4",
                                "waned_overall" = "#283618" ), 
                     labels = c('Primary course (waned)',
                                "Booster 1 (waned)"  ,
                                "Booster 2 (waned)", 
                                "Booster 3 (waned)", 
                                "Overall coveraged (waned)"))

ggsave(filename = "vaccination_coverage (with waning).png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 10, height = 9,
       device="png", dpi=600)


rm(tmp)


# -------------------------------------------#
## WANING BY COUNTRY -----
# -------------------------------------------#
tmp <- vaccination_waned
colnames(tmp) <- c("date_week","Country", "waned_primarycourse", "waned_booster1","waned_booster2", "waned_booster3", "waned_overall")
tmp <- melt(tmp, id.vars=c("date_week", "Country"))
tmp <- tmp %>% filter(variable == "waned_overall")

ggplot(data = tmp, aes(date_week, y = Country, fill = value)) + 
  geom_tile() +
  scale_fill_viridis(discrete = FALSE, limit = c(0,1), name="")+
  labs(title = "Vaccination coverage by country",
       x = "Week of report",
       y = "") +
  theme_bw() + 
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%Y")

ggsave(filename = "vaccination_coverage (with waning) by country.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 8, height = 6,
       device="png", dpi=600)


rm(tmp)

# -------------------------------------------#
## WANING FUNCTION -----
# -------------------------------------------#

tmp <- data.frame(waning_rate = waning(0 : 52, val = 1),
                 week = c(0:52))

ggplot(data = tmp, aes(x=week,y = waning_rate)) +
  geom_line() + 
  geom_point() +
  theme_bw() + 
  xlab("Weeks passed since vaccine given") + 
  ylab("Waning rate") +
  scale_x_continuous(breaks=c(0:52), limits = c(0, 52))

ggsave(filename = "waning function.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 10, height = 3.5,
       device="png", dpi=600)


rm(tmp)

# -------------------------------------------#
## VACCINE PRODUCTS -----
# -------------------------------------------#
# vaccine_products <- readRDS(paste0(path_data_clean, "vaccine_products.rds"))

# Total doses
tmp <- vaccine_products %>%
  filter(date == "2022-12-18", !total_doses == 0) %>%
  mutate(percent = percent*100)
ggplot(data = tmp) +
  geom_col(aes(x=reorder(vaccine_group, percent, decreasing = TRUE), y=percent), position = position_stack())  +
  geom_text(aes(x=reorder(vaccine_group, percent, decreasing = TRUE), y=percent, label = round(percent,2)), vjust = -0.5) +
  xlab("") +
  ylab("Percentage of total doses administered") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  
ggsave(filename = "vaccine_products_total_doses_percent.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 9, height = 6,
       device="png", dpi=600)


rm(tmp)


# --------------------------------------------------------------------#
# ==================== DATA VARIANTS =================================
# --------------------------------------------------------------------#

# ---------------------------------#
## ALL COUNTRIES -----
# ---------------------------------#

tmp <- variants_wide %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(total_allsequences = sum(number_sequenced_known_variant, na.rm = TRUE),
            total_alpha = sum(Alpha, na.rm = TRUE), 
            total_delta = sum(Delta, na.rm = TRUE), 
            total_omicron = sum(Omicron, na.rm = TRUE)) %>%
  mutate(Alpha = if_else(total_allsequences==0,0,
                         total_alpha/total_allsequences),
         Delta = if_else(total_allsequences==0,0,
                         total_delta/total_allsequences),
         Omicron = if_else(total_allsequences==0,0,
                           total_omicron/total_allsequences)) %>%
  mutate(Others = 1 - (Alpha + Delta + Omicron)) %>%
  select(date_week, Alpha, Delta, Omicron, Others) %>%
  pivot_longer(cols=c('Alpha', 'Delta', "Omicron", "Others"),
               names_to='Variants', values_to='Percentage')

## Plot
ggplot(data = tmp) +
  #geom_col(aes(x = date_week, y = Percentage, fill = Variants)) +
  geom_line(aes(x = date_week, y = Percentage, color = Variants, linetype=Variants), linewidth=1) +
  ylab("Percentage") +
  xlab("Week of report") +
  labs(title = "Variants circulation in 30 EU/EEA countries (GISAID database)",
       subtitle = "Between week 13-2020 and week 50-2022") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text=element_text(colour="black"), legend.position="top") +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y")

ggsave(filename = paste0("ECDC_GISAID_percent_variants_all_lines.png"),
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 9, height = 5, 
       device="png", dpi=600)


rm(tmp)



# ---------------------------------#
## BY COUNTRY - ORIGINAL DATA VARIANTS -----
# ---------------------------------#

tmp <- variants_wide %>%
  select(date_week, country, Alpha, Delta, Omicron, number_sequenced_known_variant) %>%
  mutate(Others = number_sequenced_known_variant - Alpha - Delta - Omicron) %>%
  pivot_longer(cols=c('Alpha', 'Delta', "Omicron", "Others"),
               names_to='Variants',
                values_to='Counts') %>%
  filter(country %in% EU_western) %>%
  filter(!country == "Liechtenstein")

ggplot(data = tmp) +
  geom_col(aes(x = date_week, y = Counts, fill = Variants), position = "fill") +
  # facet_wrap(~country, ncol = 5, scales = "free_x") +
  facet_wrap(~country, ncol = 3, scales = "free_x") +
  ylab("Percent variant") +
  scale_y_continuous(labels = scales::percent) +
  # geom_col(aes(x = date_week, y = Counts, fill = Variants), position = position_stack()) +
  # facet_wrap(~country, ncol = 5, scales = "free") +
  # ylab("Number of sequenced known variants") +
  xlab("Week of report") +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y")

ggsave(#filename = paste0("ECDC_GISAID_bycountry_percentsequenced_variants_original.png"),
       filename = paste0("ECDC_GISAID_western_percentsequenced_variants_original.png"),
       path = paste0(path_plot,"data_description/"), 
       # units = "in",  width = 10, height = 8, 
       units = "in",  width = 7, height = 4, 
       device="png", dpi=600)

rm(tmp)


# ---------------------------------#
## BY COUNTRY - DATA VARIANTS AFTER MODIFIED -----
# ---------------------------------# 

tmp <- variants_wide %>%
  select(date_week, country, percent_alpha, percent_delta, percent_omicron, percent_others)

tmp <- tmp %>%
  rename("Alpha" = "percent_alpha", 
         "Delta" = "percent_delta",
         "Omicron" = "percent_omicron", 
         "Others" = "percent_others") %>%
  pivot_longer(cols=c('Alpha', 'Delta', "Omicron", "Others"),
               names_to='Variants',
               values_to='Percentage')

ggplot(data = tmp) +
  geom_col(aes(x = date_week, y = Percentage, fill = Variants)) +
  facet_wrap(~country, ncol = 5, scales = "free_x") +
  xlab("Week of report") +
  ylab("Percent variant") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text=element_text(colour="black")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y")

ggsave(filename = paste0("ECDC_GISAID_bycountry_percent_variants_modified.png"),
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 10, height = 8, 
       device="png", dpi=600)


rm(tmp)


# --------------------------------------------------------------------#
# ================= NEIGHBOURHOOD MATRICES ===========================
# --------------------------------------------------------------------#

# ---------------------------#
## Neighbourhood matrices -----
# ---------------------------#
# New way to plot the matrix 
# The COVID-19 vaccination campaign in Switzerland and its impact on disease spread
# https://www.sciencedirect.com/science/article/pii/S1755436524000069

# tmp <- neighbor_original
tmp <- neighbor_adjusted

mat_melt <- melt(tmp) %>%
  mutate_at(c("value"), as.factor)
ggplot(data = mat_melt, aes_string(x = names(mat_melt)[1], 
                                   y = names(mat_melt)[2], fill = names(mat_melt)[3])) + 
  geom_tile() +
  labs(x = "", y = "", title = "(B) Maritime neighbourhood matrix", fill = "Order") +
  geom_text(aes(x=Var2, y=Var1), label=mat_melt$value, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(face = "bold")) +
  scale_fill_viridis_d(direction = - 1) +
  coord_fixed()

ggsave(filename = "neighborhood_matrix_maritime.png",
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 7, height = 7, 
       device="png", dpi=600)

rm(tmp)



# -------------------------#
## PLOT CONNECTION -----
# -------------------------#
mat_melt <- melt(neighbor_original, na.rm = TRUE) 
colnames(mat_melt) <- c("origin", "destination", "order")
map_connection_original <- mat_melt %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("origin" = "country")) %>%
  rename("X_origin" = "X", "Y_origin" = "Y") %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("destination" = "country")) %>%
  rename("X_destination" = "X", "Y_destination" = "Y") %>%
  select(origin, destination, order, X_origin, Y_origin, X_destination, Y_destination)

mat_melt <- melt(neighbor_adjusted, na.rm = TRUE) 
colnames(mat_melt) <- c("origin", "destination", "order")
map_connection <- mat_melt %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("origin" = "country")) %>%
  rename("X_origin" = "X", "Y_origin" = "Y") %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("destination" = "country")) %>%
  rename("X_destination" = "X", "Y_destination" = "Y") %>%
  select(origin, destination, order, X_origin, Y_origin, X_destination, Y_destination)

# for (i in 1:10){
i <- 1 # ranging from 1 to 10 (adjusted neighbourhood matrix) or from 1 to 8 (original neighbourhood matrix)

ggplot() +
  geom_sf(data = map_europe, size = 0.15, color = "black", fill = "#eef0f2") +
  geom_link(data = map_connection %>% filter(order == i), 
            aes(x=X_origin, y = Y_origin,
                xend = X_destination, yend = Y_destination), color="red") +
  geom_link(data = map_connection_original %>% filter(order == i),
            aes(x=X_origin, y = Y_origin,
                xend = X_destination, yend = Y_destination)) +
  geom_point(data = map_centroid, aes(x=X, y= Y), color = "red", size=1) +
  geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
  labs(x="", y="") +
  ggtitle(paste0("(A) The ",i,"st-order neighbor (maritime matrix)"))+
  #xlim(-20,35) + ylim(34,66)+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave(filename = paste0("maritime_adjusted_connection_order_",i,".png"),
       path = paste0(path_plot,"data_description/"), 
       units = "in",  width = 5, height = 7, 
       device="png", dpi=600)


# }


# --------------------------------------------------------------------#
# ======= POPULATION BY COUNTRY ======================================
# --------------------------------------------------------------------##

# Plot barchart
pop_country_year %>%
  group_by(country) %>%
  summarise(mean_pop = mean(population)) %>%
  ggplot(aes(x=reorder(country, -mean_pop), y=mean_pop)) +   
  #ggplot(aes(x=reorder(country, log(mean_pop)), y=log(mean_pop))) +    
  geom_bar(stat="identity", color='black',fill='lightgrey', width = 0.8) +
  labs(x="", y="Mean population size") + 
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  coord_flip()
ggsave(filename = "mean population size.png",
       path = paste0(path_plot,"data_description/"), units = "in",
       width = 6,   height = 5, 
       device="png", dpi=300)



# --------------------------------------------------------------------#
# ======= EPI CURVES - STRINGENCY - VACCINATION -  VARIANTS ==========
# --------------------------------------------------------------------#

## Total cases
plot1 <- cases_owid_week_long %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(cases_wk = sum(cases_wk)) %>%
  mutate(caseswk_per_million = cases_wk/455993375*1000000) %>%
  ggplot() +
  geom_area(aes(x = date_week, y = cases_wk), fill="#003049") +
  xlab("Week of report") +
  ylab("New cases") +
  ggtitle("(A) Number of reported cases, aggregated over all 30 EU/EEA countries, W13/2020 - W50/2022") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0))
plot1

#Stringency Index
tmp <- str_index_week_long %>%
  group_by(date_week) %>%
  mutate(median = median(mean_stringency, na.rm = T),
         upper = quantile(mean_stringency,probs = 0.975, na.rm = T),
         lower = quantile(mean_stringency,probs = 0.025, na.rm = T))
plot2 <- ggplot(data = tmp) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=date_week), fill = "blue", alpha = 0.3) +
  geom_line(aes(x=date_week, y=median), color="blue", linewidth=0.6) + 
  xlab("Week of report") +
  ylab("Mean of the Stringency Index") +
  ggtitle("(B) Weekly mean of the Stringency Index, across 30 EU/EEA countries, W11/2020 - W50/2022",
          subtitle = "The solid line is the median of weekly means, the ribbon is the 95% quantiles of the weekly means accross EU/EEA countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100))
plot2
rm(tmp)

#Vaccination raw
tmp <- vaccination_clean %>%
  select(date_week, country, 
         percent_dose2nd, percent_booster1, percent_booster2, percent_booster3) %>%
  filter(between(date_week, dateweek_start, dateweek_end))
colnames(tmp) <- c("date_week","Country", "primarycourse", "booster1","booster2", "booster3")
tmp <- melt(tmp, id.vars=c("date_week", "Country"))

tmp <- tmp %>%
  group_by(date_week, variable) %>%
  mutate(median = median(value, na.rm = T),
         upper = quantile(value,probs = 0.975, na.rm = T),
         lower = quantile(value,probs = 0.025, na.rm = T))

plot3 <- ggplot() +
  geom_ribbon(data = tmp, aes(x=date_week, ymin=lower, ymax=upper, group = variable, fill = variable), alpha = 0.3) +
  geom_line(data = tmp, aes(y=median, x=date_week, group=variable, colour=variable),linewidth=0.6) + 
  #geom_line(data = tmp %>% filter(Country == "Belgium"), aes(y=value, x=date_week, colour=variable),linewidth=0.6, linetype="dashed") +
  xlab("Week of report") +
  ylab("Vaccination coverage") +
  ggtitle("(C) Raw vaccination coverage, across 30 EU/EEA countries, W50/2020 - W50/2022",
          subtitle = "The solid lines are the median of weekly percentages, the ribbons are the 95% quantiles of the weekly percentages accross EU/EEA countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = c(0.1,0.5)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               #limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "Dose",
                     guide = "legend",
                     values = c("primarycourse" = "#ef7b45",
                                "booster1" = "#0077b6" ,
                                "booster2" = "#f15bb5",
                                "booster3" = "#00f5d4"), 
                     labels = c('Primary course',
                                "Booster 1"  ,
                                "Booster 2", 
                                "Booster 3" ),
                     aesthetics = c("fill", "colour"))
plot3
rm(tmp)

# ggsave(filename = paste0("raw vaccination coverage EU and BEL.png"), plot3,
#        path = paste0(path_plot,"data_description/"), 
#        units = "in",  width = 9, height = 5, 
#        device="png", dpi=600)


# Variant
tmp <- variants_wide %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(total_allsequences = sum(number_sequenced_known_variant, na.rm = TRUE),
            total_alpha = sum(Alpha, na.rm = TRUE), 
            total_delta = sum(Delta, na.rm = TRUE), 
            total_omicron = sum(Omicron, na.rm = TRUE)) %>%
  mutate(Alpha = if_else(total_allsequences==0,0,
                         total_alpha/total_allsequences),
         Delta = if_else(total_allsequences==0,0,
                         total_delta/total_allsequences),
         Omicron = if_else(total_allsequences==0,0,
                           total_omicron/total_allsequences)) %>%
  mutate(Others = 1 - (Alpha + Delta + Omicron)) %>%
  select(date_week, Alpha, Delta, Omicron, Others) %>%
  pivot_longer(cols=c('Alpha', 'Delta', "Omicron", "Others"),
               names_to='Variants', values_to='Percentage')

## Plot
plot4 <- ggplot(data = tmp) +
  #geom_col(aes(x = date_week, y = Percentage, fill = Variants)) +
  geom_line(aes(x = date_week, y = Percentage, color = Variants, linetype=Variants), linewidth=1) +
  ylab("Percentage") +
  xlab("Week of report") +
  ggtitle("(D) Variants circulation, aggregated over all 30 EU/EEA countries, W13/2020 - W50/2022") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text=element_text(colour="black"), 
        plot.title = element_text(face = "bold"),
        legend.position= c(0.1,0.5)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") 
plot4
rm(tmp)

## combine plots
plotcombine <- plot_grid(plot1, plot2, plot3, plot4, ncol = 1, align = "v", axis = "b")
plotcombine
ggsave(file="data combine.png", plotcombine, 
       path = paste0(path_plot,"data_description/"), 
       height=12.5, width=10.5, units="in")


# --------------------------------------------------------------------#
# ======= VACCINATION WANNING ========================================
# --------------------------------------------------------------------#

#Vaccination waning
tmp <- vaccination_waned
colnames(tmp) <- c("date_week","Country", "waned_primarycourse", "waned_booster1","waned_booster2", "waned_booster3", "waned_overall")
tmp <- melt(tmp, id.vars=c("date_week", "Country"))

tmp <- tmp %>%
  group_by(date_week, variable) %>%
  mutate(median = median(value, na.rm = T),
         upper = quantile(value,probs = 0.975, na.rm = T),
         lower = quantile(value,probs = 0.025, na.rm = T))

plot1 <- ggplot(data = tmp) +
  geom_ribbon(aes(x=date_week, ymin=lower, ymax=upper, group = variable, fill = variable), alpha = 0.3) +
  geom_line(aes(y=median, x=date_week, group=variable, colour=variable),linewidth=0.6) + 
  xlab("Week of report") +
  ylab("Vaccination coverage") +
  ggtitle("(A) Vaccination coverage with immunity waning across countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "",
                     guide = "legend",
                     values = c("waned_primarycourse" = "#ef7b45",
                                "waned_booster1" = "#0077b6" ,
                                "waned_booster2" = "#f15bb5",
                                "waned_booster3" = "#00f5d4",
                                "waned_overall" = "#283618" ), 
                     labels = c('Primary course (waned)',
                                "Booster 1 (waned)"  ,
                                "Booster 2 (waned)", 
                                "Booster 3 (waned)", 
                                "Overall coveraged (waned)"),
                     aesthetics = c("fill", "colour"))
plot1
rm(tmp)

# waning by country
tmp <- vaccination_waned
colnames(tmp) <- c("date_week","Country", "waned_primarycourse", "waned_booster1","waned_booster2", "waned_booster3", "waned_overall")
tmp <- melt(tmp, id.vars=c("date_week", "Country"))
tmp <- tmp %>% filter(variable == "waned_overall")

plot2 <- ggplot(data = tmp, aes(date_week, y = Country, fill = value)) + 
  geom_tile() +
  scale_fill_viridis(discrete = FALSE, limit = c(0,1), name="")+
  labs(title = "(B) The overall vaccination coverage with immunity waning by country",
       x = "Week of report", y = "") +
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5))

plot2
rm(tmp)

## combine plots
#library("cowplot")
plotcombine <- plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "b")
ggsave(file="vaccine combined.png", plotcombine, 
       path = paste0(path_plot,"data_description/"), 
       height=10, width=10, units="in")
