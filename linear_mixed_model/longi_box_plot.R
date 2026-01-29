significant_met <- new_vs_never_model %>%
  filter(p <0.05)

significant_met <- significant_met$Outcome
significant_met <- c(significant_met, "f.eid", "time", "ad_use_longi")

new_never <- readRDS("new_never.rds")

# Filter dataframe to only these columns
sig_met <- new_never[, colnames(new_never) %in% significant_met] 

scale_sig_met <- sig_met %>%
  mutate(across(3:32, scale))  #scale the metabolites

top3_met <- scale_sig_met %>%
  select(c(f.eid, time, ad_use_longi,
           f.23520,
           f.23519,
           f.23516))

top3_met <- rename(top3_met, ID = 'f.eid')

df_long <- top3_met |>
  pivot_longer(
    cols = starts_with("f."),     # or c(y1, y2, y3)
    names_to = "Metabolites",        # new column that stores the y-column name
    values_to = "Value"          # new column that stores the y values
  )

ukb_abbre <- read.xlsx(ukb_abbre.xlsx)
ukb_abbre$FieldID <- paste0("f.", ukb_abbre$FieldID)
ukb_abbre <- rename(ukb_abbre, Metabolites = FieldID)

df_long <- df_long %>% left_join(ukb_abbre %>% select(Metabolites, Shortened.name), by = "Metabolites")
df_long <- rename(df_long, Outcome = Metabolites)

df_long$ad_use_longi <- as.character(df_long$ad_use_longi)
df_long$ad_use_longi[df_long$ad_use_longi == 0] <- "Controls"
df_long$ad_use_longi[df_long$ad_use_longi == 1] <- "New Users"

df_long$time <- as.character(df_long$time)
df_long$time[df_long$time == 0] <- "T0"
df_long$time[df_long$time == 1] <- "T1"

df_long$Shortened.name[df_long$Shortened.name == "XS_VLDL_CE"] <- "Cholesteryl Esters in XS VLDL"
df_long$Shortened.name[df_long$Shortened.name == "XS_VLDL_C"] <- "Cholesterol in XS VLDL"
df_long$Shortened.name[df_long$Shortened.name == "XS_VLDL_P"] <- "XS VLDL Conc."

df_long$Shortened.name <- factor(df_long$Shortened.name, levels = c("Cholesteryl Esters in XS VLDL", "Cholesterol in XS VLDL", "XS VLDL Conc."))


ggplot(df_long, aes(x=Shortened.name, y=Value, fill = time)) + 
  geom_boxplot(
    color="black",  #color of outline
    alpha=0.7,
    width = 0.6,
    # Notch?
    notch=TRUE,
    notchwidth = 0.7,
    
    # custom outliers
    outlier.colour="grey",
    outlier.fill="grey",
    outlier.size=3
    
  ) + facet_wrap(vars(ad_use_longi)) +
  scale_fill_manual(values=c("T0" = NA, "T1" = "red")) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  
  labs(
    x = "Metabolite",
    y = "Z score",
    fill = "Timepoint"
  ) +
  
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 15, hjust=0.5),     # rotate x labels
    axis.text.y  = element_text(size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 20, face = "bold")
  ) 
