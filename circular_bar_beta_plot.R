
library(tidyverse)
library(dplyr)
library(ggplot2)
library("wesanderson")
library(httpgd)
library(openxlsx)

main_model_never <- readRDS("main_model_never.rds")

grouping_met <- read.xlsx("ukb_abbre.xlsx")
grouping_met <- grouping_met %>%
    mutate(Shortened.name = gsub("_pct", "%", Shortened.name)) 
write.xlsx(grouping_met, "ukb_abbre.xlsx", overwrite=TRUE)

main_model_never <- select(main_model_never, -c(Metabolite, Group))
grouping_met <- rename(grouping_met, FieldID= UKB.Field.ID)
main_model_never <- left_join(main_model_never, grouping_met, by="FieldID")


never_sig <- main_model_never %>%
    select(Shortened.name, Group, Beta, p.fdr) %>%
    filter(main_model_never$p.fdr < 0.05)

never_sig$Group <- as.factor(never_sig$Group)

# Order data:
never_sig = never_sig %>% arrange(Group, Beta)

never_sig$id <- seq(1, nrow(never_sig))

# Create new "dummy" rows for the annotate labels (the scale)
dummy_row <- data.frame(
  id = c(190, 191, 192),        # new id for the dummy row
  Beta = c(1, 1, 1),    # value beyond the bar height
  Group = c("Triglycerides", "Triglycerides", "Triglycerides"),     # dummies, won't show up anyway cos beta is out of range
  Shortened.name = c("", "", ""),  # empty label
  p.fdr = c(1, 1, 1)
)

never_sig <- rbind(never_sig, dummy_row)

# Get the name and the y position of each label
label_data <- never_sig
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

hgd()

# Make the plot

ggplot(never_sig, aes(x=as.factor(id), y=Beta, fill=Group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=Beta, fill=Group), stat="identity", alpha=0.8, width=0.9) +
    geom_hline(yintercept = c(-0.15, -0.05, 0, 0.05, 0.15), color="grey", linetype="dotted", size=5) +
    annotate("text",
         x = -3.35,  # adjust where to place the sacle labels
         y = c(-0.15, -0.05, 0, 0.05, 0.15) + 0.02,  # push outside the scale lines
         label = c("- 0.15", "- 0.05", "0", "0.05", "0.15"),
         color = "black", size = 15, angle = 0, fontface = "bold", hjust = 0.5) +
  ylim(-0.2, 0.2) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size=60),
    legend.title = element_text(size=60),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(clip = "off")  +   
  geom_text(data=label_data, aes(x=id, y=0.2, label=Shortened.name, hjust=hjust, angle=angle), color="black",alpha=1, size=15, inherit.aes = FALSE ) +
    scale_fill_manual(values = wes_palette("Zissou1", length(unique(data$Group)), type = "continuous")) 
