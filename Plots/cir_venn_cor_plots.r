
library(tidyverse)
library(dplyr)
library(ggplot2)
library("wesanderson")
library(httpgd)
library(openxlsx)
library(wesanderson)

setwd("/Users/angelatsaii/Desktop/ukbrap_analyses")
main_model_never <- readRDS("main_model_never.rds")

ukb_abbre <- read.xlsx("ukb_abbre.xlsx")
ukb_abbre <- ukb_abbre %>%
    mutate(Shortened.name = gsub("_pct", "%", Shortened.name)) 
write.xlsx(ukb_abbre, "ukb_abbre.xlsx", overwrite=TRUE)

main_model_never <- select(main_model_never, -c(Metabolite, Group))
ukb_abbre <- rename(ukb_abbre, FieldID= UKB.Field.ID)
main_model_never <- left_join(main_model_never, ukb_abbre, by="FieldID")


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
never_sig$Group <- as.factor(never_sig$Group)

# Make the plot

circle <- ggplot(never_sig, aes(x=as.factor(id), y=Beta, fill=Group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=Beta, fill=Group), stat="identity", alpha=0.8, width=0.9) +
    geom_hline(yintercept = c(-0.15, -0.05, 0, 0.05, 0.15), color="grey", linetype="dotted", size=0.7) +
    annotate("text",
         x = -3.35,  # adjust where to place the sacle labels
         y = c(-0.15, -0.05, 0, 0.05, 0.15) + 0.02,  # push outside the scale lines
         label = c("- 0.15", "- 0.05", "0", "0.05", "0.55"),
         color = "black", size = 3, angle = 0, fontface = "bold", hjust = 0.5) +
  ylim(-0.2, 0.2) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size=9),
    legend.title = element_text(size=8, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(clip = "off")  +   
  geom_text(data=label_data, aes(x=id, y=0.2, label=Shortened.name, hjust=hjust, angle=angle), color="black",alpha=1, size=3, inherit.aes = FALSE ) +
    scale_fill_manual(values = wes_palette("Zissou1", length(unique(never_sig$Group)), type = "continuous")) 


#### Venn

library(ggplot2)
library(dplyr)
library(ggVennDiagram)
library(httpgd)
hgd()

main_model_never <- readRDS("main_model_never.rds")
main_model_past <- readRDS("main_model_past.rds")
main_model_future <- readRDS("main_model_future.rds")


grouping_met <- readRDS("grouping_met.rds")

main_model_never <- left_join(main_model_never, grouping_met, by = 'Outcome')
main_model_past <- left_join(main_model_past, grouping_met, by = 'Outcome')
main_model_future <- left_join(main_model_future, grouping_met, by = 'Outcome')


full.set1.fdr <- main_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- main_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- main_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
 
venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr), set_size = 4.5, label = "count", label_alpha=0, label_size = 4.5,
                category.names = c(" "," ","Current/Future")) 
venn$layers

# Move category labels upward/downward manually
venn$layers[[3]]$data$Y <- venn$layers[[3]]$data$Y * 1.15
venn$layers[[3]]$data$X <- venn$layers[[3]]$data$X * 0.85

venn <- venn +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15)) +
  ggtitle("Current/Never   Current/Past") +
  theme(
    legend.position = "none",
    plot.title = element_text(size=13)
  ) 
  
#### correlation plot

setwd("/Users/angelatsaii/Desktop/ukbrap_analyses")

library(patchwork)

main_model_never <- readRDS("main_model_never.rds")
main_model_past <- readRDS("main_model_past.rds")
main_model_future <- readRDS("main_model_future.rds")

colnames(main_model_never) <- paste0('never_', colnames(main_model_never))
colnames(main_model_never)

colnames(main_model_past) <- paste0('past_', colnames(main_model_past))
colnames(main_model_past)

colnames(main_model_future) <- paste0('future_', colnames(main_model_future))
colnames(main_model_future)

main_model_never <- rename(main_model_never, FieldID = never_FieldID)
main_model_past <- rename(main_model_past, FieldID = past_FieldID)
main_model_future <- rename(main_model_future, FieldID = future_FieldID)

combined_beta <- merge(main_model_never, main_model_past, by = "FieldID")
combined_beta <- merge(combined_beta, main_model_future, by = "FieldID")
combined_beta <- left_join(combined_beta, grouping_met, by = "FieldID")
combined_beta$Sig_all <- ifelse(combined_beta$never_p.fdr < 0.05 & 
        combined_beta$past_p.fdr < 0.05 & combined_beta$future_p.fdr < 0.05, "Yes", "No")

cor_neverpast <- ggplot(combined_beta, aes(x= never_Beta, y = past_Beta)) + 
	geom_point(size=1, aes(color = Group, shape = Sig_all)) + 
	xlim(-0.2, 0.2) + ylim(-0.2, 0.2) + 
	scale_shape_manual(values = c("Yes" = 16, "No" = 1)) + 
	labs(x = "Current/Never", y = "Current/Past") + 
    scale_color_manual(values = wes_palette("Zissou1", n = 18, type = "continuous")) +
    theme(legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text(size=12),
          axis.title = element_text(size=13)) +
    annotate("text", x = 0.1, y = -0.15, label = "r = 0.99", size = 5)

	

cor.test(combined_beta$never_Beta, combined_beta$past_Beta, 
		use="complete.obs", method="pearson")


cor.test(combined_beta$never_Beta, combined_beta$future_Beta, 
		use="complete.obs", method="pearson")

cor_neverfuture <- ggplot(combined_beta, aes(x= never_Beta, y = future_Beta)) + 
	geom_point(size=1, aes(color = Group, shape = Sig_all)) + 
	xlim(-0.2, 0.2) + ylim(-0.2, 0.2) + 
	labs(shape="Significant in all three models", color="Group") + 
	scale_shape_manual(values = c("Yes" = 16, "No" = 1)) + 
	labs(x = "Current/Never", y = "Current/Future") + 
    scale_color_manual(values = wes_palette("Zissou1", n = 18, type = "continuous")) +
        theme(legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text(size=12),
          axis.title = element_text(size=13)) +
    annotate("text", x = 0.1, y = -0.15, label = "r = 0.98", size = 5)

cor_neverpast <- cor_neverpast + coord_fixed()
cor_neverfuture <- cor_neverfuture + coord_fixed()
cor_plot <- cor_neverpast + cor_neverfuture 

design <- c(
  area(t = 1, l = 1, b = 12, r = 3),  # A
  area(t = 12, l = 1, b = 14, r = 1),  # B
    area(t = 12, l = 2, b = 14, r = 2),  # B
  area(t = 12, l = 3, b = 14, r = 3)   # C
)

theme_tag <- theme(
  plot.tag = element_text(face = "bold", size = 15)
)

circle   <- circle + theme_tag
cor_neverpast <- cor_neverpast + theme_tag
cor_neverfuture <- cor_neverfuture + theme_tag
venn <- venn + theme_tag


circle + cor_neverpast + cor_neverfuture + venn +
  plot_layout(design = design) +
  plot_annotation(tag_levels = "a")

ggsave(
  "plot.png",
  width = 11,   # match Word page width
  height = 12,
  dpi = 600
)
