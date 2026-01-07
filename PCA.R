install.packages("psych")
library(psych)
library(dplyr)

main_model_never <- readRDS("main_model_never.rds")
main_model_past <- readRDS("main_model_past.rds")
main_model_future <- readRDS("main_model_future.rds")

met_data <- readRDS("met_data.rds")
sig <- main_model_never %>% filter(sig == "Yes")
sig2 <- main_model_past %>% filter(sig == "Yes")
sig3 <- main_model_future %>% filter(sig == "Yes")

ls.met1 <- sig$Outcome
ls.met2 <- sig2$Outcome
ls.met3 <- sig3$Outcome

overlap_met <- Reduce(intersect, list(ls.met1, ls.met2, ls.met3))  

# ============ PCA

dat.pca = met_data %>% select(overlap_met) %>% .[complete.cases(.),]

fit.pca = prcomp(dat.pca,center=T,scale=T)

# Variance explained
var.explained = fit.pca$sdev^2/sum((fit.pca$sdev)^2) *100

dat.var.explained = data.frame(npc = 1:length(var.explained), R2 = var.explained)

dat.var.explained$R2 <- round(dat.var.explained$R2, 2)

plot(var.explained %>% head(n=50), type = "b",
     
     xlab = "Principal Component",
     
     ylab = "Percentage of Variance Explained")

# Check loadings
dat.loading.correlation = cor(dat.pca,fit.pca$x)  #correlations between original variables and PCs = how strongly each original variable is associated with each PC

loading_9pcs <- dat.loading.correlation[, 1:9]
library(tibble)
loading_9pcs <- rownames_to_column(as.data.frame(loading_9pcs), var = "Outcome")

grouping_met <- readRDS("grouping_met.rds")
loading_9pcs <- left_join(loading_9pcs, grouping_met, by = "Outcome")

loading_9pcs <- loading_9pcs %>%
  mutate(across(starts_with("PC"), abs))

pc1 <- loading_9pcs %>% select(Outcome, PC1, Metabolite, Group) %>% filter(PC1 > 0.3) 
pc1_num <- as.data.frame(table(pc1$Group)) 
pc1_num <- rename(pc1_num, Pathway = "Var1", PC1_Count = "Freq")

pc2 <- loading_9pcs %>% select(Outcome, PC2, Metabolite, Group) %>% filter(PC2 > 0.3) 
pc2_num <- as.data.frame(table(pc2$Group))
pc2_num <- rename(pc2_num, Pathway = "Var1", PC2_Count = "Freq")

pc3 <- loading_9pcs %>% select(Outcome, PC3, Metabolite, Group) %>% filter(PC3 > 0.3) 
pc3_num <- as.data.frame(table(pc3$Group))
pc3_num <- rename(pc3_num, Pathway = "Var1", PC3_Count = "Freq")

pc4 <- loading_9pcs %>% select(Outcome, PC4, Metabolite, Group) %>% filter(PC4 > 0.3) 
pc4_num <- as.data.frame(table(pc4$Group))
pc4_num <- rename(pc4_num, Pathway = "Var1", PC4_Count = "Freq")

pc5 <- loading_9pcs %>% select(Outcome, PC5, Metabolite, Group) %>% filter(PC5 > 0.3)
pc5_num <- as.data.frame(table(pc5$Group))
pc5_num <- rename(pc5_num, Pathway = "Var1", PC5_Count = "Freq")

pc6 <- loading_9pcs %>% select(Outcome, PC6, Metabolite, Group) %>% filter(PC6 > 0.3)
pc6_num <- as.data.frame(table(pc6$Group))
pc6_num <- rename(pc6_num, Pathway = "Var1", PC6_Count = "Freq")

pc7 <- loading_9pcs %>% select(Outcome, PC7, Metabolite, Group) %>% filter(PC7 > 0.3)
pc7_num <- as.data.frame(table(pc7$Group))
pc7_num <- rename(pc7_num, Pathway = "Var1", PC7_Count = "Freq")

pc8 <- loading_9pcs %>% select(Outcome, PC8, Metabolite, Group) %>% filter(PC8 > 0.3)
pc8_num <- as.data.frame(table(pc8$Group))
pc8_num <- rename(pc8_num, Pathway = "Var1", PC8_Count = "Freq")

pc9 <- loading_9pcs %>% select(Outcome, PC9, Metabolite, Group) %>% filter(PC9 > 0.3)
pc9_num <- as.data.frame(table(pc9$Group))
pc9_num <- rename(pc9_num, Pathway = "Var1", PC9_Count = "Freq")

all_pc_nums <- left_join(pc1_num, pc2_num, by = "Pathway") %>%
  left_join(pc3_num, by = "Pathway") %>%
  left_join(pc4_num, by = "Pathway") %>%
  left_join(pc5_num, by = "Pathway") %>%
  left_join(pc6_num, by = "Pathway") %>%
  left_join(pc7_num, by = "Pathway") %>%
  left_join(pc8_num, by = "Pathway") %>%
  left_join(pc9_num, by = "Pathway") 

write_xlsx(all_pc_nums, "all_pc_nums.xlsx")

all_pc_nums$num_group <- rowSums(all_pc_nums[, 2:10], na.rm = TRUE)

install.packages("wesanderson")

library(cluster)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(wesanderson)

dat.loading.correlation <- cor(dat.pca, fit.pca$x)
loading_2pcs <- dat.loading.correlation[, 1:2]
loading_df <- as.data.frame(loading_2pcs) %>%
  mutate(Outcome = rownames(loading_2pcs))  # example grouping

grouping_met_enrich <- readRDS("grouping_met_enrich.rds")
loading_df <- left_join(loading_df, grouping_met_enrich, by = "Outcome")

loading_df <- loading_df %>%
  filter()

# choose base palette (Zissou1 has 5 colors by default)
pal <- wes_palette("Zissou1", type = "continuous")

# extend to 9 colors
pal9 <- colorRampPalette(pal)(15)

ggplot(loading_df, aes(x = PC1, y = PC2, fill = enrich_pathway, color = enrich_pathway)) +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_manual(values = pal9) +
  scale_fill_manual(values = pal9) + ### ggplot labels
  labs(
    title = "Metabolite Loadings in PC1 and PC2",
    x = "PC1",
    y = "PC2",
    color = "Group"    # or colour = "Legend Title"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0),  # title size, bold, centered
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 20),     # rotate x labels
    axis.text.y  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 15),
    legend.position = "right",   # "top", "left", "right", or c(0.8, 0.2) for coordinates
    
  ) + guides(fill = "none")   # hides the fill legend

save.image("PCA_main_models.RData")
