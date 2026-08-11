
library(DT)
library(fgsea)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(stringr)
library(httpgd)
library(patchwork)
library(readxl)

hgd()

setwd("/Users/angelatsaii/Desktop/ukbrap_analyses/all_models")

main_model_never <- readRDS("main_model_never.rds")
main_model_past <- readRDS("main_model_past.rds")
main_model_future <- readRDS("main_model_future.rds")
metabolites_regrouped <- read_xlsx("metabolites_regrouped.xlsx")

metabolites_regrouped <- metabolites_regrouped %>% add_count(`New group`) %>% filter(n > 2) %>% select(-n) #remove groups with less than 3 metabolites

met.classes = vector("list",length = length(unique(metabolites_regrouped$`New group`)))
names(met.classes) = unique(metabolites_regrouped$`New group`)

for(i in 1:length(met.classes)){
  met.classes[[i]] = metabolites_regrouped$FieldID[which(metabolites_regrouped$`New group` == names(met.classes)[i])]
}

beta_never = main_model_never$Beta
names(beta_never) = main_model_never$FieldID

beta_past = main_model_past$Beta
names(beta_past) = main_model_past$FieldID

beta_future = main_model_future$Beta
names(beta_future) = main_model_future$FieldID


runFGSEA = function(pathways = met.classes, stats = ovca.mets, minSize=3, maxSize=500, file.result){
  fgsea.ovca <- fgseaMultilevel(pathways = pathways, stats = stats, minSize=minSize, maxSize=maxSize, eps=0)
  fgsea.ovca = fgsea.ovca[order(fgsea.ovca$padj),]
  to.print <- subset(fgsea.ovca, select = -leadingEdge)
    return(to.print)
  
}

never.fgsea = runFGSEA(pathways = met.classes, stats = beta_never)
never.fgsea$x = "All"
never.fgsea$stars = ""
never.fgsea$stars[which(never.fgsea$padj<0.05)] = "*"
never.fgsea$stars[which(never.fgsea$padj<0.001)] = "**"
never.fgsea = never.fgsea[order(never.fgsea$NES),]
never.fgsea$pathway = factor(never.fgsea$pathway, levels = never.fgsea$pathway)


past.fgsea = runFGSEA(pathways = met.classes, stats = beta_past)
past.fgsea$x = "All"
past.fgsea$stars = ""
past.fgsea$stars[which(past.fgsea$padj<0.05)] = "*"
past.fgsea$stars[which(past.fgsea$padj<0.001)] = "**"
past.fgsea = past.fgsea[order(past.fgsea$NES),]
past.fgsea$pathway = factor(past.fgsea$pathway, levels = past.fgsea$pathway)


future.fgsea = runFGSEA(pathways = met.classes, stats = beta_future)
future.fgsea$x = "All"
future.fgsea$stars = ""
future.fgsea$stars[which(future.fgsea$padj<0.05)] = "*"
future.fgsea$stars[which(future.fgsea$padj<0.001)] = "**"
future.fgsea = future.fgsea[order(future.fgsea$NES),]
future.fgsea$pathway = factor(future.fgsea$pathway, levels = future.fgsea$pathway)

never.fgsea$Model = "Current vs Never"
past.fgsea$Model = "Current vs Past"
future.fgsea$Model = "Current vs Future"

all_fgsea = rbind(never.fgsea, past.fgsea, future.fgsea)

all_fgsea$Model <- factor(all_fgsea$Model, levels = c("Current vs Never", "Current vs Past", "Current vs Future"))

a <- ggplot(data = all_fgsea, aes(x = x, y = pathway, fill = NES)) +
  geom_tile(color = "white", linewidth = 0.1, width = 0.7, alpha = 0.85) +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 9, type = "continuous"), limits = c(-3,3)) +
  geom_text(aes(y = pathway, label=stars), color="black", size=4, 
            nudge_x = 0, nudge_y = -0.3, fontface = "bold") + theme_minimal() +         
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15, color = "black"),
        legend.title = element_text(face = "bold", size = 15, color = "black"),
        legend.text = element_text(face = "bold", size = 15, color = "black"),
        strip.text = element_text(size = 15, face = "bold", color = "black"),
        strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, face = "bold", 
                                    size = 15), legend.position="bottom") +
  guides(fill=guide_colorbar(title = "Normalised Enrichment Score",title.position = "top",
          barwidth = 15, barheight = 1, label.position = "bottom"))+
  facet_wrap(~Model) +
  coord_fixed(ratio = 0.2) 

  #### Group metabolites into VLDL/IDL/LDL/HDL subclasses for enrichment analysis ####

main_model_never <- main_model_never %>%
  mutate(subgroup = case_when(
    Metabolite == "Total Cholesterol Minus HDL-C" ~ "NA",
    Metabolite == "Remnant Cholesterol (Non-HDL, Non-LDL -Cholesterol)" ~ "NA",
    grepl("VLDL", Metabolite) ~ "VLDL",
    grepl("LDL", Metabolite) ~ "LDL",
    grepl("HDL", Metabolite) ~ "HDL",
    grepl("IDL", Metabolite) ~ "IDL",
    TRUE ~ "NA"
  ))

main_model_never_subgroup <- main_model_never %>%
  filter(subgroup != "NA")

main_model_past <- main_model_past %>%
  mutate(subgroup = case_when(
    Metabolite == "Total Cholesterol Minus HDL-C" ~ "NA",
    Metabolite == "Remnant Cholesterol (Non-HDL, Non-LDL -Cholesterol)" ~ "NA",
    grepl("VLDL", Metabolite) ~ "VLDL",
    grepl("LDL", Metabolite) ~ "LDL",
    grepl("HDL", Metabolite) ~ "HDL",
    grepl("IDL", Metabolite) ~ "IDL",
    TRUE ~ "NA"
  ))

main_model_past_subgroup <- main_model_past %>%
  filter(subgroup != "NA")

main_model_future <- main_model_future %>%
  mutate(subgroup = case_when(
    Metabolite == "Total Cholesterol Minus HDL-C" ~ "NA",
    Metabolite == "Remnant Cholesterol (Non-HDL, Non-LDL -Cholesterol)" ~ "NA",
    grepl("VLDL", Metabolite) ~ "VLDL",
    grepl("LDL", Metabolite) ~ "LDL",
    grepl("HDL", Metabolite) ~ "HDL",
    grepl("IDL", Metabolite) ~ "IDL",
    TRUE ~ "NA"
  ))

main_model_future_subgroup <- main_model_future %>%
  filter(subgroup != "NA")

sub_met.classes = vector("list",length = length(unique(main_model_never_subgroup$subgroup)))
names(sub_met.classes) = unique(main_model_never_subgroup$subgroup)

for(i in 1:length(sub_met.classes)){
  sub_met.classes[[i]] = main_model_never_subgroup$FieldID[which(main_model_never_subgroup$subgroup == names(sub_met.classes)[i])]
}

runFGSEA = function(pathways = sub_met.classes, stats = ovca.mets, minSize=3, maxSize=500, file.result){
  fgsea.ovca <- fgseaMultilevel(pathways = pathways, stats = stats, minSize=minSize, maxSize=maxSize, eps=0)
  fgsea.ovca = fgsea.ovca[order(fgsea.ovca$padj),]
  to.print <- subset(fgsea.ovca, select = -leadingEdge)
  return(to.print)
  
}

beta_never_sub = main_model_never_subgroup$Beta
names(beta_never_sub) = main_model_never_subgroup$FieldID
never.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_never_sub)
never.fgsea$x = "All"
never.fgsea$stars = ""
never.fgsea$stars[which(never.fgsea$padj<0.05)] = "*"
never.fgsea$stars[which(never.fgsea$padj<0.001)] = "**"
never.fgsea = never.fgsea[order(never.fgsea$NES),]
never.fgsea$pathway = factor(never.fgsea$pathway, levels = never.fgsea$pathway)


beta_past_sub = main_model_past_subgroup$Beta
names(beta_past_sub) = main_model_past_subgroup$FieldID
past.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_past_sub)
past.fgsea$x = "All"
past.fgsea$stars = ""
past.fgsea$stars[which(past.fgsea$padj<0.05)] = "*"
past.fgsea$stars[which(past.fgsea$padj<0.001)] = "**"
past.fgsea = past.fgsea[order(past.fgsea$NES),]
past.fgsea$pathway = factor(past.fgsea$pathway, levels = past.fgsea$pathway)


beta_future_sub = main_model_future_subgroup$Beta
names(beta_future_sub) = main_model_future_subgroup$FieldID
future.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_future_sub)
future.fgsea$x = "All"
future.fgsea$stars = ""
future.fgsea$stars[which(future.fgsea$padj<0.05)] = "*"
future.fgsea$stars[which(future.fgsea$padj<0.001)] = "**"
future.fgsea = future.fgsea[order(future.fgsea$NES),]
future.fgsea$pathway = factor(future.fgsea$pathway, levels = future.fgsea$pathway)

never.fgsea$Model = "Current vs Never"
past.fgsea$Model = "Current vs Past"
future.fgsea$Model = "Current vs Future"

all_sub_fgsea = rbind(never.fgsea, past.fgsea, future.fgsea)
all_sub_fgsea$Model <- factor(all_sub_fgsea$Model, levels = c("Current vs Never", "Current vs Past", "Current vs Future"))


b <- ggplot(data = all_sub_fgsea, aes(x = x, y = pathway, fill = NES)) +
  geom_tile(color = "white", linewidth = 0.1, width = 0.7, alpha = 0.85) +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 9, type = "continuous"), limits = c(-3,3)) +
  geom_text(aes(y = pathway, label=stars), color="black", size=4, 
            nudge_x = 0, nudge_y = -0.3, fontface = "bold") + theme_minimal() +         
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15, color = "black"),
        legend.title = element_text(face = "bold", size = 15, color = "black"),
        legend.text = element_text(face = "bold", size = 15, color = "black"),
        plot.caption = element_text(hjust = 1,size = 11, face = "bold"),
        strip.text = element_text(size = 15, face = "bold", color = "black"),
        strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, face = "bold", 
                                    size = 15), legend.position="bottom") +
  guides(fill=guide_colorbar(title = "Normalised Enrichment Score",title.position = "top",
          barwidth = 15, barheight = 1, label.position = "bottom"))+
  labs(caption = "* FDR<0.05; ** FDR<0.001", y = "Pathway") + 
  facet_wrap(~Model) +
  coord_fixed(ratio = 0.2) 


# Combine the plots
combined <- a / b + 
  plot_layout(guides = "collect", heights = c(2, 0.6), tag_level = "new") &
  theme(legend.position = "bottom", plot.tag = element_text(size = 13, face = "bold"))

combined <- combined + plot_annotation(tag_levels = list(c("a", "b"))) 

jpeg(
  "all_enrich_plot.jpeg",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)

combined

dev.off()
