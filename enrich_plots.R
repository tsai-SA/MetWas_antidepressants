
library(DT)
library(fgsea)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(stringr)
library(httpgd)
hgd()
library(patchwork)



main_model_never <- readRDS("main_model_never.rds")
main_model_past <- readRDS("main_model_past.rds")
main_model_future <- readRDS("main_model_future.rds")
grouping_met <- readRDS("grouping_met_enrich.rds")

grouping_met <- grouping_met %>%
  mutate(enrich_pathway = case_when(
    Group == "Apolipoproteins" ~ "Others",
    Group == "Fluid balance" ~ "Others",
    Group == "Lipoprotein particle sizes" ~ "Others",
    Group == "Inflammation" ~ "Others",
    TRUE ~ Group
  ))

met.classes = vector("list",length = length(unique(grouping_met$enrich_pathway)))
names(met.classes) = unique(grouping_met$enrich_pathway)

for(i in 1:length(met.classes)){
  met.classes[[i]] = grouping_met$Outcome[which(grouping_met$enrich_pathway == names(met.classes)[i])]
}


beta_never = main_model_never$Beta
names(beta_never) = main_model_never$Outcome

beta_past = main_model_past$Beta
names(beta_past) = main_model_past$Outcome

beta_future = main_model_future$Beta
names(beta_future) = main_model_future$Outcome


runFGSEA = function(pathways = met.classes, stats = ovca.mets, minSize=3, maxSize=500, file.result){
  fgsea.ovca <- fgseaMultilevel(pathways = pathways, stats = stats, minSize=minSize, maxSize=maxSize, eps=0)
  fgsea.ovca = fgsea.ovca[order(fgsea.ovca$padj),]
  to.print = fgsea.ovca[,-8]
  
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
  scale_fill_gradientn(colours = wes_palette("Zissou1", 9, type = "continuous"), limits = c(-3,2.6)) +
  geom_text(aes(y = pathway, label=stars), color="black", size=10, 
            nudge_x = 0, nudge_y = -0.3, fontface = "bold") + theme_minimal() +         
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 25, color = "black"),
        legend.title = element_text(face = "bold", size = 25, color = "black"),
        legend.text = element_text(face = "bold", size = 25, color = "black"),
        plot.caption = element_text(hjust = 1,size = 20, face = "bold"),
        strip.text = element_text(size = 25, face = "bold", color = "black"),
        strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, face = "bold", 
                                    size = 25), legend.position="bottom") +
  guides(fill=guide_colorbar(title = "Normalised Enrichment Score",title.position = "top",
          barwidth = 25, barheight = 1, label.position = "bottom"))+
  labs(caption = "* FDR<0.05; ** FDR<0.001", y = "Pathway") + 
  facet_wrap(~Model) +
  coord_fixed(ratio = 0.2) 
  

  #### lipoprotein subclasses


lipoprotein_sub <- grouping_met %>%
  filter(str_detect(Group, regex("Lipoprotein subclasses", ignore_case = TRUE)))

lipoprotein_sub <- lipoprotein_sub %>%
  mutate(subgroup = case_when(
    grepl("VLDL", Metabolite) ~ "VLDL",
    grepl("LDL", Metabolite) ~ "LDL",
    grepl("HDL", Metabolite) ~ "HDL",
    grepl("IDL", Metabolite) ~ "IDL",
    TRUE ~ "NA"

  ))

sub_met.classes = vector("list",length = length(unique(lipoprotein_sub$subgroup)))
names(sub_met.classes) = unique(lipoprotein_sub$subgroup)

for(i in 1:length(sub_met.classes)){
  sub_met.classes[[i]] = lipoprotein_sub$Outcome[which(lipoprotein_sub$subgroup == names(sub_met.classes)[i])]
}

runFGSEA = function(pathways = sub_met.classes, stats = ovca.mets, minSize=3, maxSize=500, file.result){
  fgsea.ovca <- fgseaMultilevel(pathways = pathways, stats = stats, minSize=minSize, maxSize=maxSize, eps=0)
  fgsea.ovca = fgsea.ovca[order(fgsea.ovca$padj),]
  to.print = fgsea.ovca[,-8]
  
}

beta_never = main_model_never$Beta
names(beta_never) = main_model_never$Outcome
never.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_never)
never.fgsea$x = "All"
never.fgsea$stars = ""
never.fgsea$stars[which(never.fgsea$padj<0.05)] = "*"
never.fgsea$stars[which(never.fgsea$padj<0.001)] = "**"
never.fgsea = never.fgsea[order(never.fgsea$NES),]
never.fgsea$pathway = factor(never.fgsea$pathway, levels = never.fgsea$pathway)


beta_past = main_model_past$Beta
names(beta_past) = main_model_past$Outcome
past.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_past)
past.fgsea$x = "All"
past.fgsea$stars = ""
past.fgsea$stars[which(past.fgsea$padj<0.05)] = "*"
past.fgsea$stars[which(past.fgsea$padj<0.001)] = "**"
past.fgsea = past.fgsea[order(past.fgsea$NES),]
past.fgsea$pathway = factor(past.fgsea$pathway, levels = past.fgsea$pathway)


beta_future = main_model_future$Beta
names(beta_future) = main_model_future$Outcome
future.fgsea = runFGSEA(pathways = sub_met.classes, stats = beta_future)
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


b <- ggplot(data = all_fgsea, aes(x = x, y = pathway, fill = NES)) +
  geom_tile(color = "white", linewidth = 0.1, width = 0.7, alpha = 0.85) +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 9, type = "continuous")[5:9], limits = c(0,2.7)) +
  geom_text(aes(y = pathway, label=stars), color="black", size=10, 
            nudge_x = 0, nudge_y = -0.3, fontface = "bold") + theme_minimal() +         
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 25, color = "black"),
        legend.title = element_text(face = "bold", size = 25, color = "black"),
        legend.text = element_text(face = "bold", size = 25, color = "black"),
        plot.caption = element_text(hjust = 1,size = 20, face = "bold"),
        strip.text = element_text(size = 25, face = "bold", color = "black"),
        strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, face = "bold", 
                                    size = 25), legend.position="bottom") +
  guides(fill="none")+
  facet_wrap(~Model) +
  coord_fixed(ratio = 0.2) 


# Combine the plots
combined <- a / b + 
  plot_layout(guides = "collect", heights = c(2, 0.6), tag_level = "new") &
  theme(legend.position = "bottom", plot.tag = element_text(size = 30, face = "bold"))

combined <- combined + plot_annotation(tag_levels = list(c("2a", "2b"))) 
