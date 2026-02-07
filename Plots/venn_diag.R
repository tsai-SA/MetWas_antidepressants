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
 
venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr), set_size = 4.5, label = "count", label_alpha=0, label_size = 5,
                category.names = c("Current/Never","Current/Past","Current/Future")) +
                scale_fill_distiller(palette = "Paired") +
                scale_x_continuous(expand = expansion(mult = .15))+
                ggtitle('FDR-sig metabolites per set') + 
                theme(plot.title = element_text(size = 15), 
                    legend.text  = element_text(size = 13),   # legend labels
                    legend.title = element_text(size = 13))   # legend title


