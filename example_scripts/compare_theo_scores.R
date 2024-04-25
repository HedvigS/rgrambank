library(tidyverse)

old <- read_tsv("../../../grambank/grambank-analysed/R_grambank/output/PCA/theo_scores.tsv")

source("make_theo_scores.R")

GB_wide <- read_csv("../../../grambank/grambank/cldf/values.csv") 


source("make_binary_ParameterTable.R")

ParameterTable <-read_tsv("../../../grambank/grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv")

new <- make_theo_scores(ValueTable = GB_wide, ParameterTable = ParameterTable)

colnames(new)[2:7] <- paste0(colnames(new)[2:7], "_cookbook")

colnames(old)[2:7] <- paste0(colnames(old)[2:7], "_grambank-analysed")

joined <- full_join(new, old)

joined %>% 
  ggplot(aes(x = Fusion_cookbook, y = `Fusion_grambank-analysed`)) +
  geom_point(color = "#FF689F") +
  theme_classic() +
  labs(x = "Fusion (Coookbook)", y = "Fusion (grambank-analysed)") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) 

ggsave("new_old_theo_score_fusion.png", height = 3, width = 3)

joined %>% 
  ggplot(mapping = aes(x = joined$Informativity_cookbook, 
                       y = joined$`Informativity_grambank-analysed`)) +
  geom_point(color = "steelblue2") +
  theme_classic() +
  labs(x = "Informativity (Coookbook)", y = "Informativity (grambank-analysed)") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) 

ggsave("new_old_theo_score_inform.png", height = 3, width = 3)