#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_version("viridis", version = "0.6.4", repos = "http://cran.us.r-project.org")
library(viridis)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

source("../functions/make_theo_scores.R")

# the function make_theo_scores needs only binary features. If the ValueTable or ParameterTable is not binary, it will binraised them using the functions below.
source("../functions/make_binary_ParameterTable.R")
source("../functions/make_binary_ValueTable.R")

theo_scores_table_cookbook <- make_theo_scores(ValueTable =  GB_rcldf_obj$tables$ValueTable, 
                                               ParameterTable =  GB_rcldf_obj$tables$ParameterTable)

contact_lgs <- read_tsv("output/tables/contact_languages.tsv", show_col_types = FALSE) %>% 
  mutate(Contact_lg = "YES")

LanguageTable <- GB_rcldf_obj$tables$LanguageTable %>%
  dplyr::select(ID, Glottocode, lineage, Name, Family_name) %>% 
  left_join(contact_lgs, by = "Glottocode") %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "?", Family_name)) %>% 
  mutate(Contact_lgs = ifelse(Contact_lg == "YES" , Family_name, NA)) %>% 
  mutate(English = ifelse(str_detect(lineage, "indo1319") |
                            str_detect(Glottocode, "indo1319") , "Indo-European", NA)) %>% 
  mutate(English = ifelse(str_detect(lineage, "germ1287") |
                        str_detect(Glottocode, "germ1287") , "Germanic", English)) %>% 
  mutate(English = ifelse(str_detect(lineage, "stan1293") |
                        str_detect(Glottocode, "stan1293") , "English", English)) %>% 
  mutate(Persian = ifelse(str_detect(lineage, "indo1319") |
                            str_detect(Glottocode, "indo1319") , "Indo-European", NA)) %>% 
  mutate(Persian = ifelse(str_detect(lineage, "indo1320") |
                        str_detect(Glottocode, "indo1320") , "Indo-Iranian", Persian)) %>% 
  mutate(Persian = ifelse(str_detect(lineage, "fars1254") |
                          str_detect(Glottocode, "fars1254") , "Persian", Persian)) %>% 
  mutate(Malay = ifelse(str_detect(lineage, "aust1307") |
                            str_detect(Glottocode, "aust1307") , "Austronesian", NA)) %>% 
  mutate(Malay = ifelse(str_detect(lineage, "mala1538") |
                          str_detect(Glottocode, "mala1538") , "Malayic", Malay)) %>% 
  mutate(Malay = ifelse(str_detect(lineage, "nucl1806") |
                          str_detect(Glottocode, "nucl1806") , "Nuclear Malayic", Malay)) %>% 
  mutate(Mandarin = ifelse(str_detect(lineage, "sino1245") |
                          str_detect(Glottocode, "sino1245") , "Tibeto-Burman", NA)) %>% 
  mutate(Mandarin = ifelse(str_detect(lineage, "sini1245") |
                             str_detect(Glottocode, "sini1245") , "Sinitic", Mandarin)) %>% 
  mutate(Mandarin = ifelse(str_detect(lineage, "mand1415") |
                             str_detect(Glottocode, "mand1415") , "Mandarin", Mandarin)) %>% 
  mutate(Arabic = ifelse(str_detect(lineage, "afro1255") |
                             str_detect(Glottocode, "afro1255") , "Afro-Asiatic", NA)) %>% 
  mutate(Arabic = ifelse(str_detect(lineage, "semi1276") |
                           str_detect(Glottocode, "semi1276") , "Semitic", Arabic)) %>% 
  mutate(Arabic = ifelse(str_detect(lineage, "najd1235") |
                           str_detect(Glottocode, "najd1235") , "Najdi Arabic", Arabic)) %>% 
  mutate(Arabic = ifelse(str_detect(lineage, "egyp1253") |
                           str_detect(Glottocode, "egyp1253") , "Egyptian Arabic", Arabic)) 

LanguageTable$English <- factor(LanguageTable$English, levels = c("English", "Germanic", "Indo-European", NA))
LanguageTable$Persian <- factor(LanguageTable$Persian, levels = c("Persian", "Indo-Iranian", "Indo-European", NA))
LanguageTable$Mandarin <- factor(LanguageTable$Mandarin, levels = c("Mandarin", "Sinitic", "Tibeto-Burman", NA))
LanguageTable$Malay <- factor(LanguageTable$Malay, levels = c("Nuclear Malayic", "Malayic", "Austronesian", NA))
LanguageTable$Arabic <- factor(LanguageTable$Arabic, levels = c("Egyptian Arabic", "Semitic", "Afro-Asiatic", NA))

joined <- theo_scores_table_cookbook %>% 
  full_join(LanguageTable, by = c("Language_ID" = "ID"))

y_jitter <- c(50,52,  55, 60,62, 64, 68, 70, 72, 74 , 77, 80,82,  88, 85, 86, 90, 93, 96, 98, 100, 102, 103, 106)


plot <- function(theo_score, focus, n){
  
  joined$Theo_score <-   joined[[theo_score]]
  joined$Focus <-   joined[[focus]]
  
  joined %>% 
  filter(!is.na(Theo_score)) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = Theo_score), 
                 alpha = 0.5,fill = "gray", bins = 20) +
  geom_point(data = joined %>% filter(!is.na(Focus)), 
             mapping = aes(x = Theo_score, color = Focus), y = sample(x = y_jitter, size = n, replace = TRUE), na.rm = T, alpha = 0.8) +
  theme_classic() +		
  theme(axis.title = element_text(size=18), 		
        legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 18, angle = 70, hjust=0.95), 		
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.3, size = 16)) +		
  labs(title=paste0(theo_score, " - ", focus),		
       x = theo_score, y = "Number of languages") 

}

dir <- "output/plots/"
if(!dir.exists(dir)){dir.create(dir)}


plot(theo_score = "Fusion", focus = "English", n = 78) 
ggsave("output/plots/Fusion_English.png")

plot(theo_score = "Fusion", focus = "Persian", n = 78)
ggsave("output/plots/Fusion_Persian.png")

plot(theo_score = "Fusion", focus = "Mandarin", n = 196)
ggsave("output/plots/Fusion_Mandarin.png")

plot(theo_score = "Fusion", focus = "Arabic", n = 120)
ggsave("output/plots/Fusion_Arabic.png")

plot(theo_score = "Fusion", focus = "Malay", n = 536)
ggsave("output/plots/Fusion_Malay.png")

plot(theo_score = "Fusion", focus = "Contact_lgs", n = 18)
ggsave("output/plots/Fusion_Contact_lgs.png")


plot(theo_score = "Informativity", focus = "English", n = 78)
ggsave("output/plots/Informativity_English.png")

plot(theo_score = "Informativity", focus = "Persian", n = 78)
ggsave("output/plots/Informativity_Persian.png")

plot(theo_score = "Informativity", focus = "Mandarin", n = 196)
ggsave("output/plots/Informativity_Mandarin.png")

plot(theo_score = "Informativity", focus = "Arabic", n = 120)
ggsave("output/plots/Informativity_Arabic.png")

plot(theo_score = "Informativity", focus = "Malay", n = 536)
ggsave("output/plots/Informativity_Malay.png")

plot(theo_score = "Informativity", focus = "Contact_lgs", n = 18)
ggsave("output/plots/Informativity_Contact_lgs.png")

