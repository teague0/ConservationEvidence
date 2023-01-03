rm(list=ls())

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(viridis)
library(scales)

datapath <- "/Users/teagueomara/Library/CloudStorage/Box-Box/- Missions & Programs/Collaborative Projects/conservation_evidence/Bio Cons SI Shortfalls Cons Sci/data and script/"
bats<- read.csv(paste0(datapath, "chiroptera_iucn_list.csv"))%>%
  mutate(redlistCategory = fct_relevel(redlistCategory, "Data Deficient",
                            "Least Concern", "Near Threatened", 
                            "Vulnerable", "Endangered", "Critically Endangered"))

dd <- bats %>%
  filter(redlistCategory == "Data Deficient")

cons_land_protection <- read.csv(paste0(datapath,"LandProtection.csv"));cons_species_mgmt <- read.csv(paste0(datapath,"SpeciesMgmt.csv"));cons_education <- read.csv(paste0(datapath,"EducationAwareness.csv"));
cons_lawpolicy <- read.csv(paste0(datapath,"LawPolicy.csv"));cons_livelihood_econ <- read.csv(paste0(datapath,"LivelihoodEconomic.csv"));res_taxonomy <- read.csv(paste0(datapath,"Taxonomy.csv"));
res_popdisttrends <- read.csv(paste0(datapath,"PopDistributionTrends.csv"));res_ecology <- read.csv(paste0(datapath,"LifeHistoryEcology.csv"));res_harvest <- read.csv(paste0(datapath,"HarvestUse&Livelihoods.csv"));
res_threats <- read.csv(paste0(datapath,"Threats.csv"));res_research <- read.csv(paste0(datapath,"Research.csv"))


bat_cons_needs <- bats %>%
  left_join(cons_land_protection, by = "scientificName")%>%
  left_join(cons_species_mgmt, by="scientificName")%>%
  left_join(cons_education, by="scientificName")%>%
  left_join(cons_lawpolicy, by="scientificName")%>%
  left_join(cons_livelihood_econ, by="scientificName")%>%
  replace(is.na(.), 0)%>%
  group_by(redlistCategory)%>%
  summarise(TotSpecies= n(),
            LandProtection = sum(LandProtectionMgmt)/TotSpecies,
            SpeciesMgmt= sum(SpeciesMgmt)/TotSpecies,
            EducationAwareness = sum(EdAwareness)/TotSpecies,
            LawAndPolicy = sum(LawPolicy)/TotSpecies,
            LivelihoodEcon = sum(LivelihoodEconomic)/TotSpecies)%>%
  gather(ConservationNeed, PropSpecies, LandProtection:LivelihoodEcon, factor_key=TRUE)


bat_res_needs <- bats %>%
  left_join(res_taxonomy, by="scientificName")%>%
  left_join(res_popdisttrends, by="scientificName")%>%
  left_join(res_ecology, by="scientificName")%>%
  left_join(res_harvest, by="scientificName")%>%
  left_join(res_threats, by="scientificName")%>%
  left_join(res_research, by="scientificName")%>%
  replace(is.na(.), 0)%>%
  group_by(redlistCategory)%>%
  summarise(TotSpecies= n(),
            Taxonomy = sum(Taxonomy)/TotSpecies,
            PopTrends = sum(PopDistTrends)/TotSpecies,
            Ecology = sum(LifeHistEcology)/TotSpecies,
            HarvestUse = sum(HarvestUseLivelihoods)/TotSpecies,
            Threats = sum(Threats)/TotSpecies,
            Research = sum(Research)/TotSpecies)%>%
  gather(ResearchNeed, PropSpecies, Taxonomy:Research, factor_key=TRUE)

ggplot(bat_cons_needs, aes(x=redlistCategory, y= PropSpecies, fill=ConservationNeed)) +
  geom_bar(stat="identity", position=position_dodge())+
  #coord_flip() +
  xlab("IUCN Red List Category")+
  ylab("Proportion of Bat Species")+
  guides(fill=guide_legend(title="Conservation Action Needed"))+
  scale_fill_brewer(palette = "Set2") + theme_bw()

ggsave("Fig_ConservationNeeds.tiff")

ggplot(bat_res_needs, aes(x=redlistCategory, y= PropSpecies, fill=ResearchNeed)) +
  geom_bar(stat="identity", position=position_dodge())+
 # coord_flip() +
  xlab("IUCN Red List Category")+
  ylab("Proportion of Bat Species")+
  guides(fill=guide_legend(title="Research Needed"))+
  scale_fill_brewer(palette = "Set2") + theme_bw()

ggsave("Fig_ResearchNeeds.tiff")

ggplot(bats, aes(x=redlistCategory)) +
  geom_bar()+
#  coord_flip() +
  xlab("IUCN Red List Category")+
  ylab("Number of Species")+
  theme_bw()
ggsave("Fig_IUCN.tiff")

cons_evidence <- read.csv(paste0(datapath,"ConservationEvidence.csv"))%>%
  mutate(Outcome = fct_relevel(Outcome, "Beneficial",
                               "Likely beneficial", "Limited evidence", 
                               "Trade-off ", "Unlikely beneficial", "Likely harmful", "No evidence"),
         IUCN_threat = fct_relevel(IUCN_threat, "Logging & plant harvesting",
                               "Agriculture", "Hunting & harvesting", 
                               "Human disturbance", "Urban development", "Energy prod. & mining", 
                               "Climate change ", "Natural sys. modifications","White-nose Syndrome", "Invasive sp","Diseases",
                               "Light pollution", "Water pollution", "Noise pollution", "Transportation & corridors", "Multiple threats"))

cons_ev<- cons_evidence%>%
  group_by(Action_Category, IUCN_threat, Outcome)%>%
  summarise(Evidence = n())

ggplot(cons_ev, aes(x=Outcome, y= Evidence, fill=Action_Category)) +
  geom_bar(stat="identity")+
  #coord_flip() +
# scale_y_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
  xlab("Outcome of Conservation Actions")+
  ylab("Number of Actions")+
  guides(fill=guide_legend(title="IUCN Conservation Action Category"))+
  facet_wrap(vars(IUCN_threat), scales = "free")+
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))

#ggsave("Fig_ConsEvidence_threats.tiff")
ggsave(
   "Fig_ConsEvidence_threats.tiff",
    width = 11,
    height = 8,
    dpi = 300
  )
ggplot(cons_ev, aes(x=Outcome, y= Evidence, fill=Action_Category)) +
  geom_bar(stat="identity", position=position_dodge())+
  # coord_flip() +
  xlab("Outcome of Conservation Actions")+
  ylab("Number of Actions")+
  guides(fill=guide_legend(title="IUCN Conservation Action Category"))+
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))

ggsave("Fig_ConsEvidence.tiff")
