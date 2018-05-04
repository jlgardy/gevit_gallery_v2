library(readxl)
library(dplyr)
library(tidyr)

#Prep Data
dat<-read_excel(path="data/figure_classification_final.xlsx",na=c("","NA"))

datSub<-dat %>%
  filter(!(chartCombinations) %in% c("BREAK THIS UP","MISSING","EXCLUDE - LEGIT TABLE"))%>%
  mutate(figID_initial = gsub("\\s+","_",figID_initial))

chartRln<-getChartRln(datSub)
paperData<-read_excel("data/MasterDocumentList.xlsx") %>% filter(Include == "Y")

datSub<-inner_join(datSub,paperData,by="PMID")

load("data/captionItems.RData")
commonWords<- inner_join(commonWords,select(datSub,figID_initial), by="figID_initial")

#saving common analysis object
save(datSub,commonWords,chartRln,usefulBigrams,usefulSingles,file="data/analysisData.RData")

#prep widget content loader

PMIDUnique<-unique((datSub$PMID))
classTags<-unique(datSub$classExample)

chartTypes<-datSub %>%
  ungroup()%>%
  mutate(chartType=strsplit(chartType,";"))%>%
  tidyr::unnest()%>%
  group_by(chartType)%>%
  count()

concept<-datSub %>%
  ungroup()%>%
  mutate(concepts=strsplit(concepts,";"))%>%
  tidyr::unnest()%>%
  group_by(concepts)%>%
  count()

pathogen<-datSub %>%
  ungroup()%>%
  mutate(Pathogen=strsplit(Pathogen,";"))%>%
  tidyr::unnest()%>%
  group_by(Pathogen)%>%
  count()

save(PMIDUnique,classTags,chartTypes,concept,pathogen,file="data/widgetPopulatingData.RData")
