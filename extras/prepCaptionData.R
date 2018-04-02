#library(tesseract)
#library(tidytext)
#library(dplyr)
#library(tidyr)
#library(stringr)
#library(qdapDictionaries)

imgFiles<-list.files("../gevitTextmining/figureAnalysisShiny/www/figures/",pattern=".jpg",full.names=TRUE)

imgText<-sapply(imgFiles,function(x){ocr(x)})

#finding all the useful single terms
tmp<-data.frame(figID_initial = names(imgText),
                caption = imgText,
                stringsAsFactors = FALSE)

saveRDS(tmp,file="data/figureCaptionsOCR.RDS")
#tmp<-readRDS("data/figureCaptionsOCR.RDS")

tmp<-readRDS(file="data/imgCaptionTextRaw_df.RDS")

#caption words
commonWords<-tmp %>% 
  unnest_tokens(bigram, caption, token = "ngrams", n = 2) %>%
  filter(!grepl("[0-9,]+",bigram))%>%
  filter(!grepl("[-_]+",bigram))%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word & !word1 %in% colors()) %>%
  filter(!word2 %in% stop_words$word & !word2 %in% colors()) %>%
  filter(stringr::str_length(word1) > 2 | stringr::str_length(word2)>2)%>%
  filter(word1 %in% GradyAugmented | word2 %in% GradyAugmented )%>%
  mutate(word1 = SnowballC::wordStem(word1))%>%
  mutate(word2 = SnowballC::wordStem(word2))

#individual words
usefulSingles<-commonWords %>% count(word1,word2) %>% gather(src,word,word1:word2)%>% count(word)%>%filter(nn>2) %>% select(-nn)

usefulBigrams<-commonWords %>%
  filter(word1 %in% usefulSingles$word | word2 %in% usefulSingles$word)%>%
  count(word1,word2)%>%
  unite(bigram,word1,word2,sep=" ") %>%
  filter(n > 2)%>%
  select(-n)

commonWords<-commonWords %>%
  filter(word1 %in% usefulSingles$word | word2 %in% usefulSingles$word)%>%
  unite(bigram,word1,word2,sep=" ") %>%
  #inner_join(usefulBigrams) %>% 
  distinct() %>% 
  mutate(figID_initial = basename(figID_initial))

save(usefulBigrams,usefulSingles,commonWords,
     list = c("usefulBigrams","usefulSingles","commonWords"),
     file="data/captionItems.RData")

#get a list of some of the missing figures
dat<-read_excel(path="data/figure_classification_final.xlsx",na=c("","NA"))

#missing figures
allImgs<-list.files(path = "../gevitTextmining/figureAnalysisShiny/www/figures/")

