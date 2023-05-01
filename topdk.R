library(stringr)

topdk_bigrams <- topdanmark %>%
  mutate(content=str_replace_all(content,"[0-9]",""),revidx=row_number(),domain="topdanmark") %>% 
  unnest_tokens(bigram, content, token = "ngrams", n = 2)


#count bigrams
# look for service
topdk_bi_count=topdk_bigrams %>%
  count(bigram, sort = TRUE) %>% 
  filter(grepl("service",bigram))

# building custom filtering lists
#pos, neg list
kwlistpos=c("god","godt","fin")
kwlistneg=c("dårlig","elendig")
#stopwords
mystopwords=c("i","jeg","se")

topdk_bi_count=topdk_bigrams %>%
  count(bigram, sort = TRUE) %>% 
  filter(bigram %in% kwlist)

# separate the bigram
topdk_bigrams_separated <- topdk_bigrams %>% 
  #filter(grepl("i|jeg|se",bigram)) %>% 
  #filter(!str_detect(mystopwords,bigram)) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram,c("w1","w2"),sep = " ") %>% 
  filter(grepl("service",w2)) %>% 
  filter(w1 %in% kwlistpos) %>% 
  filter(n > 5)

topdk_bigrams_united = topdk_bigrams_separated %>% 
  unite(bigrams, w1,w2, sep = " ")

ggplot(topdk_bigrams_united, aes(bigrams,n))+
  geom_bar(stat="identity")
