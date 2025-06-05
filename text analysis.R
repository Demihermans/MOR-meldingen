#tekst 

library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(lexicon)
library(dplyr)
library(tidyr)
library(tidytext)

##tokens samenvatten per document (melding)
#bag of words/ tf-idf / n-grams

#  Zet je tokens om naar een “tidy” dataset per document
tidy_tokens <- df_clean_MOR_gefilterd_stopwoorden %>%
  select(doc_id, lemma) %>%
  rename(word = lemma)

#  Tel woorden per document
doc_term_counts <- tidy_tokens %>%
  count(doc_id, word, sort = TRUE)

# Maak er een Document-Term Matrix van
dtm <- doc_term_counts %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Nu is `dtm` een sparse matrix die je bijvoorbeeld in een classifier kunt gooien


# TF-IDF
# 3.2a) Voeg tf-idf-scores toe
doc_term_tfidf <- doc_term_counts %>%
  bind_tf_idf(term = word, document = doc_id, n = n)

# 3.2b) Indien je een wide-matrix wil voor modeling
tfidf_wide <- doc_term_tfidf %>%
  select(doc_id, word, tf_idf) %>%
  cast_dtm(document = doc_id, term = word, value = tf_idf)


# Meest gebruikte woorden in omschrijving
freq <- df_clean_MOR_gefilterd_stopwoorden %>%
  count(lemma, sort = TRUE)

# Maak een wordcloud
set.seed(123)  # voor reproduceerbare layout
wordcloud(
  words      = freq$lemma,
  freq       = freq$n,
  min.freq   = 10,                # alleen woorden ≥10×
  max.words  = 200,               # max aantal woorden in de cloud
  random.order = FALSE,           # hoge frequenties in het midden
  rot.per    = 0.25,              # 25% woorden gekanteld
  colors     = brewer.pal(8, "Dark2")
)

##meest onderscheidende woorden:
top_global <- doc_term_tfidf %>% 
  group_by(word) %>% summarise(mean_tfidf = mean(tf_idf)) %>% 
  slice_max(mean_tfidf, n = 100)

wordcloud(
  words   = top_global$word,
  freq    = top_global$mean_tfidf,
  random.order = FALSE, max.words = 100
)
# Voornamelijk typfouten, nummers/codes en emoticons


### sentiment analyse

# n-grams(bi-grams 2 woorden)

# Data-frame met minimaal: doc_id | lemma
df_bigrams <- df_clean_MOR_gefilterd_stopwoorden %>%
  group_by(doc_id) %>%                                    # alle tokens v/d melding
  summarise(text_combined = paste(lemma, collapse = " "),
            .groups = "drop") %>%
  unnest_tokens(bigram, text_combined, token = "ngrams", n = 2)

# Bigram splitsen in 2 aparte woorden
bigrams_separated <- df_bigrams %>%
  separate(bigram, into = c("w1", "w2"), sep = " ")

# lijst met nederlandse negators en intensifiers
negators <- c("geen", "niet", "nooit", "zonder", "noch")
intensifiers <- c("heel", "erg", "hele", "zeer", "helemaal", "uitermate", "super", "ontzettend")

## NL-AFINN ophalen (scores –5 … +5)
afinn_nl_raw <- read_json(
  "https://cdn.jsdelivr.net/npm/multilang-sentiment@2.0.0/build/languages/AFINN-nl.json",
  simplifyVector = FALSE
)
afinn_nl <- tibble(
  word  = names(afinn_nl_raw[setdiff(names(afinn_nl_raw), "negators")]),
  value = map_dbl(
    afinn_nl_raw[setdiff(names(afinn_nl_raw), "negators")],
    ~ as.numeric(.x)[1]
  )
)

#) Sentiment-scores aan beide woorden koppelen

bigrams_scored <- bigrams_separated %>% 
  left_join(afinn_nl, by = c("w1" = "word")) %>%   # scoor beide
  left_join(afinn_nl, by = c("w2" = "word"), suffix = c("_w1","_w2")) %>% 
  mutate(
    score_w1 = coalesce(value_w1, 0),
    score_w2 = coalesce(value_w2, 0),
    score_w2 = case_when(
      w1 %in% negators     ~ -score_w2,  # negator ⇒ teken omkeren  
      w1 %in% intensifiers ~ 2 *score_w2, # intensifier ⇒ 2× op de score van het tweede woord
      TRUE                 ~ score_w2),
    bigram_sentiment = score_w1 + score_w2       # telt beide mee
  )

# korte analyse van de negated_words in mijn dataset

negated_words <- bigrams_separated %>% 
  filter(w1 %in% negators) %>%             # alleen bigrams met negator
  inner_join(afinn_nl, by = c(w2 = "word")) %>%  # NL-AFINN op 2e woord
  count(w1, w2, value, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(contribution = n * (-value))   # negator ⇒ teken omkeren

##Plot: top-10 omgekeerde bijdragen per negator
negated_words %>% 
  arrange(desc(abs(contribution))) %>% 
  group_by(w1) %>% 
  slice_max(abs(contribution), n = 10) %>% 
  ungroup() %>% 
  mutate(w2 = reorder_within(w2, contribution, w1)) %>% 
  ggplot(aes(w2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ w1, scales = "free_y") +
  coord_flip() +
  labs(title = "Negator-bigrams: bijdrage aan sentiment (NL-AFINN)",
       x = NULL, y = "n × (-score)")

##dus woorden als niet slecht en nooit probleem, nooit last worden omgekeerd als positief en woorden als niet handig, niet of nooit goed, nooit schoon etc. worden negatief.

# korte analyse van de intensifier_words in mijn dataset
intensifier_words <- bigrams_separated %>%          # basis-bigramtabel
  filter(w1 %in% intensifiers) %>%                  # bigrams met versterker
  inner_join(afinn_nl, by = c("w2" = "word")) %>%   # score op 2e woord
  count(w1, w2, value, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(contribution = n * 2 * value)            # 2 × bijdrage

#plot

intensifier_words %>% 
  arrange(desc(abs(contribution))) %>% 
  group_by(w1) %>% 
  slice_max(abs(contribution), n = 10) %>% 
  ungroup() %>% 
  mutate(w2 = reorder_within(w2, contribution, w1)) %>% 
  ggplot(aes(w2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ w1, scales = "free_y") +
  coord_flip() +
  labs(title = "Intensifier-bigrams: bijdrage aan sentiment (NL-AFINN)",
       x = NULL, y = "n × 1.5 × score")

#vooral woorden icm met erg heel en zeer. erg groot, heel fijn, zeer wenselijk etc. positief n woorden als erg gevaarlijk, heel slecht, zeer hard negatief.


#) Document-niveau aggregates

sentiment_per_doc <- bigrams_scored %>%
  group_by(doc_id) %>%
  summarise(
    sentiment_sum      = sum(bigram_sentiment), #is bij elkaar opgeteld dus hele lange meldigen meer kans op meer sentiment woorden en dus hogere scores (positief of negatief)
    sentiment_mean     = mean(bigram_sentiment), #sentiment mean is eerlijker omdat je korte en lange meldingen hebt.
    sentiment_n_bigrams = n(),
    .groups = "drop"
  )


# ) Aan originele metadata koppelen (indien nodig)

df_with_sentiment <- df_clean_MOR_gefilterd_stopwoorden %>%
  distinct(doc_id) %>%           # unieke meldingen
  left_join(sentiment_per_doc, by = "doc_id")

##of aan tabel met melding per regel
df_with_sentiment_permelding <- df_clean_MOR %>%          # bevat Omschrijving
  left_join(sentiment_per_doc, by = "doc_id")  # voegt sentiment-kolommen toe


# ) Analyse: top-10 positiefste meldingen

df_with_sentiment %>%
  arrange(desc(sentiment_mean)) %>%
  slice_head(n = 10)

df_clean_MOR_gefilterd_stopwoorden %>% filter(doc_id == 58600)
df_clean_MOR %>% filter(doc_id == 58600)
df_clean_MOR %>% filter(doc_id == 30188)
#Dit zijn eigenlijk meer bedankjes dan meldingen: 'heel blij mee', 'fantastisch jullie niet gezien maar geweldig bedankt'. 

# ) top-10 negatiefste meldingen
df_with_sentiment %>%
  arrange(sentiment_mean) %>%
  slice_head(n = 10)

df_clean_MOR_gefilterd_stopwoorden %>% filter(doc_id == 61455)
df_clean_MOR %>% filter(doc_id ==  32680) 
df_clean_MOR %>% filter(doc_id ==  92209)
df_clean_MOR %>% filter(doc_id ==  105655)
#meldingen met erg of zeer gevaarlijk. 

#Andere ideeen voor analyses:
# - het is sentiment in ene wijk anders dan in de andere
# - het is sentiment over tijd veranderd
# - het is sentiment per type melding anders
# - geografische heatmap aantal meldingen maar ook sentiment van de meldingen?
