##import libraries
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(dplyr)
library(lubridate)
library(SnowballC)
library(stringr)
library(tsne)
library(gridExtra)
library(tm)
library(Matrix)
library(slam)
library(ggplot2)


##import and preprocess data (change this to your path)
df  = read.csv("C:/Users/12174/statin_csv/Cholesterol_statins.csv",encoding="UTF-8") 

df <- df %>% 
  mutate(created_date = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"),
         created_month = format(created_date, format = "%Y-%m"))%>%
  select(created_month, num_comments,score,text,extracted_text)

data_corpus <- corpus(df$extracted_text)

data_dfm <- dfm(data_corpus,
                stem = TRUE,
                tolower = TRUE,
                remove_punct = TRUE,
                remove_numbers =TRUE,
                verbose = TRUE,
                remove =c(stopwords('english'),c("|","l","c","=","#x200b","s","m","it","It","amp","png","gt","webp","auto")))


out <- convert(data_dfm, to = "stm", docvars=df)


##Plot mean of semantic coherence and exclusivity
#calculate model scores

stm_2_20 <- tibble(K = seq(2, 20)) %>%
  mutate(model = map(K, ~ stm(out$documents,
                              out$vocab,
                              data=out$meta,
                              max.em.its = 75,
                              K = .,
                              verbose = FALSE)))

model_scores <- stm_2_20  %>% 
  mutate(exclusivity = map(model, exclusivity),
         semantic_coherence = map(model, semanticCoherence, out$documents)) %>% 
  select(K, exclusivity, semantic_coherence)
   
model_scores %>% 
  unnest(c(exclusivity, semantic_coherence)) %>% 
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence)) %>% 
  ggplot(aes(x = semantic_coherence, y = exclusivity)) +
  geom_point(aes(color = as.factor(K)), size = 8) +  # increase size of points to 5
  geom_text(aes(label = K), vjust = 1, hjust = 1) +
  scale_color_discrete(guide = FALSE) +
  theme_bw() +
  geom_vline(aes(xintercept = mean(semantic_coherence)), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean(exclusivity)), linetype = "dashed", color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16)) +  # set font size for axis titles
  labs(x = "Semantic Coherence", y = "Exclusivity")  # add axis labels


#topic number k =11 yields the best
stm_11 <- stm(
  documents=out$documents,
  vocab=out$vocab,
  data = out$meta,
  init.type = 'Spectral', 
  max.em.its = 75,
  K = 11,
  verbose = FALSE
)

summary(stm_11)


##Plot topics based on beta
td_beta <- tidy(stm_11)

# calculate the average proportion of each topic in the dataset
topic_proportions <- apply(stm_11$theta, 2, mean)

# sort the topics based on their average proportions in descending order
sorted_topics <- order(topic_proportions, decreasing = TRUE)

# use the sorted_topics to create a factor variable for the topics
td_beta$topic <- factor(td_beta$topic, levels = sorted_topics)

# define labeller function
my_labeller <- function(variable, value){
  topic_number <- as.numeric(gsub("topic_", "", value))
  proportion <- scales::percent(topic_proportions[topic_number],accuracy = 0.1)
  topic_word <- word_list[topic_number] # assuming topic_words is a character vector with 15 strings
  paste0(" \""," ",word_list, " ", "(", proportion, ")", "\"")
}

#label topic numbers
word_list <- list(
  c("#1"),
  c("#2"),
  c("#3"),
  c("#4"),
  c("#5"),
  c("#6"),
  c("#7"),
  c("#8"),
  c("#9"),
  c("#10"),
  c("#11")
)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, nrow = 4, ncol = 3, scales = "free_y", labeller = my_labeller) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics") +
  theme(text = element_text(size = 10, face = "bold"))
