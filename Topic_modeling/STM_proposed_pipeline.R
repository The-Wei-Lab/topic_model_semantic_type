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
library(purrr)


##import and preprocess data (change this to your path)
df  = read.csv("./Cholesterol_cui_sdmb.csv",encoding="UTF-8")

df <- df %>% 
  mutate(created_date = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"),
         created_month = format(created_date, format = "%Y-%m"))%>%
  select(created_month, num_comments,score,cui_text)


##apply concept decomposition as you want
df$cui_text <- gsub("C0231528", "C0030193 C4083049", df$cui_text)
df$cui_text <- gsub("C0002962", "C0008031 C0018799", df$cui_text)

data_corpus <- corpus(df$cui_text)

#remove any mismatched/incorrect CUI mapped by Metamap if you find
data_dfm <- dfm(data_corpus,
                stem = FALSE,
                tolower = FALSE,
                remove_punct = FALSE,
                remove_numbers = FALSE,
                verbose = FALSE,
                remove = c("C0227958","C0270724","C0004135","C0394996","C0230426","C1535939"))

out <- convert(data_dfm, to = "stm", docvars=df) 


#model k from 2 to 6
stm_2_6 <- tibble(K = seq(2, 6)) %>%
  mutate(model = map(K, ~ stm(out$documents,
                              out$vocab,
                              data=out$meta,
                              max.em.its = 75,
                              K = .,
                              verbose = FALSE)))


##Calculate and Plot mean semantic coherence and exclusivity

model_scores <- stm_2_6 %>% 
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


## We choose topic number = 3 yields the best (change k as you think is the best)
stm_3 <- stm(
  documents=out$documents,
  vocab=out$vocab,
  data = out$meta,
  init.type = 'Spectral', 
  max.em.its = 75,
  K = 3,
  verbose = FALSE
)


##Calculate theta and save it for multi-panel plot

theta <- make.dt(stm_3)

df$Rank1_K <- NA 

for (i in 1:nrow(df)){
  column <- theta[i,-1]
  maintopic <- colnames(column)[which(column==max(column))]
  df$Rank1_K[i] <- maintopic
}
table(df$Rank1_K)

write.csv(theta_before,"./theta.csv") # change this to your saving path


##calculate tf-idf

dtm <- as.matrix(data_dfm)

tfidf_matrix <- dtm
for (i in 1:nrow(dtm)) {
  tfidf_matrix[i, ] <- dtm[i, ]/sum(dtm[i, ]) * log2( nrow(dtm) / col_sums(dtm > 0))
}

pca_result <- prcomp(tfidf_matrix, center = TRUE, scale. = TRUE)

# Access the principal components
principal_components <- pca_result$x

# Access the standard deviations of the principal components
standard_deviations <- pca_result$sdev

# Access the proportion of variance explained by each component
variance_explained <- (standard_deviations^2) / sum(standard_deviations^2)

# Number of components to retain (e.g., retaining 95% of the variance)
cumulative_variance <- cumsum(variance_explained)
num_components <- which(cumulative_variance >= 0.95)[1]

# Keep only the desired number of components
tf_idf <- principal_components[, 1:num_components]

#plot t-SNE and save tsne results

# Assuming 'tf_idf' and 'df' have the same number of rows and can be concatenated horizontally
merged_data <- cbind(tf_idf, Rank1_K = df$Rank1_K)

tsne_result <- tsne(tf_idf)

# Assuming "df" has a "Rank1_K" column that indicates the topic
tsne_data <- data.frame(tsne_result, Topic = df$Rank1_K)

# Create the t-SNE plot
p <- ggplot(tsne_data, aes(x = X1, y = X2, color = Topic)) +
  geom_point() +
  labs(x = "Dimension 1", y = "Dimension 2") +
  scale_color_manual(values = c("Topic1" = "red", "Topic2" = "green", "Topic3" = "blue")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid
  theme(axis.title.x = element_text(size = 14, face = "bold"),  # Increase x-axis label size and boldness
        axis.title.y = element_text(size = 14, face = "bold")) +  # Increase y-axis label size and boldness
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.3))  # Modify the rectangle frame size

# Add a legend
p <- p + guides(color = guide_legend(title = "Topic"))

# Display the plot
print(p)

write.csv(tsne_data,"./tsne_after.csv") #change this to your saving path


##Plot CUI with its interpretation for each topic

td_beta <- tidy(stm_3)

#you need to manually add any possible interpretation here
td_beta <- td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         # Replace CUIs with their corresponding terms
         term = case_when(
           term == "C0020443" ~ "Hypercholesterolemia(C0020443)",
           term == "C0011849" ~ "Diabetes Mellitus(C0011849)",
           term == "C0227230" ~ "Body of stomach(C0227230)",
           term == "C0011847" ~ "Diabetes(C0011847)",
           term == "C0009676" ~ "Confusion(C0009676)",
           term == "C0745103" ~ "Hypercholesterolemia Type IIa(C0745103)",
           term == "C0020445" ~ "Hypercholesterolemia, Familial(C0020445)",
           term == "C0003842" ~ "Arteries(C0003842)",
           term == "C0020538" ~ "Hypertensive disease(C0020538)",
           term == "C0037088" ~ "Signs and Symptoms(C0037088)",
           term == "C0018787" ~ "Heart(C0018787)",
           term == "C0018799" ~ "Heart Diseases(C0018799)",
           term == "C0003467" ~ "Anxiety(C0003467)",
           term == "C0003469" ~ "Anxiety Disorders(C0003469)",
           term == "C0020557" ~ "Hypertriglyceridemia(C0020557)",
           term == "C0683278" ~ "Mental Suffering(C0683278)",
           term == "C0027769" ~ "Nervousness(C0027769)",
           term == "C0040132" ~ "Thyroid Gland(C0040132)",
           term == "C0040822" ~ "Tremor(C0040822)",
           term == "C0231218" ~ "Malaise(C0231218)",
           term == "C4083049" ~ "Muscle(C4083049)",
           term == "C0027051" ~ "Myocardial Infarction(C0027051)",
           term == "C0030193" ~ "Pain(C0030193)",
           term == "C0344315" ~ "Depressed mood(C0344315)",
           term == "C0038454" ~ "Cerebrovascular accident(C0038454)",
           term == "C1457887" ~ "Symptoms(C1457887)",
           term == "C0015672" ~ "Fatigue(C0015672)",
           term == "C0007222" ~ "Cardiovascular Diseases(C0007222)",
           term == "C0234238" ~ "Ache(C0234238)",
           term == "C0023884" ~ "Liver(C0023884)",
           term == "C0231528" ~ "Myalgia(C0231528)",
           term == "C0006104" ~ "Brain(C0006104)",
           term == "C0021853" ~ "Intestines(C0021853)",
           term == "C0015292" ~ "Exolipase(C0015292)",
           term == "C0221423" ~ "Illness(C0221423)",
           term == "C0003850" ~ "Arteriosclerosis(C0003850)",
           term == "C0015392" ~ "Eye(C0015392)",
           TRUE ~ term
         ),
         # Remove "_topic x" from any token
         term = str_replace(term, "_topic\\s\\d+$", ""),
         term = reorder_within(term, beta, topic))

td_beta <- td_beta %>%
  mutate(topic = case_when(
    topic == "Topic 1" ~ "#1",
    topic == "Topic 2" ~ "#2",
    topic == "Topic 3" ~ "#3",
    TRUE ~ topic  
  )) # you can add your topic proportion in the title 
     # based on "table(df$Rank1_K)" given before

ggplot(td_beta, aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics") +
  theme(
    axis.text.y = element_text(size = 20),       # Adjusts the size of y-axis text
    axis.text.x = element_text(size = 20),       # Adjusts the size of x-axis text
    strip.text = element_text(size = 25),        # Adjusts the size of facet header text
    plot.title = element_text(size = 18),        # Adjusts the size of the main title
    plot.subtitle = element_text(size = 16)      # Adjusts the size of the subtitle
  )
