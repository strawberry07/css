library(openalexR)
library(httr)
library(jsonlite)
library(dplyr)
options(openalexR.mailto = "jjiali@connect.hku.hk")

ai_terms <- c(
  '"artificial intelligence"',
  # Exact phrase
  '"AI"',
  "algorithms",
  # Exact term
  '"machine learning"',
  # Exact phrase
  '"deep learning"',
  # Exact phrase
  '"natural language processing"',
  '"generative AI"',
  '"LLM"',
  '"LLMs"',
  '"large language model"',
  '"neural network"'
)

older_terms <- c(
  '"older adults"',
  # Exact phrase
  '"older people"',
  # Exact phrase
  '"elderly"',
  # Exact term
  '"geriatric"',
  # Exact term
  '"gerontology"',
  # Exact term
  '"gerontological"',
  '"aging population"',
  '"aged population"',
  # Exact term
  '"ageing population"',
  # Alternative spelling
  '"older population"',
  # Exact phrase
  '"aged 60 and above"',
  # Exact phrase
  '"aged 60+"',
  # Exact term
  '"young-old"',
  # Exact term
  '"old-old"',
  # Exact term
  '"oldest-old"',
  "dementia",
  "Alzheimer's"        # Exact term
)

ai_query <- paste(ai_terms, collapse = " OR ")
older_query <- paste(older_terms, collapse = " OR ")
# Full query
query <- paste0("(", ai_query, ") AND (", older_query, ")")

recent_works <- oa_fetch(
  entity = "works",
  title.search = query,
  type = "article",
  language = "EN",
  from_publication_date = "2020-01-01",
  abstract = TRUE,
  to_publication_date = "2024-12-31",
  version = "publishedVersion",
  output = "dataframe",
  is_paratext = "false",
  has_fulltext = TRUE,
  options = list(
    select = c(
      "id",
      "title",
      "publication_date",
      "keywords",
      "relevance_score",
      "sources",
      "primary_location",
      "corresponding_institution_ids",
      "abstract_inverted_index",
      "cited_by_count",
      "authorships",
      "doi",
      "biblio",
      "topics",
      "primary_topic",
      "referenced_works"
    )
  )
)
recent_works %>% filter(last_page != first_page) -> recent_works
recent_works %>% filter(as.numeric(last_page) - as.numeric(first_page) >
                          1) -> recent_works
recent_works %>% filter(so != "Deleted Journal") -> recent_works

filter_exact_matches <- function(titles) {
  # Clean the terms (remove quotes and convert to lowercase)
  ai_patterns <- gsub('^"|"$', '', ai_terms) %>% tolower()
  older_patterns <- gsub('^"|"$', '', older_terms) %>% tolower()
  
  # Check each title
  relevant_titles <- titles[# Must contain at least one AI term AND one older term
    sapply(tolower(titles), function(t) {
      has_ai <- any(sapply(ai_patterns, function(term)
        grepl(term, t, fixed = TRUE)))
      has_older <- any(sapply(older_patterns, function(term)
        grepl(term, t, fixed = TRUE)))
      return(has_ai && has_older)
    })]
  
  return(relevant_titles)
}
filtered_works <- recent_works %>%
  filter(title %in% filter_exact_matches(title)) %>% distinct(title, .keep_all = TRUE)
#select useful variables
filtered_works %>% select(
  id,
  title,
  author,
  ab,
  publication_date,
  first_page,
  last_page,
  relevance_score,
  so,
  so_id,
  topics
) -> work_data

work_data %>% filter(!is.na(ab)) -> work_data  #191 observation

#plot the journal led by IEEE Access,journal of health engineering....predominantly engineering journals.
journal_count <- table(work_data$so)
journal_count_df <- as.data.frame(journal_count)
colnames(journal_count_df) <- c("journal", "count")

ggplot(journal_count_df %>% arrange(desc(count)) %>%  # Sort by count in descending order
         slice_head(n = 15) ,
       aes(x = reorder(journal, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot with color
  labs(title = "Journal Counts from Highest to Lowest", x = "Journal", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Rotate x-axis labels
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )




# #are they published by ageing journals or tech journals?
# table(work_data$so)
#
# #get discipline from WoS
# SSCI <- read.csv("C:/Users/b151041/Downloads/Social Sciences Citation Index (SSCI).csv")
# ESCI <- read.csv("C:/Users/b151041/Downloads/Emerging Sources Citation Index (ESCI).csv")
# AHCI <- read.csv("C:/Users/b151041/Downloads/Arts & Humanities Citation Index (AHCI).csv")
# SCIE <- read.csv("C:/Users/b151041/Downloads/Science Citation Index Expanded (SCIE).csv")
#
# #combine journals
# journal_list<-rbind(SSCI,ESCI,AHCI,SCIE) %>% select(Journal.title,Web.of.Science.Categories) %>% distinct(Journal.title,.keep_all = TRUE)
# journal_list<-journal_list %>% rename(so=Journal.title,disc=Web.of.Science.Categories)
# #match journal discipline  ##pay attention to "&" and "and" that cannot be matched need to be manually checked later
#
# work_data %>% mutate(so_lower=tolower(so)) %>% left_join(journal_list %>% mutate(so_lower=tolower(so)),by="so_lower")->work_data
# #only choose the first discipline #even with | they tend to be in similar domain, e.g. neuroscience and neuro imaging
# work_data %>% mutate(disc=sub("\\|.*$", "", disc)) ->work_data
#
# ## word cloud of journal names
# library(wordcloud)
# library(RColorBrewer)
# library(tidytext)
# df<-work_data %>% select(so_lower)
# # Step 1: Tokenize the 'so_lower' column into individual words
# tokenized <- df %>%
#   unnest_tokens(word, so_lower)  # Split journal names into individual words
#
# # Step 2: Remove stopwords and custom unwanted words
# # Define custom unwanted words
# custom_stopwords <- c("journal", "and", "international", "of","research","frontiers","indonesian","\\b\\d+\\b")
#
# # Remove stopwords and custom unwanted words
# filtered_words <- tokenized %>%
#   filter(!word %in% c(custom_stopwords, stop_words$word))  # Remove both standard and custom stopwords
#
# # Step 3: Count word frequencies
# word_counts <- filtered_words %>%
#   count(word, sort = TRUE)
#
# # View word frequencies
# print(word_counts)
#
# # Step 4: Create the word cloud
# set.seed(123)  # For reproducibility
# wordcloud(
#   words = word_counts$word,
#   freq = word_counts$n,
#   min.freq = 1,  # Minimum frequency for a word to be included
#   max.words = 100,  # Maximum number of words to display
#   random.order = FALSE,  # Display most frequent words in the center
#   colors = brewer.pal(8, "Dark2")
# )
#
# #count frequencies of discipline
# disc_count<-table(work_data$disc)
# disc_count_df<-as.data.frame(disc_count)
# colnames(disc_count_df)<-c("disc","count")
#
# cleaned_df <- disc_count_df %>%
#   mutate(
#     disc_clean = str_trim(disc),  # Remove leading/trailing spaces
#     disc_clean = tolower(disc_clean)  # Convert to lowercase
#   )
#
# # Step 2: Group and aggregate counts by cleaned discipline names
# grouped_df <- cleaned_df %>%
#   group_by(disc_clean) %>%
#   summarise(total_count = sum(count), .groups = "drop")  # Sum counts for identical names
#
# # Step 3: Sort by total count
# grouped_df <- grouped_df %>%
#   arrange(desc(total_count))
#
# # View the cleaned and grouped data
# print(grouped_df)
#
# # Step 4: Plot the cleaned data (e.g., top 30 disciplines)
# ggplot(grouped_df %>% slice_head(n = 30), aes(x = reorder(disc_clean, total_count), y = total_count)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "Top Disciplines by Count (Cleaned)",
#     x = "Discipline",
#     y = "Total Count"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.text = element_text(size = 10),
#     axis.title = element_text(size = 12)
#   )
#
#
# ##wordcloud of disciplines
# df<-work_data %>% select(disc)
# # Step 1: Tokenize the 'so_lower' column into individual words
# tokenized <- df %>%
#   unnest_tokens(word, disc)  # Split journal names into individual words
#
# # Step 2: Remove stopwords and custom unwanted words
# # Define custom unwanted words
# custom_stopwords <- c("journal", "and", "international", "of","research","&",",")
#
# # Remove stopwords and custom unwanted words
# filtered_words <- tokenized %>%
#   filter(!word %in% c(custom_stopwords, stop_words$word))  # Remove both standard and custom stopwords
#
# # Step 3: Count word frequencies
# word_counts <- filtered_words %>%
#   count(word, sort = TRUE)
#
# # View word frequencies
# print(word_counts)
#
# # Step 4: Create the word cloud
# set.seed(123)  # For reproducibility
# wordcloud(
#   words = word_counts$word,
#   freq = word_counts$n,
#   min.freq = 1,  # Minimum frequency for a word to be included
#   max.words = 100,  # Maximum number of words to display
#   random.order = FALSE,  # Display most frequent words in the center
#   colors = brewer.pal(8, "Dark2")
# )
#
#
#
# ##group similar disciplines
# library(stringdist)
# work_data %>% filter(!is.na(disc)) ->work_data
# distance_matrix<-stringdistmatrix(work_data$disc,work_data$disc,method = "jw")
# hc <- hclust(as.dist(distance_matrix))
# plot(hc,labels = work_data$disc)

# 3. Apply filter
results <- data.frame(
  title = character(),
  name = character(),
  score = numeric(),
  type = character(),
  path = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each row in filtered_works
for (i in 1:nrow(work_data)) {
  # Get the article name
  title <- work_data$title[[i]]
  # Get the concepts for the current article
  topics_list <- work_data$topics[[i]]
  if (!is.null(nrow(topics_list))  && nrow(topics_list) > 0) {
    topics_list %>% rename(path = i) -> topics_list
    for (j in 1:nrow(topics_list)) {
      name <- topics_list$display_name[[j]]
      score <- topics_list$score[[j]]
      type <- topics_list$name[[j]]
      path <- topics_list$path[[j]]
      
      # Add a new row to the results dataframe
      results <- rbind(
        results,
        data.frame(
          title = title,
          name = name,
          score = score,
          type = type,
          path = path,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

left_join(
  work_data %>% select(
    id,
    title,author,ab,
    publication_date,
    relevance_score,so
  ),
  results,
  by = "title"
) -> results_w_topics

domain<-results_w_topics %>% filter(type=="domain")
field<-results_w_topics %>% filter(type=="field")
subfield<-results_w_topics %>% filter(type=="subfield")
topic<-results_w_topics %>% filter(type=="topic")






results_w_topics$id <- gsub("https?://orcid.org/|https?://openalex.org/",
                            "",
                            results_w_topics$id)
results_w_topics$source_id <- gsub("https?://orcid.org/|https?://openalex.org/",
                                   "",
                                   results_w_topics$source_id)
