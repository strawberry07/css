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
  from_publication_date = "2024-12-15",
  abstract = TRUE,
  to_publication_date = "2024-12-31",
  version = "publishedVersion",
  output = "dataframe",
  options = list(
    select = c(
      "id",
      "title",
      "publication_date",
      "relevance_score",
      "sources",
      "primary_location",
      "abstract_inverted_index",
      "cited_by_count",
      "authorships",
      "doi",
      "biblio","topics"
    )
  )
)

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

# 3. Apply filter
filtered_works <- recent_works %>%
  filter(title %in% filter_exact_matches(title))

results <- data.frame(
  title = character(),
  topic = character(),
  score = numeric(),
  type = character(),
  path=numeric(),
  stringsAsFactors = FALSE
)

# Loop through each row in filtered_works
for (i in 1:nrow(filtered_works)) {
  # Get the article name
  title <- filtered_works$title[i]
  
  # Get the concepts for the current article
  topics_list <- filtered_works$topics[[i]]
  
  for (j in 1:nrow(topics_list)) {
    topic <- topics_list$display_name[[j]]
    score <- topics_list$score[[j]]
    type <- topics_list$type[[j]]
    path <- topics_list$i[[j]]
    
    # Add a new row to the results dataframe
    results <- rbind(
      results,
      data.frame(
        title = title,
        topic = topic,
        score = score,
        type = type,
        path = path,
        stringsAsFactors = FALSE
      )
    )
  }
}

left_join(
  filtered_works %>% select(
    id,
    title,
    publication_date,
    relevance_score,
    cited_by_count,
    source_display_name,
    source_id
  ),
  results,
  by = "title"
) -> results_w_topics

results_w_topics$id<-gsub("https?://orcid.org/|https?://openalex.org/","",results_w_topics$id)
results_w_topics$source_id<-gsub("https?://orcid.org/|https?://openalex.org/","",results_w_topics$source_id)
# # List of 19 level 0 concepts
# concept_list <- concept_abbrev$display_name
# results_w_concepts %>% filter(score>0) %>% group_by(title,level) %>% count(level) -> concept_count
# 
# full_df <- expand.grid(
#   title = unique(filtered_works$title),
#   concept = concept_list
# )

results_w_topics %>% filter(type=="domain") %>% group_by(title,topic) %>% summarise(score)->title_domain

highest_score<-results_w_concepts %>% 
  filter(score>0) %>% group_by(title,level) %>% 
  slice_max(order_by = score) %>% select(title,concept,score,level)



full_df %>%
  left_join(results_w_concepts, by = c("title", "concept")) %>%
  mutate(score = replace_na(score, 0))  -> full_df

full_df$id<-gsub("https?://orcid.org/|https?://openalex.org/","",full_df$id)
full_df$source_id<-gsub("https?://orcid.org/|https?://openalex.org/","",full_df$source_id)

non_zero_counts <- full_df %>%
  group_by(title) %>%
  summarise(non_zero_disciplinary_count = sum(score > 0))


# 4. Show results
cat("Original articles:", nrow(recent_works), "\n")
cat("Filtered articles:", nrow(filtered_works), "\n\n")


max_authors <- max(unlist(lapply(filtered_works$author, function(x) {
  if (is.data.frame(x))
    nrow(x)
  else if (is.list(x))
    length(x)
  else
    1
})))

works_df <- data.frame(
  # Basic article info
  title = filtered_works$title,
  publication_date = filtered_works$publication_date,
  cited_by_count = filtered_works$cited_by_count,
  relevance_score = filtered_works$relevance_score,
  journal = filtered_works$so,
  number_of_authors = sapply(filtered_works$author, NROW)
)

for (author_num in 1:max_authors) {
  # Create new columns for each author
  works_df[paste0("au", author_num, "_id")] <- NA_character_
  works_df[paste0("au", author_num, "_name")] <- NA_character_
  works_df[paste0("au", author_num, "_orcid")] <- NA_character_
  works_df[paste0("au", author_num, "_institution_id")] <- NA_character_
  works_df[paste0("au", author_num, "_institution")] <- NA_character_
  works_df[paste0("au", author_num, "_institution_country")] <- NA_character_
  works_df[paste0("au", author_num, "_institution_type")] <- NA_character_
}

# Initialize concept columns
level0_concept = NA_character_
level0_score = NA_real_
level1_concept = NA_character_
level1_score = NA_real_
level2_concept = NA_character_
level2_score = NA_real_
level3_concept = NA_character_
level3_score = NA_real_

for (i in 1:nrow(filtered_works)) {
  # # Extract author information
  # authors <- filtered_works$author[[i]]
  #
  # # Process authors
  # for(author_num in 1:nrow(authors)) {
  #   author <- authors[author_num,]
  #
  #   # Fill in author information
  #   works_df[i, paste0("au", author_num, "_id")] <- author$au_id
  #   works_df[i, paste0("au", author_num, "_name")] <- author$au_display_name
  #   works_df[i, paste0("au", author_num, "_orcid")] <- author$au_orcid
  #   works_df[i, paste0("au", author_num, "_institution_id")] <- author$institution_id
  #   works_df[i, paste0("au", author_num, "_institution")] <- author$institution_display_name
  #   works_df[i, paste0("au", author_num, "_institution_country")] <- author$institution_country_code
  #   works_df[i, paste0("au", author_num, "_institution_type")] <- author$institution_type
  # }
  #
  # Extract concepts
  work_concepts <- filtered_works$concepts[[i]]
  if (length(work_concepts) > 0) {
    for (level in 0:3) {
      level_mask <- work_concepts$level == level
      if (any(level_mask)) {
        works_df[i, paste0("level", level, "_concept")] <-
          work_concepts$display_name[level_mask][1]
        works_df[i, paste0("level", level, "_score")] <-
          work_concepts$score[level_mask][1]
      }
    }
  }
}
# Fill in the data
for (i in 1:nrow(filtered_works)) {
  # Extract author information
  authors <- filtered_works$author[[i]]
  
  # Process first 3 authors
  if (length(authors) > 0) {
    for (author_num in 1:nrow(authors)) {
      author <- authors[author_num, ]
      
      # Fill in author information
      works_df[i, paste0("au", author_num, "_id")] <- author$au_id
      works_df[i, paste0("au", author_num, "_name")] <- author$au_display_name
      works_df[i, paste0("au", author_num, "_orcid")] <- author$au_orcid
      works_df[i, paste0("au", author_num, "_institution_id")] <- author$institution_id
      works_df[i, paste0("au", author_num, "_institution")] <- author$institution_display_name
      works_df[i, paste0("au", author_num, "_institution_country")] <- author$institution_country_code
      works_df[i, paste0("au", author_num, "_institution_type")] <- author$institution_type
    }
  }
  
  # Extract concepts
  work_concepts <- filtered_works$concepts[[i]]
  if (length(work_concepts) > 0) {
    for (level in 0:3) {
      level_mask <- work_concepts$level == level
      if (any(level_mask)) {
        works_df[i, paste0("level", level, "_concept")] <-
          work_concepts$display_name[level_mask][1]
        works_df[i, paste0("level", level, "_score")] <-
          work_concepts$score[level_mask][1]
      }
    }
  }
}

# Clean up the data
works_df <- works_df %>%
  mutate(across(everything(), ~ ifelse(.x == "NULL", NA, .x))) %>%
  mutate(publication_date = as.Date(publication_date),
         across(
           everything(),
           ~ gsub("https?://orcid.org/|https?://openalex.org/", "", .)
         ))
journal_summary <- works_df %>%
  group_by(journal) %>%
  summarise(
    number_of_papers = n(),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    avg_citations = mean(cited_by_count, na.rm = TRUE),
    avg_authors = mean(number_of_authors, na.rm = TRUE),
    avg_relevance = mean(relevance_score, na.rm = TRUE)
  ) %>%
  arrange(desc(number_of_papers))

total_papers <- sum(journal_summary$number_of_papers)
journal_summary <- journal_summary %>%
  mutate(percentage_of_papers = (number_of_papers / total_papers) * 100)

# Print top journals
print(head(journal_summary))

concept0_summary <- works_df %>%
  group_by(level0_concept) %>%
  summarise(
    frequency = n(),
    avg_score = mean(level0_score, na.rm = TRUE),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    avg_citations = mean(cited_by_count, na.rm = TRUE)
  ) %>%
  arrange(desc(frequency)) %>%
  filter(!is.na(level0_concept))  # Remove NA if any

concept1_summary <- works_df %>%
  group_by(level1_concept) %>%
  summarise(
    frequency = n(),
    avg_score = mean(level1_score, na.rm = TRUE),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    avg_citations = mean(cited_by_count, na.rm = TRUE)
  ) %>%
  arrange(desc(frequency)) %>%
  filter(!is.na(level1_concept))  # Remove NA if any

concept2_summary <- works_df %>% filter(level0_concept == "Computer science") %>%
  group_by(level2_concept) %>%
  summarise(
    frequency = n(),
    avg_score = mean(level2_score, na.rm = TRUE),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    avg_citations = mean(cited_by_count, na.rm = TRUE)
  ) %>%
  arrange(desc(frequency)) %>%
  filter(!is.na(level2_concept))  # Remove NA if any

# Print top concepts
print(head(concept0_summary, 10))
print(head(concept1_summary, 10))
print(head(concept2_summary, 10))

# Calculate percentage
concept1_summary <- concept1_summary %>%
  mutate(percentage = (frequency / sum(frequency)) * 100)

# Visualize top 10 concepts
ggplot(head(concept1_summary, 10), aes(x = reorder(level1_concept, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Level 1 Concepts", x = "Concept", y = "Frequency")

# View the first few rows
head(works_df)

# Get a summary of the dataframe
summary(works_df)
