---
title: "MY360_project_24980"
output: html_document
date: "2024-04-10"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#importing relevant libraries
library(quanteda)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(quanteda.textstats)
```

# FRIENDS – ANALYSIS OF CRITICISM

**Main question**: How does the text reflect criticism of the series, particularly regarding its homophobic, fat-shaming, or sexist content?

Topics of criticism:

-   gender roles / sexism
-   `approach to weight` - special focus on fat shaming targeted towards Monica
-   homophobia

*Ideas* *for methods*:

-   self-defined dictionaries for each topic - analyse which main character is associated with the most offensive language

    -   see which episodes contain the most offensive language

-   select episodes where topic of Monica's weight more heavily discussed (based on article) - run k means clustering model to see whether topic of weight can be detected as a cluster

    -   gender and weight – contrast sentiment of episodes relating to Joey / Will / Ugly Naked Guy with Monica (and see context)
    -   probabilistic topic model of Monica - contrast the topics of dialogues Monica is saying vs ones that mention her – does she talk about her past weight problems or is it just mentioned by others? (add wordclouds)

```{r}
# load library
library(friends)
```

## Overall analysis of offensive language in Friends

The criticism that the show received can be grouped into 3 categories of offensive language used by the characters:

1.  Homophobia
2.  Sexism
3.  Fat shaming

It is important to mention that as the show does not contain explicit hate speech, it only jokes about certain sensitive topics, thus it is more challenging to identify these topics precisely.

Here we will analyze the 6 main characters and how much they mentioned something offensive, furthermore explore which episodes contain the most offensive language. The main method here is automated dictionary methods, but explorations with regular expressions and kwic (key words in context) are also included.

-   occurrences of main words/expressions related to each topic + key words in context + mean sentiment score of sentences containing main words
-   create self-defined dictionary for each topic – see which main character used the most offensive language throughout the series + which episode has the most offensive language

### Homophobic content

First, lets explore the occurrences of homophobic content in the series. For this we will explore main words relating to LGBTQ+ individuals and see the context in which these words are used.\

```{r}
#occurrence of main words relating to LGBTQ+ individuals 

length(grep("gay*", friends$text, ignore.case=TRUE))
length(grep("lesbian*", friends$text, ignore.case=TRUE))
length(grep("homosexual*", friends$text, ignore.case=TRUE))
length(grep("transsexual | trans-sexual", friends$text, ignore.case=TRUE))
```

As we can see, the word "gay" occurred by far the most, while the words "lesbian", "homosexual" and "transsexual" rarely appeared in the series. Lets see the context in which these words appeared.

```{r}
kwic(friends$text, "trans-sexual*")
```

The word "transsexual" only appears once, and that 1 occurrence is part of a joke.

```{r}
kwic(friends$text, "lesbian*")
```

The word "lesbian" mostly occurs in the context of Carol, the ex-wife of Ross. Most of the times these sentences mock Ross for having a lesbian ex-wife, thus this word is also often the subject of jokes.

```{r}
kwic(friends$text, "homosexual*")
```

The word "homosexual" appears rarely, but the context of "homosexual hair" suggests that this word was also part of a joke.

```{r}
kwic_gay <- kwic(friends$text, "gay*")
head(kwic_gay, 10) #show first 10 instances
```

The word "gay" often comes up in the context of Chandler's appearance throughout the show. While there are some sentences relating to the acceptance of homosexuality, the word "gay" still often appears as part of jokes.

Lets see the average sentiment scores of sentences containing any of these words above.

```{r}
text_homosexual <- friends$text[grep("transsexual* | homosexual* | gay | lesbian*", friends$text, ignore.case=TRUE)]

#using positive - negative sentiment dictionary
data(data_dictionary_geninqposneg)

pos.words <- data_dictionary_geninqposneg[['positive']]
neg.words <- data_dictionary_geninqposneg[['negative']]
sent_dictionary <- dictionary(list(positive = pos.words,
                          negative = neg.words))

text_corpus_homosexual <- corpus(text_homosexual)
toks <- tokens(text_corpus_homosexual, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
dfm <- dfm(toks)
dfm_removed <- dfm_remove(dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok"))
#textplot_wordcloud(dfm_removed, rotation=0, min_size=.75, max_size=3, max_words=50)
sent_homosexual <- dfm_lookup(dfm_removed, sent_dictionary)

sentiment_scores <- rowSums(sent_homosexual[, c("positive", "negative")])
text_sentiment_df <- data.frame(
  text = text_homosexual)

#add sentiment scores to dataframe
text_sentiment_df$score <- as.numeric(sent_homosexual[,1]) - as.numeric(sent_homosexual[,2])

# what is the average sentiment score?
mean(text_sentiment_df$score)
# what is the most positive and most negative tweet?
most_positive_row <- text_sentiment_df[which.max(text_sentiment_df$score), ]
cat("Most positive text:", most_positive_row$text, "\n")
most_negative_row <- text_sentiment_df[which.min(text_sentiment_df$score), ]
cat("Most negative text:", most_negative_row$text, "\n")

```

Even though the topic of homosexuality mostly occurred as part of jokes, the mean sentiment score is overall positive for the texts mentioning homosexuality. The most positive text referring to homosexuality occurs when Ross tries to make up with his ex-wife despite her having come out as a lesbian earlier. The most negative text occurs when Joey advised a fellow actor at an audition to play the part gay, and the person was cast for the role. This sentence mentions "the stupid gay thing" as a negative occurrence.\
\
Lets explore which main character and which episode mentions the topic of homosexuality the most. For this, we will create a self-defined dictionary.

```{r}
#dictionary for homosexuality
homosexuality_dict <- dictionary(list(
  homosexuality = c("homosexual*", "gay", "lesbian", "same-sex", "transsex*", "bisexual*", "life partner", "coming out", "came out", "queer", "LGBTQ*", "pansexual", "non-binary", "Chandler's dad", "Chandler's father" )))

#subset of text with only 6 main characters 
main_characters_text <- friends$text[friends$speaker == c("Monica Geller", 'Ross Geller', 'Phoebe Buffay', 'Chandler Bing', 'Joey Tribbiani', 'Rachel Green')]

#creating corpus
main_char_corpus <- corpus(main_characters_text)
docvars(main_char_corpus, "speaker") <- friends$speaker[friends$speaker == c("Monica Geller", 'Ross Geller', 'Phoebe Buffay', 'Chandler Bing', 'Joey Tribbiani', 'Rachel Green')]
#remove numbers, punctuation, symbols
toks <- tokens(main_char_corpus,remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
main_char_dfm <- dfm(toks)
#remove stopwords and additional words that do not carry any meaning 
dfm_removed <- dfm_remove(main_char_dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "take", "got", "hi"))

#group based on main character
main_char_grouped <- dfm_group(dfm_removed, groups = speaker)
#weight to get relative frequencies
main_char_final <- dfm_weight(main_char_grouped, scheme = "prop")

dfm_dict <- dfm_lookup(main_char_final, dictionary = homosexuality_dict)
dfm_dict
```

Ross mentions the topic of homosexuality the most, which makes sense as he mentions his lesbian ex-wife several times throughout the series. It is surprising that Phoebe mentions homosexuality the most after Ross, as one would expect Chandler to mention it more. This is because of his dad being transsexual and also the ongoing jokes of him "looking gay".

What about which episode mentions homosexuality the most?

```{r}
# Group by title of the episodes 

# Merge episode titles with the friends dataframe based on episode number and season number
filtered_friends <- friends[friends$speaker != "Scene Directions", ]
friends_with_titles <- merge(filtered_friends, friends_info, by.x = c("episode", "season"), by.y = c("episode", "season"), all.x = TRUE)
episodes_data <- friends_with_titles[, c("text", "title")]
episodes_merged <- aggregate(text ~ title, data = episodes_data, FUN = paste, collapse = " ")

episodes_corpus <- corpus(episodes_merged)
docvars(episodes_corpus, "title") <- episodes_merged$title

#Tokenize corpus
toks <- tokens(episodes_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE)

#Create dfm from tokens
episodes_dfm <- dfm(toks)

# Remove stopwords and other common words
dfm_removed <- dfm_remove(episodes_dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "take", "got", "hi"))
dfm_removed <- dfm_trim(dfm_removed, min_docfreq = 3)

# Get episode titles
titles <- docvars(episodes_corpus, "title")

# Group dfm by episode title
episode_grouped <- dfm_group(dfm_removed, groups = titles)
#weight to get relative frequencies
episode_final <- dfm_weight(episode_grouped, scheme = "prop")

```

```{r}
#dictionary with words related to homosexuality
dfm_dict <- dfm_lookup(episode_final, dictionary = homosexuality_dict)

# Convert dfm_dict to a dataframe
dfm_df <- convert(dfm_dict, to = "data.frame")

# Sort the dataframe by the "homosexuality" column in descending order
dfm_df_sorted <- dfm_df[order(-dfm_df$homosexuality), ]

# Select the top 10 episodes
top_10_episodes <- head(dfm_df_sorted, 10)

# Print the top 10 episodes
top_10_episodes
```

It can be observed that the overall topics of the top 10 episodes based on homosexual content are very different, there do not seems to be a pattern or a large connection of homosexuality and the overall theme of the episodes. The top episode is about Pheobe's childhood friend who she officially married, but thought that he was gay. This makes sense that this episode is the most relevant in this case. However, it interesting that the episode related to the wedding of Carol (Ross' ex-wife) and Susan does not appear in the top 10 despite having the main focus of the episode on the wedding. It could be the case that the dictionary could not detect the jokes and references to lesbianism for that episode.

Finally, as Carol is a significant character for this topic, lets observe a wordcloud related to the texts that mention her.

```{r}
#wordcloud relating to Carol - Ross' ex-wife

text_carol <- friends$text[grep("Carol*", friends$text, ignore.case=TRUE)]

text_corpus_carol <- corpus(text_carol)
toks <- tokens(text_corpus_carol, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem <- tokens_wordstem(toks)
dfm <- dfm(toks_stem)
dfm_removed <- dfm_remove(dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "Carol*", "get", 'let', 'scene', 'hi'))
textplot_wordcloud(dfm_removed, rotation=0, min_size=1, max_size=4, max_words=70)
```

Even though the word "lesbian" is mostly used in jokes based on the context, when looking at the words occurring in text parts with Carol's name we cannot see any negative words relating to her character or sexuality.

Concluding the section on homosexuality, we can say that this topic is mostly subject to jokes and sarcasm, thus is it challenging to identify with dictionary methods. Based on the context and sentiments relating to the topics, there does not seem like the episodes have strong homophobic tendency. The average sentiment score of sentences mentioning homosexuality is still positive and one of the most relevant homosexual character (Carol) is also not mentioned in a negative context. Ross seems to talk about this topic the most, and this topic is not the primary focus of the episodes where it is mentioned the most. Nevertheless, the fact that this topic is the subject of several jokes throughout the seasons does reflect some homophobic tendency.

### Sexist content

Now lets move on to the topic of sexism. Lets first compare the context in which the words "male" and "female" appear.

```{r}
#comparing the context the words "male" and "female" are mentioned
toks <- tokens(friends$text, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks <- tokens_wordstem(toks)

kwic(toks, "female*")
kwic(toks, 'male*')
```

Already by inspecting this comparison we can observe some sexist language. The word "female" is mostly used in the context of sexuality and the looks of women, while the word "male" occurs when mentioning non-traditional male jobs (nanny, assistant, nurse etc.). This clearly separates traditional female and male roles.

```{r}
# context of the word "girl" - with a focus on the word appearing before "girl"

#kwic(toks_stem, pattern = phrase("\\b[0-9_A-Za-z]+\\b girl(?!friend)\\b"), valuetype = "regex")


```

After inspecting the context in which the world "girl" appears, one can observe that when an adjective is in front of the word, it usually refers somehow to the body or look of women.

Lets visualize this on a wordcloud and compare it to a wordcloud about men.

```{r}
text_women <- friends$text[grep("women*|woman*|girl*|lady", friends$text, ignore.case=TRUE)]

text_corpus_women <- corpus(text_women)
toks <- tokens(text_corpus_women, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem <- tokens_wordstem(toks)
dfm <- dfm(toks_stem)
dfm_removed <- dfm_remove(dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "girl*", "get", 'let', 'scene', 'hi', 'woman', 'women', "y'know"))
textplot_wordcloud(dfm_removed, rotation=0, min_size=1, max_size=4.5, max_words=100)
```

```{r}
text_men <- friends$text[grep("men|male|boy*|manly", friends$text, ignore.case=TRUE)]

text_corpus_men <- corpus(text_men)
toks <- tokens(text_corpus_men, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem <- tokens_wordstem(toks)
dfm <- dfm(toks_stem)
dfm_removed <- dfm_remove(dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know", 'take', 'um', 'even', 'much', 'two', 'yes'))
textplot_wordcloud(dfm_removed, rotation=0, min_size=1, max_size=4.5, max_words=100)
```

Comparing the wordclouds from the texts mentioning men and women, there first seems to be little difference between them. Both mostly mention words that do not relate to gender stereotypes. Looking at the lower frequency words, there is a small difference related to gender differences. Namely, the wordcloud visualizing texts mentioning women include several words more frequently like pretty, beautiful, love and do not mention anything related to work. In contrast to this, the wordcloud for texts mentioning men these adjectives are not present and the topic of boxing and working does come up (although not so significantly).

```{r}
#more explicit words objectifying women
toks <- tokens(friends$text, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks <- tokens_wordstem(toks)
kwic(toks, "whore")
kwic(toks, "slut")
women_bodies_kwic <- kwic(friends$text, "breast*")
head(women_bodies_kwic, 10)
#kwic(toks, "ass")
```

Women are often mentioned in the context of inappropriate words relating to their bodies.

\
Lets now create a dictionary and see which character mentions the topic of gender the most.

```{r}
#dictionary for sexism
sexism_dict <- dictionary(list(
  sexism = c("male", "female", "male role", "female role", "manly", "women", "woman", "girly", "like a girl", "like a woman", "hot girl", "pretty girl", "beautiful girl", "hot woman", "pretty woman", "slutty", "slut", "whore", "breast")))

#we will use the main_char_final dfm (pre-processing same as before)

dfm_dict_sexism <- dfm_lookup(main_char_final, dictionary = sexism_dict)
dfm_dict_sexism
```

It can be seen that the male main characters use sexist language more than female characters do, which is in line with the observations thus far. It is surprising that Joey is not in the first place, as criticism is mainly targeted towards him for objectifying women.

Lets look at which episodes mention the most sexist content.

```{r}
#we will use the episode_final dfm

#dictionary with words related to sexism
dfm_dict_sexism2 <- dfm_lookup(episode_final, dictionary = sexism_dict)

# Convert dfm_dict to a dataframe
dfm_df2 <- convert(dfm_dict_sexism2, to = "data.frame")

# Sort the dataframe by the "homosexuality" column in descending order
dfm_df_sorted2 <- dfm_df2[order(-dfm_df2$sexism), ]

# Select the top 10 episodes
top_10_episodes2 <- head(dfm_df_sorted2, 10)

# Print the top 10 episodes
top_10_episodes2
```

Even based on the titles of the top 10 episodes containing sexist content, one can say that the result is relatively accurate. The top episode containing sexist content relates to a Valentine's day dinner between Ross and his ex-wife, while the female main characters burn mementos of their old boyfriends, this episode mentions several gender-based stereotypes. The other episodes on this top 10 list also contain several gender-based stereotypes.

As Joey is the main target of the criticism related to sexism, lets inspect his lines further.

```{r}
#subset of text with only Joey 
joey_text <- friends$text[friends$speaker == 'Joey Tribbiani']

#creating corpus
joey_corpus <- corpus(joey_text)

#remove numbers, punctuation, symbols
toks <- tokens(joey_corpus,remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
joey_dfm <- dfm(toks)
#remove stopwords and additional words that do not carry any meaning 
dfm_removed <- dfm_remove(joey_dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "take", "got", "hi", 'i-i'))

textplot_wordcloud(dfm_removed, rotation=0, min_size=1, max_size=4.5, max_words=120)
```

Based on this wordcloud, there does not seem to be enough evidence for sexist content against Joey.

Concluding the part about sexist language, in particular regarding the objectification of women and gender-based roles. We have seen that women are often mentioned in context of sexuality, their bodies and looks. Additionally, the word "male" often appeared when describing nontraditional male career choices, such as being a nanny. The male main characters seem to have used significantly more sexist language than the women main characters. Although Joey's character is heavily criticized for objectifying women, so far we have not seen enough text to prove this.

### Fat shaming content

Finally, lets turn to the 3rd topic of offensive language: fat phobia.

This is mostly related to the character of Monica, as her past weight issues are often a subject of jokes throughout the series. However, other characters have also been criticized for their weight, this will be further analyzed in the upcoming section.

Lets first look at the number of occurrences of weight-related words and some of the key words in context.

```{r}
#redefine tokens
toks <- tokens(friends$text, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks <- tokens_wordstem(toks)

#number of occurrences for key words
length(grep("fat", toks, ignore.case=TRUE)) #cannot be fat*, since that would include words like father etc.
length(grep("weight*", toks, ignore.case=TRUE))

```

```{r}
#context of words
head(kwic(toks, pattern = 'fat'), 15)
kwic(toks, pattern = 'weight*')

```

Lets now inspect the wordcloud with the text mentioning "fat" or "weight".

```{r}
text_weight <- friends$text[grep("fat|weight", friends$text, ignore.case=TRUE)]

text_corpus_weight <- corpus(text_weight)
toks_weight <- tokens(text_corpus_weight, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem_weight <- tokens_wordstem(toks_weight)
weight_dfm <- dfm(toks_stem_weight)
dfm_removed_weight <- dfm_remove(weight_dfm, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know", "father", "godfath", "fath"))
textplot_wordcloud(dfm_removed_weight, rotation=0, min_size=1, max_size=4.5, max_words=100)
```

Which line mentioning this topic is the most negative and most positive? What is the average sentiment of these lines?

```{r}
#using sentiment dictionary from before

sent_fatphobia <- dfm_lookup(dfm_removed_weight, sent_dictionary)

sentiment_scores_fatphobia <- rowSums(sent_fatphobia[, c("positive", "negative")])
weight_sentiment_df <- data.frame(
  text = text_weight)

#add sentiment scores to dataframe
weight_sentiment_df$score <- as.numeric(sent_fatphobia[,1]) - as.numeric(sent_fatphobia[,2])

# what is the average sentiment score?
mean(weight_sentiment_df$score)
# what is the most positive and most negative tweet?
most_positive_row_weight <- weight_sentiment_df[which.max(weight_sentiment_df$score), ]
cat("Most positive text:", most_positive_row_weight$text, "\n")
most_negative_row_weight <- weight_sentiment_df[which.min(weight_sentiment_df$score), ]
cat("Most negative text:", most_negative_row_weight$text, "\n")
```

The average sentiment is lower than for homosexuality. The most positive text does not make sense in this context, but the most negative text indeed contains fatphobic content.

Lets now create a dictionary for fatphobic content and examine which main character mentions the issue the most, additionally which episode contains the most references to the topic.

```{r}
#dictionary for fat shaming
fatphobia_dict <- dictionary(list(
  fatphobia = c("fat", "weight", "fat girl", "fat guy", "fat man", "big girl", "big guy", "overeat*", "obese", "overweight", "chubby", "lose weight", "fat camp")))

#we will use the main_char_final dfm (pre-processing same as before)

dfm_dict_fatphobia <- dfm_lookup(main_char_final, dictionary = fatphobia_dict)
dfm_dict_fatphobia
```

Chandler refers to weight issues the most, while Monica hardly mentions the topic even though she was often the subject of these mentions.

Which episode mentions weight issues the most?

```{r}
#we will use the episode_final dfm

#dictionary with words related to sexism
dfm_dict_fatphobia2 <- dfm_lookup(episode_final, dictionary = fatphobia_dict)

# Convert dfm_dict to a dataframe
dfm_df_fat <- convert(dfm_dict_fatphobia2, to = "data.frame")

# Sort the dataframe by the "fatphobia" column in descending order
dfm_df_fat_sorted <- dfm_df_fat[order(-dfm_df_fat$fatphobia), ]

# Select the top 10 episodes
top_10_episodes_weight <- head(dfm_df_fat_sorted, 10)

# Print the top 10 episodes
top_10_episodes_weight
```

Interestingly, the above list contains episode names that are not the ones that some articles mention as the most fat phobic episodes.

Concluding the part on fat shaming content, one can see that insensitivity towards weight is mostly targeted against Monica due to her past issues, but other characters are also subjects of fat shaming. The mean sentiment score of texts mentioning the words “fat” or “weight” is slightly positive (0.62) but still significantly lower than the sentiment of texts mentioning homosexuality. The most negative sentence indicates harsh fatphobic content. Out of the main characters, Chandler mentions the topic the most frequently, while Monica hardly mentions the topic even though she was often the subject of these.

Now lets move on to a more focused analysis on the fat shaming against Monica.

## Focused analysis on fat shaming against Monica

3 parts:

-   Can the topic of weight be identified in the main episodes where Monica's weight is criticized? - k means clustering
-   Is there a difference when the script refers to male or female weight issues? - sentiment analysis
-   Does Monica mention her past weight issues or do the others mention it when talking about her?

### K means clustering on fat phobic episodes

Based on this article: <https://screenrant.com/friends-characters-fat-shamed/>

Episodes with fat shaming:

-   Season 9, episode 16: The One With The Boob Job,
-   Season 7, episode 6: The One With The Nap Partners
-   Season 2, episode 14: The One With The Prom Tape
-   Season 8, episode 9: The One With The Rumor
-   Season 6, episode 15 - 16: The One That Could Have Been Pt1 &Pt2
-   Season 10, episode 12: The One Where The Stripper Cries
-   Season 7, episode 16: The One With the Truth About London
-   Season 5, episode 8: The One With All the Thanksgivings
-   Mentions of "Ugly Naked Guy" (person living across the street that they often spy on) - across multiple episodes

Out of these, the ones mainly targeted **against Monica**:

-   Season 9, episode 16: The One With The Boob Job
-   Season 2, episode 14: The One With The Prom Video
-   Season 6, episode 15 - 16: The One That Could Have Been (Pt1 &Pt2)
-   Season 5, episode 8: The One with All the Thanksgivings

Lets see if we can identify the topic of weight in these episodes.

First, lets select the relevant episodes.

```{r}
#select text from the above episodes

friends_merged <- merge(friends, friends_info, by.x = c("episode", "season"), by.y = c("episode", "season"), all.x = TRUE)

# Filter episodes based on title
selected_episodes <- friends_merged[friends_merged$title %in% c("The One with the Boob Job",
                                                               "The One with the Prom Video",
                                                               "The One That Could Have Been",
                                                               "The One with All the Thanksgivings"), ]
selected_text <- selected_episodes$text
#length(selected_text)
```

Can K-means clustering identify the topic of weight in these episodes?

```{r}
#pre-processing
selected_corpus <- corpus(selected_text)
toks_selected <- tokens(selected_corpus, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem_selected <- tokens_wordstem(toks_selected)
dfm_selected <- dfm(toks_stem_selected)
dfm_selected_removed <- dfm_remove(dfm_selected, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know", 'take', 'um', 'even', 'much', 'two', 'yes', 'i-i', 'wow', 'god', 'uhh', 'still', 'and-and-and', 'sooo', 'la-la-la'))
cdfm <- dfm_weight(dfm_trim(dfm_selected_removed, min_termfreq = 3, verbose=TRUE), "prop")
```

```{r}
set.seed(1234) # set random seed to ensure replicability
kc <- kmeans(cdfm, centers=7)
table(kc$cluster)


# Plots of each cluster

#only showing relevant clusters
head(textstat_keyness(cdfm, target=kc$cluster==1),n=20)
head(textstat_keyness(cdfm, target=kc$cluster==2),n=20)
#head(textstat_keyness(cdfm, target=kc$cluster==3),n=20)
#head(textstat_keyness(cdfm, target=kc$cluster==4),n=20)
#head(textstat_keyness(cdfm, target=kc$cluster==5),n=20)
head(textstat_keyness(cdfm, target=kc$cluster==6),n=20)
head(textstat_keyness(cdfm, target=kc$cluster==7),n=20)

```

As we can see, there is one large cluster and several really small clusters, all of which contain seemingly unrelated words. Increasing the value of K only results in the creation of many tiny clusters, but no large insights could be made. When K=7, the topic of weight comes up as a small cluster, as the words "woman" and "fat" appear together. Additional clusters reveal the topics of divorce, marriage and jobs.

Can we identify the topic of weight using topic modelling?

```{r}
library("stm")
```

Lets prepare the input for the structural topic model function.

```{r}
dfm_trimmed <- dfm_trim(dfm_selected_removed, min_termfreq = 3)
stm_input <- convert(dfm_trimmed, to = "stm")
```

Lets run the model:

```{r}
#structural topic model
ctmodel <- stm(stm_input$documents, stm_input$vocab, K = 10,
               data = stm_input$meta, verbose = FALSE,
               init.type = c("Spectral"), seed = 123) 
```

```{r}
#visualization
plot(ctmodel)
```

Interestingly, even when increasing the value of K, the topics rarely refer to Monica or the topic of weight. K-means clustering has proven to be slightly more useful in this case, although neither could find a strong relevance of this topic in the text. The reason for this could be that these topics are more relevant in acting and not explicitly in the text.

### Sentiment comparison of fat shaming against men vs women

gender and weight – contrast sentiment of episodes / text relating to Joey / Will / Ugly Naked Guy with Monica (and see context)

Episodes with reference to weight issues for women:

-   mostly for Monica, same episodes as before

Episodes with reference to weight issues for men:

-   Season 7, episode 16: The One with the Truth about London (- Joey portrayed as overweight)
-   mentions of "Ugly Naked Guy" -multiple episodes
-   Season 8, episode 9: The one with the Rumor (- the character of Will lost weight)

```{r}
#average sentiment of episodes mentioning Monica's past weight issues

#using cdfm - dfm with episodes relating to Monica's weight
#using sent_dictionary -- sentiment dictionary
sent_monica <- dfm_lookup(cdfm, sent_dictionary)

sentiment_scores_monica <- rowSums(sent_monica[, c("positive", "negative")])
text_sentiment_monica <- data.frame(
  text = selected_text)

#add sentiment scores to dataframe
text_sentiment_monica$score <- as.numeric(sent_monica[,1]) - as.numeric(sent_monica[,2])

# what is the average sentiment score?
mean(text_sentiment_monica$score)

```

The mean sentiment score for Monica is very close to 0 – indicating many mentions of negative words around these topics.

```{r}
text_ugly_naked_guy <- friends$text[grep("Ugly Naked Guy", friends$text, ignore.case=TRUE)]

will_episode <- friends_merged[friends_merged$title %in% c("The One with the Rumor"), ]
will_text <- will_episode$text[grep("Will", will_episode$text, ignore.case=FALSE)] #include only uppercase version

joey_weight <- friends_merged[friends_merged$title %in% c("The One with the Truth About London"), ]
joey_text <- joey_weight$text[grep("Joey", joey_weight$text, ignore.case=FALSE)] #include only uppercase version

merged_text <- c(text_ugly_naked_guy, will_text, joey_text)
text_corpus_male <- corpus(merged_text)
toks_male <- tokens(text_corpus_male, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem_male <- tokens_wordstem(toks_male)
dfm_male <- dfm(toks_stem_male)
dfm_removed_male <- dfm_remove(dfm_male, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know"))

sent_male <- dfm_lookup(dfm_removed_male, sent_dictionary)

sentiment_scores_male <- rowSums(sent_male[, c("positive", "negative")])
text_sentiment_male <- data.frame(
  text = merged_text)

#add sentiment scores to dataframe
text_sentiment_male$score <- as.numeric(sent_male[,1]) - as.numeric(sent_male[,2])

# what is the average sentiment score?
mean(text_sentiment_male$score)
```

The episodes relating to male weight issues are *slightly more positive* than for the episodes mentioning Monica's weight issues, however, the difference might not be statistically significant due to the small number of texts referring to male weight issues.

### Topic Modelling: Is Monica mentioning her past weight issues or are the others bringing it up?

Lets create 2 separate document feature matrices: one where Monica is the speaker, and one where others mention Monica (but she is not the speaker)

First lets analyse the topics Monica is frequently talking about:

```{r}
# Monica is the speaker 

monica_text <- friends$text[friends$speaker == 'Monica Geller']

text_corpus_monica <- corpus(monica_text)
toks_monica <- tokens(text_corpus_monica, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem_monica <- tokens_wordstem(toks_monica)
dfm_monica <- dfm(toks_stem_monica)
dfm_monica <- dfm_remove(dfm_monica, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know", 'take', 'um', 'even', 'much', 'two', 'yes', 'i-i', 'wow', 'god', 'uhh', 'still', 'and-and-and', 'sooo', 'la-la-la'))

dfm_monica_trimmed <- dfm_trim(dfm_monica, min_termfreq = 3)
stm_input_monica <- convert(dfm_monica_trimmed, to = "stm")
```

```{r}
#structural topic model for Monica's lines
ctmodel_monica <- stm(stm_input_monica$documents, stm_input_monica$vocab, K = 15,
               data = stm_input_monica$meta, verbose = FALSE,
               init.type = c("Spectral"), seed = 123) 
plot(ctmodel_monica)
```

Now, lets contrast this with what the others say about Monica. For this lets exclude Monica as the speaker and take the texts mentioning Monica by the other characters.

```{r}
#what others say about Monica

not_monica_text <- friends$text[friends$speaker != 'Monica Geller']
mention_monica_text <- not_monica_text[grep("Monica", not_monica_text, ignore.case=TRUE)]

text_corpus_monica_mention <- corpus(mention_monica_text)
toks_monica_mention <- tokens(text_corpus_monica_mention, remove_numbers = TRUE,  remove_punct = TRUE, remove_url=TRUE, remove_symbols = TRUE)
toks_stem_monica_mention <- tokens_wordstem(toks_monica_mention)
dfm_monica_mention <- dfm(toks_stem_monica_mention)
dfm_monica_mention <- dfm_remove(dfm_monica_mention, c(stopwords("english"), "well", "okay", "so", "oh", "umm", "uh", "ooh", "ah", "hey", "yeah", "ok", "get", 'let', 'scene', 'hi', "y'know", 'take', 'um', 'even', 'much', 'two', 'yes', 'i-i', 'wow', 'god', 'uhh', 'still', 'and-and-and', 'sooo', 'la-la-la', 'monica'))

dfm_monica_trimmed_mention <- dfm_trim(dfm_monica_mention, min_termfreq = 3)
stm_input_monica_mention <- convert(dfm_monica_trimmed_mention, to = "stm")
```

```{r}
#structural topic model for mentions of Monica
ctmodel_monica_mention <- stm(stm_input_monica_mention$documents, stm_input_monica_mention$vocab, K = 15,
               data = stm_input_monica_mention$meta, verbose = FALSE,
               init.type = c("Spectral"), seed = 123) 
plot(ctmodel_monica_mention)
```

The topic of weight does not seem to be relevant in either of these cases. It could be that this topic is rarely mentioned in absolute terms but the way it is mentioned is more negative.
---
title: "MY360 Group"
output: html_document
date: "2024-03-22"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Source: https://github.com/emilhvitfeldt/friends

#character development -- from a storytelling side: relationships between characters 
#market reaction -- from a business side? for example, most watched episode -- are there any common features 
#values for language learnings 
#production -- from production side

#there should be a link connecting each other, like one part can be applied to another part 

## Imoport the dataset

```{r cars}
library("friends")

library("quanteda", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library("quanteda.textplots")
library(quanteda.textstats)
library(quanteda.textmodels)
library(streamR)
library(stringr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(stm)
library(tidyr)
library(zoo)
library(patchwork)
library(syuzhet)
```


## Display the data

```{r pressure, echo=FALSE}
friends
```


```{r pressure, echo=FALSE}
dplyr::glimpse(friends)
```

```{r pressure, echo=FALSE}
dplyr::glimpse(friends_emotions)
```

```{r pressure, echo=FALSE}
dplyr::glimpse(friends_entities)

```

```{r pressure, echo=FALSE}
dplyr::glimpse(friends_info)
```

##Character development

## pre-processing the data

```{r pressure, echo=FALSE}

friends$text <- str_replace_all(friends$text,"<U\\+[0-9A-Z]+>"," ")

#create a function for creating dfm
create_dfm <- function(text_input) {
  
  # Tokenize and preprocess the text
  fr_corpus <- corpus(text_input, text_field ='text')
  
  # Tokenization
  toks <- tokens(fr_corpus, remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE)
  toks <- tokens_remove(toks, stopwords("english"))
  toks <- tokens_remove(toks, c("ooh", "oh", "uh", "wanna", "gonna", "uh-huh", "wow", "ugh","emm","hey","hi",
                              "uhh","umm","um","ya","ok","yeah","ah","gotta","just","go","okay","know","well",
                              "like","right","get","uhm","good","one","think","c'mon","look","guy","now","yes","no",
                              "y'know","come","tell","nice","wait","mean","want","can","really","thank","please",
                              "say","see","thing","got","god","huh","ohh","sorry","i-i","sure","yay","ow","alright",
                              "fine","great","maybe","time","something","some","back"))
  toks <- tokens_wordstem(toks)
  toks <- tokens_remove(toks, c("guy","go"))
  
  # Create the document-feature matrix (dfm)
  frdfm <- dfm(toks, tolower = TRUE)
  frdfm <- dfm_group(frdfm, group = text_input$speaker)
  
  return(frdfm)
}

```

In this session, we want to particularly focus on the character development of Rachel and Ross -- who were confirmed by the officials that are the "main" of main characters

Rachel, as the representative of women self-awareness

Rachel herself: her awareness/change of a sweet daddy's girl to a successful businesswomen 

- season 3 episode 12, Rachel starts to work in Bloomingdale (from waiter )
- season 5 episode 17，Rachel get a job from Ralph Lauren 

ideas
-periods length not equal so cannot treat the top-frequency as counts 


#Divide the dataset into three subset 
```{r pressure, echo=FALSE}

rachel1 <- friends %>%
  filter(season >= 1 & season <= 3) %>%   # Select seasons 1 through 3
  filter(!(season == 3 & episode > 11))

rachel2 <- friends %>%
  filter(
    (season == 3 & episode >= 12) |  # Season 3, episodes 12 and higher
    (season == 4) |                  # All of Season 4
    (season == 5 & episode <= 16)    # Season 5, up to episode 16
  )

rachel3 <- friends %>%
  filter(
    (season == 5 & episode >= 17) |  # Season 5, episodes 17 and higher
    (season > 5)                     # All episodes from Season 6 onwards
  )

```

#Create dfm
```{r pressure, echo=FALSE}

frdfm <- create_dfm(friends)

dfm1 <- create_dfm(rachel1)
dfm2 <- create_dfm(rachel2)
dfm3 <- create_dfm(rachel3)

r1dfm <- dfm1[docvars(dfm1, "speaker") %in% c("Rachel Green"), ]
r2dfm <- dfm2[docvars(dfm2, "speaker") %in% c("Rachel Green"), ]
r3dfm <- dfm3[docvars(dfm3, "speaker") %in% c("Rachel Green"), ]

r_dfm <- rbind(r1dfm, r2dfm, r3dfm)
r_dfm
```

#The network graph

```{r pressure, echo=FALSE}

maindfm <- frdfm[docvars(frdfm, "speaker") %in% c("Joey Tribbiani","Ross Geller","Chandler Bing",
                                                  "Rachel Green","Monica Geller","Phoebe Buffay"), ]

feat_freq <- textstat_frequency(maindfm)

# Get the top N features by frequency
top_feats <- feat_freq %>%
  arrange(desc(frequency)) %>%
  head(30) %>%
  pull(feature)

maindfm <- dfm_select(maindfm, top_feats)

# Create an fcm from the top features dfm
fcm_top <- fcm(maindfm, context = "document")

# Plot network with quanteda.textplots
quanteda.textplots::textplot_network(fcm_top, min_freq = 0.7, vertex_labelsize = 5)

```

```{r pressure, echo=FALSE}

#The fist session 
stm_input <- convert(maindfm, to = "stm")

ctmodel <- stm(stm_input$documents, stm_input$vocab, K = 7,
               data = stm_input$meta, verbose = FALSE,
               init.type = c("Spectral"), seed = 123) 

plot(ctmodel)

cloud(ctmodel, topic = 2, scale = c(2,.25))
cloud(ctmodel, topic = 4, scale = c(2,.25))

```


#An Overview of the character: Change alongside different periods 
```{r pressure, echo=FALSE}

p1 <- textplot_wordcloud(r1dfm, rotation=0, max_words = 30)
p2 <- textplot_wordcloud(r2dfm, rotation=0, max_words = 30)
p3 <- textplot_wordcloud(r3dfm, rotation=0, max_words = 30)
```


#Topic fearures along with the periods 
```{r pressure, echo=FALSE}
r_dfm_tfidf <- dfm_tfidf(r_dfm)
r_dfm_tfidf

features1 <- topfeatures(r_dfm_tfidf[1,],n=10)
features2 <- topfeatures(r_dfm_tfidf[2,],n=10)
features3 <- topfeatures(r_dfm_tfidf[3,],n=10)

features_df1 <- data.frame(feature = names(features1), score = features1)
features_df2 <- data.frame(feature = names(features2), score = features2)
features_df3 <- data.frame(feature = names(features3), score = features3)

p1 <- ggplot(features_df1, aes(x = feature, y = score)) +
  geom_bar(stat = "identity") +
  labs(title = "Period 1: Top 10 Features")

p2 <- ggplot(features_df2, aes(x = feature, y = score)) +
  geom_bar(stat = "identity") +
  labs(title = "Period 2: Top 10 Features")

p3 <-ggplot(features_df3, aes(x = feature, y = score)) +
  geom_bar(stat = "identity") +
  labs(title = "Period 3: Top 10 Features")

p1
p2
p3
```

#Sentiment analysis: 
```{r pressure, echo=FALSE}

# Take the text
text_r1 <- subset(rachel1$text, rachel1$speaker %in% c('Rachel Green'))
text_r2 <- subset(rachel2$text, rachel2$speaker %in% c('Rachel Green'))
text_r3 <- subset(rachel3$text, rachel3$speaker %in% c('Rachel Green'))

# Calculate sentiment for each document
sentiment_r1 <- get_sentiment(text_r1)
sentiment_r2 <- get_sentiment(text_r2)
sentiment_r3 <- get_sentiment(text_r3)

# Create data frame for plotting 
sentiment_data_long <- data.frame(
  sentence = c(1:length(sentiment_r1), 1:length(sentiment_r2), 1:length(sentiment_r3)),
  sentiment = c(sentiment_r1, sentiment_r2, sentiment_r3),
  document = rep(c("Document 1", "Document 2", "Document 3"), times = c(length(sentiment_r1), length(sentiment_r2), length(sentiment_r3)))
)

#Smooth the sentiment value 
sentiment_data_long <- sentiment_data_long %>%
  group_by(document) %>%
  arrange(document, sentence) %>%
  mutate(sentiment_smooth = rollmean(sentiment, 50, fill = NA, align = 'right'))

# Plot with ggplot2 using the smoothed values
ggplot(sentiment_data_long, aes(x = sentence, y=sentiment_smooth, color=document)) +
  geom_line(na.rm = TRUE, alpha = 0.3) +
  geom_smooth(aes(group = document), method = "loess", se = FALSE, na.rm = TRUE) +  # Add a LOESS smoothed trend line
  labs(title = "Smoothed Sentiment Trajectory of Rachel Green", y = "Smoothed Sentiment Score", x = "Sentence") +
  theme_minimal() +
  theme(legend.title = element_blank())+ scale_color_manual(values = c("red", "green", "blue"),
                     labels = c("Period 1", "Period 2", "Period 3"))


# Create a bar plot using ggplot2
avg_sentiment <- data.frame(
  document = c("Period 1", "Period 2", "Period 3"),
  average_sentiment = c(mean(sentiment_r1), mean(sentiment_r2), mean(sentiment_r3))
)

ggplot(avg_sentiment, aes(x = document, y = average_sentiment, fill = document)) +
  geom_bar(stat = "identity", width = 0.5) + # "identity" to use the actual values from data 
  labs(title = "Average Sentiment Score per Document", x = "Document", y = "Average Sentiment Score", labels= c("Period 1","Period 2","Period 3")) +
  theme_minimal() +
  theme(legend.position = "none") # Remove legend if not needed



```

#Dictionary methods: the measurements of her "indendency"

```{r pressure, echo=FALSE}
myDict <- dictionary(list(work_related = c("job*", "work*","success*","congrat*","honor*",
                                           "dream*","grow*","career*","interview*"),
                          money_related = c("rich*","salary*","money*","poor*","dollar*","wealth*"),
                          career_related =c("merchandise*","*waitress*", "fashion*","retail*","assistant*",
                                            "executive*","director*","ralph*","lauren*","buyer*") ))

# glob formats
dic_dfm <- dfm_lookup(r_dfm, myDict, valuetype = "glob")
dic_dfm

#back to the instances
friends_rachel1 <- subset(rachel1$text, rachel1$speaker %in% c('Rachel Green'))
corpus_rachel1 <- corpus(friends_rachel1, text_field ='text')
kwic(corpus_rachel1, c("job*", "work*","success*","congrat*","honor*",
                                           "dream*","grow*","fashion*","career*","interview*"), window=10)[1:10,]

friends_rachel2 <- subset(rachel2$text, rachel2$speaker %in% c('Rachel Green'))
corpus_rachel2 <- corpus(friends_rachel2, text_field ='text')
kwic(corpus_rachel2, c("job*", "work*","success*","congrat*","honor*",
                                           "dream*","grow*","fashion*","career*","interview*"), window=10)[1:10,]

friends_rachel3 <- subset(rachel3$text, rachel3$speaker %in% c('Rachel Green'))
corpus_rachel3 <- corpus(friends_rachel3, text_field ='text')
kwic(corpus_rachel3, c("job*", "work*","success*","congrat*","honor*",
                                           "dream*","grow*","fashion*","career*","interview*"), window=10)[1:10,]


```
# Move to gender disclouse

#first of all wordcloud
```{r pressure, echo=FALSE}
textplot_wordcloud(genderdfm, comparison = TRUE, rotation=0, max_words = 100, 
                   min_size = c(2, 0.5))
```







---
title: "Analysis of Language diversity and complexity across contexts in Friends"
output: html_document
date: "2024-04-24"
---
Loading libraries and setting working directory
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(stringr)
library(officer)
library(dplyr)
library(tokenizers)
library(koRpus)
library(ggplot2)

setwd("C:/Users/PC/OneDrive/Bureau/friends")

```

Loading and merging datasets and running preprocessing steps to it by removing punctuation, tokenizing, converting to lowercase, and specifying 6 main characters for analysis. 
```{r}
load("friends_info.rda")
load("friends.rda")
load("friends_emotions.rda")

friends_data <- merge(friends, friends_info, by = c("season", "episode"))

allowed_characters <- c("Rachel Green", "Ross Geller", "Chandler Bing", "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

friends_data <- friends_data[friends_data$speaker %in% allowed_characters, ]

merged_data <- merge(friends_data, friends_emotions, by = c("season", "episode", "scene", "utterance"), all.x = TRUE)

merged_data$text <- tolower(merged_data$text)  
merged_data$text <- gsub("[[:punct:]]", "", merged_data$text)  
```


******* Section 1:  Description of Language Diversity and Complexity in the script********

I set up the functions for Type-Token Ratio and Mean Length of Utterance metrics and applied them to the merged dataframe. I then calculated the top and bottom 20% threshold according to viewership and ratings and I subset the merged dataframe into four separate dataframes. 
```{r}
# Function to calculate Type-Token Ratio (TTR)
calculate_ttr <- function(text) {
  tokens <- unlist(tokenize_words(text))
  if (length(tokens) > 0) {
    unique_words <- unique(tokens)
    ttr_value <- length(unique_words) / length(tokens)
    return(ttr_value)
  } else {
    return(NA)
  }
}

# Function to calculate Mean Length of Utterance (MLU)
calculate_mlu <- function(text) {
  sentences <- strsplit(text, "\\. ")
  total_words <- sum(sapply(sentences, function(x) length(unlist(strsplit(x, "\\s+")))))
  total_sentences <- length(sentences)
  if (total_sentences > 0) {
    mlu_value <- total_words / total_sentences
    return(mlu_value)
  } else {
    return(NA)
  }
}

# Calculating TTR and MLU for the merged dataframe
merged_data <- merged_data %>%
  mutate(TTR = sapply(text, calculate_ttr),
         MLU = sapply(text, calculate_mlu))

# Calculating the threshold for top 20% and bottom 20%, viewership
top_threshold <- quantile(merged_data$us_views_millions, 0.8)
bottom_threshold <- quantile(merged_data$us_views_millions, 0.2)

# Subsetting the data for  20% most viewed episodes and 20% least viewed episodes
top_20_percent <- merged_data %>%
  filter(us_views_millions >= top_threshold)
bottom_20_percent <- merged_data %>%
  filter(us_views_millions <= bottom_threshold)

# Calculating the threshold ratings for top 20% and bottom 20%, ratings
best_threshold <- quantile(merged_data$imdb_rating, 0.8)
worst_threshold <- quantile(merged_data$imdb_rating, 0.2)

# Subseting merged_data to create dataframes for top 20% and bottom 20% rated episodes
best_20_percent <- merged_data %>%
  filter(imdb_rating >= best_threshold)
worst_20_percent <- merged_data %>%
  filter(imdb_rating <= worst_threshold)
```

I calculated and printed the average TTR and MUL for the most and least viewed & highest and lowest rated episodes. I then used the results to create a table in the overleaf report using Latex syntax.
```{r}

# Calculating average TTR and MLU for top 20% episodes
avg_ttr_top <- mean(top_20_percent$TTR, na.rm = TRUE)
avg_mlu_top <- mean(top_20_percent$MLU, na.rm = TRUE)

# Calculating average TTR and MLU for bottom 20% episodes
avg_ttr_bottom <- mean(bottom_20_percent$TTR, na.rm = TRUE)
avg_mlu_bottom <- mean(bottom_20_percent$MLU, na.rm = TRUE)

# Printing the results
cat("Top 20% Episodes:\n")
cat("Average TTR:", avg_ttr_top, "\n")
cat("Average MLU:", avg_mlu_top, "\n\n")

cat("Bottom 20% Episodes:\n")
cat("Average TTR:", avg_ttr_bottom, "\n")
cat("Average MLU:", avg_mlu_bottom, "\n")


# Calculating average TTR and MLU for top 20% episodes
avg_ttr_best <- mean(best_20_percent$TTR, na.rm = TRUE)
avg_mlu_best <- mean(best_20_percent$MLU, na.rm = TRUE)

# Calculating average TTR and MLU for bottom 20% episodes
avg_ttr_worst <- mean(worst_20_percent$TTR, na.rm = TRUE)
avg_mlu_worst <- mean(worst_20_percent$MLU, na.rm = TRUE)

# Printing the results
cat("Best 20% Episodes:\n")
cat("Average TTR:", avg_ttr_best, "\n")
cat("Average MLU:", avg_mlu_best, "\n\n")

cat("Worst 20% Episodes:\n")
cat("Average TTR:", avg_ttr_worst, "\n")
cat("Average MLU:", avg_mlu_worst, "\n")
``` 

I read the static idioms wordlist from a txt file and preprocessed it. I created a function to check for idioms in a given text and applied it to the four categories of sampled episodes. 
```{r}
idioms <- readLines("idioms.txt")

# Function to check for idioms in a given text
check_idioms <- function(text, idioms_list) {
  idioms_list <- tolower(idioms_list)
  
  found_idioms <- idioms_list[sapply(idioms_list, function(idiom) grepl(idiom, tolower(text)))]
  
  return(unique(found_idioms))
}

# Checking for idioms in the most viewed episodes
most_viewed_idioms <- lapply(top_20_percent$text, check_idioms, idioms_list = idioms)
# Checking for idioms in the least viewed episodes
least_viewed_idioms <- lapply(bottom_20_percent$text, check_idioms, idioms_list = idioms)
# Checking for idioms in the best rated episodes
best_rated_idioms <- lapply(best_20_percent$text, check_idioms, idioms_list = idioms)
# Checking for idioms in the worst rated episodes
worst_rated_idioms <- lapply(worst_20_percent$text, check_idioms, idioms_list = idioms)

``` 

I created a table of idiom frequencies in the most and least viewed episodes and exported the output in a word document titled idioms_table. I did the same steps for the highest and lowest rated episodes and saved the output in a word document titled idioms_rating_table.
```{r}
# Creating a table of idiom frequencies for most and least viewed episodes
most_viewed_idiom_table <- table(unlist(most_viewed_idioms))
least_viewed_idiom_table <- table(unlist(least_viewed_idioms))

doc <- read_docx()

doc <- doc %>%
  body_add_par("Idioms and Their Frequencies in Top 20% Viewed Episodes:", style = "heading 1") %>%
  body_add_table(as.data.frame(most_viewed_idiom_table), style = "table_template") %>%
  body_add_par("") %>%
  body_add_par("Idioms and Their Frequencies in Bottom 20% Viewed Episodes:", style = "heading 1") %>%
  body_add_table(as.data.frame(least_viewed_idiom_table), style = "table_template")

print(doc, target = "idioms_table.docx")


# Create a table of idiom frequencies for highest and lowest rated episodes
best_rated_idiom_table <- table(unlist(best_rated_idioms))
worst_rated_idiom_table <- table(unlist(worst_rated_idioms))

doc <- read_docx()

doc <- doc %>%
  body_add_par("Idioms and Their Frequencies in Best 20% Rated Episodes:", style = "heading 1") %>%
  body_add_table(as.data.frame(best_rated_idiom_table), style = "table_template") %>%
  body_add_par("") %>%
  body_add_par("Idioms and Their Frequencies in Worst 20% Rated Episodes:", style = "heading 1") %>%
  body_add_table(as.data.frame(worst_rated_idiom_table), style = "table_template")

print(doc, target = "idioms_rating_table.docx")

```


*********** Section 2: language diversity and complexity across thematic contexts***********

**** Sentiment Vocabulary Analysis********

I created a loop function to analyse the vocabulary and language complexity across sentiments. I specified dataframes and created a list to store the results of the function.

```{r}

dataframes <- list(top_20_percent, bottom_20_percent, best_20_percent, worst_20_percent)
emotion_analysis_list <- list()

for (i in seq_along(dataframes)) {
  # Perform the analysis for each dataframe
  emotion_analysis <- dataframes[[i]] %>%
    group_by(emotion) %>%
    summarise(avg_TTR = mean(TTR, na.rm = TRUE),
              avg_MLU = mean(MLU, na.rm = TRUE))
  
    emotion_analysis_list[[i]] <- emotion_analysis
    cat("Analysis for", names(dataframes)[i], ":\n")
  print(emotion_analysis)
  cat("\n")
}

```

I then applied the loop function to the dataframes with their correspending titles and plotted the average TTR and MLU per episode cateogry and saved them in my working directory.
```{r}
dataframes <- list(top_20_percent = top_20_percent, 
                   bottom_20_percent = bottom_20_percent, 
                   best_20_percent = best_20_percent, 
                   worst_20_percent = worst_20_percent)

titles <- c("Emotion analysis for top 20% viewed episodes",
            "Emotion analysis for bottom 20% viewed episodes",
            "Emotion analysis for 20% highest ranked episodes",
            "Emotion analysis for 20% lowest ranked episodes")

for (i in seq_along(dataframes)) {
  # Perform the analysis for each dataframe
  emotion_analysis <- dataframes[[i]] %>%
    group_by(emotion) %>%
    summarise(avg_TTR = mean(TTR, na.rm = TRUE),
              avg_MLU = mean(MLU, na.rm = TRUE))
  
  # Plotting average TTR by emotion
  p1 <- ggplot(emotion_analysis, aes(x = emotion, y = avg_TTR, fill = emotion)) +
    geom_bar(stat = "identity") +
    labs(title = titles[i],
         x = "Emotion",
         y = "Average TTR") +
    theme_minimal()
  
  # Plotting average MLU by emotion
  p2 <- ggplot(emotion_analysis, aes(x = emotion, y = avg_MLU, fill = emotion)) +
    geom_bar(stat = "identity") +
    labs(title = titles[i],
         x = "Emotion", 
         y = "Average MLU") +
    theme_minimal()
  
    ggsave(filename = paste0("plot_", i, "_avg_TTR.png"), plot = p1)
  ggsave(filename = paste0("plot_", i, "_avg_MLU.png"), plot = p2)
}

```

***** Context Dictionary Analysis******

I read the relevant csv files of the New General Service Project wordlists of the most frequent general, spoken, academic, business, and toeic English words. I then created a context-based dictionary after combining and preprocessing the wordlists.

```{r}

general_words <- read.csv("general_words.txt", header = FALSE)
academic_words <- read.csv("academic_words.txt", header = FALSE)
business_words <- read.csv("business_words.txt", header = FALSE)
toeic_words <- read.csv("toeic_words.txt", header = FALSE)
spoken_words <- read.csv("spoken_words.txt", header = FALSE)

# Adding a column to indicate the category
general_words$category <- "general"
academic_words$category <- "academic"
business_words$category <- "business"
toeic_words$category <- "toeic"
spoken_words$category <- "spoken"

# Combining all dataframes into one
dictionary <- bind_rows(general_words, academic_words, business_words, toeic_words, spoken_words)

# Renaming columns
colnames(dictionary) <- c("word", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16","category")

# Subseting the dataframe to keep only "word" and "category" columns
dictionary <- subset(dictionary, select = c("word", "category"))

```

I created a function to perform context category analysis by matching words from the dictionary with the words in the merged dataframe and calculating the percentage frequency of words in each dictionary category. To present the results, I created and printed four seperate tables and created visualizations with correspinding titles.

```{r}
# Function to perform word category analysis and visualization
analyze_word_categories <- function(data, dictionary, title) {
  
  matched_words <- intersect(dictionary$word, data$text)
  word_frequency <- table(dictionary$category[dictionary$word %in% matched_words])
  category_percentage <- prop.table(word_frequency) * 100
    category_table <- data.frame(
    Category = names(category_percentage),
    Percentage = category_percentage
  )
  
  print(category_table)
  
  barplot(category_percentage, 
          main = title,
          xlab = "Category",
          ylab = "Percentage",
          col = rainbow(length(category_percentage)))
}

dataframes <- list(top_20_percent = "Percentage of Words in Each Category in Top 20% viewed episodes",
                   bottom_20_percent = "Percentage of Words in Each Category in Bottom 20% viewed episodes",
                   best_20_percent = "Percentage of Words in Each Category in 20% highest ranked episodes",
                   worst_20_percent = "Percentage of Words in Each Category in 20% lowest ranked episodes")

for (df_name in names(dataframes)) {
  analyze_word_categories(get(df_name), dictionary, dataframes[[df_name]])
}

```
---
title: "R Notebook"
output: html_notebook
---


```{r}
library(dplyr)
library(friends)
library(ggplot2)
library(wordcloud)
library(doParallel)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(caret)
library(glmnet)
library(RColorBrewer)
library(quanteda.textplots)
```
Character Detection



Character Detection
```{r}
dplyr::glimpse(friends)
df_cha <- friends
cha_utter <-  df_cha |> 
  group_by(speaker) |> 
  summarise(num_utterances = n()) |> 
  arrange(desc(num_utterances))
# calculate the percante of six main characters' utterance as a percentage of total utterances
cha_utter <- cha_utter |> 
  mutate(percentage = round(num_utterances / sum(num_utterances) * 100, 1))
cha_utter
cha_utter[1:6, 3] |> sum()
# first six rows of character_counts
topsix <- (head(cha_utter, 6))
topsix
# calculate percentage of utterances for top six
topsix <- topsix |> 
  mutate(percentage = round(num_utterances / sum(num_utterances) * 100, 1))
print(topsix)
cha_utter
cha_utter[, 'percentage']
```
The baseline model which always predictes the most common character will have an accuracy of 18.24
```{r}
# number of utterances: 67373
length(friends$speaker)
# Filter friends only keep top six characters and columns text and character
friends_filtered <- friends |>
  filter(speaker %in% c('Rachel Green', 'Ross Geller', 'Chandler Bing', 'Monica Geller', 'Joey Tribbiani', 'Phoebe Buffay'))
# number of utterances by sixe main characters: 51,047 rows
friends_filtered |> nrow()

# chart for number of utterances
topsix |> 
  ggplot(aes(x = reorder(speaker, -num_utterances), y = num_utterances, fill = speaker)) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of Utterances by Character",
       x = "Character",
       y = "Number of Utterances") +
  theme_minimal()+
  theme(legend.position = "none")

```
Describe the plot in report

```{r}
# Inner join friends_emotions with friends on season, episode, and scene
emotions <- friends_emotions |> 
  inner_join(friends, by = c("season", "episode", "scene", "utterance"))
# keep only text and emotion columns
emotions <- emotions |> 
  select(text, emotion)
# number of rows 12,606
emotions |> nrow()
# percentages of each emotion
emotions_counts <- emotions |> 
  group_by(emotion) |> 
  summarise(num_emotions = n()) |> 
  mutate(percentage = round(num_emotions / sum(num_emotions) * 100, 1)) |> 
  arrange(desc(num_emotions))
print(emotions_counts)
# Plot for number of emotions
emotions_counts |> 
  ggplot(aes(x = reorder(emotion, -num_emotions), y = num_emotions, fill = emotion)) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of Emotions",
       x = "Emotion",
       y = "Number of Emotions") +
  theme_minimal()

```
Imbalanced Dataset

Build a Document-Feature Matrix for character detection and emotion detection
```{r}
cha_texts <- friends_filtered$text
cha_labels <- friends_filtered$speaker
emo_texts <- emotions$text
emo_labels <- emotions$emotion
length(cha_labels)
length(emo_labels)
```

```{r}
friends_filtered <- friends_filtered |> subset(select = c(text, speaker))
```

```{r}
cha_toks <- friends_filtered |> 
  corpus(text_field = 'text') |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE) |>
  tokens_remove(c(stopwords("en"))) |> 
  tokens_wordstem()
```

```{r}
cha_dfm <- cha_toks |> 
  dfm() |> 
  dfm_trim(min_termfreq = 5, min_docfreq = 5) 
cha_dfm_grouped <- cha_dfm |> 
  dfm_group(groups = friends_filtered$speaker)
  
```

```{r}
emotion_dfm <- emotions |> 
  corpus(text_field = 'text') |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE) |>
  tokens_remove(c(stopwords("en"))) |> 
  tokens_wordstem() |> 
  dfm() |> 
  dfm_trim(min_termfreq = 5, min_docfreq = 5)
emotion_dfm_grouped <- emotion_dfm |>
  dfm_group(groups = emotions$emotion)
```
Apply tfidf to all dfms
```{r}
cha_dfm <- dfm_tfidf(cha_dfm)
emotion_dfm <- dfm_tfidf(emotion_dfm)
```

```{r}
topfeatures(cha_dfm[1, ], 10)
print('###################')
topfeatures(emotion_dfm[2, ], 10)
```
```{r}
colors <- brewer.pal(8, "Dark2") 
textplot_wordcloud(cha_dfm_grouped, min_count = 2, max_words = 100, color = colors, random_color = T, comparison = TRUE, labelsize = 0.7, labeloffset = -0.08)
```

```{r}
textplot_wordcloud(emotion_dfm_grouped, min_count = 2, max_words = 100, color = colors, random_color = T, comparison = TRUE, labelsize = 0.7, labeloffset = -0.07)
```

```{r}
 # textplot_wordcloud(emotion_dfm_tfidf, min_count = 2, max_words = 50, color = colors, random_color = T)
```
```{r}
precrecall <- function(mytable, verbose=TRUE) {
    truePositives <- mytable[1,1]
    falsePositives <- sum(mytable[1,]) - truePositives
    falseNegatives <- sum(mytable[,1]) - truePositives
    precision <- truePositives / (truePositives + falsePositives)
    recall <- truePositives / (truePositives + falseNegatives)
    if (verbose) {
        print(mytable)
        cat("\n precision =", round(precision, 2), 
            "\n    recall =", round(recall, 2), "\n")
    }
    invisible(c(precision, recall))
}
```

Naive Bayes Classifier for Character Detection
```{r}
set.seed(24155)
train_indices <- sample(1:nrow(cha_dfm), nrow(cha_dfm)*0.8)
cha_train <- cha_dfm[train_indices,]
cha_test <- cha_dfm[-train_indices,]
cha_train_labels <- friends_filtered$speaker[train_indices]
cha_test_labels <- friends_filtered$speaker[-train_indices]
# train a naive Bayes classifier
classifier <- textmodel_nb(cha_train, cha_train_labels)
# make predictions
cha_test_preds <- predict(classifier, newdata = cha_test)
# compute the confusion matrix
confusion <- table(cha_test_preds, cha_test_labels)
confusionMatrix((confusion), mode="everything")

```

Naive Bayes Classifier for Emotion Detection
```{r}
set.seed(24155)
emotion_train_indices <- sample(1:nrow(emotion_dfm), nrow(emotion_dfm)*0.8)
emotion_train <- emotion_dfm[emotion_train_indices,]
emotion_test <- emotion_dfm[-emotion_train_indices,]
emotion_train_labels <- emotions$emotion[emotion_train_indices]
emotion_test_labels <- emotions$emotion[-emotion_train_indices]
# train a naive Bayes classifier
emotion_classifier <- textmodel_nb(emotion_train, emotion_train_labels)
# make predictions
emotion_test_preds <- predict(emotion_classifier, newdata = emotion_test)
# compute the confusion matrix
confusion <- table(emotion_test_preds, emotion_test_labels)
confusionMatrix((confusion), mode="everything")
```
# Character Detection with lasso
```{r}
cl = makeCluster(16)
registerDoParallel(cl)
weights <- 1 / topsix$percentage
weights <- weights / sum(weights) * length(weights)
print(weights)
final_weights <- sapply(cha_train_labels, function(x) weights[which(topsix$speaker == x)])

# Use these weights in cv.glmnet
cha_lasso <- cv.glmnet(x = as.matrix(cha_train), y = cha_train_labels,
                       alpha = 1, nfolds = 5, family = "multinomial",
                       parallel = TRUE, weights = final_weights)

stopCluster(cl)
```


```{r}
plot(cha_lasso)
```
```{r}
cha_preds_lasso <- predict(cha_lasso, newx = cha_test, s = "lambda.min", type = "class")
cha_table_lasso <- table(cha_preds_lasso, cha_test_labels)
caret::confusionMatrix(cha_table_lasso, mode = "everything")
```


















```{r}
cl = makeCluster(8)
registerDoParallel(cl)
weights <- 1 / emotions_counts$percentage
weights <- weights / sum(weights) * length(weights)
print(weights)
final_weights <- sapply(emotion_train_labels, function(x) weights[which(emotions_counts$emotion == x)])

emotion_lasso <- cv.glmnet(x = as.matrix(emotion_train), y = emotion_train_labels,
                       alpha = 1, nfolds = 5, family = "multinomial",
                       parallel = TRUE, weights = final_weights)

stopCluster(cl)

plot(emotion_lasso)
emotion_preds_lasso <- predict(emotion_lasso, newx = emotion_test, s = "lambda.min", type = "class")
emotion_table_lasso <- table(emotion_preds_lasso, emotion_test_labels)
caret::confusionMatrix(emotion_table_lasso, mode = "everything")



```

