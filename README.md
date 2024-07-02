# 📺 Cultural Relevance of *Friends* Sitcom through Text Analysis

Welcome to the *Friends* Sitcom Quantitative Text Analysis project! This repository explores the enduring popularity, cultural impact, and linguistic richness of the iconic TV show *Friends* through quantitative text analysis.

![Friends TV Show](https://upload.wikimedia.org/wikipedia/en/0/0c/Friends_complete_series_dvd.jpg)

## 📜 Table of Contents

1. [Introduction](#introduction)
2. [Distinctiveness of Character Speech and Clarity of Emotional Conveyance](#distinctiveness-of-character-speech-and-clarity-of-emotional-conveyance)
    - [Character Detection](#character-detection)
    - [Emotion Detection](#emotion-detection)
3. [Representation of Women Characters in Friends](#representation-of-women-characters-in-friends)
    - [Data Pre-processing](#data-pre-processing)
    - [Exploratory Analysis of the Characters Relationships](#exploratory-analysis-of-the-characters-relationships)
    - [The Analysis of Female Character](#the-analysis-of-female-character)
4. [Friends as an English Language Learning Resource](#friends-as-an-english-language-learning-resource)
    - [The Diversity and Complexity of Language in Friends](#the-diversity-and-complexity-of-language-in-friends)
    - [Variations in Language Diversity and Complexity Across Thematic Contexts](#variations-in-language-diversity-and-complexity-across-thematic-contexts)
5. [Unveiling Criticism: Analysis of Offensive Content in Friends](#unveiling-criticism-analysis-of-offensive-content-in-friends)
    - [General Analysis of Offensive Content](#general-analysis-of-offensive-content)
    - [Exploring Weight Criticism Towards Monica](#exploring-weight-criticism-towards-monica)
6. [Conclusion](#conclusion)
7. [Appendix](#appendix)

## 🌟 Introduction

The beloved sitcom *Friends*, which aired from 1994 to 2004, features six unforgettable characters: Rachel Green, Ross Geller, Chandler Bing, Monica Geller, Joey Tribbiani, and Phoebe Buffay. Over its 10-season run, viewers around the globe became intimately familiar with each character’s unique personality, quirks, and memorable catchphrases. 

This project investigates how the distinct characters, dynamic language, and sometimes controversial content in *Friends* contribute to its lasting appeal and cultural relevance. Using the R Friends package by EmilHvitfeldt, we delve into the show's scripts to uncover insights into character speech patterns, emotional expressions, and societal impacts.

## 🎭 Distinctiveness of Character Speech and Clarity of Emotional Conveyance

### Character Detection

How distinct are the characters in their way of speaking? We aim to answer this by analyzing the dialogues of the six main characters. Using models like Naive Bayes and fine-tuned transformers, we predict which character is speaking based on their unique speech patterns.

- **Key Findings**: 
  - Joey and Rachel are easier to classify due to their iconic catchphrases.
  - Ross, Chandler, and Monica are harder to identify due to fewer unique linguistic markers.
  - Fine-tuned models provide slightly better accuracy but still face challenges.

### Emotion Detection

We compared four models for emotion classification: Naive Bayes, Multinomial Lasso, BERT-base-uncased, and a fine-tuned RoBERTa model. 

- **Key Findings**: 
  - Transformer-based models significantly outperform simpler models.
  - Emotions like 'Joyful' and 'Neutral' are easier to detect, reflecting the show's comedic nature.
  - 'Peaceful' and 'Powerful' emotions are more challenging to identify.

## 👩‍🦰 Representation of Women Characters in Friends

### Data Pre-processing

We pre-processed the scripts by removing punctuation, tokenizing, converting to lowercase, and focusing on the six main characters.

### Exploratory Analysis of the Characters Relationships

Using network graphs and topic models, we explored the relationships between characters based on their dialogues. Notably, Ross and Joey appear more connected than other characters, revealing interesting dynamics.

### The Analysis of Female Character

Focusing on Rachel Green, we analyzed her character development from a cafe waitress to a fashion professional. The analysis was divided into three periods based on significant career milestones.

- **Key Findings**: 
  - Early reliance on her father and relationships.
  - Transition to professionalism and independence.
  - Emphasis on family life and career achievements.

## 📚 Friends as an English Language Learning Resource

### The Diversity and Complexity of Language in Friends

We evaluated the language diversity and complexity using metrics like Token-to-Type Ratios (TTR), Mean Length of Utterance (MLU), and Idioms Frequency.

- **Key Findings**: 
  - Both highly viewed and least viewed episodes exhibit similar high levels of vocabulary diversity.
  - Least viewed episodes often contain more complex language structures.

### Variations in Language Diversity and Complexity Across Thematic Contexts

We analyzed how language diversity and complexity vary across different thematic contexts. 

- **Key Findings**: 
  - Vocabulary diversity remains consistent across emotions.
  - Language complexity varies, with intense emotions like sadness and anger having higher MLU scores.

## 🚨 Unveiling Criticism: Analysis of Offensive Content in Friends

### General Analysis of Offensive Content

We analyzed homophobic, sexist, and fat-shaming content in the script using self-defined dictionaries, “key words in context,” and sentiment analysis.

- **Key Findings**: 
  - Homophobic content often appears as sarcastic jokes.
  - Male characters use sexist language more frequently.
  - Fat-shaming mostly targets Monica but also affects other characters.

### Exploring Weight Criticism Towards Monica

We focused on fatphobia in *Friends*, examining whether weight criticism can be identified in specific episodes and comparing sentiments related to male and female weight issues.

- **Key Findings**: 
  - Sentiments related to male weight issues are slightly more positive.
  - Monica's weight issues are rarely mentioned explicitly but often in a negative context.

## 🏁 Conclusion

This project revealed that *Friends*' lasting popularity is driven by a mix of distinctive characters, relatable themes, language learning potential, and its reflection of evolving social norms. However, criticisms regarding occasional offensive content highlight the need for a nuanced understanding of its cultural impact.

## 📁 Appendix

Additional tables, figures, and detailed analyses are included in the appendix.

## 🛠️ Tools and Technologies

- **Languages**: R, Python
- **Libraries**: Quanteda, Transformers, Scikit-learn, Pandas, Matplotlib
- **Data Source**: R Friends package by EmilHvitfeldt

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙌 Acknowledgements

We would like to thank the creators of *Friends* and the developers of the R Friends package for making this analysis possible.

---
