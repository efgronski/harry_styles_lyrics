# Harry Styles' Lyric Analysis
## Beth Gronski

### **Introduction**

Insert introduction about your analysis and what questions you hope to answer and how you have accomplished answering those questions.

#### *Harry Styles' Debut*
Harry Styles' debuted his first album on May 12, 2017 according to Spotify. This album is the first time Harry Styles is playing music without his previous band, One Direction. According to Apple Music, "Across 10 songs, Harry Styles has the time of his life and emerges from his boy-band chrysalis a rock star."

#### *Fine Line*
**Fine Line** is Harry Styles' sophomore album which was released on December 13, 2019 with 12 songs. Apple Music describes **Fine Line** as, "Identity—more specifically, self-discovery—is at the core of his sophomore album **Fine Line**...Unlike his last album, **Fine Line** practically explodes in color. High-pitched harmonies, buoyant string arrangements, and gently psychedelic melodies evoke an almost dreamlike abandon, and once in a while he goes for broke."

#### *Harry's House*
**Harry's House** is Harry Styles' most recent album, which was freshly released on May 20, 2022. There are 13 songs on the album. Written during the pandemic, **Harry's House** was completed before Harry Styles was able to tour for his **Fine Line** Album. "His upbeat, lightly electronic third LP riffs on the concept of home, viewing it less as a geographical location and more as a state of mind—his mind. 'Imagine it’s a day in my house, a day in my mind,' he said. 'What do I go through? I’m playing fun music. I’m playing sad music. I have doubts. I’m feeling stuff.'" Apple Music writes in the biography of **Harry's House**.

```{r load-packages, message=FALSE, echo=FALSE, warning=FALSE}
library(openintro)
library(tidytext)
library(tidyverse)
library(dplyr)
library("stopwords")
library(ggrepel)
library(plotly)
hs_songs <- read_csv(file="harry_styles_songs.csv")
```

```{r dataset, message=FALSE, echo=FALSE, warning=FALSE}
# Split text into one word per row 
tidy_harry <- hs_songs %>% 
  unnest_tokens(word,lyrics)

# Count words
hs_word<-tidy_harry %>% 
  count(word, sort=TRUE)
## STOPWORDS
#install.packages("stopwords")
library("stopwords")

en_stopwords <- data.frame(word = stopwords(language = "en", source = "snowball"))
View(en_stopwords)

# Remove stopwords
hs_word_count <- tidy_harry %>% 
  anti_join(en_stopwords) %>% 
  count(word, sort=TRUE)

```


### **Harry Styles v. Fine Line**

This is my analysis of the lyrics used in Harry Styles' debut album and album entitled **Fine Line**. The red line down the center of the graph depicts the divide between the two albums, and the dots closest to the line depicts words that the albums have the most in common. In this analysis, the word "home" is used exactly 14 times in both albums. This is a curious discovery especially because Harry's third album, **Harry's House**, is centered around the theme of home. Both albums use the words "know" and "just" many times, but they are used more on **Fine Line**. This could be because Harry's sophomore album has more songs on it. There are also words that are unique to each of the albums, and rarely used on the other. On **Harry Styles**, the word "la" is used many times, but there is most likely debate on whether this word is meaningful to the analysis. Regardless, the debut album also uses "none" and "angel" unlike **Fine Line**. The sophomore album uses "going", "ever", and "oh" which are rarely used on the debut album. 

```{r hsfineline, message=FALSE, echo=FALSE, warning=FALSE}
plot_texts <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Fine Line")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

library(ggrepel)
library(plotly)

my_plotly_plot<-ggplot(data = plot_texts) +
  geom_abline(color = "red") +
  geom_point(aes(x=`Harry Styles`, y=`Fine Line`, label = word)) 

ggplotly(my_plotly_plot)
```

### **Harry Styles v. Harry's House**

This is my analysis of the lyrics used in **Harry Styles** and **Harry's House**. Between these two albums, the word "baby" is used exactly 15 times between both albums. Like the comparison with **Harry Styles** and **Fine Line**, the debut album and **Harry's House** also use the words "know" and "just" most often. They are used more on **Harry's House** then **Harry Styles** for most likely the same reason as before. This discovery leads me to believe these are Harry's most used words in lyrics. After checking words that the albums have in common, I looked for unique words in **Harry's House**. The most unique words are "cinema", "love", and "got". "Got" is used a few times in **Fine Line** but "cinema" is never used and "love" is only used once. The use of "cinema" can be explained by the song with that title. "Cinema" is only used in that song, and is repeated often. However "love" is used throughout several songs on **Harry's House** and I believe can be another theme on the album. 

```{r hsharryhouse, message=FALSE, echo=FALSE, warning=FALSE}
plot_texthouse <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Harry's House")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

my_plotly<-ggplot(data = plot_texthouse) +
  geom_abline(color = "blue") +
  geom_point(aes(x=`Harry Styles`, y=`Harry's House`, label = word)) 

ggplotly(my_plotly)

```

### **Fine Line v. Harry's House**

This is my analysis of the lyrics used in **Fine Line** and **Harry's House**. These albums use the word "sorry" the exact same amount of times, but only 6 each. These albums are more lyrically diverse than my previous analyses. These albums also use the words "know" and "just" very often. **Harry's House** uses "just" more than **Fine Line**, and **Fine Line** uses "know" more. Regardless. each album uses both words at least 29 times or more. It is also interesting to note that **Harry's House** uses the word "home" less than both of the albums before it. 


```{r finelineharryhouse, message=FALSE, echo=FALSE, warning=FALSE}
plot_textline <- tidy_harry %>%
  filter(album %in% c("Fine Line", "Harry's House")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

my_plotly_line<-ggplot(data = plot_textline) +
  geom_abline(color = "orange") +
  geom_point(aes(x=`Fine Line`, y=`Harry's House`, label = word)) 

ggplotly(my_plotly_line)
```


```{r hsunreleased, message=FALSE, echo=FALSE, warning=FALSE, include=FALSE}
plot_text_hs <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Unreleased")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

my_plotly_hs<-ggplot(data = plot_text_hs) +
  geom_abline(color = "purple") +
  geom_point(aes(x=`Harry Styles`, y=`Unreleased`, label = word)) 

ggplotly(my_plotly_hs)

```


```{r finelineunreleased, message=FALSE, echo=FALSE, warning=FALSE, include=FALSE}
plot_text_lineunre <- tidy_harry %>%
  filter(album %in% c("Fine Line", "Unreleased")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

my_plotly_lineunre<-ggplot(data = plot_text_lineunre) +
  geom_abline(color = "green") +
  geom_point(aes(x=`Fine Line`, y=`Unreleased`, label = word)) 

ggplotly(my_plotly_lineunre)

```


```{r harryhouseunreleased, message=FALSE, echo=FALSE, warning=FALSE,include=FALSE}
plot_text_houseunre <- tidy_harry %>%
  filter(album %in% c("Harry's House", "Unreleased")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

my_plotly_houseunre<-ggplot(data = plot_text_houseunre) +
  geom_abline(color = "yellow") +
  geom_point(aes(x=`Harry's House`, y=`Unreleased`, label = word)) 

ggplotly(my_plotly_houseunre)

```

### **Conclusion**

Concluding thoughts on the analysis.

...
