hs_songs<-read.csv("harry_styles_songs.csv")
#install.packages("tidytext")
library(tidytext)
library(tidyverse)
library(dplyr)

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
zh_stopwords <- data.frame(word = stopwords(language = "zh", source = "misc"))
es_stopwords <- data.frame(word = stopwords(language = "es"))

View(en_stopwords)
View(zh_stopwords)
View(es_stopwords)

# Remove stopwords
hs_word_count <- tidy_harry %>% 
  anti_join(en_stopwords) %>% 
  count(word, sort=TRUE)
  
# Create a dataframe for plotting word frequency
# between two speakers

plot_texts <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Fine Line")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

# Plot and compare word use between 2 speakers
#install.packages("ggrepel")
library(ggrepel)
library(plotly)

my_plotly_plot<-ggplot(data = plot_texts) +
  geom_abline(color = "purple") +
  geom_point(aes(x=`Harry Styles`, y=`Fine Line`, label = word)) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(x=`Harry Styles`, y=`Fine Line`, label = word)) 

ggplotly(my_plotly_plot)

# Now pick two new speakers to compare!!!
# Experiment with colors, design, etc.
# What did you find?

plot_texthouse <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Harry's House")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

# Plot and compare word use between 2 speakers
#install.packages("ggrepel")
library(ggrepel)
library(plotly)

my_plotly<-ggplot(data = plot_texthouse) +
  geom_abline(color = "purple") +
  geom_point(aes(x=`Harry Styles`, y=`Harry's House`, label = word)) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(x=`Harry Styles`, y=`Harry's House`, label = word)) 

ggplotly(my_plotly)


plot_textline <- tidy_harry %>%
  filter(album %in% c("Fine Line", "Harry's House")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

# Plot and compare word use between 2 speakers
#install.packages("ggrepel")
library(ggrepel)
library(plotly)

my_plotly_line<-ggplot(data = plot_textline) +
  geom_abline(color = "purple") +
  geom_point(aes(x=`Fine Line`, y=`Harry's House`, label = word)) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(x=`Fine Line`, y=`Harry's House`, label = word)) 

ggplotly(my_plotly_line)


plot_text_hs <- tidy_harry %>%
  filter(album %in% c("Harry Styles", "Unreleased")) %>%
  anti_join(en_stopwords) %>%
  count(album, word) %>%
  group_by(word) %>%
  filter(sum(n) > 5) %>%
  ungroup() %>%
  pivot_wider(names_from = "album", values_from = "n", values_fill = 0)

# Plot and compare word use between 2 speakers
#install.packages("ggrepel")
library(ggrepel)
library(plotly)

my_plotly_hs<-ggplot(data = plot_text_hs) +
  geom_abline(color = "purple") +
  geom_point(aes(x=`Harry Styles`, y=`Unreleased`, label = word)) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(x=`Harry Styles`, y=`Unreleased`, label = word)) 

ggplotly(my_plotly_hs)
