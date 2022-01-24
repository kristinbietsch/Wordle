# How to solve Wordle
# Kristin Bietsch, PhD

library(tidyverse)
setwd("C:/Users/KristinBietsch/Desktop")

# https://www.bestwordlist.com/

# Load the vowel list
words_a <- read.csv("wordle a.csv")
words_e <- read.csv("wordle e.csv")
words_i <- read.csv("wordle i.csv")
words_o <- read.csv("wordle o.csv")
words_u <- read.csv("wordle u.csv")
words_y <- read.csv("wordle y.csv")


# Example Word: Trees
# Guess 1: Audio
# Guess 2: Stern

# Grey Letters:
nogo <- c('A', 'U' , 'D' , 'I', '0', 'N') 


words_little_test <- words_e %>% # Pick your starting Vowel
  mutate(l1 = str_sub(words, 1, 1), 
         l2 = str_sub(words, 2, 2), 
         l3 = str_sub(words, 3, 3), 
         l4 = str_sub(words, 4, 4), 
         l5 = str_sub(words, 5, 5)) %>% 
  filter(!l1 %in% nogo & 
           !l2 %in% nogo & 
           !l3 %in% nogo & 
           !l4 %in% nogo & 
           !l5 %in% nogo ) %>%
  filter(( l2=="S" | l3=="S" | l4=="S" | l5=="S"   ) & l1!="S") %>%  # Yellow Letter
  filter(( l1=="T" | l3=="T" | l4=="T" | l5=="T"   ) & l2!="T") %>%  # Yellow Letter
  filter(( l1=="R" | l2=="R" | l3=="R" | l5=="R"   ) & l4!="R") %>%  # Yellow Letter
  filter(l3=="E") # Green Letter


# To find the most common letters left in my list to get rid of
words_long <- words_little_test %>% 
  gather(Position, Letter, l1:l5) %>%
  mutate(n=1) %>%
  group_by(Letter) %>%
  summarise(total=sum(n))


