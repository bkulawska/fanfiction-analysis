#instalowanie bibliotek
install.packages("tidyverse")
install.packages("skimr")
install.packages("hrbrthemes")
install.packages("dplyr")
install.packages("hexbin")

#importowanie bibliotek
library(tidyverse)
library(skimr)
library(hrbrthemes)
library(dplyr)
library(ggplot2)
library(lattice)

#ró¿ne ustawienia
options(scipen=999)

#wczytywanie danych z pliku
data <- read.csv("C:/Users/Basia/Desktop/projekt R/data.csv")

#zapisywanie danych do pliku
write.csv(data,"C:\\Users\\Basia\\Desktop\\projekt R\\hpffdata.csv", row.names = TRUE)
  
#PUNKT 4 - ANALIZA CECH ILOŒCIOWYCH

#wyznaczenie min, max, 1st Qu, 3rd Qu, median, mean,
#rozstêpu próby, odchylenia standardowego i wariancji
#dla wszystkich cech iloœciowych
summary(data$Chapters)
diff(range(data$Chapters))
sd(data$Chapters)
var(data$Chapters)

summary(data$Words)
diff(range(data$Words))
sd(data$Words)
var(data$Words)

summary(data$Favs)
diff(range(data$Favs))
sd(data$Favs)
var(data$Favs)

#CHAPTERS - HISTOGRAM

chh1 <- data %>%
  ggplot(aes(x=data$Chapters)) +
  geom_histogram(binwidth=1, fill="magenta", alpha=0.6) +
  ggtitle("Number of chapters, bin size = 1")+
  xlab("chapters")+
  ylab("frequency")+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
chh1

#CHAPTERS - WYKRES W SKALI LOGARYTMICZNEJ
chh2 <- data %>%
  ggplot(aes(x=data$Chapters)) +
  geom_histogram(binwidth=1, fill="magenta", alpha=0.6) +
  ggtitle("Number of chapters, bin size = 1, logarithmic scale")+
  xlab("chapters")+
  ylab("frequency")+
  scale_y_log10()+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
chh2

#THE ONE CHAPTER STORIES PERCENTAGE
count(data %>% filter (Chapters==1))/count(data)

#WORDS - HISTOGRAM

wh1 <- data %>%
  ggplot(aes(x=data$Words)) +
  geom_histogram(binwidth=1000, fill="chocolate", alpha=0.6) +
  ggtitle("Number of words, bin size = 1000")+
  xlab("words")+
  ylab("frequency")+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
wh1

#WORDS - WYKRES W SKALI LOGARYTMICZNEJ
wh2 <- data %>%
  ggplot(aes(x=data$Words)) +
  geom_histogram(binwidth=1000, fill="chocolate", alpha=0.6) +
  ggtitle("Number of words, bin size = 1000, logarithmic scale")+
  xlab("words")+
  ylab("frequency")+
  scale_y_log10()+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
wh2

#THE UP-TO-1000-WORDS STORIES PERCENTAGE
count(data %>% filter (Words<=1000))/count(data)

#THE UP-TO-10000-WORDS STORIES PERCENTAGE
count(data %>% filter (Words<=10000))/count(data)

#FAVS - HISTOGRAM 1

fh1 <- data %>%
  ggplot(aes(x=data$Favs)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.6) +
  ggtitle("Number of favourites, bin size = 1")+
  xlab("favs")+
  ylab("frequency")+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
fh1

#FAVS - WYKRES W SKALI LOGARYTMICZNEJ
fh2 <- data %>%
  ggplot(aes(x=data$Favs)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.6) +
  ggtitle("Number of favourites, bin size = 1, logarithmic scale")+
  xlab("favs")+
  ylab("frequency")+
  scale_y_log10()+
  theme_ipsum() +
  theme(text=element_text(size=8), plot.title = element_text(size=11)
  )
fh2

#THE UP-TO-10-FAVS STORIES PERCENTAGE
count(data %>% filter (Favs<=10))/count(data)

#PUNKT 5 - ANALIZA CECH JAKOŒCIOWYCH

#GENRE - BARPLOT
data %>%
  filter(genre!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(genre)), fill=genre)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories in genres")+
  coord_flip()+
  xlab("genre")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#GENRE - PERCENTAGES
count(data %>% filter (genre=="Romance"))/count(data)
count(data %>% filter (genre=="Humor"))/count(data)
count(data %>% filter (genre=="Drama"))/count(data)
count(data %>% filter (genre=="Angst"))/count(data)

#LANGUAGE - BARPLOT
ggplot(data, aes(forcats::fct_rev(fct_infreq(language)), fill=language)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories in languages")+
  coord_flip()+
  xlab("language")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#LANGUAGE - PERCENTAGES
count(data %>% filter (language=="English"))/count(data)
count(data %>% filter (language=="Spanish"))/count(data)
count(data %>% filter (language=="French"))/count(data)
count(data %>% filter (language=="Polish"))/count(data)

#RATING - BARPLOT
ggplot(data, aes(forcats::fct_rev(fct_infreq(rating)), fill=rating)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories in ratings")+
  coord_flip()+
  xlab("rating")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#RATING - PERCENTAGES
count(data %>% filter (rating=="T"))/count(data)
count(data %>% filter (rating=="K+"))/count(data)
count(data %>% filter (rating=="M"))/count(data)
count(data %>% filter (rating=="K"))/count(data)

#PUBLISHED - BARPLOT
ggplot(data, aes(Published, fill=Published)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories published in years")+
  xlab("year")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTER - BARPLOT
data %>%
filter(main_character!="other") %>%
ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories by main character")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTER - PERCENTAGES
count(data %>% filter (main_character=="Harry P."))/count(data)
count(data %>% filter (main_character=="Hermione G."))/count(data)
count(data %>% filter (main_character=="Draco M."))/count(data)

#PAIRING - BARPLOT
data %>%
filter(pairing!="other", pairing!="undefined") %>%
ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("Number of stories by pairing")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PAIRING - PERCENTAGES
count(data %>% filter (pairing=="Draco M., Hermione G."))/count(data)
count(data %>% filter (pairing=="Draco M., Harry P."))/count(data)
count(data %>% filter (pairing=="James P., Lily Evans P."))/count(data)
count(data %>% filter (pairing=="Hermione G., Ron W."))/count(data)
count(data %>% filter (pairing=="Harry P., Ginny W."))/count(data)

#PUNKT 6 - ANALIZA ZALE¯NOŒCI MIÊDZY DANYMI

#FAVS (WORDS)
#DENSITY 2D
data %>%
ggplot(aes(x=Words, y=Favs)) +
  geom_hex() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_distiller(palette=4, direction=1) +
  ggtitle("Favorites and word count correlation")+
  theme_bw()

#CHAPTERS (WORDS)
#DENSITY 2D
data %>%
ggplot(aes(x=Words, y=Chapters)) +
  geom_hex() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  ggtitle("Chapters and word count correlation")+
  theme_bw()

#G£ÓWNI BOHATEROWIE A JÊZYKI TEKSTÓW

#MAIN CHARACTERS - ENGLISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="English"),]
data1$main_character <- fct_lump_n(data1$main_character, 6, w = NULL, other_level = "other")
data1 %>%
  filter(main_character!="other") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("English language: most popular characters")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTERS - SPANISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Spanish"),]
data1$main_character <- fct_lump_n(data1$main_character, 6, w = NULL, other_level = "other")
data1 %>%
  filter(main_character!="other") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("Spanish language: most popular characters")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTERS - GERMAN language - BARPLOT
data1<-data
data1<-data1[(data1$language=="German"),]
data1$main_character <- fct_lump_n(data1$main_character, 6, w = NULL, other_level = "other")
data1 %>%
  filter(main_character!="other") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("German language: most popular characters")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTERS - POLISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Polish"),]
data1$main_character <- fct_lump_n(data1$main_character, 6, w = NULL, other_level = "other")
data1 %>%
  filter(main_character!="other") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("Polish language: most popular characters")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#MAIN CHARACTERS - INDONESIAN language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Indonesian"),]
data1$main_character <- fct_lump_n(data1$main_character, 6, w = NULL, other_level = "other")
data1 %>%
  filter(main_character!="other") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(main_character)), fill=main_character)) +
  geom_bar(stat = "count") +
  ggtitle("Indonesian language: most popular characters")+
  coord_flip()+
  xlab("main character")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PARINGI A JÊZYKI TEKSTÓW

#PAIRINGS - ENGLISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="English"),]
data1$pairing <- fct_lump_n(data1$pairing, 7, w = NULL, other_level = "other")
data1 %>%
  filter(pairing!="other", pairing!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("English language: most popular pairings")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PAIRINGS - SPANISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Spanish"),]
data1$pairing <- fct_lump_n(data1$pairing, 7, w = NULL, other_level = "other")
data1 %>%
  filter(pairing!="other", pairing!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("Spanish language: most popular pairings")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PAIRINGS - GERMAN language - BARPLOT
data1<-data
data1<-data1[(data1$language=="German"),]
data1$pairing <- fct_lump_n(data1$pairing, 7, w = NULL, other_level = "other")
data1 %>%
  filter(pairing!="other", pairing!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("German language: most popular pairings")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PAIRINGS - POLISH language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Polish"),]
data1$pairing <- fct_lump_n(data1$pairing, 7, w = NULL, other_level = "other")
data1 %>%
  filter(pairing!="other", pairing!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("Polish language: most popular pairings")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")

#PAIRINGS - INDONESIAN language - BARPLOT
data1<-data
data1<-data1[(data1$language=="Indonesian"),]
data1$pairing <- fct_lump_n(data1$pairing, 7, w = NULL, other_level = "other")
data1 %>%
  filter(pairing!="other", pairing!="undefined") %>%
  ggplot(aes(forcats::fct_rev(fct_infreq(pairing)), fill=pairing)) +
  geom_bar(stat = "count") +
  ggtitle("Indonesian language: most popular pairings")+
  coord_flip()+
  xlab("pairing")+
  ylab("number of stories")+
  theme_ipsum() +
  theme(legend.position="none")