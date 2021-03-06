# Tackling Total Withdrawals with Text Analytics

  At the university I work for, there is an adminstrative process for students that want to withdraw from the university or take a leave of absence; this process requires them to submit an online Total Withdrawal/Leave of Absence (TWLOA) form. It is has been determined that the current process does not adequately inform students of the consequences that could result from this leave, both academically and financially. The goal of this project is to analyze the university’s TWLOA data in order to improve the current process and find a way to assist students more successfully. This project will use exploratory data analysis and text analytics. The exploratory data analysis will be applied to data on students who have utilized the form; it will allow us to gain insight into what demographics of students are leaving, where these students are from, and if they are planning on transferring to a different school. From there text analytics will be applied to the drop-down text options and free-form text data that students submit via the form; this will include LSA, TF-IDF, and sentiment analysis. 
  
## Loading the Required Libraries
  To begin the process in R, we will need to load the required libraries:
```{r libraries, eval=FALSE, message=FALSE, warning=FALSE}
#Loading the required libraries
library(xlsx)
library(tm)
library(readr)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(proxy)
library(dplyr)
library(lsa)
library(LSAfun)
library(stylo)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
```
These libraries will allow for the data import of Excel files, exploratory and text analytics, as well as visual repsentations of the outcomes. As this project progesses, some libraries may be added or removed as needed. These many libraries are a combination of multiple prior projects using the same skills.

## Loading and Reviewing the Data
  The data for this project is approximately 334 KB of masked demographics and text responses saved as an Excel file. This Excel file is available for download in the git repository. Utilizing the **xlsx** library, we can easily import Excel files and review the data:
```{r dataset, message=FALSE, warning=FALSE}
#Load the dataset
twloa_full <- read.xlsx("D:/Users/Christianna Brown/Desktop/MSDS 692/Data Files/TWLOA All - Masked.xlsx", 1, header = T) #This imports the data into R
twloa_full <- twloa_full[, -c(12:25)] #This removes unnecessary, hidden rows
attach(twloa_full) #This allows the user to call specific rows easily
```
```{r head, echo=FALSE, message=FALSE, warning=FALSE}
#Data overview
head(twloa_full)
```
```{r str, echo=FALSE, message=FALSE, warning=FALSE}
str(twloa_full)
```
From this we can see that this dataset contains 905 observations across 15 variables. These variables include:

1.  **Reference** - The unique identifier for each student submission
2.  **Status** - The status of the form: Complete/Incomplete (This project only evaluates Complete forms)
3.  **City** - The student's city of residence
4.  **State** - The student's state of residence
5.  **Zip Code** - The student's residential zip code
6.  **Class** - The student's class level (e.g. Freshman, Sophomore, etc.)
7.  **College** - The student's college of enrollment (e.g. Regis College, College of Computer & Information Systems, etc.)
8.  **Period** - The length of the withdrawal/leave period
9.  **Withdrawal** - Whether the student wants to be withdrawn from their current registration
10. **Start** - The start of leave
11. **End** - The end of leave
12. **Text** - The text answers submitted by the student
13. **Duration** - The time the student took to complete the form
14. **Browser** - The web browser used to complete the form
15. **OS** - The operating system used to complete the form

With these different variables we are able to complete both the exploratory and text analytics. The full dataset will be broken down into two smaller sets, one for the EDA and another for the text analytics.

## Exploratory Data Analysis
  The exploratory analysis will review the student demographic and educational information from the submitted forms. First, we will create our EDA dataset and review the pairs data to determine if there are any interesting relationships present:
```{r pairs, fig.align='center', fig.height=10, fig.width=10}
#EDA
twloa_EDA <- twloa_full[, c(1, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15)]
pairs(twloa_EDA)
```
This shows us that certain variables may have some relationships; such as certain class levels may be from certain colleges or the browser used may be dependant on the operating system. However, it appears that most of these items may need to be reviewed individually. 
  We will now review plots of the state and city information to determine where students that leave are from; this will help us to better understand if we are losing more local students or more students from out-of-state:
```{r state, echo=FALSE, fig.align='center', fig.height=7, fig.width=7}
twloa_state <- within(twloa_EDA, State <- factor(State, levels = names(sort(table(State), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_state, !is.na(State)), aes(State, fill=State)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("State") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
This clearly shows that the majority of students leaving are from Colorado, with California students leaving second. This is disheartening to see, since we definitely need our local student population to thrive. Perhaps looking into the city data with provide us with greater insight on where they are from within Colorado:
```{r city, echo=FALSE, fig.align='center', fig.height=7, fig.width=7}
twloa_city <- within(twloa_EDA, City <- factor(City, levels = names(sort(table(City), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_city, !is.na(City)), aes(City, fill=City)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("City") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
The city data provides us with further detail emphasizing that the majority of students leaving are from the Metro-Denver area or a suburb. This is something that could be analyzed deeper and perhaps we could pinpoint what student populations are leaving from specific cities, but this will be reviewed later. 
  We will now review the college and class level of students leaving. It is important for us to understand which colleges within the university are at the greatest risk of losing students:
```{r college, echo=FALSE, fig.align='center', fig.height=7, fig.width=7}
twloa_college <- within(twloa_EDA, College <- factor(College, levels = names(sort(table(College), decreasing = TRUE))))
ggplot(twloa_college, aes(College, fill=College)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("College") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
This shows us that the majority of students who have left and completed this form are from Regis College (RC). This is not particularly surprising at this time, since the form was originally intended for only the traditional undergraduate students. However, now that the form has been opened up to all programs and colleges it would be beneficial to track which students are leaving from which colleges. This may be accomplished by combinging class level and filling with the college data:
```{r level, echo=FALSE, fig.align='center', fig.height=10, fig.width=10}
twloa_class <- within(twloa_EDA, Class <- factor(Class, levels = names(sort(table(Class), decreasing = TRUE)[1:5])))
ggplot(subset(twloa_class, !is.na(Class)), aes(Class, fill=College)) + geom_bar(position="stack") + ylab("Count") + xlab("Class Level") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
This provides us with further insight into which students are leaving from which college. The majority of undergraduates that leave are from Regis College; this is not surprising since, once again, this form was originally intended for that population of student. CCLS has the fewest number of undergraduate withdrawals, but they are losing a large number of graduate students. When examining graduate programs, all colleges are evenly split, with the exception of Regis College, since they only have four, highly competitive graduate programs available. There is in interesting spike in RHCHP withdrawals during the junior year, this could be due to the make-or-break clinicals required during that year. There are definitely some very apparent issues that need to be addressed in both undergraduate and graduate programs in order to prevent future withdrawals. 
  We will now examine whether students have elected to totally withdrawal or simply take a break and whether they want to be withdrawn from current registration:
```{r period, echo=FALSE, fig.height=5, fig.width=5}
twloa_period <- within(twloa_EDA, Period <- factor(Period, levels = names(sort(table(Period), decreasing = TRUE))))
ggplot(twloa_period, aes(Period, fill=Period)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Leave Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
```{r withdrawal, echo=FALSE, fig.height=5, fig.width=5}
twloa_wd <- within(twloa_EDA, Withdrawal <- factor(Withdrawal, levels = names(sort(table(Withdrawal), decreasing = TRUE))))
ggplot(twloa_wd, aes(Withdrawal, fill=Withdrawal)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Current Withdrawal") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
This shows that most students submitting this form have determined they will not be returning to Regis and they want to be withdrawn immediately. This indicates that when students are unhappy, they are extremely unhappy and are completely done with the university.
  The final bit of EDA is on technical items; including the duration, browser, and operating system. This will examine how long it takes students to complete the form and their method of submission:
```{r duration, echo=FALSE, fig.height=3, fig.width=3}
twloa_time <- within(twloa_EDA, Duration <- factor(Duration, levels = names(sort(table(Duration), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_time, !is.na(Duration)), aes(Duration, fill=Duration)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Completion Time (in seconds)") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
```{r browser, echo=FALSE, fig.height=3, fig.width=3}
twloa_browser <- within(twloa_EDA, Browser <- factor(Browser, levels = names(sort(table(Browser), decreasing = TRUE))))
ggplot(twloa_browser, aes(Browser, fill=Browser)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Web Browser") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
```{r os, echo=FALSE, fig.height=3, fig.width=3}
twloa_os <- within(twloa_EDA, OS <- factor(OS, levels = names(sort(table(OS), decreasing = TRUE))))
ggplot(twloa_os, aes(OS, fill=OS)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("OS Used") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```
The average student takes between three and five minutes to complete the form and the majority of them use Chrome and on a Windows machine. This further emphsizes the fact that when these students complete the form, they are really ready to be done with Regis and do not need to take much time to complete the form.

## Text Analytics
  The text analytics portion will utilize TF-IDF, LSA, and Cosine Similarity. It will also review the sentiment of the words used within the form. We will start by creating the text analytics dataset and creating the corpus that will be used for the TF-IDF, LSA, and Cosine Similarity:
```{r corpus, eval=FALSE, message=FALSE, warning=FALSE}
#Text analytics
twloa_text <- twloa_full[, c(1, 12)]
twloa_corpus <- VCorpus(VectorSource(twloa_text$Text))
twloa_corpus

twloa_corpus <- tm_map(twloa_corpus, removePunctuation)
twloa_corpus <- tm_map(twloa_corpus, removeNumbers)
twloa_corpus <- tm_map(twloa_corpus, content_transformer(tolower))
twloa_corpus <- tm_map(twloa_corpus, removeWords, stopwords("english"))
twloa_corpus <- tm_map(twloa_corpus, removeWords, c("regis", "need", "change", "person", "time", "direct", "longer", "fit", "feel", "student", "didnt", "felt"))
twloa_corpus <- tm_map(twloa_corpus, stripWhitespace)
twloa_corpus <- tm_map(twloa_corpus, stemDocument)
```
Once the corpus is created we can review the most frequent terms used:
```{r freq}
#Inspect words used at least 100 times and at least 300 times
#Creating a Document-Term Matrix
twloa_dtm <- DocumentTermMatrix(twloa_corpus)
inspect(twloa_dtm)

#Finding terms used at least 100 times and 300 times
findFreqTerms(twloa_dtm, 100)
findFreqTerms(twloa_dtm, 300)

#Creating a frequency table for the wordclouds
twloa_freq <- colSums(as.matrix(twloa_dtm))
ord <- order(twloa_freq, decreasing = T)

#Listing the most and least frequent terms
twloa_freq[head(ord)]
twloa_freq[tail(ord)]
```
This shows that there are 17757 non-sparse terms within the documents, which is a 99% sparsity rate. The most frequently used terms are *work*, *person*, *need*, and *financi*. We can now review the plots for words used at least 100 times and at least 300 times:
```{r 100}
#Plotting the words used at least 100 and 300 times
twloa_wf <- data.frame(word = names(twloa_freq), freq = twloa_freq)
head(twloa_wf)
```
```{r 100plot, echo=FALSE, fig.align='center', fig.height=7, fig.width=7}
p <- ggplot(subset(twloa_wf, freq>100), aes(x = reorder(word, -freq), y = freq)) + xlab("Frequently Used Words") + ylab("Frequency") + geom_bar(stat = "identity", color = "darkmagenta", fill = "darkmagenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
```
```{r 300, echo=FALSE, fig.align='center', fig.height=7, fig.width=7}
p <- ggplot(subset(twloa_wf, freq>300), aes(x = reorder(word, -freq), y = freq)) + xlab("Frequently Used Words") + ylab("Frequency") + geom_bar(stat = "identity", color = "darkslategray2", fill = "darkslategray2") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
```
There are 40 words that are used at least 100 times; these including *work*, *health*, *program*, and *conflict*. There are only six words that are used at least 300 times; one of the most troubling for any university is *transfer*, because no institution wants to lose students to another institution. This can allso be reviewed using word clouds:
```{r 100word, echo=FALSE, fig.height=5, fig.width=5}
#Word Clouds
#Plotting words that occur at least 100 times
set.seed(1234)
wordcloud(names(twloa_freq), twloa_freq, min.freq = 100, scale = c(5, 0.1), colors = brewer.pal(9, "Set1"))
```
```{r 300word, echo=FALSE, fig.height=5, fig.width=5}
#Plotting words that occur at least 300 times
set.seed(1234)
wordcloud(names(twloa_freq), twloa_freq, min.freq = 300, scale = c(5, 0.1), colors = brewer.pal(10, "Paired"))
```
This clearly shows that when I student leaves the university, their main focus is to work. This could be due to persoanl reasons, financial reason, or institutional unhappiness. 
  We can now review specific terms with TF-IDF and LSA word associations and the Cosine Similarity of two frequent terms. The first term will be *financi*:
```{r financi}
#Applying TF-IDF, LSA, and cosine distance to specific terms in the dataset
#Reviewing the term 'financi'
#TF-IDF
twloa_dtm <- DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F)))
findAssocs(twloa_dtm, "financi", 0.5)
inspect(DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F), dictionary = c("financi"))))

#LSA
twloa_tdm <- TermDocumentMatrix(twloa_corpus)
twloa_lsa <- lsa(twloa_tdm, dims = dimcalc_share())
twloa_matrix <- as.textmatrix(twloa_lsa)
as.textmatrix(twloa_lsa)
associate(twloa_matrix, "financi", threshold = 0.5)
```
Both TF-IDF and LSA produce the same word associations, but their association percentages differ. The most highly associated term is *aid*, this makes sense since many students rely on financial aid to get the education and with Regis being an expensive institution, it would make it difficult to complete without enough financial aid. Now we will review the associations between the term *health*:
```{r health}
#Reviewing the term 'health'
#TF-IDF
twloa_dtm <- DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F)))
findAssocs(twloa_dtm, "health", 0.5)
inspect(DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F), dictionary = c("health"))))

#LSA
twloa_tdm <- TermDocumentMatrix(twloa_corpus)
twloa_lsa <- lsa(twloa_tdm, dims = dimcalc_share())
twloa_matrix <- as.textmatrix(twloa_lsa)
as.textmatrix(twloa_lsa)
associate(twloa_matrix, "health", threshold = 0.5)
```
The TF-IDF analysis shows only three associations, while the LSA shows five; once again the LSA analysis appears to be more thorough and shows higher rates of association. The main *health* associated term is *medicalhealth*, which implies that students leave the university due to physical health issues. This is heartbreaking and I wonder if there is something we could do to help assist them through this process. Finally we can review the Cosine Similarity between *academ* and *financi*:
```{r cosine}
#Looking at how 'academ' and 'financi' are related with Cosine
Cosine("academ", "financi", tvectors = twloa_matrix)
```
These results are rather surprising, since I expected there to be a closer relationship between the two. However, since this is a form that students submit on their own, there is still a possibility that the two are related. It would be interesting to dive deeper and determine if student who leave the university for financial reasons are also experience academic hardship.
  The last bit of text analytics utilizes bing and nrc sentiment analysis. We will be able to interpret the sentiment behind the words student are using on the form. We will first review the bing sentiment:
```{r bing, message=FALSE, warning=FALSE}
#Sentiment analysis
twloa_txt <- twloa_full[, c(12:12)]
twloa_txt <- trimws(twloa_txt)
twloa_txt <- gsub("\\$", "",twloa_txt)
twloa_tokens <- data_frame(text = twloa_txt) %>% unnest_tokens(word, text)

bing_sent <- twloa_tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
bing_sent
```
Within the dataset there are 1853 negative words and 2265 positive words. We can now review a plot of the bing sentiments:
```{r bingplot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', fig.height=7, fig.width=7}
bing_word_counts <- twloa_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment", x = NULL) +
          coord_flip()
```
Looking at these two bar charts, the negative words clearly fit, however, I believe their may be some false-positives present. Leaving school to go *work* without completing your degree does not sound positive. Being that *sufficient* was also highly associated with *financi*, this is actual a negative term since many students indicate that their aid package is not sufficient enough for them to complete their program. Let's review the nrc sentiments to see if the same false-positives are present:
```{r nrc, message=FALSE, warning=FALSE}
nrc_sent <- twloa_tokens %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)
nrc_sent
```
Here we can see there are 4992 positive terms, 4457 negative, and 3203 trust terms. Looking at the plot could further enhance our understanding of the terms used:
```{r nrcplot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', fig.height=10, fig.width=10}
nrc_word_counts <- twloa_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment", x = NULL) +
          coord_flip()
```
It appears once again there are terms that are being placed in false categories. The term *personal* is in the trust category, which is pink, which implies a postive sense of trust, but in all actuality, many students state personal reasons for leaving, without elaborating, which shows a lack of trust in the university. Similarly, with *academic*, when I studet leaves for academic reasons this is rarely a positive thing; it could either mean they are leaving because they are unhappy with the academics or because the university was unhappy with their academics. In the future, it would be helpful to perform sentiment analysis on *n*-grams from the forms in order to greater understand the full sentiment of the statements being made.

## Conclusion
  Working on this project has been extremely insightful and has aided me in gaining a greater understanding of the TWLOA process for students. We now understand which students are leaving which colleges, where these students are from, and what words they are using. Ultimately, this project is barely scratching the surface of the TWLOA analysis possible and I am looking forward to diving deeper in. It would be extremely helpful to understand in more detail about the students and find ways to relate the text analytics to the EDA. In regards to the text analytics, it is clear that certain terms are generating false-positives and a more involved sentiment analysis will need to be completed in order to fully understand the sentiments of withdrawn students. I would also like to perform predictive analytics to help prevent student withdrawals in the future, but that is another project for another time.

## References
ilir. (2014, April 15). *Only displaying the top 3 bars in a ggplot2 chart*. Retrieved from https://stackoverflow.com/questions/23095129/only-displaying-top-3-bars-in-a-ggplot2-chart
Machlis, S. (2015, June 17). *My ggplot2 cheat sheet: Searh by task*. Retrieved from https://www.computerworld.com/article/2935394/business-intelligence/my-ggplot2-cheat-sheet-search-by-task.html
rafa.pereira. (2016, April 21). *Eliminating NAs from a ggplot*. Retrieved from https://stackoverflow.com/questions/17216358/eliminating-nas-from-a-ggplot
Tatman, R. (2017, October 5). *Data science 101: Sentiment analysis in R tutorial*. Retrieved from http://blog.kaggle.com/2017/10/05/data-science-101-sentiment-analysis-in-r-tutorial/
Ulrich, J. (2012, April 10). *Extracting specific columns from a data frame*. Retrieved from https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
