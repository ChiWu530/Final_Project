packages<-c("dplyr", "stringr", "gutenbergr", "httr", "rvest", # first required by "load_all_text"
            "tidytext", # first required by stop words
            "textstem", "ggplot2", # first required by "make_plot"
            "quanteda", "stm" # first required by "topic_modeling"
            ) 

lapply(packages, require, character.only = TRUE)

# set working directory to the folder that contains this "gutenberg 2.0.R", "function source.R", and two subfolders "TF-IDF images" and "Topic model"
setwd("C:/users/User/Desktop/University/2021 Spring/Introduction to programming for data science/Gutenberg")

#### DOWNLOAD EVERY WORK ####
# set bookshelves for each country
GBR_book_title <- list()
USA_book_title <- list()
GER_book_title <- list()
FRA_book_title <- list()
all_book_title <- list()

GBR_book_id<-c(1342, 98, 2814, 829, 29220)
USA_book_id<-c(37106, 64317, 25344, 2701, 76)
GER_book_id<-c(2527, 2500, 5200, 3744, 36028)
FRA_book_id<-c(36098, 2413, 2610, 175, 19942)
all_book_id<-c(1342, 98, 2814, 829, 29220, 37106, 64317, 25344, 2701, 76, 
               2527,2500, 5200, 3744, 36028, 36098, 2413, 2610, 175, 19942)

#### LOAD_ALL_TEXT ####
# loads in texts of all books in a country
load_all_text<-function(list, shelf){
    for (i in list){
        title<-gutenberg_works() %>%
            filter(gutenberg_id == i) %>%
            select(title) %>%
            as.character() %>%
            str_remove_all(";.+") %>%
            str_replace_all(" ", "_")
        # some books are not in the gutenbergr package :(((
        if (title == "character(0)"){
            # using web scraping to download from site
            title_url<-paste0("https://www.gutenberg.org/ebooks/",i)
            url<-paste0("https://www.gutenberg.org/files/",i,"/",i,"-h/",i,"-h.htm")
            # finding title
            title_new<-GET(title_url) %>%
                content() %>%
                html_nodes("#content > h1") %>%
                html_text() %>%
                str_to_title() %>%
                str_remove_all("By .+") %>%
                str_trim() %>%
                str_replace_all(" ", "_")
            # assigning text object according to its title
            assign(title_new, GET(url) %>%
                       content() %>%
                       html_nodes("p") %>%
                       html_text() %>%
                       str_trim() %>%
                       as.data.frame() %>%
                       setNames("text") %>%
                       mutate(book=title_new) %>%
                       select(book, text), envir = .GlobalEnv)
            # adding text object to a book list
            shelf<-append(shelf, title_new)
        } else {
            # assigning text object according to its title
            assign(title, gutenberg_works()%>%
                       filter(gutenberg_id == i)%>%
                       select(gutenberg_id)%>%
                       as.vector()%>%
                       gutenberg_download(strip = TRUE)%>% 
                       mutate(book=title) %>%
                       select(book, text), envir = .GlobalEnv)
            # adding text object to a book list
            shelf<-append(shelf, title)
        }
    }
    return(shelf)
}

# XXX_book_shelf stores book titles from XXX (country)
GBR_book_shelf<-load_all_text(list = GBR_book_id, shelf = GBR_book_title)
USA_book_shelf<-load_all_text(list = USA_book_id, shelf = USA_book_title)
GER_book_shelf<-load_all_text(list = GER_book_id, shelf = GER_book_title)
FRA_book_shelf<-load_all_text(list = FRA_book_id, shelf = FRA_book_title)

# put all texts of the same country into one doc "all_..._texts"
all_GBR_texts<-list()
for (i in unlist(GBR_book_shelf)){
    all_GBR_texts<-rbind(all_GBR_texts, get(i))
}
all_USA_texts<-list()
for (i in unlist(USA_book_shelf)){
    all_USA_texts<-rbind(all_USA_texts, get(i))
}
all_GER_texts<-list()
for (i in unlist(GER_book_shelf)){
    all_GER_texts<-rbind(all_GER_texts, get(i))
}
all_FRA_texts<-list()
for (i in unlist(FRA_book_shelf)){
    all_FRA_texts<-rbind(all_FRA_texts, get(i))
}

# see how many lines in each country's texts
country_texts_str<-c("all_GBR_texts", "all_USA_texts", "all_GER_texts", "all_FRA_texts")
for (i in country_texts_str){print(paste(i, "has", dim(get(i))[1], "lines in it"))}

# manage titles, authors, and ids
titles<-all_book_shelf %>% unlist()
authors<-c("Austen", "Dickens", "Joyce", "Swift", "Woolf",
           "Alcott", "Fitzgerald", "Hawthorne", "Melville", "Twain",
           "Goethe", "Hesse", "Kafka", "Kafka", "Mann",
           "Baudelaire", "Flaubert", "Hugo", "Leroux", "Voltaire")
df<-cbind(titles, authors, all_book_id)%>%as.data.frame()

# remove stop words with anti_join
other_stopwords<-c("de", "von", "er", "mo", "hl", "ly", "ter", "zu", "ye", "da", "la", "aide", "thou", "thy")%>%
    as.vector()%>%
    as.data.frame()
colnames(other_stopwords)<-"word"
stop_words_new<-stop_words %>% 
    select(word) %>% 
    rbind(other_stopwords)

#### BUILD TF-IDF ####
setwd("./TF-IDF images")

# "book_acronym" shortens excess book titles
book_acronym<-c("Pride_and_Prejudice"="Pride and Prejudice",                                      
                "A_Tale_of_Two_Cities"="A Tale of Two Cities",                                       
                "Dubliners"="Dubliners",
                "Gulliver's_Travels_into_Several_Remote_Nations_of_the_World"="Gulliver's Travels",
                "Monday_or_Tuesday"="Monday or Tuesday",                                     
                "Little_Women"="Little Women",                                               
                "The_Great_Gatsby"="The Great Gatsby",
                "The_Scarlet_Letter"="The Scarlet Letter",                                         
                "Moby_Dick;_Or,_The_Whale"="Moby Dick",                                
                "Adventures_of_Huckleberry_Finn"="Huckleberry Finn",                         
                "The_Sorrows_of_Young_Werther"="Young Werther",
                "Siddhartha"="Siddhartha",
                "Metamorphosis"="Metamorphosis",
                "The_Trial"="The Trial",
                "Royal_Highness"="Royal Highness", 
                "The_Flowers_of_Evil"="The Flowers of Evil",
                "Madame_Bovary"="Madame Bovary",
                "Notre-Dame_De_Paris"="Notre-Dame de Paris",
                "The_Phantom_of_the_Opera"="Phantom of Opera",                             
                "Candide"="Candide")

# the function "make_plot" save each country's TF-IDF plot to folder "TF-IDF images"
make_plot<-function(text, topwords){
    # see if "cleaned_books_XXX" exists, if yes, skip this process for higher efficiency
    if (exists(paste0("cleaned_books_", country))==F){
        assign(paste0("cleaned_books_", country), 
               text %>%
                   unnest_tokens(word, text, to_lower=F) %>% # unnest tokens (to_lower=F so for easier name removal)
                   mutate(word = lemmatize_words(word)) %>%
                   anti_join(stop_words_new, by=c("word"="word")) %>% 
                   filter(!grepl(pattern = ".*[A-Z].*", x=word))  %>% # remove capitalized name entities
                   filter(!grepl(pattern = ".*[0-9_’'].*", x=word)), envir = .GlobalEnv) # remove numbers
    }
    
    assign(paste0("tf_idf_", country), 
           get(paste0("cleaned_books_", country)) %>%
               count(book, word, sort = TRUE) %>%
               bind_tf_idf(word, book, n) %>%
               arrange(-tf_idf) %>%
               group_by(book) %>%
               top_n(topwords) %>%
               ungroup %>%
               mutate(word = reorder_within(word, tf_idf, book)), envir = .GlobalEnv)
    
    plot<-get(paste0("tf_idf_", country)) %>%
        ggplot(aes(word, tf_idf, fill = book)) +
        geom_col(alpha = 0.5, show.legend = FALSE) +
        facet_wrap(~ book, scales = "free_y", ncol = 5, 
                   labeller = labeller(book = book_acronym)) +
        scale_x_reordered() +
        coord_flip() +
        theme(plot.title = element_text(size=36), 
              strip.text.x = element_text(size=24, hjust = 0.0), 
              axis.text.y.left = element_text(size=24, hjust = 1.0)) +
        labs(x = NULL, y = "tf-idf",
             title = paste("Highest tf-idf words in", country, "novels"), 
             subtitle = "")
    
    png(paste0("tf-idf_of_", country, "(", topwords, ")1080p.png"), width=1920, height=1080)
    print(plot)
    dev.off()
}

# use "make_plot" in loop for each country
for (i in country_texts_str){
    for (j in (5:15)){
        country<-i%>%str_replace("all_(...)_texts", "\\1") %>% unlist()
        text<-get(i)
        make_plot(text = text, topwords = j)
        Sys.sleep(1)
    }
}

#### TOPIC MODELING BY COUNTRIES ####
# the function "topic_modeling" 
topic_modeling<-function(x){
    # select country code as XXX[3]
    XXX<-deparse(substitute(x))%>%strsplit("_")%>%unlist()

    dfm <- get(paste0("cleaned_books_", XXX[3])) %>%
        count(book, word, sort = TRUE) %>%
        mutate(book=as.character(book))%>%
        cast_dfm(book, word, n)

    sparse <- get(paste0("cleaned_books_", XXX[3])) %>%
        count(book, word, sort = TRUE) %>%
        mutate(book=as.character(book))%>%
        cast_sparse(book, word, n)

    assign(paste0("topic_model_", XXX[3]), stm(dfm, K = 3, verbose = FALSE, init.type = "Spectral"))
    
    td_beta <- tidy(get(paste0("topic_model_", XXX[3])))
    
    plot1<-td_beta %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        mutate(topic = paste0("Topic ", topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = as.factor(topic))) +
        geom_col(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free_y") +
        coord_flip() +
        scale_x_reordered() +
        theme(plot.title = element_text(size=36), 
              plot.subtitle = element_text(size=24), 
              strip.text.x = element_text(size=24, hjust = 0.0), 
              axis.text.y.left = element_text(size=24, hjust = 1.0)) +
        labs(x = NULL, y = expression(beta),
             title = "Highest word probabilities for each topic",
             subtitle = "Different words are associated with different topics")
    
    png(paste0("plot1_", XXX[3], ".png"), width=1920, height=1080)
    print(plot1)
    dev.off()
    
    td_gamma <- tidy(get(paste0("topic_model_", XXX[3])), matrix = "gamma",
                     document_names = rownames(dfm))
    
    plot2<-ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
        geom_histogram(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ topic, ncol = 3) +
        theme(plot.title = element_text(size=36), 
              plot.subtitle = element_text(size=24),
              strip.text.x = element_text(size=24, hjust = 0.0),
              axis.text.y.left = element_text(size=24, hjust = 1.0)) +    
        labs(title = "Distribution of document probabilities for each topic",
             y = "Number of stories", x = expression(gamma))
    
    png(paste0("plot2_", XXX[3], ".png"), width=1920, height=1080)
    print(plot2)
    dev.off()
}

setwd("./../Topic modeling")

topic_modeling(cleaned_books_GBR)
topic_modeling(cleaned_books_USA)
topic_modeling(cleaned_books_GER)
topic_modeling(cleaned_books_FRA)
