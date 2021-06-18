# 原始碼說明文件

### 資料蒐集及前處理
1. 在`load_all_text`這個function中，我們將文本資料以`guternbergr`套件從[Guternber](https://www.gutenberg.org/)上載入，其中有一些文本沒有被包含在套件中，我們會以爬蟲的方式取得資料
2. 藉由`load_all_text`將文本分為美國、英國、德國、法國分別存入`all_GBR_texts`、`all_USA_texts`、`all_GER_texts`、`all_FRA_texts`四個list
3. 在清理Stopwords的時候，我們加入了針對本次文本需要移除的字(`other_stopwords <- c("de", "von", "er", "mo", "hl", "ly", "ter", "zu", "ye", "da", "la", "aide", "thou", "thy")`)，進行清理

### TF-IDF
1. 在function `make_plot`中，我們對文本進行tokenize以及lemmatize的動作，並移除數字和大寫開頭的名字及地名；將清理乾淨的以`bind_tf_idf`函數做出tf-idf的分析，最後以`ggplot`繪製成長條圖
2. 繪製關鍵字數量為5-15的長條圖

### Topic Modeling
在function `topic_modeling`中，將資料轉為dtm的形式，並以`stm`套件做主題標記的分析，並分別作出個主題3-5個關鍵字的長條圖，以及主題字分布的長條圖