# # install.packages("stringr")
# # install.packages("rJava")
# # install.packages("multilinguer")
# # install.packages("remotes")
# # install.packages("readxl")
# # # install.packages("magrittr")
# # install.packages('ggplot2')
# # install.packages("dplyr")
# # install.packages("tm")
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools','tidyverse','ggplot2','dplyr','readr','lubridate','tidytext','topicmodels'), type = "binary")
# install.packages("https://CRAN.R-project.org/package=KoNLP", repos =NULL, type="source", INSTALL_opts =c('--no-lock'))
#remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
#devtools::install_github('haven-jeon/KoNLP')
#download.file(url = "https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar", destfile = paste0(.libPaths()[1], "/KoNLP/Java/scala-library-2.11.8.jar"))

install.packages(c('stringr', 'rJava', 'multilinguer', 'remotes', 'readxl', 'magrittr','ggplot2','dplyr','tm','devtools','tidyverse','tidytext','topicmodels','RcppMeCab',' wordcloud', 'wordcloud2','showtext'), type = "binary")
install.packages("ggthemes")
install.packages("extrafont")
install.packages('lubridate')
# install.packages("N2H4")

install.packages("https://CRAN.R-project.org/package=KoNLP", repos =NULL, type="source", INSTALL_opts =c('--no-lock'))


library(lubridate)
library(stringr)
library(rJava)
library(KoNLP)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readxl)
library(tm)
library(RcppMeCab)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(showtext)
library(RColorBrewer)
library(ggthemes)
library(extrafont)
# library(N2H4)

#KoNLP 확인용 문장
# extractNoun("대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")

useNIADic()

font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

buildDictionary()
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="대로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="고속도로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="추돌사고", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="국회대로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="하위차로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="가지치기", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="1차로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="2차로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="3차로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="오거리", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="삼거리", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="사거리", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="교차로", tag ='ncn'))
buildDictionary(ext_dic='nanumgothic', user_dic = data.frame(term="홍대입구역", tag ='ncn'))





#전처리
# install.packages("rJava")

# install.packages("XLConnect")
# Sys.setenv(JAVA_HOME="D:/00_PROGRAM/java/")
# library(XLConnect)
# options <- (java.parameters = "-Xmx1024m")
# TBS_whole <- loadWorkbook("D:/99_빅데이터 자료/10009013_TBS 교통정보를 통한 교통사고 및 행사로 인한 도로상 영향파악/교통상황 제보 데이터(미디어재단 TBS)2.xlsx")

TBS <-read_xlsx("D:/99_빅데이터 자료/10009013_TBS 교통정보를 통한 교통사고 및 행사로 인한 도로상 영향파악/교통상황 제보 데이터(미디어재단 TBS)3.xlsx")
colnames(TBS) <- c("date","event", "broad", "road","report","contents")
head(TBS)

TBS$contents <-str_replace_all(TBS$contents, pattern="\r", replacement="") %>%
  #str_replace_all(pattern="\t",  replacement="") %>%)
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[\u3000]", replacement=" ") %>%
  str_replace_all(pattern="[  ]{2}", replacement="") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[→]", replacement=" ") %>%
  str_replace_all(pattern="[\u4E00-\u9FD5o]", replacement="")  %>%
  str_replace_all(pattern="  ", replacement=" ")
head(TBS)
############################################################여기 안돌림
# #불용어 삭제
# install.packages("kableExtra")
# library(kableExtra)
# ## data(stop_words) 영어 불용어 사전
# TBS %>% glimpse()
# unique_TBS <- TBS$contents %>% unique
# 
# data(unique_TBS)
# 
# tibble(text = text_v) %>%
#   unnest_tokens(output = word, input = text) %>% 
#   anti_join(unique_TBS) %>% 
#   count(word, sort = TRUE)
#################################################################


#############RcppMeCab 확인############
# test <- "한글 테스트 입니다."
# pos(sentence= test)
# text2 = enc2utf8(test)
# pos(sentence= text2)
# #####################################

#######tidyverse 테스트############
# enc2utf8(test) %>% pos
####################################

TBS %>%
  select(date, contents) %>%
  unnest_tokens(pos, contents, token = SimplePos09) %>%
  group_by(date) %>%
  mutate(pos_order = 1:n()) ->
  pos_res
head(pos_res)

##불용어 제거
#명사
pos_res %>%
  #우선 'filter()'와 'str_detect()' 함수를 활용하여 명사(n)만 추출
  filter(str_detect(pos, "/n")) %>%
  #형태소 정보를 제거(정규표현식: 굉장히 복잡함)
  mutate(pos_done = str_remove(pos, "/.*$")) ->
  n_done
n_done

pos_res %>%
  filter(str_detect(pos, "/p")) %>%
  mutate(pos_done = str_replace_all(pos, "/.*$", "다")) ->
  p_done
p_done

bind_rows(n_done, p_done) %>%
  arrange(pos_order) %>%
  filter(nchar(pos_done) > 1) %>%
  select(date, pos_done) ->
  pos_done

view(pos_done)
# view(pos_res)

#str_replace_all(pattern="\t",  replacement="") %>%)

###########Wordcloud를 위해 정리##############

pos_done$pos_done <-
  str_replace_all(pos_done$pos_done, pattern="\r", replacement="") %>%
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[\u3000]", replacement=" ") %>%
  str_replace_all(pattern="[  ]{2}", replacement="") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[→]", replacement=" ") %>%
  str_replace_all(pattern="[\u4E00-\u9FD5o]", replacement="")  %>%
  str_replace_all(pattern="사고", replacement="") %>%
  str_replace_all(pattern="지점", replacement="")  %>%
  str_replace_all(pattern="부근", replacement="")  %>%
  str_replace_all(pattern="거리", replacement="")  %>%
  str_replace_all(pattern="서울", replacement="")  %>%
  str_replace_all(pattern="지나", replacement="")  %>%
  str_replace_all(pattern="남단", replacement="")  %>%
  str_replace_all(pattern="북단", replacement="")  %>%
  str_replace_all(pattern="관련", replacement="")  %>%
  str_replace_all(pattern="하다", replacement="")  %>%
  str_replace_all(pattern="관련", replacement="")  %>%
  str_replace_all(pattern="1차", replacement="")  %>%
  str_replace_all(pattern="2차", replacement="")  %>%
  str_replace_all(pattern="3차", replacement="")  %>%
  str_replace_all(pattern="4차", replacement="")  %>%
  str_replace_all(pattern="있다", replacement="")  %>%
  str_replace_all(pattern="4km", replacement="")  %>% 
  str_replace_all(pattern="걷다", replacement="")  %>%
  str_replace_all(pattern="걸리다", replacement="")  %>%
  str_replace_all(pattern="여러", replacement="")  %>%
  str_replace_all(pattern="구간", replacement="")  %>%
  str_replace_all(pattern="600m", replacement="")  %>%
  str_replace_all(pattern="800m", replacement="")  %>%
  str_replace_all(pattern="6차", replacement="")  %>%
  str_replace_all(pattern="7차", replacement="")  %>%
  str_replace_all(pattern="8차", replacement="")  %>%
  str_replace_all(pattern="300m", replacement="")  %>%
  str_replace_all(pattern="1km", replacement="")  %>%
  str_replace_all(pattern="100m", replacement="")  %>%
  str_replace_all(pattern="500m", replacement="")  %>%
  str_replace_all(pattern="200m", replacement="")  %>%
  str_replace_all(pattern="방면", replacement="")  %>%
  str_replace_all(pattern="5차", replacement="")  %>%
  str_replace_all(pattern="2km", replacement="")  %>%
  str_replace_all(pattern="5km", replacement="")  %>%
  str_replace_all(pattern="넘다", replacement="")  %>%
  str_replace_all(pattern="많다", replacement="")  %>%
  str_replace_all(pattern="참다", replacement="")  %>%
  str_replace_all(pattern="가다", replacement="")  %>%
  str_replace_all(pattern="하시", replacement="")  %>%
  str_replace_all(pattern="도로이용", replacement="")  %>%
  str_replace_all(pattern="막다", replacement="")  %>%
  str_replace_all(pattern="사다", replacement="")  %>%
  str_replace_all(pattern="서다", replacement="")  %>%
  str_replace_all(pattern="150m", replacement="")  %>%
  str_replace_all(pattern="400m", replacement="")  %>%
  str_replace_all(pattern="3거리", replacement="삼거리")  %>%
  str_replace_all(pattern="4거리", replacement="사거리")  %>%
  str_replace_all(pattern="5거리", replacement="오거리")  %>%
  str_replace_all(pattern="어렵다", replacement="")  %>%
  str_replace_all(pattern="로에", replacement="")  %>%
  str_replace_all(pattern="소통원", replacement="")  %>%
  str_replace_all(pattern="완료", replacement="")  %>%
  str_replace_all(pattern="오늘", replacement="")  %>%
  str_replace_all(pattern="바라다", replacement="")  %>%
  str_replace_all(pattern="운행", replacement="")  %>%
  str_replace_all(pattern="공항하남", replacement="공항 하남")  %>%  ##한단어 취급 해제
  str_replace_all(pattern="성수대교의정부", replacement="성수대교 의정부")  %>%  ##한단어 취급 해제
  str_replace_all(pattern="판교일산", replacement="판교 일산")  %>%  ##한단어 취급 해제
  str_replace_all(pattern="구리양평", replacement="구리 양평")  %>%  ##한단어 취급 해제
  str_replace_all(pattern="처리", replacement="")  


pos_done$date <- as.POSIXct(pos_done$date)



# pos_done$date_lt <- as.POSIXlt(pos_done$date, orders = '%Y-%m-%d %H:%M:%S')
# year(pos_done$date_lt)


pos_dateerase <- pos_done[,-1]

df_word<-filter(pos_dateerase, nchar(pos_done)>=2)

TBS_list <- df_word

# TBS_character <- as.character(TBS_list)
# mode(TBS_character)
# TBS_character <- gsub("\"\",", "", TBS_character)
# TBS_character <- gsub("\"다\",", "", TBS_character)
# TBS_character <- gsub("[[:punct:]]", "", TBS_character)
# TBS_character <- gsub("\n","", TBS_character)
# TBS_character <- as.character(TBS_character)

mode(pos_dateerase)
mode(TBS_list)
mode(TBS_character)

filter_df_word <- 
  
  write.csv(df_word, file = "D:/$$$$연택 개인용$$$$/df_word.csv")

df_word3 <-df_word %>%
  count(pos_done, sort =T) ->
  wn2
wn2

unlist_nouns <- unlist(wn2)
view(unlist_nouns)
########wordcloud########

buildDictionary(ext_dic='woorimalsam')
showtext_auto()
# pal<- brewer.pal(6, "Dark2")[1:8]
# set.seed(1234)

# png(filename="wordcloud.png", width=2000, height=2000)
# dev.new(width=2000,height=2000, unit="px")
# 
# wc <-wordcloud(words = wn2$pos_done,
#           freq = wn2$n,
#           min.freq = 3, 
#           max.words = 200,
#           random.order =F,
#           rot.per = .1,
#           scale =c(2, 0.4),
#           colors = pal)
################################################################################
windows(width=10, height=10, rescale="fit", title="fixed")
pal <- brewer.pal(12,"Paired")
windowsFonts(malgun=windowsFont("맑은고딕"))
wordcloud(wn2$pos_done, wn2$n, scale =c(4,0.5), min.freq=30, random.order = F,
          rot.per = .1, colors =pal, family = "malgun")


# wc3 <- wordcloud2(wn3, size = 10, minSize = 5, gridSize = 1, fontFamily = "malgun", fontWeight = "bold", color = 'random-light', backgroundColor = "white", minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE)

wc3 <- wordcloud2(wn2, size = 1.5, minSize = 1, gridSize = 1, fontFamily = "malgun", fontWeight = "bold", color = 'random-light')

wc3



# # install webshot
# library(webshot)
# webshot::install_phantomjs()
# # html형식으로 temp.html 파일 저장하기
# library("htmlwidgets")
# saveWidget(my_graph,"tmp.html",selfcontained = F)
# #temp.html 파일 → fig_1.pdf로 변환하기
# webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480) 
# ############ ############ ############ ############ ############ ############
# setwd('D:/$$$$연택 개인용$$$$/01_Lecture Note_/04_도시교통빅데이터응용_변지혜/LDA_R')
# png("test.png", width=1200, height = 1000)
# dev.off



fonts()
dev.new(width=2000,height=2000, unit="px")
wn2 %>%
  with(wordcloud(pos_done, n, family = "woorimalsam"), colors(pal))


################################################################
####################예제#######################################
# install.packages("N2H4")
# library(N2H4)
# getAllComment("https://news.naver.com/main/ranking/read.nhn?mode=LSD&mid=shm&sid1=001&oid=437&aid=0000261701&rankingType=RANKING") %>%
#   select(userName, contents) -> tar
# 
# tar %>%
#   filter(str_detect(contents, "백신"))
# 
# tar %>%
#   filter(str_length(contents) < 200) %>%
#   unnest_tokens(pos, contents, token=SimplePos09) %>%
#    #2. 그 중 명사만 남기고, 형태소 정보는 지워주세요.
#   filter(str_detect(pos, "/n")) %>%
#   mutate(pos_done = str_remove(pos, "/.*$")) %>%
#   #한글자 명사도 지워주세요
#   filter(str_length(pos_done) > 1) %>%
#    #3. 단어출현빈도 계산
#   count(pos_done, sort=T) %>%
#   with(
#     wordcloud(pos_done, n)
#       )
# 

################################################################
# enc2utf8(unique_TBS) %>%       #일단 마지막으로미뤄둠 

# 
# TBS_character %>% 
#   pos(format = "data.frame") %>% 
#   as_tibble() %>% 
#   select(token, pos) %>% 
#   count(token, sort = TRUE) %>% 
#   filter(str_length(token) > 1) %>% 
#   slice_max(n, n = 20) %>% 
#   mutate(token = reorder(token, n)) %>% 
#   ggplot(aes(token, n)) + 
#   geom_col() +
#   coord_flip()
#####df_word2<- as.character(df_word2)#####필요없는듯?

df_word3 <- rename(wn2, word=pos_done, Freq=n)
df_word3<-filter(df_word3, nchar(word)>=2)

top20<- df_word3 %>%
  arrange(desc(Freq)) %>%
  head(20)
head(top20)
top20

dev.off()

dev.new(width=3000,height=3000, unit="px")

ggplot(data = top20, aes(x=word, y=Freq, fill=word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Freq), vjust=-0.3)


###승용차랑 승용차사고랑 합치고 300m 없애고 화물차도 합치고 해야함 
#################################################################
#############Frequency About Time table#########################
#################################################################



time_cycle <-pos_done
time_cycle$pos_done <-
  str_replace_all(time_cycle$pos_done, pattern="\r", replacement="") %>%
  #str_replace_all(pattern="\t",  replacement="") %>%)
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[\u3000]", replacement=" ") %>%
  str_replace_all(pattern="[  ]{2}", replacement="") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[→]", replacement=" ") %>%
  str_replace_all(pattern="[\u4E00-\u9FD5o]", replacement="")  %>%
  str_replace_all(pattern="사고", replacement="") %>%
  str_replace_all(pattern="지점", replacement="")  %>%
  str_replace_all(pattern="부근", replacement="")  %>%
  str_replace_all(pattern="거리", replacement="")  %>%
  str_replace_all(pattern="서울", replacement="")  %>%
  str_replace_all(pattern="지나", replacement="")  %>%
  str_replace_all(pattern="남단", replacement="")  %>%
  str_replace_all(pattern="북단", replacement="")  %>%
  str_replace_all(pattern="관련", replacement="")  %>%
  str_replace_all(pattern="하다", replacement="")  %>%
  str_replace_all(pattern="관련", replacement="")  %>%
  str_replace_all(pattern="1차", replacement="")  %>%
  str_replace_all(pattern="2차", replacement="")  %>%
  str_replace_all(pattern="3차", replacement="")  %>%
  str_replace_all(pattern="4차", replacement="")  %>%
  str_replace_all(pattern="있다", replacement="")  %>%
  str_replace_all(pattern="4km", replacement="")  %>% 
  str_replace_all(pattern="걷다", replacement="")  %>%
  str_replace_all(pattern="걸리다", replacement="")  %>%
  str_replace_all(pattern="여러", replacement="")  %>%
  str_replace_all(pattern="구간", replacement="")  %>%
  str_replace_all(pattern="600m", replacement="")  %>%
  str_replace_all(pattern="800m", replacement="")  %>%
  str_replace_all(pattern="6차", replacement="")  %>%
  str_replace_all(pattern="7차", replacement="")  %>%
  str_replace_all(pattern="8차", replacement="")  %>%
  str_replace_all(pattern="300m", replacement="")  %>%
  str_replace_all(pattern="1km", replacement="")  %>%
  str_replace_all(pattern="100m", replacement="")  %>%
  str_replace_all(pattern="500m", replacement="")  %>%
  str_replace_all(pattern="200m", replacement="")  %>%
  str_replace_all(pattern="방면", replacement="")  %>%
  str_replace_all(pattern="5차", replacement="")  %>%
  str_replace_all(pattern="2km", replacement="")  %>%
  str_replace_all(pattern="5km", replacement="")  %>%
  str_replace_all(pattern="넘다", replacement="")  %>%
  str_replace_all(pattern="많다", replacement="")  %>%
  str_replace_all(pattern="참다", replacement="")  %>%
  str_replace_all(pattern="하시", replacement="")  %>%
  str_replace_all(pattern="도로", replacement="")  %>%
  str_replace_all(pattern="도로이용", replacement="")  %>%
  str_replace_all(pattern="막다", replacement="")  %>%
  str_replace_all(pattern="사다", replacement="")  %>%
  str_replace_all(pattern="서다", replacement="")  %>%
  str_replace_all(pattern="150m", replacement="")  %>%
  str_replace_all(pattern="400m", replacement="")  %>%
  str_replace_all(pattern="어렵다", replacement="")  %>%
  str_replace_all(pattern="로에", replacement="")  %>%
  str_replace_all(pattern="완료", replacement="")  %>%
  str_replace_all(pattern="오늘", replacement="")  %>%
  str_replace_all(pattern="운행", replacement="")  %>%
  str_replace_all(pattern="처리", replacement="")  


time_cycle$date <- as.POSIXct(time_cycle$date)
view(time_cycle)

####시계열분석 분석 좀따
# year1 <- year(time_cycle$date)
# year1 <- as.character(year1)
# time_cycle <- cbind(time_cycle, year = year1)
# TBS_2022 <- subset(time_cycle, date >= "2022-01-01" & date <= "2023-01-01")
# TBS_2021 <- subset(time_cycle, date >= "2021-01-01" & date < "2022-01-01")
# TBS_2020 <- subset(time_cycle, date >= "2020-01-01" & date < "2021-01-01")
# 
# Time_pos_done <- pos_done
# Time_pos_done$date <- as.Date(Time_pos_done$date)
# 
# Time_cycle_done <- time_cycle
# Time_cycle_done <- as.Date(Time_cycle_done$date)

dev.new(width=500,height=500, unit="px")



dev.off()
windows(width=10, height=10, rescale="fit", title="fixed")
Time_frequency <- Time_cycle_done %>%  #Time_cycle-> Time_cycle_done
  ggplot(aes(Time_cycle_done$date)) +   #time_cycle ->  Time_cycle_done
  geom_freqpoly( alpha = 1 , color="blue") + #751680 12월까지 365day*24시간*60분
  scale_colour_manual(values=c("blue")) +
  labs(title = "Frequency by time") +
  xlab("date") +
  scale_y_continuous("Frequency") +
  scale_x_continuous(limits = "2020-01-01","2022-10-31") +
  scale_colour_manual("", breaks =c("TempMin"),  values = c("blue")) 
# theme(axis.title.x = element_text(size=1000, face='bold')) +
# theme(axis.title.y = element_text(size=1000, face='bold')) +
# theme(axis.text.x = element_text(size=1000)) +
# theme(axis.title.y = element_text(size=1000))
# 
plot(Time_frequency)
ggsave("Frequency by time1.jpg", plot = Time_frequency, dpi=300)

####시계열분석 분석 좀따
# plot(stl(Time_cycle_done, "periodic"))
# install.packages("TTR")
# install.packages("forecast")
# library(TTR)
# library(forecast)
# 
# 
# Time_series_cycle_done <- ts(Time_cycle_done)
# plot.ts(Time_series_cycle_done)
# auto.arima(Time_cycle_done)
# arima(Time_cycle_done, order =c())



Time_frequency2 <- Time_frequency + ggtitle("Frequency by Time") +
  theme(plot.title = element_text(family = "serif", face="bold", hjust = 0.5, size =50, color =        "darkblue"), axis.text.x = element_text(size=20, face='bold'), axis.text.y = element_text(size=20, face='bold'))  + 
  scale_x_datetime(breaks = 'month', date_labels = '%y년 %B') + 
  labs(fill="2022") 
#######범례 legend 안들어가는 문제 해결중#########
Time_frequency2 <-
  Time_frequency2  +
  theme(legend.title = element_text(face ="bold", size = 13, color ="darkblue")) +
  theme(legend.text = element_text(face = "bold", size = 11, color ="#330066")) +
  theme(legend.key = element_rect(color = "red", fill = "white"), legend.key.size = unit(1,"cm")) 
# scale_x_date(breaks = '4 month', date_labels = '%y년 %B')


ggsave("title_edit.jpg", plot = Time_frequency2, dpi =300)

# #범례 추가
# theme(legend.position = 'right') +
# #범례 타이틀 색,크기,진하게 설정
# theme(legend.title = element_text(color = "black", size = 20, face = "bold"))+
# #범레 텍스트 색,크기,진하게 설정
# theme(legend.text = element_text(color = "black", size = 12, face = "bold"))+
# #범례 색 설정
# scale_color_manual(values=c('red','blue','green'))
# #범례 이름 강제 설정(선택)
# labs(color="legend")


################연도별로 붙여서 비교해보자##############
################################################################


##################################################################
####Leaflet 지도시각화#####

# install.packages(c("leaflet", "raster", "htmltools"), type = "binary")
# library(leaflet)
# library(raster)
# library(htmltools)




########################################################################

YMD_pos_done <- pos_done
YMD_pos_done$date <- strptime(YMD_pos_done$date, format=c("%Y-%m-%d"))
YMD_pos_done$date <- as.POSIXct(YMD_pos_done$date)

YMD_TBS_erase <- TBS_erase
YMD_TBS_erase$date <-strptime(YMD_TBS_erase$date, format=c("%Y-%m-%d"))
YMD_TBS_erase$date <- as.POSIXct(YMD_pos_done$date)


data.wide2 <- spread(YMD_TBS_erase, id.vars = 'date',variable.name = 'contents')

YMD_TBS_erase$mm <-0
temp2 <- YMD_TBS_erase %>%
  spread(key = "mm", value = "contents")

mm3 <- YMD_TBS_erase %>% 
  pivot_wider(names_from = mm, values_from = contents)

mm4 <-mm3[,-1]


wn3 <- t(wn2)













install.packages("reshape2")
library(reshape2)
data.wide <- spread(YMD_pos_done, id.vars = 'date',variable.name = 'pos_done')

YMD_pos_done$mm <-c(0:100)
temp <- YMD_pos_done %>%
  spread(key = "mm", value = "pos_done")

mm <- YMD_pos_done %>% 
  pivot_wider(names_from = mm, values_from = pos_done)

mm2 <-mm[,-1]
wn3 <- t(wn2)
########################################################################



############################################################################
# ##########################################################################
###################연관성분석###############################################
# wordnoun3 <- t(wn2)
# wordnoun4 <- wordnoun3[wordnoun3]
# wordnoun5 <- sort(table(unlist(pos_done)), decreasing =T)
# keyword_TBS <-dimnames(wordnoun4[1:20])
# str(keyword_TBS)


#re 시도
# nouns <- sapply(wn2$pos_done, extractNoun, USE.NAMES = F)
# 
# wordnoun3 <- wn2
# wordnoun4 <- wordnoun3[wordnoun3$pos_done >=100,]
# wordnoun4 <- sort(table(unlist(nouns)), decreasing = T)
# keyword_TBS <-dimnames(wordnoun4[1:20])
# str(keyword_TBS)

install.packages("arules")
library(arules)

unlist_wordcount<- unlist(wordcount)

wordcount <- wn2[1:20]
str(wordcount)

TBS_erase <- TBS[,-c(1:5)]

# noun <- sapply(TBS_erase, extractNoun, USE.NAMES = F)
# noun2 <- sapply(pos_done, extractNoun, USE.NAMES = F)
# noun2 <- pos_done[,-1]

mm3 <- unlist(mm2)
view(mm3)


contents <- c()
for (i in 1 :800) {
  inter <- intersect(mm2[[i]], unlist(unlist_wordcount))
  contents <- rbind(contents, table(inter)[unlist(unlist_wordcount)])
}
colnames(contents) <- unlist(unlist_wordcount)
contents[which(is.na(contents))] <- 0
head(contents)
view(wordcount)
view(unlist_wordcount)


################################################################################
install.packages("arules")
library(arules)
trans <- as.matrix(contents)
rules1 <- apriori(trans, parameter = list(supp = 0.005, conf = 0.3, target = "rules"))
summary(rules1)

rules1.sorted <- sort(rules1, by="lift")
install.packages("arulesViz")
library(arulesViz)
plot(rules1.sorted, method = "scatterplot")



plot(rules1.sorted, method = "graph", control = list(type="items", alpha = 0.8))

plot(rules1.sorted, method = "grouped")

corrplot(cor(contents),
         method = 'square',
         type = "lower", order = "FPC", 
         tl.col = "black", tl.cex = 0.9, sig.level = 0.05, pch.cex = 0.9, insig = "pch")

# corrplot(cor(contents), method="number", diag=F)

corrplot(cor(contents), method="number", order="hclust", addrect=2, diag=F, 
         tl.col = "black", tl.cex = 1.1, sig.level = 0.05, pch.cex = 0.9, insig = "pch")


corrplot.mixed(cor(contents), upper = "ellipse", lower = "number")


#checking it works
a<-1
b<-a+1

