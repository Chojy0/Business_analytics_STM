# -------------------------------------------------------------------------
# 0. Library 호출 ---------------------------------------------------------
# -------------------------------------------------------------------------

if(!require("pacman", character.only = TRUE)) install.packages("pacman")

pacman::p_load(
  stringr, tm, tidytext, widyr, textclean, tidyverse, cleanNLP, tidylo, topicmodels,
  igraph, visNetwork, elbird, ggplot2, tidygraph, here, data.table, topicmodels,
  ggwordcloud, showtext, readr, reticulate, spacyr, stm)

# 글꼴 설정
font_add_google(name = "Song Myung", family = "Song Myung")
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
here()

generateIDs <- function(obj, index = "id") {
  
  # 객체의 종류에 따라 길이를 계산합니다.
  if (obj %>% class() == "data.frame") {
    n <- nrow(x = obj)
  } else {
    n <- NROW(x = obj)
  }
  
  # id를 생성합니다.
  id <- str_c(
    index,
    str_pad(
      string = 1:n,
      width = ceiling(x = log10(x = n)),
      side = "left",
      pad = "0"
    )
  )
  
  # 결과를 반환합니다.
  return(id)
}

# -------------------------------------------------------------------------
# 1. 데이터 가져오기 ------------------------------------------------------
# -------------------------------------------------------------------------

# 1) 파일 불러오기: 예시는 RDS 포맷 파일 활용
MGZ_kor <- readRDS('data/sample_kor.RDS')

# 2) 연도별 문서량 확인하기 

# 연도별 개수 확인
MGZ_kor %>% group_by(Year) %>% summarise(n())

# 연도별 개수 그래프 확인
MGZ_kor %>% select(Year) %>% count(Year, sort = T) %>% 
  ggplot(aes(x=Year, y=n))+geom_line(col='Tomato')+geom_point(col="Tomato")+
  geom_text(aes(y= n+5, label = n))+scale_x_continuous()+
  labs(title = "Number of Document count", x="Year",y= "Number of document")+theme_bw()

# 합치기 위한 id 부여 
MGZ_kor$id <- generateIDs(as.data.frame(MGZ_kor), index = "doc")


# -------------------------------------------------------------------------
# 2. 데이터 준비하기 ------------------------------------------------------
# -------------------------------------------------------------------------

# 1. 형태소 분석 ----------------------------------------------------------

# 3) 토큰화 : elbird 사용
pos_content <- MGZ_kor %>%
  unnest_tokens(
    input = contents,
    output = word_pos,
    token = elbird::tokenize_tidy,
    drop = F
  )

# 4) 품사 태그 정리
separate_pos_content <- pos_content %>%
  separate_rows(word_pos, sep = "[+]") %>%           # 품사 태그 분리
  dplyr::filter(str_detect(word_pos, "/nng|/nnp|/nnb|/np|/vv|/va|/vcp|/vcn")) %>% # 특정 품사만 추출
  mutate(word = str_remove(word_pos, "/.*$")) %>%    # 태그 제거
  dplyr::filter(str_count(word) >= 2)            # 2글자 이상 추출

# 2. 데이터 전처리 --------------------------------------------------------------

# word 기준으로 데이터 전처리하고 원문으로 합치기 
tidy_abs <- separate_pos_content %>% dplyr::select(id, Year, word)


## 불용어 제거 ------------------------------------------------------------------

# 1) 단어 통일시키기 
# word에서 발견한 단어 고치기 
tidy_abs <- tidy_abs %>%
  mutate(word = recode(word,
                           '해촉할' = "해촉"
                           
  ))

# 2) 필요 없는 단어 없애기  
tidy_abs %>% filter(!(word %in% c("수석","수석연구"
)) )-> tidy_abs


# 빈도수 기준 상위 20개 단어 복사 붙여넣기 
tidy_abs %>% count(word, sort=TRUE) %>% 
  arrange(-n) %>% head(20) %>% clipr::write_clip()

# tf-idf 기준 상위 단어
tidy_abs %>% count(Year, word, sort=TRUE) %>% 
  bind_tf_idf(term = word, document = Year, n = n) %>%  
  arrange(desc(tf_idf)) %>% clipr::write_clip()

# tf-idf 기준 연도별 상위 7개 단어
tidy_abs %>% count(Year, word, sort=TRUE) %>% 
  bind_tf_idf(term = word, document = Year, n = n) %>% group_by(Year) %>% 
  arrange(desc(tf_idf)) %>% slice(1:7) %>% clipr::write_clip()


# 5. STM Topicmodeling --------------------------------------------------------

# combiend_df 준비

tidy_abs %>% na.omit() %>%
  group_by(id) %>% 
  summarise(Abstract=str_flatten(word, " ")) %>% 
  ungroup() %>% inner_join(tidy_abs %>% select(id, Year), by= 'id') %>% unique() -> combined_df

# stm 형식으로 변환

processed <-
  combined_df %>% stm::textProcessor(
    documents = combined_df$Abstract,
    metadata = .  , stem=FALSE)

summary(processed)

# 없어진 문서 번호 확인
processed$docs.removed %>% length()
combined_df %>% unique() %>% nrow()


combined_df %>% dplyr::filter(!(No %in% processed$docs.removed)) -> combined_df

combined_df %>% nrow()


# 데이터 준비

out <-
  prepDocuments(processed$documents,
                processed$vocab,
                processed$meta,
                lower.thresh = 0)
summary(out)


docs <- out$documents
vocab <- out$vocab
meta <- out$meta



# k값 찾기 : 시간이 오래 걸립니다.
# 아래서 그래프를 그리고, 어떤 K 값을 가진 stm Object를 만들지 확인합니다. 

topicN <- seq(5,50,1)
storage <- searchK(docs, vocab, K = topicN)
plot(storage)

set.seed(123)
storage$results %>% dplyr::select(K, exclus, semcoh) %>%
  dplyr::filter(K %in% c(1:50)) %>%
  unnest(cols = c(K, exclus, semcoh)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K)) +
  geom_text(aes(label=K)) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")+
  theme(legend.position = 'none')+theme_bw()+theme(legend.title=element_blank())


# -------------------------------------------------------------------------
# 3. stminsights용 데이터 만들기 ------------------------------------------
# -------------------------------------------------------------------------


# 1. 원하는 개수만큼 K값을 정해서 stm object와 estimateEffect object를 만든 뒤 RData로 저장합니다. 

# 1) k=12짜리 object
meta_12 <- 
  stm(documents = docs,
      vocab = vocab,
      data = meta,
      K = 12,         
      prevalence = ~ Year, # 출판 연도에 따른 변화량 확인 
      max.em.its = 50,                # 최대 반복계산 회수 
      verbose = F,                    # 반복계산결과 화면출력 여부
      init.type = "Spectral",
      seed = 37 
  )

prep_12 <- estimateEffect(formula = 1:12 ~ Year, 
                       stmobj = meta_12,
                       metadata = out$meta,
                       uncertainty = "Global")

# K = 10짜리 Object
meta_10 <- 
  stm(documents = docs,
      vocab = vocab,
      data = meta,
      K = 12,         
      prevalence = ~ Year, # 출판 연도에 따른 변화량 확인 
      max.em.its = 50,                # 최대 반복계산 회수 
      verbose = F,                    # 반복계산결과 화면출력 여부
      init.type = "Spectral",
      seed = 37 
  )

prep_10 <- estimateEffect(formula = 1:12 ~ Year, 
                       stmobj = meta_10,
                       metadata = out$meta,
                       uncertainty = "Global")

# RData 저장 
save.image('4stminsight.RData')
