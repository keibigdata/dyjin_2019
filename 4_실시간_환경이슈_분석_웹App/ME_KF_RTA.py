
# coding: utf-8

# ## 네이버 환경뉴스 실시간 분석

# ### 라이브러리 import

# In[96]:


import re
import gensim 
import mglearn
import numpy as np
import pandas as pd
import os
import matplotlib.pyplot as plt
from konlpy.utils import pprint
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.decomposition import LatentDirichletAllocation
from ckonlpy.tag import Twitter
import pyLDAvis.gensim
import pyLDAvis.sklearn
import collections
from gensim.models.word2vec import Word2Vec
import copy
import soynlp
from soynlp.utils import DoublespaceLineCorpus
from soynlp.noun import LRNounExtractor_v2
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.pyplot as plt2
import matplotlib
import datetime
from textrankr import TextRank
pd.options.display.float_format = '{:.3f}'.format
plt.rc('font', family='NanumBarunGothicOTF')


# ## 인덱스 파일 로드

# In[92]:


dir_path = '/media/Data/Naver_News/result/'

idx_result_df = pd.read_csv(dir_path + 'indexing.txt',encoding="UTF8",header=None)
idx_result_df.columns = ['date','title','press','content_url_list','file_list']
file_list = idx_result_df['file_list']
file_list = file_list.replace("./result",dir_path,regex=True)
idx_result_df['file_list'] = file_list

idx_result_df = idx_result_df.sort_values('date')
idx_result_df = idx_result_df.drop_duplicates(subset=['date','title'],keep="last")
idx_result_df = idx_result_df.reset_index(drop=True)


# ## 형태소 분석기 지정

# In[97]:


# Add Words

# Stopwords 

s_words = pd.read_csv("/media/Realtime_Analysis/stop_words.txt",header=None)
s_words = s_words[0].tolist()
r_words = pd.read_csv("/media/Realtime_Analysis/region_words.txt",header=None)
r_words = r_words[0].tolist()
s_words = list(set(s_words + r_words))




e_words = pd.read_csv("/media/Realtime_Analysis/e_words.txt",header=None)
e_words = e_words[0].tolist()
twitter = Twitter()
twitter.add_dictionary(e_words,'Noun')


# ## 실시간 키워드 빈도수 분석

# In[ ]:


import datetime

# 일주일 단위로 계산
periods = [7,14,30]
tr_cnt = 10
dd = 3
num = 1000

for period in periods:
    for d in range(1,dd):
        print(d)
        now = datetime.datetime.now().date() - datetime.timedelta(d)
        start_date = str(now - datetime.timedelta(period-1))
        path = "/media/Data/Realtime_Analysis/Keyword_Frequency/"


        # 날짜 필터링
        dt_index = pd.date_range(start=start_date, end = now)
        dt_list = dt_index.strftime("%Y-%m-%d").tolist()
        idx = list(map(lambda x: x in dt_list, idx_result_df['date'].tolist()))
        filtered_idx_result_df = idx_result_df.iloc[idx]
        filtered_idx_result_df = filtered_idx_result_df.reset_index(drop=True)


        # 옵션 설정
        corpus = []
        is_tfidf = True
        fn = str(now) + "_" + str(period) + "_" + "NN" + "_"


        file_list = filtered_idx_result_df['file_list'].tolist()

        for i in range(len(file_list)):
            #print(i)
            f = open(file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            corpus.append(" ".join(twitter.nouns(doc)))

        # Stop words 
        stop_words = s_words

        vect = CountVectorizer(stop_words = stop_words,ngram_range=(1, 2),min_df=0.001,max_df=0.5)
        X = vect.fit_transform(corpus)
        X = TfidfTransformer().fit_transform(X)
        fn = fn + "norm_TF.txt"

        count = X.toarray().sum(axis=0)
        idx = np.argsort(-count)
        count = count[idx]

        feature_name = np.array(vect.get_feature_names())[idx]

        keyword_list = list(zip(feature_name[:num], count[:num]))
        result = pd.DataFrame(keyword_list)
        result.to_csv(path + fn,header=None)


        # 키워드 빈도수 분석 (TF)

        fn = str(now) + "_" + str(period) + "_"  + "NN" + "_"
        fn = fn + "TF.txt"
        vect = CountVectorizer(stop_words = stop_words,ngram_range=(1, 2),min_df=0.001,max_df=0.5)
        X = vect.fit_transform(corpus)

        count = X.toarray().sum(axis=0)
        idx = np.argsort(-count)
        count = count[idx]

        feature_name = np.array(vect.get_feature_names())[idx]

        keyword_list = list(zip(feature_name[:num], count[:num]))
        result = pd.DataFrame(keyword_list)
        result.to_csv(path + fn,header=None)


        # 문서요약

        path = "/media/Data/Realtime_Analysis/Doc_Summary/"
        t_list = []
        m_list = []
        s_list = list(filtered_idx_result_df['title'])
        for s in s_list:
            t_list.append(s)

        t_list = list(set(t_list))
        p_sentences = ". ".join(t_list)
        textrank = TextRank(p_sentences)
        t = textrank.summarize(count=tr_cnt)
        t = t.split("\n")

        t = pd.DataFrame(t)
        t.columns=['Sentences']

        fn = str(now) + "_" + str(period) + "_" + "NN_" + str(tr_cnt) + "_TR_Summary" + ".txt"
        t.to_csv(path + fn,header=None)


        # 인덱스 저장
        path = "/media/Data/Realtime_Analysis/Indexing/"
        fn = str(now) + "_" + str(period) + "_" + "NN_" + "idx" + ".txt"
        filtered_idx_result_df.to_csv(path + fn,header=None)

