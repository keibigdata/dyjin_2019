
# coding: utf-8

# In[1]:


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
from textrankr import TextRank
import re
from sklearn.metrics import pairwise_distances
from scipy.spatial.distance import cosine
import datetime
from sklearn.cluster import DBSCAN
import matplotlib.pyplot  as plt
import seaborn as sns
plt.rc('font', family='NanumBarunGothicOTF')


# # 실시간 네이버 뉴스

# In[2]:


dir_path = '/media/Data/Naver_News/result/'

idx_result_df = pd.read_csv(dir_path + 'indexing.txt',encoding="UTF8",header=None)
idx_result_df.columns = ['date','title','press','content_url_list','file_list']
file_list = idx_result_df['file_list']
file_list = file_list.replace("./result",dir_path,regex=True)
idx_result_df['file_list'] = file_list

idx_result_df = idx_result_df.sort_values('date')
idx_result_df = idx_result_df.drop_duplicates(subset=['date','title'],keep="last")
idx_result_df = idx_result_df.reset_index(drop=True)


# In[3]:


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


# In[ ]:


import datetime

# 일주일 단위로 계산
periods = [7,14,30]
dd = 3
cnt = 10

path = "/media/Data/Realtime_Analysis/Doc_Summary/"

for period in periods:
    print(period)
    for d in range(1,dd):
        print(d)
        now = datetime.datetime.now().date() - datetime.timedelta(d)
        start_date = str(now - datetime.timedelta(period))

        # 날짜 필터링
        dt_index = pd.date_range(start=start_date, end = now)
        dt_list = dt_index.strftime("%Y-%m-%d").tolist()
        idx = list(map(lambda x: x in dt_list, idx_result_df['date'].tolist()))
        filtered_idx_result_df = idx_result_df.iloc[idx]
        filtered_idx_result_df = filtered_idx_result_df.reset_index(drop=True)
        
        corpus = []
        
        # 코퍼스 데이터 구축
        title_list = filtered_idx_result_df['title']
        title_list = list(set(title_list))
            
        for i in range(len(title_list)):
            #print(i)
            doc = title_list[i]
            corpus.append(" ".join(twitter.nouns(doc)))
            
        # DB SCAN
        vect = CountVectorizer(stop_words = s_words,ngram_range=(1, 1),min_df=0.005,max_df=0.5)
        X = vect.fit_transform(corpus)

        feature = pd.DataFrame(X.toarray())
        model = DBSCAN(eps=0.3,metric="cosine",min_samples=period)
        
        group_label = model.fit_predict(feature)
        result_mat = pd.concat([pd.DataFrame(title_list), pd.DataFrame(group_label)],axis=1) 
        result_mat.columns = ['contents','group']
        
        #print(result_mat)
        #각 트리 그룹에 대해서 실행
        s_list = []
        n_list = []
        keyword_list = []
        for gidx in range(0,max(group_label+1)):
            sub_result_mat = result_mat[result_mat['group'] == gidx]             
#           sub_corpus = pd.DataFrame(corpus)[0][result_mat['group'] == gidx].tolist()
            
#           vect = CountVectorizer(stop_words=s_words, ngram_range=(1,1))
#           X = vect.fit_transform(sub_corpus)
   
#           count = X.toarray().sum(axis=0)
#           idx = np.argsort(-count)
#           count = count[idx]
#           feature_name = np.array(vect.get_feature_names())[idx]  
            #keywords = " ".join(feature_name[0:15]).strip()
            
            t_list = list(set(sub_result_mat['contents']))
            
            p_sentences = ". ".join(t_list)
            
            textrank = TextRank(p_sentences)
            t = textrank.summarize(count=1)
            
            s_list.append(t)      
            n_list.append(len(t_list))
            #keyword_list.append(keywords)
            
        s_df = pd.DataFrame(s_list)
        s_df.columns=['Sentences']
        s_df['cnt'] = n_list
        #s_df['keyword'] = keyword_list

        fn = str(now) + "_" + str(period) + "_" + "NN_" +  "DBSCAN_TR_Summary" + ".txt"
        s_df.to_csv(path + fn,header=None)

