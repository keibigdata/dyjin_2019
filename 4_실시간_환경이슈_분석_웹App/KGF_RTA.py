
# coding: utf-8

# ## 네이버 환경뉴스 실시간 분석

# ### 라이브러리 import

# In[6]:


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
from konlpy.tag import Mecab, Twitter
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

# In[2]:


dir_path = '/media/Data/Naver_News/result/'

idx_result_df = pd.read_csv(dir_path + 'indexing.txt',encoding="UTF8",header=None)
idx_result_df.columns = ['date','title','press','content_url_list','file_list']
file_list = idx_result_df['file_list']
file_list = file_list.replace("./result",dir_path,regex=True)
idx_result_df['file_list'] = file_list

idx_result_df = idx_result_df.sort_values('date')
idx_result_df = idx_result_df.drop_duplicates(subset=['date','title'],keep="last")
s_idx_result_df = idx_result_df.reset_index(drop=True)


# In[7]:


dir_path = '/media/Data/Me_Report/result/'
dir_path2 = '/media/Data/Me_ENews/result/'

idx_result_df1 = pd.read_csv(dir_path + 'indexing.txt',encoding="UTF8",header=None)
idx_result_df1.columns = ['date','title','content_url_list','file_list']
file_list = idx_result_df1['file_list']
file_list = file_list.replace("./result",dir_path,regex=True)
idx_result_df1['file_list'] = file_list

idx_result_df2 = pd.read_csv(dir_path2 + 'indexing.txt',encoding="UTF8",header=None)
idx_result_df2.columns = ['date','title','content_url_list','file_list']
file_list = idx_result_df2['file_list']
file_list = file_list.replace("./result",dir_path2,regex=True)
idx_result_df2['file_list'] = file_list

idx_result_df = pd.concat([idx_result_df1, idx_result_df2])
idx_result_df = idx_result_df.sort_values('date')
idx_result_df = idx_result_df.drop_duplicates(subset=['date','title'],keep="last")
d_idx_result_df = idx_result_df.reset_index(drop=True)


# 
# ## 환경분야별 키워드 비율 분석

# In[ ]:


import datetime

periods = [7,14,30]
dd = 300

for period in periods:
    print(period)
    for d in range(1,dd):
        now = datetime.datetime.now().date() - datetime.timedelta(d)
        start_date = str(now - datetime.timedelta(period-1))
        path = "/media/Data/Realtime_Analysis/Keyword_Group_Frequency/"

        # 날짜 필터링
        dt_index = pd.date_range(start=start_date, end = now)
        dt_list = dt_index.strftime("%Y-%m-%d").tolist()
        idx = list(map(lambda x: x in dt_list, s_idx_result_df['date'].tolist()))
        s_filtered_idx_result_df = s_idx_result_df.iloc[idx]
        s_filtered_idx_result_df = s_filtered_idx_result_df.reset_index(drop=True)


        idx = list(map(lambda x: x in dt_list, d_idx_result_df['date'].tolist()))
        d_filtered_idx_result_df = d_idx_result_df.iloc[idx]
        d_filtered_idx_result_df = d_filtered_idx_result_df.reset_index(drop=True)

        s_file_list = s_filtered_idx_result_df['file_list'].tolist()
        s_ndocs = len(s_file_list)

        s_corpus = []

        for i in range(len(s_file_list)):
            #print(i)
            f = open(s_file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            s_corpus.append(doc) 

        s_corpus_str = "".join(s_corpus)

        d_file_list = d_filtered_idx_result_df['file_list'].tolist()
        d_ndocs = len(d_file_list)

        d_corpus = []

        for i in range(len(d_file_list)):
            #print(i)
            f = open(d_file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            d_corpus.append(doc) 

        d_corpus_str = "".join(d_corpus)

        kg_df = pd.read_excel("./data/kg.xlsx")
        kg_df = kg_df.fillna("None")

        result_df = pd.DataFrame()
        result_df2 = pd.DataFrame()

        for kg in kg_df:
            temp_df = pd.DataFrame(columns=[kg,'F1','F2'])
            temp_df2 = pd.DataFrame(columns=[kg,'F1','F2'])

            k_list = kg_df[kg]
            temp_df[kg] = k_list
            temp_df2[kg] = k_list

            result_list = []
            result_list2 = []
            for k in k_list:
                if k == "None":
                    result_list.append([-1,-1])  
                    result_list2.append([-1,-1])  
                else:
                    result_list.append([s_corpus_str.count(k), d_corpus_str.count(k)])  
                    result_list2.append([s_corpus_str.count(k) / (s_ndocs+1), d_corpus_str.count(k) / (d_ndocs+1)])  
                    
            temp_df[['F1','F2']] = result_list
            temp_df = temp_df.sort_values(by=['F1',kg],ascending=[False,False])
            temp_df = temp_df.reset_index(drop=True)
            result_df = pd.concat([result_df,temp_df],axis=1)
                        
            temp_df2[['F1','F2']] = result_list2
            temp_df2 = temp_df2.sort_values(by=['F1',kg],ascending=[False,False])
            temp_df2 = temp_df2.reset_index(drop=True)
            result_df2 = pd.concat([result_df2,temp_df2],axis=1)
                                    
        result_df['F1'] = result_df['F1'].apply(pd.to_numeric)
        result_df['F2'] = result_df['F2'].apply(pd.to_numeric)
            
        result_df2['F1'] = result_df['F1'].apply(pd.to_numeric)
        result_df2['F2'] = result_df['F2'].apply(pd.to_numeric)
        fn1 = str(now) + "_" + str(period) + "_" + "NN_ME_KGF" + ".txt"
        fn2 = str(now) + "_" + str(period) + "_" + "NN_ME_KGF_NORM" + ".txt"
        
        result_df.to_csv(path + fn1, float_format='%.0f')
        result_df2.to_csv(path + fn2, float_format='%.3f')


# In[ ]:


import datetime

for period in periods:
    print(period)
    for d in range(1,dd):
        now = datetime.datetime.now().date() - datetime.timedelta(d)
        start_date = str(now - datetime.timedelta(period-1))
        path = "/media/Data/Realtime_Analysis/Keyword_Group_Trends/"

        # 날짜 필터링
        dt_index = pd.date_range(start=start_date, end = now)
        dt_list = dt_index.strftime("%Y-%m-%d").tolist()
        idx = list(map(lambda x: x in dt_list, s_idx_result_df['date'].tolist()))
        s_filtered_idx_result_df = s_idx_result_df.iloc[idx]
        s_filtered_idx_result_df = s_filtered_idx_result_df.reset_index(drop=True)


        idx = list(map(lambda x: x in dt_list, d_idx_result_df['date'].tolist()))
        d_filtered_idx_result_df = d_idx_result_df.iloc[idx]
        d_filtered_idx_result_df = d_filtered_idx_result_df.reset_index(drop=True)

        s_file_list = s_filtered_idx_result_df['file_list'].tolist()
        s_ndocs = len(s_file_list)

        s_corpus = []

        for i in range(len(s_file_list)):
            f = open(s_file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            s_corpus.append(doc) 

        d_file_list = d_filtered_idx_result_df['file_list'].tolist()
        d_ndocs = len(d_file_list)
        
        d_corpus = []
       
        for i in range(len(d_file_list)):
            f = open(d_file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            d_corpus.append(doc) 

        kg_df = pd.read_excel("/media/Data/Keyword_List/KG.xlsx")
        kg_df = kg_df.fillna("None")
   
        result_df = pd.DataFrame()
        result_df2 = pd.DataFrame()
        
        group_list = list(kg_df.columns)
        
        result_df1 = pd.DataFrame(columns=['GROUP','COUNT']) 
        result_df2 = pd.DataFrame(columns=['GROUP','COUNT']) 
        result_df1['GROUP'] = group_list
        result_df2['GROUP'] = group_list
        
        for idx,g in enumerate(group_list):
            key_list = kg_df.loc[:,g].dropna()
            rexp = "|".join(key_list)
            p = re.compile(rexp)
            
            sc_cnt = 0 
            dc_cnt = 0 
            
            for sc in s_corpus:
                ridx = p.search(sc)
                if(ridx != None):
                    sc_cnt = sc_cnt + 1
                    
            for dc in d_corpus:
                ridx = p.search(dc)
                if(ridx != None):
                    dc_cnt = dc_cnt + 1
                    
            result_df1.loc[idx,'COUNT'] = sc_cnt
            result_df2.loc[idx,'COUNT'] = dc_cnt
            
        fn1 = str(now) + "_" + str(period) + "_" + "NN_KGT" + ".txt"
        fn2 = str(now) + "_" + str(period) + "_" + "ME_KGT" + ".txt"
        
        result_df1.to_csv(path + fn1,index=None)
        result_df2.to_csv(path + fn2,index=None)

