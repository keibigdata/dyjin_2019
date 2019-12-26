
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
import datetime
from textrankr import TextRank
#pd.options.display.float_format = '{:.3f}'.format
#plt.rc('font', family='NanumBarunGothicOTF')


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


keyword_list = pd.read_csv("/media/Realtime_Analysis/data/c_keyword_list.txt",engine="python",header=None)
keyword_list = keyword_list[0].tolist()


# In[4]:


import datetime

periods = [7,14,30]
dd = 3


for period in periods:
    print(period)
    for d in range(1,dd):
        print(d)
        now = datetime.datetime.now().date() - datetime.timedelta(d)

        #now = datetime.datetime(2019,7,1).date() - datetime.timedelta(180*d)
        start_date = str(now - datetime.timedelta(period-1))
        print(start_date)
        print(now)
        path = "/media/Data/Realtime_Analysis/Keyword_Cooccurence/"

        # 날짜 필터링
        dt_index = pd.date_range(start=start_date, end = now)
        dt_list = dt_index.strftime("%Y-%m-%d").tolist()
        idx = list(map(lambda x: x in dt_list, idx_result_df['date'].tolist()))
        filtered_idx_result_df = idx_result_df.iloc[idx]
        filtered_idx_result_df = filtered_idx_result_df.reset_index(drop=True)

        file_list = filtered_idx_result_df['file_list'].tolist()
        

        corpus = []

        for i in range(len(file_list)):
            f = open(file_list[i], 'r',encoding='utf-8',errors="ignore")
            doc = f.read()
            corpus.append(doc)  
        
        ndocs = len(corpus)
        result_mat = pd.DataFrame(columns=keyword_list,index=keyword_list,dtype=float)
        result_mat[:] = 0.0    
        for k1 in keyword_list:
            for k2 in keyword_list:
                if(k1 == k2):
                    result_mat[k1][k2] = 0
                    continue
                kcnt1 = 0
                kcnt2 = 0
                ccnt = 0
                for i in range(ndocs):
                    if corpus[i].count(k1) > 0 and corpus[i].count(k2) > 0:
                        ccnt = ccnt + 1
                    elif corpus[i].count(k1) > 0:
                        kcnt1 = kcnt1 + 1
                    elif corpus[i].count(k2) > 0:
                        kcnt2 = kcnt2 + 1

                result_mat[k1][k2] = (ccnt+1) / (kcnt1 + kcnt2 + ccnt+1)
                #print(result_mat)
                
        fn = str(now) + "_" + str(period) + "_" + "NN_" + "KCO" + ".txt"
        result_mat.to_csv(path + fn,header=None,index=None)

