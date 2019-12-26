# dyjin_2019
2019_연구과제_알고리즘_모음

## 1. 네이버_실시간_데이터_수집

1) naver_crawling.ipynb
- 네이버에서 실시간으로 등장하는 검색시 실시간 검색에 나타나는 네이버 댓글, 블로그 글, 트위터 댓글 등을 수집

## 2. LDA_분석_웹_App
1) app.R
- 사용자 입력한 파일에 대해 LDA 기반 동향 분석 One-Stop 서비스
- 텍스트(문서)들의 집합에 포함된 주제를 분석하고, 이를 토대로 기간별 동향을 파악하는 서비스
- R Shiny로 Webapp 구현


## 3_키워드_네트워크_웹App
1) app.R
- 사용자가 입력한 파일에 대해 네트워크 구성 One-Stop 서비스
- 텍스트의 키워드를 파악하고 키워드 사이의 연관 빈도가 높은 키워드 들의 네트워크를 도출
- R Shiny로 Webapp 구현


## 4_실시간_환경이슈_분석_웹App
1) app.R
- 실시간 환경 주요 키워드 빈도수, 주요 문장(제목)추출, 네트워크 분석, 키워드 그룹 분석 등을 활용한 환경 이슈 종합 분석 서비스
- 키워드 네트워크 시각화, 키워드 그룹 시각화 등의 소스코드는 이 파일에 포함
- R Shiny로 Webapp 구현

2) NN_KF_RTA.py, ME_KF_RTA.py
- 키워드 빈도수 분석 수행

3) KGF_RTA.py
- 키워드 그룹 빈도수 분석 수행

4) RTA_AA.R
- 키워드 네트워크 구성을 위한 연관분석 수행

5) NN_DBSCAN_RTA.py, ME_DBSCAN_RTA.py
- DBSCAN + TEXTRANK 문서요약 알고리즘 수행

6) stop_words.txt, region_words.txt
- 불용어 및 지역 관련 키워드들 모음

7) RTA.sh
- 실시간 분석을 위한 자동실행 스크립트

8) crontab.jpg
- 리눅스에서 자동실행을 위한 시간 지정

## 5_중국대기질_크롤링
1) china_air_quality_crawler.ipynb
- 중국 대기질 크롤링 소스코드 (https://www.aqistudy.cn/historydata/)

## 6. 기타
1) 키워드_기반_이미지_크롤링.ipynb
- 키워드_기반_이미지_크롤링


