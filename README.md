### [경영프로그래밍 - R데이터 분석] 2022 한국소비자원 생필품 가격을 이용한 상품군 별 저렴한 마트 조사


# 1.	서론
### 연구 배경과 목적
자취를 하다 보니 평소 생필품 가격에 관심이 많다. 최근 러시아-우크라이나 전쟁과 유가급등, 세계적으로 팜유, 닭 등 농산물 파동으로 인해 물가가 지속적으로 상승하고 있다. 자주 가던 마트에서 2년 넘게 항상 알뜰 가격 1000원을 유지했던 참치캔, 햄이 1200원이 되면서 물가가 상승되는 것을 직접적으로 느꼈다. 그리고 공산품과 가공식품은 이마트를, 축산품은 집 근처 마트를 이용하고 있는데, 이는 마트 별로 저렴한 상품군이 있다는 것을 알게 된 후다. 이번 연구에서는 생필품의 평균가격, 표준편차를 집중적으로 조사했으며, 마트 별로 어떤 상품군이 저렴한지 알아보았다. 어떤 마트가 가격이 낮게 형성되었는지 안다면, 앞으로 구매할 제품이 있을 때 도움이 될 것으로 기대된다.

# 2.	데이터 소개
### 데이터 출처
https://www.data.go.kr/data/15083256/fileData.do 
공공데이터 포털에서 “한국소비자원 생필품 및 서비스 가격 정보” 자료를 이용했다. CSV 파일이며 원본은 7개의 feature (상품명, 조사일, 판매가격, 판매업소, 제조사, 세일여부, 원플러스원)으로 구성되어 있었다. <br/> <br/>
<img src = "https://user-images.githubusercontent.com/105165279/184503554-631f7c84-b835-4562-a530-669593b62afd.png" width="800px">  

<표1. 데이터 구성>

### 데이터 구성
원본 데이터에는 조사를 시행한 일자가 2022년 5월 6일과 2022년 5월 20일로 2일 인데, 일자 별로 조사한 마트 개수가 달라서 데이터 개수가 더 많은 2022년 5월 20일만 사용하기로 했다. 세일여부와 원플러스원은 결측치가 많다. Yes 와 No로 이루어진 값이라 평균이나 최소값, 최대값을 넣을 수 없어서 세일여부 결측치는 결측치 그대로 사용하기로 했으며, 원플러스원은 세일여부가 대체 가능하여 사용하지 않기로 했다. 판매 가격을 제외하고 모두 character 값이다. 식품군은 상품명에 따라 가공식품, 공산품, 과자류, 기름/양념, 농산품, 수산, 유제품, 음료, 주류, 축산으로 나누어 새로 생성했다. 그리고 원본 데이터는 182,381개의 행으로 파일크기가 너무 커서 분석 속도가 느리며, 상품군을 하나하나 분류하기 어려운 관계로 생필품을 대표하는 35개의 상품, 11,740개의 데이터 행으로 진행했다.

### 가설
가설1. 프랜차이즈 마트 별로 판매하는 가격이 동일할 것이다. 프랜차이즈는 지역별로 가격을 관리하는 MD가 있기 때문에, 판매하는 가격이 같을 것이라 생각했다. 
가설2. 프랜차이즈 마트 별로 저렴히 판매하는 상품이 있을 것이다. 프랜차이즈 마트마다 수익을 상품군과, 할인으로 고객을 이끄는 상품군이 다를 것이라 예상했다. 그리고 마트 별로 제조사로부터 오는 유통 경로에 차이가 있으므로 소비자가에 차이가 있을 것이라 생각했다.
가설3. 생필품의 판매가격은 표준편차가 크지 않을 것이다. 생필품은 소비자가가 정해져 있고, 가격이 높지 않은 제품이 많다 보니 고가제품보다 표준편차가 작을 것이라 예상했다.

 
# 3.	결과
### 분석 내용
#### (1)	제품 별 평균 가격, 가격 표준 편차 분석
<img src = "https://user-images.githubusercontent.com/105165279/184503606-009f4cf5-7651-4b65-887a-baed72926edc.png" width="800px">  
 
<그래프1. 제품별 평균 가격>

<img src = "https://user-images.githubusercontent.com/105165279/184503607-68b86c96-0f9f-4840-827d-c7bd53385491.png" width="800px">  
 
<그래프2. 제품별 표준편차> 
 
<img src = "https://user-images.githubusercontent.com/105165279/184503673-ef97162e-62b9-4e8c-addf-0ec97af72855.png" width="800px">  

<표2. 제품별 평균 가격, 최대 가격, 최소 가격, 표준 편차>
그래프1 을 보면 평균 가격은 생필품 중 쇠고기 등심이 가장 비싸며, 감자가 제일 저렴한 것을 볼 수 있다. 그래프2 에서는 제품별 표준편차는 대부분 낮은 편이지만 허니버터 아몬드, 해태 오예스, 쇠고기 등심, 샴푸, 리앤 염색약은 큰 것으로 보인다. 리앤 염색약의 경우 할인하는 지점에서 1+1 증정도 하고, 가격할인이 큰 것으로 확인되었다. 그리고 마미손 고무장갑의 표준편차가 0인 것은 이상치로 보일 수 있는데, 고무장갑을 판매하는 모든 곳에서 4880원으로 판매하는 것을 표2에서 볼 수 있다. 따라서 표준편차 0은 정상이다. 가설3에서 생필품의 표준편차는 크지 않을 것이라 예상했는데, 생필품도 제품에 따라 표준편차가 없을 수도, 클 수도 있다는 것을 보여준다.

#### (2)	제품군 별 평균 가격, 가격 표준 편차 분석

<img src = "https://user-images.githubusercontent.com/105165279/184503706-048c55be-4b6f-4276-9202-92f0bc871c7c.png" width="800px">  
 
<그래프3. 제품군 별 가격 표준편차>
 
<img src = "https://user-images.githubusercontent.com/105165279/184503711-d056f1bc-922b-47d7-9358-9eaed9cbeb44.png" width="800px"> 

<표3. 제품군 별 평균 판매가격, 최대 판매가격, 최소 판매가격, 표준편차>
그래프3을 보면 제품군 별 표준편차는 축산에서 가장 크고, 유제품과 농산품은 거의 없는 것으로 보인다. 이는 상품 하나 당 판매가격이 다른 것보다, 동일 제품군 내에 여러 상품들의 판매 가격이 천차만별이기 때문이다. 축산은 소고기, 돼지고기, 계란이 해당되어 금액이 다양한 것으로 설명할 수 있다. 유제품의 경우 대부분 1L, 900mL 라는 규격이 정해져 있기 때문에 제품마다 가격이 비슷하며, 농산품의 경우 대부분 100g 등의 무게 별 금액으로 가격을 산정하기 때문에 표준편차가 적은 것으로 보인다.

#### (3)	마트 별 제품 개수와 크기의 상관성
<img src = "https://user-images.githubusercontent.com/105165279/184503728-55dec22b-6ba8-4e5a-bf28-1a3389a5b83e.png" width="800px"> 
<img src = "https://user-images.githubusercontent.com/105165279/184503730-cbeccbd9-3f04-4ad6-aec6-5d465e55d5a9.png" width="800px"> 

<그래프4-5. 마트 별 제품 개수>

 이 연구의 생필품 표본 개수는 35개이다. 마트 별로 몇 개씩 보유하고 있는지 분석했더니 하위 15개는 현대백화점 목동점을 제외하고 모두 GS더프레시가 나왔다. 상위 15개는 이마트 가양점을 제외하고 롯데슈퍼가 차지했다. 이를 보아 GS더프레시는 대부분 점포 크기가 작아 진열해둔 생필품 종류가 적으며, 롯데슈퍼는 대부분 점포 크기가 커서 진열해둔 생필품 종류가 크다는 것을 유추할 수 있다. 

#### (4)	상품 별 판매 마트 개수 및 인기 상품의 연관성
  
<img src = "https://user-images.githubusercontent.com/105165279/184503740-3ddf34bc-b60f-4294-8b2c-c52b88673624.png" width="800px"> 

<표4-5. 상품명 별 판매 마트 개수>
 이 연구의 마트 표본 개수는 522개이다. 서울우유 흰우유는 모든 마트에 판매되고 있으며, 삼다수는 마트 1개를 제외한 52개에서 판매되고 있다. 모두 판매량이 많고 대중적인 생필품이지만, 해당 표에서 상위권을 차지한 상품일수록 판매량이 많은 필수품으로 유추할 수 있다. 다음은 음료 제품군의 마트 가격을 알아보기 위해, 연구 표본 전체에서 판매되고 있는 서울우유의 가격이다.

#### (5)	마트 별 서울우유 흰우유(1L)의 가격
<img src = "https://user-images.githubusercontent.com/105165279/184503742-e518236a-98cc-4918-b8ce-6be2e48369c1.png" width="800px"> 
 
<그래프6-7. 마트 별 제품 개수>
그래프6은 서울우유 흰우유(1L)를 비싸게 판매하는 상위 15개 지점, 그래프7은 삼다수를 저렴하게 판매하는 하위 15개 지점이다. 그래프에서 보이는 것처럼 프랜차이즈 별 가격 차이를 볼 수 없었다. 삼다수도 마찬가지로 프랜차이즈 별 가격 차이는 없었다. 이를 통해 프랜차이즈마다 가격이 동일하다는 가설1은 사실이 아닌 것을 알 수 있다. 연관된 가설2 또한 프랜차이즈마다 특성 제품군이 있는 것이 아니라는 것을 알게 되었다. 

# 4.	논의
### 분석 의미
처음에 설정한 가설과 분석 결과를 비교하자 프랜차이즈별로 항상 가격이 동일한 것은 아니며, 프랜차이즈별로 가격 경쟁력이 특성화된 상품군이 있는 것은 아니라는 것을 알게 되었다. 그리고 생필품은 고가 제품에 비하면 가격 편차가 낮긴 하지만, 정육 등 일부 상품들은 가격 편차가 클 수 있음을 보였다. 이는 정육의 원산지와 신선도에 따른 것으로 유추된다. 반면 판매 단위가 동일한 농산물이나 크기가 정형화된 유제품 등은 가격 편차가 적은 것을 보아, 공산품이 비교적 가격 편차가 낮다는 것을 알 수 있었다.
그리고 마트 별 제품의 개수를 비교했을 때 롯데슈퍼가 가장 많고, GS더프레시가 가장 적은 수치를 보였는데 이는 주변에서도 자주 보이는 것처럼 롯데마트가 크고, GS더프레시가 작기 때문이라는 점을 알 수 있다. R 분석으로 나온 데이터가 실제 마트 크기에 비례한다는 점이 신기하게 다가왔으며, 유의미했다.

### 한계
 제품군을 나눠야 하고, 노트북으로 작업하다 보니 원본데이터의 제품의 전체 데이터 수가 커서 임의로 개수 지정한 점이 아쉽다. 추후 기회가 된다면 더 많은 데이터를 한 번에 다뤄보고 싶다. 마트마다 모두 동일한 제품을 판매하는 것이 아니다 보니, 상품 군별 저렴한 마트를 정확히 연구하지 못했다. 현재는 무의미한 수치를 보았는데, 추후 모두 동일한 제품을 판매할 때 분석하면 다른 결과가 나올 수도 있을 것 같다. 마지막으로 원본데이터에 조사날짜마다 상품 개수가 상이하여 조사일을 하루로 설정했는데, 추후 여러 날짜로 비교해보면 세일 상품도 더 잘 볼 수 있을 것으로 예상된다.
