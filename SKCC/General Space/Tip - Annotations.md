## Tip - Annotations

  

### Object Model

- Provide definitions of structural as well as transactional related aspects of the business data model
    - 구조정의
    - 비지니스데이터의 트랜잭션 관련

  

**ObjectModel.usageType.sizeCategory**  

- Scope : #ENTITY, #TABLE\_FUNCTION
- Environment : RAP, S4HANA
- 런타임처리엔진 : 없음
- 설명 : Each CDS-View shall have assigned a size category. The size category enables the consumer to judge the possible result set . It reflects the set of data which has to be searched through to compute for example a count(\*) of the CDS view. The labels correspond to the following size categories:
    - 데이터 개수에 대한 추정치 제공
    - Size Category
        - S : 1000 이하
        - M : 100,000 이하
        - L : 10,000,000 이하
        - XL : 100,000,000 이하
        - XXL : 100,000,000 이상

  

**ObjectModel.usageType.serviceQuality**  

- Scope : #ENTITY, #TABLE\_FUNCTION
- Environment : RAP
- 런타임처리엔진 : 없음
- 설명 : The category serviceQuality reflects the quality of service with regards to the expected performance of the CDS view. Based on the serviceQuality, the consumer can decide if a view fits to the demanded response time and throughput requirements. There are 4 levels of service quality: 'A', 'B', 'C' and 'D' corresponding to decreasing KPIs. 'A' has the highest requirements and 'D' the lowest. There is one additional level 'X' where no KPIs are specified. This level is used as the default. serviceQuality 'P' is reserved for views that are used to structure the view hierarchy and that don't have any consumer outside of the hierarchy.
    - CDS뷰의 성능(서비스 레벨) 추정치
    - 서비스 레벨
        - A > B > C > D : A가 가장높은 Service Quality를 가진다
        - X : 성능지표 없음
        - P : 계층구조에만 사용

  

**ObjectModel.usageType.dataClass**  

- Scope : 없음
- Environment : RAP
- 런타임처리엔진 : 없음
- 설명 : To support the decision on cache strategies for higher layers and to enable client-side statement routing using these caches, the data class is used. The different data classes correspond to different life time cycles. The annotation dataClass has the following values:
    - 해당 뷰를 사용하는 상위 레이어의 캐시 전략을 결정 지원
    - 종류
        - TRANSACTIONAL
            - 대량의 트랜잭션 및 변경이 많은 데이터
        - MASTER
            - 다른 곳에서 많이 읽히는 데이터
            - 변경이 많지 않은 데이터
        - ORGANIZATIONAL
            - 회사구조 및 비지니스 프로세스 데이터
        - CUSTOMIZING
            - 비지니스 프로세스 실행에 관련된 데이터
            - 고객의 설정에 의해서 프로세스를 변경하는데 사용되는 데이터
        - META
            - 시스템 구성 및 엔터티의 기술적 구조 설명
            - DDIC 구조, 필드제어, 권한객체
        - MIXED
            - 여러 종류의 Data Class를 가지고 있는 데이터

  

* * *

  

### EndUserText

- label : 필드 라벨로 사용 (60자이하)
- quickInfo : 필드의 추가 정보로 사용. 화면에서는 마우스를 올려놓으면 나타나는 데이터