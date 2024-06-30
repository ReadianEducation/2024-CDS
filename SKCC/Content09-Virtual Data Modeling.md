## Content09-Virtual Data Modeling

  

#### VDM (Virtual Data Modeling)

- SAP CDS를 이용해서 비즈니스데이터를 모델링하는데 사용되는 모델링 방법
- 비즈니스 의미를 데이터와 함께 제공하여 데이터를 좀더 이해하기 쉽게 사용할 수 있도록 해준다.
- 데이터모델은 S/4HANA 시스템의 분석모델, 트랜잭션모델 등의 기초가 된다.
- Fiori 등의 새로운 개발방법은 VDM 기반으로 진행된다.
- VDM모델은 계속업그레이드가 되고 변경이 되고 있기 때문에 해당 모델이 안정화 되면 그때 고객이 사용하도록 Release 하여 고객이 사용가능하도록 하고 있다. 모든 모델 관련 항목을 사용하는 것이 아니라 해당 상태와 사용 가능 여부를 항상확인하여 개발에 이용해야 한다. (모델의 라이프사이클)

  

#### VDM 개발 원칙

- 비기술적인 비즈니스 관점에서도 이해하기 쉽도록 다른 뷰와의 관계와 같은 의미론적 요소를 충분히 제공하도록 모델링 해야 한다.
- 항상 모델의 재사용성을 고려하여 개발해야 한다.
- CDS뷰는 VDM모델의 가장 기본이 되는 모델이며, OData모델이나 분석모델의 기초가 된다
- VDM의 뷰는 HANA 데이터베이스에서 실행이 되어 빠른속도가 이점이다
- VDM 모델은 새로운 표준앱의 기초가 된다
- VDM 모델은 CDS를 기본으로 하여 강력한 ABAP 실행 환경을 활용한다.

  

## 라이프사이클, 출시, 안정성 관련

SAP에서는 고객이 사용할 수 있는 표준개발객체의 라이프사이클 관리를 하고 있다.

  

### Release contract and Visibility

  

라이프사이클 제어항목

- 새로운 소프트웨어 버전으로 업그레이드시에 안정성을 제어
- key user app이나 개발환경에서 사용가능한 항목을 제어

  

### **Release Contract (출시 계약)**

|     |     |     |
| --- | --- | --- |
| **Release Contract** | **Code** | **설명** |
| Use System-Internally | C1  | S4HANA 또는 BTP, BTP ABAP 환경에서 개발 가능 여부에 대한 계약 |
| Use as Remote API | C2  | SAP 내부 안정된 API 개발을 위해 제공되는 계약으로 Custom 개발에 사용할 수 없다 |
| Extend | C0  | 확장가능한 개발객체에 적용되는 계약 |

  

#### 계약사항 확인 방법

먼저 사용하고자 하는 개발오브젝트를 검색한다. 여기서는 I\_Country를 검색한다.

![](Files/image%20204.png)  

  

속성 (Properties)에 API State를 확인한다.

![](Files/image%20205.png)  

- 개발관련 C1 계약에 대해서 
    - Cloud 개발이 가능
    - Key User Apps에서 사용 가능

  

### Release 상태 

|     |     |
| --- | --- |
| **Release 상태** | **설명** |
| NOT\_RELEASED | 아직 변경이 가능한 상태이며, 향후에 사용가능하도록 RELESED 될수 있는 오브젝트. 사용자 개발환경에서는 보이지 않으며 새롭게 생성된 객체인 경우 해당 상태를 가진다 |
| NOT\_TO\_BE\_RELEASED | 아직 변경이 가능한 상태이며, 향후에 RELEASED 되지 않을 오브젝트. 사용자 개발 환경에서 보이지 않는다 |
| NOT\_TO\_BE\_RELEASED\_STABLE | RELEASED되지 않는 SAP 내부 용도 오브젝트. 사용자 개발 환경에서 보이지 않는다 |
| RELEASED\_WITH\_FETURE\_TOGGLE | 일반적으로 RELEASED되지 않는 오브젝트. 파일럿 테스트를 위한 일부 고객에게만 제공되며 사용자 개발환경에서는 보이지 않는다 |
| RELEASED | 커스텀개발환경에서 사용가능하며, 사용자개발환경에서 보이는 오브젝트 |
| DEPRECATED | 제한된 기간동안 RELEASED 된 오브젝트처럼 처리되며 사용자정의 개발환경에서 사용가능하다 |
| DECOMMISSIONED | 더이상 사용하지 말아야 하는 오브젝트. 사용자 개발환경에서 보이지 않는다 |

  

VDM 모델이 RELEASED 될 예정이 없는 항목이고 SAP 내부 용도이면

- @VDM.lifecycle.contract.type: #SAP\_INTERNAL\_API 
- @VDM.private: true

  

VDM 모델이 RELEASED될 예정이 항목은

- @VDM.lifecycle.contract.type: #PUBLIC\_LOCAL\_API → C1 계약에 의해

  

DEPRECATED 정책 - [참조 URL](http://s-prs.co/v529408 "http://s-prs.co/v529408")

  

DEPRECATION 또는 DECOMMISIONING은 어느정도 기간이 걸려야 해당 상태로 변경이 된다

- RELEASED → DECOMMISIONED : 적어도 24개월
- DEPRECATED → DECOMMISIONED : 적어도 12개월

  

DEPRECATED 된 오브젝트는 다음의 어노테이션을 이용해서 successor를 설정한다.

- VDM.lifecycle.successor : ‘<successor>’

![](Files/image%20206.png)  

  

  

### Supported Capabilities

- CDS뷰가 제공할 수 있는 기능을 기술
- @ObjectModel.supportedCapabilities 
- 여러개 설정 가능
- 모델링패턴에 대해서는 @ObjectModel.modelingPattern 어노테이션으로 기술
- 소비자가 필요로하는 엔터티를 찾을 수 있는데 도움을 주기 위해 제공

  

|     |     |     |
| --- | --- | --- |
| **Name** | **C = 지원가능 기능  <br>M =모델링 패턴** | **설명** |
| SQL\_DATA\_SOURCE | C   | SELECT문이나 PATH 표현식으로 데이터 검색기능 제공 |
| CDS\_MODELING\_DATA\_SOURCE | C   | 데이터소스로 사용가능한 엔터티 |
| CDS\_MODELING\_ASSOCIATION\_TARGET | C   | CDS 모델을 만들때 Association의 대상 CDS로 사용 가능 |
| DATA\_STRUCTURE | C, M | RAP 또는 ABAP에서 Structure로 사용 가능 |
| LANGUAGE\_DEPENDENT\_TEXT | C, M | 언어에 따른 텍스트를 제공 |
| VALUE\_HELP\_PROVIDER | C, M | Fiori 또는 분석 앱에서 Value Help를 제공 |
| EXTRACTION\_DATA\_SOURCE | C   | 원격시스템으로 부터 데이터를 추출하는데 사용 |
| DERIVATION\_FUNCTION | C, M | @Consumption.derivation 에 사용 |
| PARENT\_CHILD\_HIERARCHY\_NODE\_PROVIDER | C, M | 계층구조의 데이터를 검색 하고 집계를 하는데 사용 |
| SEARCHABLE\_ENTITY | C   | 특수 검색 작업에 사용 |
| ANALYTICAL\_QUERY | C, M | SAP SAC같은 앱에서 사용가능한 분석쿼리로 사용 |
| ANALYTICAL\_PROVIDER | C   | 분석쿼리에 사용가능한 엔터티 |
| ANALYTICAL\_CUBE | M   | 분석큐브의 용도로 개발된 엔터티 |
| ANALYTICAL\_DIMENSION | C, M | 분석Dimension으로 개발된 엔터티 |
| ANALYTICAL\_PARENT\_CHILD\_HIERARCHY\_NODE | C, M | 분석에서 계층구조로 사용되는 엔터티 |
| OUTPUT\_FORM\_DATA\_PROVIDER<br>OUTPUT\_EMAIL\_DATA\_PROVIDER<br>OUTPUT\_PARAMETER\_DETERMINATION\_DATA\_SOURCE | C, M | 출력 관리에 사용되는 엔터티<br>[참고](https://community.sap.com/t5/enterprise-resource-planning-blogs-by-sap/output-management-in-sap-s-4hana/ba-p/13492419 "https://community.sap.com/t5/enterprise-resource-planning-blogs-by-sap/output-management-in-sap-s-4hana/ba-p/13492419") |
| SIMULATION\_ANCHOR<br>SIMULATION\_TRIGGER<br>SIMULATION\_DATACONTEXT | C, M | 시뮬레이션 관련 처리를 할때 사용이되는 엔터티<br>[참고](https://community.sap.com/t5/enterprise-resource-planning-blogs-by-members/sap-s-4hana-situation-handling-overview/ba-p/13438718 "https://community.sap.com/t5/enterprise-resource-planning-blogs-by-members/sap-s-4hana-situation-handling-overview/ba-p/13438718")<br><br> |

  

  

## Virtual Data Model의 구조

  

### VDM Layers

- 단순히 CDS의 집합이 아니라 가각 레이어별로 역할이 나누어져 있다

![](Files/image%20207.png)  

  

  

## Basic Interface View

  

- 테이블 바로 위에 위치
- @VDM.viewType: #BASIC 
- 필드나 다른 데이터와의 Association에 대해서 잘 정의할 필요가 있는 가장 중요한 뷰
- 이름을 비즈니스적으로 알기 쉽게 변경. 해당 변경은 Basic 뷰에서만 해야 한다
- 특정 필드의 데이터타입을 비즈니스적으로 사용되는 항목으로 바꾸기 위해서는 cast를 Basic 뷰에서 처리
- 계산필드나 변환로직은 없이 만드는 것이 원칙
- 필드라벨을 비즈니스환경에 맞게 적절히 변경작업을 수행하는 뷰이며, 추후에 재사용시에 표준 라벨로 사용할 수 있음
- “I\_” Naming Rule을 가진다

  

#### 설정 Semantic 어노테이션

|     |     |
| --- | --- |
| **필드 어노테이션** | **의미** |
| @Semantics.amount.currencyCode | 특정 통화필드를 참조하는 금액 필드 |
| @Semantics.currency | 통화필드 |
| @Semantics.quantity.unitOfMeasure | 특정 단위필드를 참조하는 수량 필드 |
| @Semantics.unitOfMeasure | 단위필드 |
| @Semantics.businessDate | 비즈니스적인 유효기간 관련 필드 |
| @Semantics.systemDate<br>@Semantics.systemTime<br>@Semantics.systemDateTime | 기술적인 시스템 시간 관련 필드 |
| @Semantics.fiscal | 회계년도 관련 필드 |
| @Semantics.text | 텍스트 필드 |
| @Semantics.language | 언어 필드 |

  

#### 설정 Association 유형

|     |     |     |
| --- | --- | --- |
| **관계 유형** | **어노테이션** | **사용처** |
| Composition and Aggregation | @ObjectModel.association.type | 포함관계의 뷰간의 구조를 정의하는데 사용 |
| 필드의 텍스트 Association | @ObjectModel.text.association | 언어별 텍스트를 가지고 있는 뷰와 관계 설정 |
| 필드의 Foreign key 관계 | @ObjectModel.foreignKey.association | 필드의 의미, 값, 세부정보를 제공하는 엔터티 뷰와의 관계 |

예시)

![](Files/image%20208.png)  

  

  

## Composite Interface Views

  

- @VDM.viewType: #COMPOSITE 
- Basic 뷰 또는 다른 Composite 뷰를 가지고 생성
- 여러 뷰를 결합하여 만들 수 있다
- 큐브 뷰등이 Composite 뷰의 하나의 종류
- Transaction을 처리하는 CDS 뷰도 Composite 뷰에 들어가지만 좀 더 명확히 구분하기 위해서 다음의 어노테이션을 사용한다.
    - @VDM.viewType: #TRANSACTION 
- 재사용을 목적으로 하는 뷰
    - 모든 뷰가 사용자에게 공개되지 않으며 SAP 내부 사용 목적인 항목도 있다
    - “R\_” (Resticed)의 Naming Rule을 가지고 있다
- “I\_” 가 들어가는 Naming Rule을 가지고 있다

  

![](Files/image%20209.png)  

  

  

## Consumption Views

  

- VDM의 최상위 계층
- @VDM.viewType: #COMSUMPTION 
- “C\_” Naming Rule을 가진다
- 용도
    - 분석시나리오의 분석 쿼리
    - 트랜잭션 처리 서비스 모델의 트랜잭션 처리 뷰
    - OData 서비스를 통해 Fiori 등에서 데이터를 가지고 오는 뷰
- 특정 어플리케이션에 맞춰서 만들어지기 때문에 재사용 목적은 아니며, 어플리케이션이 변경될때 같이 변경이 되므로 호환성 문제가 발생할 수 있으므로 고객에게 공개되지 않는다
    - 분석쿼리는 여러 앱에서 사용할 수 있기 때문에 해당 뷰들은 예외적으로 계속적인 유지보수를 진행하고 있다
- 사용하는 어플리케이션의 레이아웃 및 동작을 제어하는 특별한 어노테이션을 제공
- 원경 API 용도로 만들어지는 경우에는 ‘A\_’ 형태의 Naming Rule을 가지거나 @VDM.lifecyucle.contract.type: #PUBLIC\_REMOTE\_API 어노테이션이 설정된 뷰도 있다
    - API 허브에 해당 서비스를 검색 가능
    - 원격 API로 사용되는 경우에는 CONPOSITE 또는 BASIC 뷰도 사용되는 경우도 많다

  

![](Files/image%20210.png)  

  

  

## Other View 유형

  

### Private

- @VDM.private: true
- “P\_”
- SAP 내부 사용 용도이며 모든 Layer에 존재

  

### Extension include

- @VDM.viewType: #EXTENSION 
- “E\_”
- 가장 낮은 레벨의 레이어 (BASIC)에 포함
- Extension의 목적 이외에는 사용하면 안됨

  

#### VDM view Extension

- @VDM.viewExtension: true
- “X\_”
- 다른 시스템에서 개발된 소프트웨어 구성요소에 속하는 VDM 뷰를 확장하는데 사용

  

  

#### VDM 엔터티 유형

|     |     |     |
| --- | --- | --- |
| **유형** | **Naming Rule** | **식별자** |
| Basic interface view | I\_\* | @VDM.viewType: #BASIC |
| Composite interface view | I\_\* | @VDM.viewType: #COMPOSITE |
| Transactional interface view | I\_\*TP | @VDM.viewType: #TRANSACTIONAL |
| Restricted reuse view | R\_\* | @VDM.lifecycle.contract.type: @SAP\_INTERNAL\_API |
| Transactional object view | R\_\*TP | @VDM.viewType: #TRANSACTIONAL <br>@VDM.lifecyle.contract.type: #SAP\_INTERNAL\_API |
| Consumption view | C\_\* | @VDM.viewType: #CONSUMPTION |
| Transactional consumption view | C\_\*TP | @VDM.viewType: #CONSUMPTION <br>@VDM.usage.type: \[#TRANSACTIONAL\_PROCESSING\_SERVICE\] |
| Remote VDM view | A\_\* | @VDM.lifecycle.contract.type: #PUBLIC\_REMOTE\_API |
| Private VDM view | P\_\* | @VDM.private: true |
| Extension include view | E\_\* | @VDM.viewType: #EXTENSION |
| VDM view extension | X\_\* | @VDM.viewExtension: true |

  

**Basic View**

![](Files/image%20211.png)  

  

**Composite interface View**

![](Files/image%20212.png)  

  

**Transactional interface view**  

![](Files/image%20213.png)  

  

**Restricted reuse view**  

![](Files/image%20214.png)  

  

**Transactional object view**  

![](Files/image%20215.png)  

  

**Consumption view**  

![](Files/image%20216.png)  

  

**Transactional consumption view**  

![](Files/image%20217.png)  

  

**Remote VDM view**  

![](Files/image%20218.png)  

  

**Private VDM view**  

![](Files/image%20219.png)  

  

**Extension include view**  

![](Files/image%20220.png)  

**VDM view extension**  

![](Files/image%20221.png)  

  

  

## Virtual Data Model Naming

  

### 필드이름

- 실 비즈니스 환경과 비슷한 이름을 사용
- 이름 충돌을 피할수 있도록 일정한 접두사를 사용하는 것도 필요
- 정확하고 명확하게
- 너무 일반적이지 않지만 가능한한 짧게
- CamelCase 사용
    - ex) SalesOrder
- 30자이내
- 약어를 사용하는 경우에는 전체적으로 표준화 하여 사용 필요
    - Quantity → Qty
- 데이터의 필드 유형을 고려하여 작명

  

### 필드

- Identificater 필드
    - 엔터티의 인스턴스를 고유하게 식별하는 필드
    - 대체적으로 엔터티의 명칭을 사용
        - I\_SalesOrder - SalesOrder

- 코드 필드
    - 고정된 값이며 설정을 통해서 추가/삭제 될수 있는 필드
    - Code를 일부러 넣을 필요 없이 해당 필드의 의미 그대로 사용
        - Currency, Language
    - 비즈니스의미가 들어가는 경우 해당 의미를 추가하여 작명
        - CompanyCurrency

- Indicator 필드
    - true/false 의 값을 가지는 필드
    - 어떤 조건의 필드인지 자세히 필드명에 들어가는 것이 좋다
        - OrderIsReleased

- 금액, 수량 필드
    - 참조하는 필드의 이름을 포함하여 표현
        - AmountInTransactionCurrency

- 카운터
    - 사물의 수를 지정하는 필드
    - NumberOf 를 사용
        - NumberOfSalesOrder

- 기간
    - 기간의 단위가 들어간 이름을 사용
        - TripDurationInDays

- 시간
    - 시간은 일자, 시간, 타임스탬프 유형이 있음
    - 일자를 표현하는 필드는 Date 가 들어가도록 명명
        - CreationDate
    - 시간을 표현하는 필드는 Time 이 들어가도록 명명
        - CreationTime
    - 타임스탬프를 표현하는 필드는 DateTime이 들어가도록 명명
- 비율
    - Percentage나 Fraction은 사용하지 않는다
    - ConditionRateIn**Percent**, Exchange**Rate**, Probability**Ratio** 등을 사용

  

SAP 표준 View에 추가되는 필드들은 다음의 Prefix를 붙여서 명명한다.

- ZZ
- YY

  

  

## VDM 뷰 이름

  

- **<ViewName>Text**
    - 언어기반의 텍스트 뷰
    - I\_TaxCodeText
- **<ViewName>Cube**
    - 분석 큐브 뷰
    - I\_SalesOrderItemCube
- **<ViewName>Query or <ViewName>Qry**
    - 분석 쿼리
    - C\_GoodsMovementQuery
- **<ViewName>ValueHelp or <ViewName>VH**
    - Value Help
    - C\_CustomerMaterialValueHelp
- **<ViewName>ObjPg**
    - Fiori의 Object Page용도 View
    - C\_InboundDeliveryObjPg

  

  

## 파라미터와 Association 명

  

**파라미터**

- P\_ 접두어 사용
- P\_ 다음에서 관련 필드의 이름을 그대로 사용하는 것이 좋다
    - P\_DisplayCurrency

  

#### Association 

- \_로 시작하고 관련 CDS의 이름 부분을 추가해서 생성
    - I\_SalesOrder인 경우에는 \_SalesOrder
- 동일한 CDS를 여러번 Association으로 사용할 경우에는 관련 이름을 추가하여 구분하여 이름을 생성