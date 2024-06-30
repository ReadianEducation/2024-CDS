## Content08-Modeling Application Data

  

## 필드라벨

  

**Annotation : @EndUserText.label: ‘<field label>’**

필드라벨은 다음과 같은 결정로직을 통해서 결정된다

- Annotation이 설정이 되지 않은 필드는 Data Element의 텍스트를 사용한다
- @EndUserText.label 을 통해서 라벨을 변경할 수 있음
- Propagated 되는 경우에는 기존 라벨이 적용됨
- 기술적으로 다른 데이터엘리먼트로 CAST되는 경우에는 해당 Data Element의 라벨이 적용됨
- 항상 Active Annotation을 확인해서 어떤 라벨이 반영되어 있는지 확인한다.

  

![](Files/image%20196.png)  

  

  

## 필드 Semantics

  

- 데이터의 비즈니스적인 의미를 주어서 해당 데이터를 사용하는 환경에서 좀 더 쉽고 편하게 기능을 제공하는데 사용된다.
- 기존에 통화필드 정도의 의미 부여 레벨에서 많은 데이터에 의미를 부여할 수 있는 다양한 기능을 CDS에서 annotaion을 통해 제공한다.

  

### 수량과 금액

- 단위 (Unit) 필드 
    - @Semantics.unitOfMeasure: true 
- 수량 (Quanitty) 필드
    - @Semantics.quantity.unitOfMeasure: ‘<unit field>’
- 통화 (Currency) 필드
    - @Semantics.currencyCode: true
- 금액 (Amount) 필드
    - @Semantics.amount.currencyCode: ‘<currency field>’

  

  

## Aggregation

  

- SELECT 구문에서 원하는 필드에 Aggregation 지정
    - 숫자필드 - summation or average 계산
    - 정렬가능 필드 - maximum or minimum
    - 모든필드 - 중복데이터제거 데이터 개수
- 필드별로 필요한 Aggregation 방식이 있으며 해당 설정을 하는 것을 의미
- 특정 기준으로 집계를 할 때 해당 설정을 이용해서 필드를 Aggregation 할 때 사용된다.

  

지원되는 Aggregation 유형은 다음과 같다.

- @Aggregation.default 어노테이션 사용

|     |     |
| --- | --- |
| **Aggregation 유형** | **설명** |
| #AVG | 평균을 계산 |
| #COUNT\_DISTINCT | 중복을 제거한 개수를 반환 |
| #FORMULA | 분석쿼리에 사용되는 특별한 수식 |
| #MAX | 최고값 |
| #MIN | 최소값 |
| #NONE | 표준 Aggregation을 사용 안함 |
| #SUM | 합계  |

  

  

## 시스템 시간

|     |     |
| --- | --- |
| **Annotation** | **필드 의미 (Semantics)** |
| @Semantics.systemDateTime.createdAt | 생성일자/시간 - TIMESTAMP or TIMESTAMPL |
| @Semantics.systemDateTime.lastChangedAt | 최근변경일자/시간 - TIMESTAMP or TIMESTAMPL |
| @Semantics.systemDate.createdAt | 생성일자 - DATS |
| @Semantics.systemDate.lastChangedAt | 최근변경일자 - DATS |
| @Semantics.systemTime.createdAt | 생성시간 - TIMS |
| @Semantics.systemTime.lastChangedAt | 최근변경시간 - TIMS |

  

  

## 텍스트와 언어

  

언어 필드

- @Semantics.language

  

텍스트 필드

- @Semantics.text: true

  

  

## 회계년도 정보

  

- 재무회계의 회계년도와 기간에 대한 적절한 서식 지정에 사용

  

|     |     |
| --- | --- |
| **Annotation** | **필드 의미 (Semantics)** |
| @Semantics.fiscal.yearVariant | 회계년도 변형. 회계년도 속성을 정의 |
| @Semantics.fiscal.period | 3자리 숫자의 회계기간 |
| @Semantics.fiscal.year | 4자리 숫자의 회계년도 |
| @Semantics.fiscal.yearPeriod | 회계년도+기간 |
| @Semantics.fiscal.quarter | 1자리 숫자의 회계 분기 |
| @Semantics.fiscal.yearQuarter | 회계년도 + 분기 |
| @Semantics.fiscal.week | 2자리 숫자의 회계주간 |
| @Semantics.fiscal.yearWeek | 회계년도 + 회계주간 |
| @Semantics.fiscal.dayOfYear | 회계년도 일수 |

  

**예제소스**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Semantics - Fiscal'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE85
  as select from I_GLAccountLineItemRawData
{
  key SourceLedger,

  key CompanyCode,

  key FiscalYear,

  key AccountingDocument,

  key LedgerGLLineItem,

      @Semantics.fiscal.year: true
      LedgerFiscalYear,

      @Semantics.fiscal.period: true
      FiscalPeriod,

      @Semantics.fiscal.yearVariant: true
      FiscalYearVariant,

      @Semantics.fiscal.yearPeriod: true
      FiscalYearPeriod
}
```

  

  

## 외례키 관계 (Foreign Key Relations)

  

기존에는 Foregin Key 설정을 DDIC 테이블에서 사용할 때 CHECK TABLE 용도로 사용하고 있었다.

![](Files/image%20197.png)  

  

CDS에서는 위의 같이 CHECK 뷰가 아니라 연관 관계에 대한 의미론적 표기를 나타낸다. 

- Foregin Key 관계를 맺기 위해서는 다음의 Annotation을 해당 필드에 설정 필요
    - @ObjectModel.foreignKey.association
- 관련 Association은 Cardinality가 0..1 이어야 한다 

![](Files/image%20198.png)  

  

**예제코드**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Foreign Key'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE86
  as select from I_ProfitCenter
  association [0..1] to I_Country as _Country on $projection.Country = _Country.Country
{
  key ControllingArea,

  key ProfitCenter,

  key ValidityEndDate,

      @ObjectModel.foreignKey.association: '_Country'
      Country,

      _Country
}
```

  

만일 다중키값의 Foregin key 관계를 설정하기 위해서

- 대표키를 제외한 키필드에 대해서 Association을 설정 및 어노테이션 설정
- 대표키에 해당하는 Association CDS에 다른 키필드로의 Foreign Key 관계 설정

![](Files/image%20199.png)  

다음은 예제소스이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Foreign Key - Multi'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE87
  as select from I_ProfitCenter
  association [0..1] to I_Country as _Country on  $projection.Country = _Country.Country
  association [0..1] to I_Region  as _Region  on  $projection.Country = _Region.Country
                                              and $projection.Region  = _Region.Region
{
  key ControllingArea,

  key ProfitCenter,

  key ValidityEndDate,

      @ObjectModel.foreignKey.association: '_Country'
      Country,

      @ObjectModel.foreignKey.association: '_Region'
      Region,

      _Country,

      _Region
}
```

다음과 같이 대표키인 Region을 제외한 Country의 Association \_Country 및 ObjectModel.foreign 설정이 되어 있다.

다음과 같이 \_Region의 CDS인 I\_Region을 보면 다음과 같이 foreign key 관계가 되어 있음을 볼수 있다.

![](Files/image%20200.png)  

  

이러한 Foreign Key 연관관계의 정합성을 높이기 위해서 각 CDS뷰에 @ObjectModel.representativeKey 설정을 한다.

- 대표키는 해당 CDS의 레코드 하나를 나타내는데 가장 대표적인 키값을 의미
- Foreign Key 관계를 가지는 CDS에는 설정 필요

![](Files/image%20201.png)  

  

  

## 텍스트 관계

  

- 코드값에 대해서 보기 좋은 텍스트로 표시하기 위해서 필요
- 모델내의 필드간에는
    - @ObjectModel.text.element
- 모델간에는
    - @ObjectModel.text.association

  

**모델내의 필드간에 설정 예제**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Text Relation'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE88
  as select from I_Bank
{
  key BankCountry,

      @ObjectModel.text.element: [ 'BankName' ]
  key BankInternalID,

      @Semantics.text: true
      BankName
}
```

  

뷰간 설정 예제

- 텍스트를 나타내는 뷰는 다음의 Annotation이 설정 필요
    - @ObjectModel.dataCategory: #TEXT 
    - @ObjectModel.representativeKey : ‘’
    - 언어 필드 : @Semantics.language : true
    - 텍스트 필드 : @Semantics.text: true

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Text Relation bt Views'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE89
  as select from I_Country
  association [0..*] to I_CountryText as _Text on $projection.Country = _Text.Country
{
      @ObjectModel.text.association: '_Text'
  key Country,

      _Text
}
```

  

텍스트 관련 CDS뷰는 다음과 같이 Annotation이 설정된다.

![](Files/image%20202.png)  

  

  

## Composition 관계

  

- 비지니스적으로 연관이 되어 있는 여러 엔터티가 부모/자식의 관계의 계층구조를 이루는 경우에 사용된다.
- 하나의 Root 엔터티가 있으며 여러개의 하위 엔터티가 존재한다.
- 부모엔터티의 데이터가 삭제되면 관련있는 하위 엔터티의 데이터도 삭제가 되어야 하는 구조
- 기존 사용하던 방식으로 현재는 RAP 프로그래밍 모델을 통해서 해당 관계를 설정하고 Transaction을 처리한다

  

Composition관계의 Association은 다음을 이용해서 설정한다.

- #TO\_COMPOSITION\_PARENT 
    - 부모노드를 가리킨다
- #TO\_COMPOSITION\_CHILD 
    - 자식노드를 가리킨다
- #TO\_COMPOSITION\_ROOT 
    - Root 노드를 가리킨다

  

Root 노드에는 다음의 어노테이션이 설정이 된다.

- @ObjectModel.compositionRoot: true

  

다음은 판매오더관련 View의 Composition 관계를 나타낸다.

![](Files/image%20203.png)  

  

각각의 CDS뷰에서 관련 항목은 다음과 같다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composition-Root'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.compositionRoot: true
define view entity ZCM_EXAMPLE90
  as select from I_SalesOrder
  association [0..*] to ZCM_EXAMPLE91 as _Item on $projection.SalesOrder = _Item.SalesOrder
{
  key SalesOrder,

      SalesOrderType,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _Item
}
```

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composition-Child'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE91
  as select from I_SalesOrderItem
  association [1..1] to ZCM_EXAMPLE90 as _SalesOrder   on  $projection.SalesOrder = _SalesOrder.SalesOrder
                                                       and $projection.SalesOrder = _SalesOrder.SalesOrder
  association [0..*] to ZCM_EXAMPLE92 as _ScheduleLine on  $projection.SalesOrder     = _ScheduleLine.SalesOrder
                                                       and $projection.SalesOrderItem = _ScheduleLine.SalesOrderItem
{
  key SalesOrder,

  key SalesOrderItem,

      @ObjectModel.association.type: [ #TO_COMPOSITION_PARENT, #TO_COMPOSITION_ROOT]
      _SalesOrder,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _ScheduleLine
}
```

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composition-Grand Child'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE92
  as select from I_SalesOrderScheduleLine
  association [1..1] to ZCM_EXAMPLE90 as _SalesOrder     on  $projection.SalesOrder = _SalesOrder.SalesOrder
  association [1..1] to ZCM_EXAMPLE91 as _SalesOrderItem on  $projection.SalesOrder     = _SalesOrderItem.SalesOrder
                                                         and $projection.SalesOrderItem = _SalesOrderItem.SalesOrderItem
{
  key SalesOrder,

  key SalesOrderItem,

  key ScheduleLine,

      @ObjectModel.association.type: [ #TO_COMPOSITION_ROOT ]
      _SalesOrder,

      @ObjectModel.association.type: [ #TO_COMPOSITION_PARENT ]
      _SalesOrderItem
}
```

  

  

## Time-Dependent 데이터

  

- 시간의 흐름에 따라서 값이 변하는 마스터성 데이터
    - 예를 들어 직원의 급여 또는 국가의 부가가치세율

  

다음과 같은 모델링 방법을 사용한다

- 두개의 필드가 각각 @Semantics.businessDate.from: true와 @Semantics.businessDate.to: true가 설정되어야 한다
- 두개의 필드중 하나는 키필드로 구성되어야 한다. 해당 필드를 제외한 키를 엔터티키라고 한다.
- 유효기간이 겹치는 항목이 없어야 한다
- 동일한 엔터티키에 대해서 유효기간의 타임라인에 여러개의 갭이 존재할 수 있다

  

이런정보를 가지고 해당 time-dependent 엔터티를 사용하는 곳에서 필터로 잘 데이터를 가져와서 올바르게 데이터를 처리할 수 있게된다.

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Time-Dependent'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE93
  as select from I_CostCenter
{
  key ControllingArea,

  key CostCenter,

      @Semantics.businessDate.to: true
  key ValidityEndDate,

      @Semantics.businessDate.from: true
      ValidityStartDate
}
```