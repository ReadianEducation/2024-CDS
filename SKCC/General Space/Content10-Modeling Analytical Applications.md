## Content10-Modeling Analytical Applications

  

### Cube 뷰 생성 예제

  

먼저 CDS뷰를 생성한다.

![](Files/image%20222.png)  

![](Files/image%20223.png)  

  

데이터소스를 설정하고 필요한 필드를 추가한다.

```
@AbapCatalog.sqlViewName: 'ZRAP0010'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cube'
define view ZRAP_I_SALES_ORDER_ITEM_CUBE
  as select from I_SalesOrderItem
{
  SalesOrder,

  _SalesOrder,

  SalesOrderItem,

  CreationDate,

  _SalesOrder.SalesOrganization,

  _SalesOrder._SalesOrganization,

  _SalesOrder.SoldToParty,

  _SalesOrder._SoldToParty,

  _SalesOrder._SoldToParty.Country as SoldToCountry,

  Material,

  _Material,

  @Aggregation.default: #SUM
  OrderQuantity,

  OrderQuantityUnit,

  _OrderQuantityUnit,

  @Aggregation.default: #SUM
  NetAmount,

  TransactionCurrency,

  _TransactionCurrency
}
```

  

다음으로 엔터티 헤더에 Cube 뷰에 대한 Annotation을 설정한다.

![](Files/image%20224.png)  

  

다음으로 분석의 측정값으로 사용할 필드(Measure)에 Aggregation 유형을 설정한다. 설정하지 않으면 합계( #SUM )가 기본으로 설정되게 된다.

![](Files/image%20225.png)  

  

데이터가 정상적으로 출력이 되는지 확인한다.

![](Files/image%20226.png)  

  

## 분석뷰 검증 및 테스트 

- Transaction : **RSRTS\_ODP\_DIS**

  

#### 테스트절차

  

먼저 **RSRTS\_ODP\_DIS**를 실행한다.

![](Files/image%20227.png)  

  

ODP Context에서 “ABAP Core Data Services”를 선택하고 SQLViewName을 입력하고 F8을 눌러서 실행한다.

![](Files/image%20228.png)  

  

“Standard Query” 버튼을 눌러서 분석을 시작한다.

![](Files/image%20229.png)  

![](Files/image%20230.png)  

- Characteristics
    - Measure를 그룹화 할 수 있는 필드 리스트
    - 해당 필드를 Dimension 이라고 함

  

  

### Analytical Cube Views

  

- Analytics.dataCategory: #CUBE 
- 집계되는 숫자필드를 제공하여 분석의 중심 데이터소스 역할
    - 최소 하나의 필드는 숫자 (수량, 금액) 이어야 한다
- 필드의 측정값
    - @Aggregation.default 
    - #SUM, #MIN, #MAX 
    - #NONE, #NOP : 해당 필드가 측정 값이 아님을 나타낸다. Aggregation을 하지 말아야 하는 경우에 #NOP를 설정한다
- 측정값이 아닌 필드를 Dimension 이라고 함
    - 필터값으로 사용
    - 그룹핑 및 소계를 하는데 사용

  

  

### Analytical Dimension Views

  

- Analytics.dataCategory: #DIMENSION 
- Dimension 뷰에 필요한 항목
    - Unique Key 필드
    - 뷰의 대표 키
        - @ObjectModel.representativeKey

- Dimension 뷰는 Dimension 필드에 대한 추가 정보를 제공하는 용도로 사용된다
    - 마스터데이터 등이 많이 쓰임
- Dimension 필드에서 Dimension 뷰로는 외래키 연관을 통해서 이루어 진다
- Dimension View의 장점
    - Dimension이 코드 또는 ID 값을 가지는 경우가 많으므로 추가적으로 설명할 텍스트등을 제공
    - Value Help로 Dimension이 사용이 되는 경우에 사용
- Cube 뷰 대신에 분석의 데이터소스로 사용 가능
    - 측정값으로는 레코드 수가 이용
    - 측정값이 있는 경우에 측정값 분석도 가능

  

#### Dimension View 예제

```
@AbapCatalog.sqlViewName: 'ZRAP0020'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '판매조직'
@Analytics.dataCategory: #DIMENSION
@ObjectModel.representativeKey: 'SalesOrganization'
define view ZRAP_I_SALES_ORGANIZATION
  as select from I_SalesOrganization
  association [0..*] to ZRAP_I_SALES_ORGANIZATION_TEXT as _Text on $projection.SalesOrganization = _Text.SalesOrganization
{
      @ObjectModel.text.association: '_Text'
  key SalesOrganization,

      SalesOrganizationCurrency,

      _Text
}
```

- Dimension 뷰
    - @Analytics.dataCategory: #DIMENSION 
- 키값 설정 필요
- 행을 고유하게 만드는 키를 대표키로 설정
    - @ObjectModel.representativeKey: ‘SalesOrganization’
- 언어별 텍스트를 나타내는 CDS에 대한 연결 설정 - 뷰에 텍스트 필드가 없는 경우에 사용
    - @ObjectModel.text.association: ‘\_Text’

  

#### Text 뷰 예제

```
@AbapCatalog.sqlViewName: 'ZRAP0030'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '판매조직 텍스트'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'SalesOrganization'
define view ZRAP_I_SALES_ORGANIZATION_TEXT
  as select from I_SalesOrganizationText
  association [0..1] to ZRAP_I_SALES_ORGANIZATION as _SalesOrganization on $projection.SalesOrganization = _SalesOrganization.SalesOrganization
  association [0..1] to I_Language                as _Language          on $projection.Language = _Language.Language
{
      @ObjectModel.foreignKey.association: '_SalesOrganization'
  key SalesOrganization,

      @ObjectModel.foreignKey.association: '_Language'
      @Semantics.language: true
  key Language,

      @Semantics.text: true
      SalesOrganizationName,

      _SalesOrganization,

      _Language
}
```

- 키값 설정 필요
- 텍스트 뷰임을 설정
    - @ObjectModel.dataCategory: #TEXT 
- 텍스트를 나타내는 필드 설정
    - @Semantics.text: true
- 언어를 나타내는 필드 설정
    - @Semantics.language: true

  

#### Cube뷰에서 Dimension 그리고 Text 뷰까지의 연관관계는 다음과 같이 설정된다.

![](Files/image%20231.png)  

- Dimension 뷰와의 외래키 관계는 반드시 ON 조건에 Dimension뷰의 키값을 모두 할당해야 한다

  

  

### Cube View 예제

  

다음과 같이 판매오더 아이템에 대해서 CUBE 뷰를 만들어서 테스트 해본다.

```
@AbapCatalog.sqlViewName: 'ZRAP0010'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cube'
@Analytics.dataCategory: #CUBE
define view ZRAP_I_SALES_ORDER_ITEM_CUBE
  as select from I_SalesOrderItem
{
  @ObjectModel.foreignKey.association: '_SalesOrder'
  SalesOrder,

  _SalesOrder,

  SalesOrderItem,

  CreationDate,

  @ObjectModel.foreignKey.association: '_SalesOrganization'
  _SalesOrder.SalesOrganization,

  _SalesOrder._SalesOrganization,

   @ObjectModel.foreignKey.association: '_SoldToParty'
  _SalesOrder.SoldToParty,

  _SalesOrder._SoldToParty,

  _SalesOrder._SoldToParty.Country as SoldToCountry,

  @ObjectModel.foreignKey.association: '_Material'
  Material,

  _Material,

  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
  OrderQuantity,

  @ObjectModel.foreignKey.association: '_OrderQuantityUnit'
  @Semantics.unitOfMeasure: true
  OrderQuantityUnit,

  _OrderQuantityUnit,

  @Aggregation.default: #SUM
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  NetAmount,
  
  @ObjectModel.foreignKey.association: '_TransactionCurrency'
  @Semantics.currencyCode: true
  TransactionCurrency,

  _TransactionCurrency
}
```

  

#### Transaction RSRTS\_ODP\_DIS 를 실행하고 해당 뷰를 넣어 실행한다.

![](Files/image%20232.png)  

![](Files/image%20233.png)  

- KEY 가 없어서 비어 있음
- UNIT : 수량과 통화 단위가 표시
- 주요 지표 : Mesure 필드

  

InfoObject Name : 분석인프라가 필드를 고유하게 구분하는데 사용

![](Files/image%20234.png)  

  

필드/필드명

![](Files/image%20235.png)  

  

Foreign Key 관계

![](Files/image%20236.png)  

  

Text 필드가 존재

![](Files/image%20237.png)  

  

Dimension 필드에 대해서 계층구조가 존재

![](Files/image%20238.png)  

  

Check metadata

- 분석모델의 메타데이터를 체크

![](Files/image%20239.png)  

![](Files/image%20240.png)  

  

  

## Analytical Queries

  

- Cube뷰의 분석모델을 기반으로 다양한 분석 평가와 계산을 분석쿼리로 정의하여 사용자에게 제공
- 하나의 분석모델은 분석쿼리를 통해서 여러번 재사용 될 수 있다
- 초기설정
    - 화면 표시를 위한 초기 레이아웃 설정
        - 출력할 행/열 및 텍스트, 서브합계, 정렬등을 설정
    - 데이터의 초기 선택 기준
        - 필터와 변수의 초기 값을 현재 컨텍스트 (현재날짜)를 기반으로 설정

- 이러한 초기 설정은 사용자가 언제든지 변경이 가능하다
- 쿼리의 진정한 힘은 계산 능력에 있음
    - 모델 자체에는 없는 새로운 측정값을 정의 가능
    - 공식을 정의해서 제한된 측정값을 가지고 비즈니스 요구를 충족하는 평가를 생성할 수 있음

  

### 분석쿼리 정의하기

  

- CDS View 또는 View 엔터티로 정의
- 분석 Cube 또는 Dimension 뷰를 데이터 소스로 사용
- @Analytics.query: true

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 1 - 판매오더 아이템 분석'
@Analytics.query: true
define view entity ZQUERY01
  as select from ZRAP_I_SALES_ORDER_ITEM_CUBE
{
  Material,

  SoldToParty,

  SoldToCountry,

  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency

}
```

  

테스트를 위해서 Query의 테스트 환경을 실행하는 T-Code : RSRT를 실행한다.

![](Files/image%20241.png)  

  

질의 필드에 2C를 입력하고 바로 CDS뷰 이름을 입력하고 엔터키를 누른다.

![](Files/image%20242.png)  

![](Files/image%20243.png)  

  

기본값을 유지하고 “ABAP BICS”를 선택하고 “실행”버튼을 누른다.

![](Files/image%20244.png)  

  

레이아웃을 변경한다.

- Dimension 필드의 텍스트나 속성을 화면에 표시
- Dimension 필드를 행/열로 그룹핑
- Dimension 필드로 필터링

![](Files/image%20245.png)  

  

  

### 쿼리의 초기 레이아웃 설정하기

  

- 어노테이션을 이용해서 초기 레이아웃을 설정할 수 있다

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 2 - 판매오더 아이템 분석'
@Analytics.query: true
define view entity ZQUERY02
  as select from ZRAP_I_SALES_ORDER_ITEM_CUBE
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  Material,
  
  @AnalyticsDetails.query.axis: #COLUMNS
  _SalesOrganization._Text.SalesOrganizationName,
  
  @AnalyticsDetails.query.totals: #SHOW
  SoldToParty,
  
  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,
  _SoldToParty.CityName,
  
  @EndUserText.label: 'Country of Sold-To Party'
  SoldToCountry,

  @AnalyticsDetails.query.hidden: true
  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency

}
```

  

RSRT를 통해서 실행하면 다음과 같은 화면을 출력한다.

![](Files/image%20246.png)  

  

사용된 Annotation의 의미는 다음과 같다

|     |     |
| --- | --- |
| **어노테이션** | **레이아웃** |
| @AnalyticsDetails.query.axis: #ROWS <br>@AnalyticsDetails.query.axis: #COLUMNS | Dimension 또는 Measure를 행 또는 열로 표시 |
| @AnalyticsDetails.query.totals: #SHOW | 합계 또는 서브합계를 Dimension 필드에 대해서 표시 |
| @AnalyticsDetails.query.display: #KEY <br>@AnalyticsDetails.query.display: $TEXT <br>@AnalyticsDetails.query.display: $TEXT\_KEY | ID 와 TEXT를 표시하는 방법 설정 |
| @AnalyticsDetails.query.sortDirection: #ASC <br>@AnlayticsDetails.query.sortDirection: #DESC | 정렬  |
| @EndUserText.label: ‘<text>’ | 기본 필드 라벨 |
| @AnalyticsDetails.query.hidden: true | Measure를 초기에 숨긴다 |

  

#### 변수를 사용해서 쿼리를 생성하는 예제

먼저 사용할 CUBE 뷰를 생성한다.

```
@AbapCatalog.sqlViewName: 'ZCUBE01V'
@EndUserText.label: 'CUBE - 판매오더 아이템'
@Analytics.dataCategory: #CUBE
define view ZCUBE01
  as select from I_SalesOrderItem
  association [0..1] to I_CalendarDate as _CreationDate on $projection.CreationDate = _CreationDate.CalendarDate
{
  SalesOrder,

  _SalesOrder,

  SalesOrderItem,

  CreationDate,

  _CreationDate.CalendarYear       as CreationYear,

  _CreationDate._CalendarYear      as _CalendarYear,

  _CreationDate.WeekDay            as CreationWeekDay,

  _CreationDate._WeekDay           as _WeekDay,

  _SalesOrder.SalesOrganization,

  _SalesOrder._SalesOrganization,

  _SalesOrder.SoldToParty,

  _SalesOrder._SoldToParty,

  _SalesOrder._SoldToParty.Country as SoldToCountry,

  Material,

  _Material,

  @Aggregation.default: #SUM
  OrderQuantity,

  OrderQuantityUnit,

  _OrderQuantityUnit,

  @Aggregation.default: #SUM
  NetAmount,

  TransactionCurrency,

  _TransactionCurrency
}
```

  

Cube 뷰를 이용해서 변수를 이용한 분석쿼리를 생성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 3 - 판매오더 아이템 with 변수'
@Analytics.query: true
define view entity ZQUERY03
  with parameters
    @Consumption.hidden: true
    @Environment.systemField: #SYSTEM_DATE
    P_Today   : abap.dats,
    @EndUserText.label: 'Country of Sold-To Party'
    @Consumption.valueHelpDefinition: [{ entity.name : 'I_Country', entity.element : 'Country' }]
    @Consumption.defaultValue: 'US'
    P_Country : land1_gp
  as select from ZCUBE01
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  @Consumption.filter: { selectionType: #RANGE, multipleSelections: true }
  Material,

  @AnalyticsDetails.query.axis: #COLUMNS
  _SalesOrganization._Text.SalesOrganizationName,

  @AnalyticsDetails.query.totals: #SHOW
  @Consumption.filter: { selectionType: #SINGLE, multipleSelections: true }
  SoldToParty,

  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,
  _SoldToParty.CityName,

  @EndUserText.label: 'Country of Sold-To Party'
  SoldToCountry,

  @AnalyticsDetails.query.axis: #ROWS
  @Consumption.filter: { selectionType: #INTERVAL, multipleSelections: false }
  @Consumption.derivation: {
    lookupEntity: 'I_CalendarDate',
    resultElement: 'CalendarYear',
    binding: [{ targetElement: 'CalendarDate', type: #PARAMETER, value: 'P_Today' }]
  }
  CreationYear,

  @AnalyticsDetails.query.hidden: true
  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency
}
where
  SoldToCountry = $parameters.P_Country
```

  

위의 쿼리를 실행하면 다음과 같이 팝업창이 뜬다.

![](Files/image%20247.png)  

- Country of Sold-To Party는 필수 필드
- 역년은 기본값을 가져와서 설정하며 사용자가 바꿀 수 있다

  

조건을 입력하고 실행하면 다음과 같다

![](Files/image%20248.png)  

  

#### 시스템변수

![](Files/image%20249.png)  

  

#### Variable derivation

- @Consumption.derivation 
- 분석 인프라에서 지원되는게 아니라 ABAP 어플리케이션에서 지원되는 기능
- **보조 뷰의 매개변수 또는 필드의 필터**로 하나 이상의 값을 사용해서 해당 뷰에서 값을 가져와서 결과값을 필드에 입력 
- derivation은 사용자에게 변수 화면이 표시되기 전에 실행

![](Files/image%20250.png)  

  

  

### Measure를 계산하여 화면에 표시

  

CUBE 뷰에 필드 계산 로직이 다음과 같이 들어간다.

```
@AbapCatalog.sqlViewName: 'ZCUBE01V'
@EndUserText.label: 'CUBE - 판매오더 아이템'
@Analytics.dataCategory: #CUBE
define view ZCUBE01
  as select from I_SalesOrderItem
  association [0..1] to I_CalendarDate as _CreationDate on $projection.CreationDate = _CreationDate.CalendarDate
{
  SalesOrder,

  _SalesOrder,

  SalesOrderItem,

  CreationDate,

  _CreationDate.CalendarYear                                     as CreationYear,

  _CreationDate._CalendarYear                                    as _CalendarYear,

  _CreationDate.WeekDay                                          as CreationWeekDay,

  _CreationDate._WeekDay                                         as _WeekDay,

  _SalesOrder.SalesOrganization,

  _SalesOrder._SalesOrganization,

  _SalesOrder.SoldToParty,

  _SalesOrder._SoldToParty,

  _SalesOrder._SoldToParty.Country                               as SoldToCountry,

  Material,

  _Material,

  @Aggregation.default: #SUM
  OrderQuantity,

  OrderQuantityUnit,

  _OrderQuantityUnit,

  @Aggregation.default: #SUM
  NetAmount,

  TransactionCurrency,

  _TransactionCurrency,

  @Aggregation.default: #MIN
  cast(division(NetAmount, OrderQuantity, 2) as abap.curr(15,2)) as AmountPerUnitMin,

  @Aggregation.default: #MAX
  cast(division(NetAmount, OrderQuantity, 2) as abap.curr(15,2)) as AmountPerUnitMax
}
where
  OrderQuantity > 0
```

  

위의 cube 뷰를 바탕으로 쿼리를 작성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 3 - 판매오더 아이템 계산'
@Analytics.query: true
define view entity ZQUERY04
  as select from ZCUBE01
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  Material,

  _SalesOrganization._Text.SalesOrganizationName,

  @AnalyticsDetails.query.totals: #SHOW
  SoldToParty,

  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,

  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency,

  @EndUserText.label: 'Minimal Amount per Unit'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  AmountPerUnitMin,

  @EndUserText.label: 'Maximal Amount per Unit'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  AmountPerUnitMax
}
```

  

RSRT를 통해서 테스트를 수행한다.

![](Files/image%20251.png)  

  

  

다음은 Query에 Formula를 적용하여 사용한 예제이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 5 - 판매오더아이템 Formula'
@Analytics.query: true
define view entity ZQUERY05
  as select from ZCUBE01
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  Material,

  _SalesOrganization._Text.SalesOrganizationName,

  @AnalyticsDetails.query.totals: #SHOW
  SoldToParty,

  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,

  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency,

  @EndUserText.label: 'Minimal Amount per Unit'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  AmountPerUnitMin,

  @EndUserText.label: 'Maximal Amount per Unit'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  AmountPerUnitMax,

  @EndUserText.label: 'Average Amount per Unit'
  @AnalyticsDetails.query.formula: 'NetAmount / OrderQuantity'
  0 as AverageAmountPerUnit
}
```

  

RSRT를 통해서 테스트 하면 다음과 같이 결과를 확인할 수 있다.

- 평균값의 단위가 추가되어 화면에 표시된다.

![](Files/image%20252.png)  

  

  

## Restricted Measures

  

- case when 구문을 가지고 조건에 따른 Measure의 값을 결정하는 것
- when구문에는 Measure 필드를 사용하지 못한다.

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 5 - 판매오더아이템 Formula'
@Analytics.query: true
define view entity ZQUERY06
  as select from ZCUBE01
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  Material,

  _SalesOrganization._Text.SalesOrganizationName,

  @AnalyticsDetails.query.totals: #SHOW
  SoldToParty,

  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,

  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency,

  @EndUserText.label: 'MO-WE Quantity'
  @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
  case when CreationWeekDay <= '3' then OrderQuantity
  else cast( 0 as kwmeng )
  end   as MondayToWednesdayQuantity,

  @EndUserText.label: 'TH-SU Quantity'
  @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
  case when CreationWeekDay >= '4' then OrderQuantity
         else cast( 0 as kwmeng )
    end as ThursdayToSundayQuantity,

  @EndUserText.label: 'MO-WE Value'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  case when CreationWeekDay <= '3' then NetAmount
       else cast( 0 as netwr_ap )
  end   as MondayToWednesdayAmount,

  @EndUserText.label: 'TH-SU Value'
  @Semantics.amount.currencyCode: 'TransactionCurrency'
  case when CreationWeekDay >= '4' then NetAmount
       else cast( 0 as netwr_ap )
  end   as ThursdayToSundayAmount
}
```

  

  

## Exception Aggregation

  

- Cube에 선언되지 않은 Aggregation을 예외적으로 적용하여 Query를 만드는 것을 의미

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Query 5 - 판매오더아이템 Formula'
@Analytics.query: true
define view entity ZQUERY07
  as select from ZCUBE01
{
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @AnalyticsDetails.query.display: #KEY_TEXT
  Material,

  _SalesOrganization._Text.SalesOrganizationName,

  @AnalyticsDetails.query.totals: #SHOW
  SoldToParty,

  @AnalyticsDetails.query.sortDirection: #ASC
  _SoldToParty.CustomerName,

  OrderQuantity,

  OrderQuantityUnit,

  NetAmount,

  TransactionCurrency,

  @EndUserText.label: 'Distinct Materials' @AnalyticsDetails: {
    exceptionAggregationSteps: [{
      exceptionAggregationBehavior: #COUNT,
      exceptionAggregationElements: [ 'Material' ]
  }] }
  0 as NumberOfDistinctMaterials
}
```

  

결과는 다음과 같다.

![](Files/image%20253.png)  

  

  

### Definition of an Exception Aggregation

  

![](Files/image%20254.png)  

exception aggregation을 적용하게 되면 다음과 같은 구문으로 변경되는 효과가 있다

![](Files/image%20255.png)  

  

exceptionAggregationSteps가 정의된 경우 SQL의 SELECT 구문에서는 모든 단계의 exceptionAggregationElements를 검색 필드로 추가하고 그룹화 한다. 그룹화 시에는 각 단계의 exceptionAggregationBehavior의 Aggregation 동작에 따라서 집계를 하게 된다.