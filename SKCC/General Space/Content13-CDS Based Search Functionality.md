## Content13-CDS Based Search Functionality

  

SADL (Service Adaptation Definition Langage)에 의해 제공되는 다음의 기능에 대해서 설명

- Value Help의 자율텍스트 검색

  

분석쿼리의 Valude Help는 Foreign Key 관계에 의한 Value Help를 제공하므로 자율텍스트 검색을 제공하지 않는다.

  

## Value Help 모델링하기

  

- Value Hep의 이름은 마지막 부분에 ValueHelp 또는 VH가 들어간 이름으로 명명하는 것이 좋다

  

Value Help View를 다음과 같이 두개를 생성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Source 1'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.representativeKey: 'FieldA1'
define view entity ZRDS_I_VALUEHELP01
  as select distinct from t000
{
      @EndUserText.label: 'FieldA1'
  key 'VH1_A1' as FieldA1,

      @Consumption.valueHelpDefinition: [{
        entity.name: 'ZRDS_I_VALUEHELP02',
        entity.element: 'FieldB1'
       }]
      @EndUserText.label: 'FieldA2'
      'VH2_A1' as FieldA2
}
union select distinct from t000
{
  key 'VH1_A2' as FieldA1,

      'VH2_A2' as FieldA2
}
union select distinct from t000
{
  key 'VH1_A3' as FieldA1,

      'VH2_A3' as FieldA2
}
```

![](Files/image%20264.png)  

- @ObjectModel.dataCategory: #VALUE\_HELP 
    - Value Help임을 지정
- @ObjectModel.representitiveKey: ‘<Field>’

    - Value Help에서 사용될 값을 지정
- @Consumption.valueHelpDefinition.entity.name
    - FieldA1에 사용할 ValueHelp 뷰
- @Consumption.valueHelpDefinition.entity.element
    - FieldA1에 값과 바인딩되는 Value Help의 필드

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Source 1'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.representativeKey: 'FieldB1'
define view entity ZRDS_I_VALUEHELP02
  as select distinct from t000
{
      @Consumption.valueHelpDefault.binding.usage: #FILTER_AND_RESULT
      @EndUserText.label: 'Field B1'
  key 'VH2_A1' as FieldB1,

      @Consumption.valueHelpDefault.display: true
      @EndUserText.label: 'Field B2'
      'VH2_B1' as FieldB2
}
union select distinct from t000
{
  key 'VH2_A2' as FieldB1,

      'VH2_B2' as FieldB2
}
union select distinct from t000
{
  key 'VH2_A3' as FieldB1,

      'VH2_B3' as FieldB2
}
```

![](Files/image%20265.png)  

- @Consumption.valueHelpDefault.binding.usage:#FILTER\_AND\_RESULT
    - FieldB1이 목록을 필터링 하는데 사용되며, 레코드를 선택할 때 리턴값으로 사용됨을 의미
- @Consumption.valueHelpDefault.display:true
    - 기본적으로 위의 어노테이션이 없는 경우에 모든 필드가 Value Help에 표시
    - 특정 필드만 보이게 하려면 위의 어노테이션을 설정해야 한다
        - FieldB2에 해당 어노테이션을 설정하였고 FieldB3에는 설정하지 않았으므로 화면에는 FieldB3는 보이지 않는다

  

다음으로 ValueHelp를 사용할 뷰의 데이터 소스를 생성한다.

- 테스트 용도로 값을 직접 입력해서 작성하였다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Source'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_I_DATASOURCE01
  as select distinct from t000
{
  key 'K1'     as KeyField,

      'VH1_A1' as FieldA,

      'VH2_A1' as FieldB
}
union select distinct from t000
{
  key 'K2'     as KeyField,

      'VH1_A3' as FieldA,

      'VH2_A3' as FieldB
}
union select distinct from t000
{
  key 'K3'     as KeyField,

      'VH1_A3' as FieldA,

      'VH2_A3' as FieldB
}
```

![](Files/image%20266.png)  

  

위의 데이터소스를 이용해서 화면에 대한 어노테이션이 들어간 Consumption 뷰를 다음과 같이 선언한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help를 가지는 CDS 뷰'
@Metadata.ignorePropagatedAnnotations: false
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_C_VIEW_WITH_VH01
  as select from ZRDS_I_DATASOURCE01
  association [0..1] to ZRDS_I_VALUEHELP01 as _ValueHelp01 on $projection.FieldA3 = _ValueHelp01.FieldA1
{
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
  key KeyField,

      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      @UI.selectionField: [{ position: 10 }]
      @Consumption.valueHelpDefinition: [{
        entity: {
          name: 'ZRDS_I_VALUEHELP01',
          element : 'FieldA1'
        }
       }]
      FieldA,

      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      @UI.selectionField: [{ position: 20 }]
      @Consumption.valueHelpDefinition: [{
        association: '_ValueHelp01'
       }]
      FieldA as FieldA3,

      @UI.lineItem: [{importance: #HIGH}]
      @UI.selectionField: [{position:30}]
      @Consumption.valueHelpDefinition: [{
        entity : {name : 'ZRDS_I_VALUEHELP01', element : 'FieldA1'},
        additionalBinding: [{ localElement : 'FieldB1',
                              element      : 'FieldA2',
                              usage        : #RESULT }]}]
      FieldA as FieldA4,

      @UI.lineItem: [{ importance: #HIGH }]
      @UI.selectionField: [{ position: 40 }]
      @Consumption.valueHelpDefinition: [{
        entity : {
          name : 'ZRDS_I_VALUEHELP02'
        }
       }]
      FieldB as FieldB1,

      _ValueHelp01
}
```

  

서비스로 제공하기 위해서 먼저 Service Definition을 생성한다.

```
@EndUserText.label: 'Search 테스트'
define service ZRDS_SD_SEARCH01 {
  expose ZRDS_C_VIEW_WITH_VH01;
}
```

  

OData V4로 Service Binding을 생성한다.

![](Files/image%20267.png)  

  

Service Binding을 활성화 한다.

![](Files/image%20268.png)  

  

Service를 Publish 한다. → /IWFND/V4\_ADMIN 

- Cloud S4HANA 에서는 바로 Publish 할 수 있지만 onPremise 환경에서는 오류가 난다.

![](Files/image%20269.png)  

![](Files/image%20270.png)  

![](Files/image%20271.png)  

![](Files/image%20272.png)  

  

Preview를 통해서 테스트를 한다.

![](Files/image%20273.png)  

  

FieldA는 다음과 같이 정상적으로 Value Help가 뜨는걸 확인할 수 있다.

![](Files/image%20274.png)  

  

FieldA3은 Value Help가 Association관계로 된 항목인데 다음과 같이 원하는 Value Help가 나오지 않는다. association 관계의 항목은 반드시 서비스에 해당 Value Help 뷰를 추가해 줘야 한다.

![](Files/image%20275.png)  

Service Defintion을 다음과 같이 수정한다.

```
@EndUserText.label: 'Search 테스트'
define service ZRDS_SD_SEARCH01 {
  expose ZRDS_C_VIEW_WITH_VH01;
  expose ZRDS_I_VALUEHELP01;
}
```

다시 실행을 하게 되면 다음과 같이 정상적으로 Value Help가 출력됨을 알 수 있다.

![](Files/image%20276.png)  

  

FieldA4는 Value Help에서 값을 고르면 FieldB1에 선택된 값이 바인딩되어 있어서 같이 입력됨을 볼 수 있다.

![](Files/image%20277.png)  

![](Files/image%20278.png)  

  

  

### OData 서비스를 이용한 Free-Text Search 기능

  

다음과 같이 Free-Text Search를 테스트하기 위해서 @Search 기능이 들어가 있는 뷰를 하나 생성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Free Text Search'
@Metadata.ignorePropagatedAnnotations: false
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@Search.searchable: true
define view entity ZRDS_C_FREE_TEXT01
  as select from ZSD_I_SALESORDERITEM
{
      @UI.hidden: true
  key SalesOrderItemId,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.4
      @Search.ranking: #HIGH
      @UI.lineItem: [{ position: 10 }]
      _Head.SalesOrder,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      @Search.ranking: #HIGH
      @UI.lineItem: [{ position: 20 }]
      SalesOrderItem,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 1
      @Search.ranking: #MEDIUM
      @UI.lineItem: [{ position: 30 }]
      _Product.Product,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 1
      @Search.ranking: #MEDIUM
      @UI.lineItem: [{ position: 40 }]
      _Product.ProductType,

      @UI.lineItem: [{ position: 50 }]
      OrderQuantity,

      @UI.lineItem: [{ position: 60 }]
      OrderQuantityUnit
}
```

  

위의 뷰를 서비스로 제공하기 Service Defintion과 Service Binding을 생성한다.

  

#### Service Definition

```
@EndUserText.label: 'Free Text Search 테스트'
define service ZRDS_SD_FREE_TEXT01 {
  expose ZRDS_C_FREE_TEXT01;
}
```

  

#### Service Binding

![](Files/image%20279.png)  

![](Files/image%20280.png)  

  

“Preview”를 실행한다.

![](Files/image%20281.png)  

먼저 그냥 실행해 본다.

![](Files/image%20282.png)  

  

Free Text Search에 PT1만 넣어보고 실행하고 PT2만 넣고 실행한다

![](Files/image%20283.png)  

![](Files/image%20284.png)  

  

Free Text Search에 “FER”을 입력할때와 “FERT”를 입력할때의 결과를 확인해 본다.

![](Files/image%20285.png)  

![](Files/image%20286.png)