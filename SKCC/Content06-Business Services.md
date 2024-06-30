## Content06-Business Services

  

비즈니스서비스 모델링을 통해서 SAP Fiori UI 및 OData 서비스를 제공하게 된다.

  

### Projection View

- 외부의 필요에 맞게 CDS뷰를 조정하여 서비스에 특화되도록 만든 View
- Restful ABAP Programming에서 도입
- 트랜젝션 처리에 대한 처리 지원

  

### Service Definition

- 서비스로 제공할 엔터티의 모음
- 서비스 제공 범위를 결정

  

### Service Binding

- 서비스 프로토콜 정의
- ODATA V2/V4/웹API

  

**분석쿼리뷰**는 OData로 제공하기 위해서는 반드시 @OData.publish를 추가해 줘야 한다.

  

  

## Projection View

  

- CDS 모델의 **서비스 인터페이스 정의**
- **RAP 모델**에서 사용

  

### 일반 CDS와 Projection CDS의 차이

- Projection CDS는 데이터소스의 단순한 Projection만 정의
- Projection 뷰는 데이터소스로 사용 불가
- 뷰의 계층구조의 최상위에만 위치
- 반드시 Composition 관계의 View를 사용하여 만들어야 한다
    - Root View와 관련된 여러개의 Child View로 이루어진 형태

  

  

### 다음은 두개의 CDS의 Composition 관계를 맺는 예제이다.

  

먼저 Parent에 대한 Root CDS뷰를 생성한다.

![](Files/image%20149.png)  

![](Files/image%20150.png)  

다음과 같이 CDS를 작성한다.

```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composition'
define root view entity ZCM_EXAMPLE79
  as select from ztsd0040
  composition [0..*] of ZCM_EXAMPLE80 as _Text
{
  key productid   as ProductId,

      product     as Product,

      producttype as ProductType,

      createdat   as CreatedAt,

      _Text
}
```

  

다음으로 Child CDS를 생성한다.

![](Files/image%20151.png)  

![](Files/image%20152.png)  

Child 뷰의 소스를 다음과 같이 작성한다.

```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composition'
define view entity ZCM_EXAMPLE80
  as select from ztsd0041
  association to parent ZCM_EXAMPLE79 as _Product on $projection.ProductId = _Product.ProductId
{
  key producttextid as ProductTextId,

      productid     as ProductId,

      language      as Language,

      productname   as ProductName,

      _Product
}
```

  

두개의 뷰를 동시에 활성화 한다.

![](Files/image%20153.png)  

  

위의 Composition 관계의 뷰를 이용해서 서비스로 제공하기 위한 Projection View를 생성한다.

Projection뷰도 동일하게 Composition 관계를 맺어야 하므로 먼저 Root Projection View를 생성한다.

![](Files/image%20154.png)  

![](Files/image%20155.png)  

Root Projection View의 소스를 다음과 같이 작성한다.

```
@EndUserText.label: 'Projection'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZCM_EXAMPLE81
  as projection on ZCM_EXAMPLE79
{
  key ProductId,

      @Consumption.valueHelpDefinition: [
        { entity : { name: 'ZCM_EXAMPLE83', element: 'Product' } } ]
      @ObjectModel.text.element: ['ProductName']
      Product,

      ProductType,

      @Semantics.text: true
      _Text[ 1:Language = $session.system_language ].ProductName,

      CreatedAt,

      /* Associations */
      _Text : redirected to composition child ZCM_EXAMPLE82
}
```

  

다음으로 Child Projection View를 생성한다.

![](Files/image%20156.png)  

![](Files/image%20157.png)  

```
@EndUserText.label: 'Projection'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZCM_EXAMPLE82
  as projection on ZCM_EXAMPLE80
{
  key ProductTextId,
      
      @Semantics.language: true
      Language,
      
      ProductId,
      
      @ObjectModel.text.element: [ 'ProductName' ]
      _Product.Product,
      
      @Semantics.text: true
      ProductName,
      
      /* Associations */
      _Product : redirected to parent ZCM_EXAMPLE81
}
```

  

다음으로 Value Help로 사용할 CDS를 선언한다.

![](Files/image%20158.png)  

![](Files/image%20159.png)  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - 자재'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true
define view entity ZCM_EXAMPLE83
  as select from ZSD_I_PRODUCT
{
  key ProductId,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @ObjectModel.text.element: [ 'ProductName' ]
      Product,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      _Text[1:Language=$session.system_language].ProductName
}
```

  

위에서 생성한 3개의 뷰를 한번에 활성화 한다.

![](Files/image%20160.png)  

  

  

## Service Definitions

- 서비스로 제공할 엔터티를 정의
- 여러곳에서 사용할 서비스를 공통으로 만드는 것 보다 각각 필요한 곳마다 서비스를 정의해서 사용하는걸 추천 
    - UI5 앱을 만드는 경우 해당 앱에서만 사용할 Entity를 모아서 하나의 Service Definition으로 정의

  

### 생성예제

먼저 개발오브젝트 생성 위저드를 실행하여 다음과 같이 Service Definition을 선택한다.

![](Files/image%20161.png)  

  

Service Definition 이름과 내역 그리고 대표 엔터티를 입력한다

![](Files/image%20162.png)  

![](Files/image%20163.png)  

```
@EndUserText.label: 'Service Definition'
define service ZSD0010 {
  expose ZCM_EXAMPLE81;
}
```

  

추가적으로 필요한 서비스를 추가한다.

```
@EndUserText.label: 'Service Definition'
define service ZSD0010 {
  expose ZCM_EXAMPLE81;
  expose ZCM_EXAMPLE82;
}
```

  

엔터티타입의 이름을 변경하기 위해서 다음과 같이 어노테이션을 통해서 설정이 가능하다.

- 외부로 제공할 서비스의 엔터티 및 엔터티Set 명은 어노테이션을 통해서 변경 가능하다

![](Files/image%20164.png)  

  

## Service Binding

  

- 서비스를 제공할 프로토콜을 정의

  

### 생성예제

  

개발오브젝트생성화면에서 Service Binding을 선택한다.

![](Files/image%20165.png)  

  

생성할 서비스바인딩의 정보를 입력한다.

- 서비스는 UI와 API 용도로 나뉘어 있다
- 예제는 특별한 Naming Rule은 없지만 나중에 UI / API등을 이름에 추가하여 구분해 주는 것이 좋다

![](Files/image%20166.png)  

  

서비스바인딩 유형 (Binding Type)에는 다음의 항목이 존재한다.

|     |     |     |
| --- | --- | --- |
| **바인딩유형** | **프로토콜** | **내역** |
| OData V2 - UI | OData V2 | UI 서비스 |
| OData V4 - UI | OData V4 | UI 서비스 |
| OData V2 - Web API | OData V2 | Remote API 서비스 |
| OData V4 - Web API | OData V4 | Remote API 서비스 |
| INA - UI | Information Access (InA) | UI에 사용할 분석쿼리 서비스 |

  

서비스이름이 Service Defintion을 기반으로 생성된다.

![](Files/image%20167.png)  

  

서비스이름을 변경하고자 하는 경우에는 서비스를 클릭하고 나오는 오르쪽화면에서 Service name을 수정하면 된다.

![](Files/image%20168.png)  

  

서비스를 사용하게 하기 위해서는 해당 서비스를 Publish (로컬)한다. 먼저 Activation을 하면 Publis 버튼이 활성화 되고 해당 버튼을 누르면 된다.

![](Files/image%20169.png)  

![](Files/image%20170.png)  

Publish를 하다 오류가 나는 경우 (On-premise) SAP GUI에서 “/IWFND/V4\_ADMIN” T-CODE를 실행해서 Publish를 하면 된다.

![](Files/image%20171.png)  

![](Files/image%20172.png)  

![](Files/image%20173.png)  

![](Files/image%20174.png)  

![](Files/image%20175.png)  

  

Publish가 되면 다음과 같이 화면이 출력된다. T-CODE를 통해서 PUBLISH를 한 경우에는 화면을 REFRESH 해서 확인한다.

![](Files/image%20176.png)  

  

  

## 간단한 테스트를 위한 UI Annotation 추가

  

간단한 UI Annotation을 Projection 뷰에 추가한다.

```
@EndUserText.label: 'Projection'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@OData.entityType.name: 'ProductEntityType'
define root view entity ZCM_EXAMPLE81
  as projection on ZCM_EXAMPLE79
{
  key ProductId,

      @Consumption.valueHelpDefinition: [
        { entity : { name: 'ZCM_EXAMPLE83', element: 'Product' } } ]
      @ObjectModel.text.element: ['ProductName']
      @EndUserText.label: '제품'
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @UI.selectionField: [{ position: 10, element: 'Product' }]
      Product,

      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      @UI.selectionField: [{ position: 20, element: 'ProductType' }]
      ProductType,

      @Semantics.text: true
      _Text[ 1:Language = $session.system_language ].ProductName,

      CreatedAt,

      /* Associations */
      _Text : redirected to composition child ZCM_EXAMPLE82
}
```

  

Service Binding에서 “Preview”를 실행한다.

![](Files/image%20177.png)  

![](Files/image%20178.png)