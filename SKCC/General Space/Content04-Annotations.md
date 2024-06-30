## Content04-Annotations

  

- CDS를 사용하는 측에 정보를 제공
    - ABAP 런타임 환경
    - UI5 Application
    - 분석 엔진

  

## 1\. Annotation 정의

  

- Annotation 정의는 SAP에서 제공하며, 개발오브젝트 DDLA 타입으로 제공

  

### Annotation 정의 부분 확인

  

Eclipse에서 개발오브젝트 찾기를 실행한다. (ctrl + shift + A)

![](Files/image%2089.png)  

다음과 같이 해당 정의 부분을 확인할 수 있다.

![](Files/image%2090.png)  

  

**Semantics.quantity.unitOfMeasure**

- Scope이 \[#ELEMENT\]는 필드 레벨에 사용가능한 Annotation임을 의미

```
 @Scope:[#ELEMENT]
   @CompatibilityContract: {
   c1: { usageAllowed: true,
         allowedChanges.annotation: [ #CUSTOM ],
         allowedChanges.value: [ #NONE ] },
   c2: { usageAllowed: true,
         allowedChanges.annotation: [ #CUSTOM ],
         allowedChanges.value: [ #NONE ] } }
   @API.state: [ #RELEASED_FOR_SAP_CLOUD_PLATFORM, #RELEASED_FOR_KEY_USER_APPS ]
   quantity
   {
       unitOfMeasure   : ElementRef;
       @CompatibilityContract: {
       c1: { usageAllowed: false },
       c2: { usageAllowed: true,
             allowedChanges.annotation: [ #ADD ],
             allowedChanges.value:      [ #NONE ] } }
       unitOfMeasureSapCode   : ElementRef;
       @CompatibilityContract: {
       c1: { usageAllowed: false },
       c2: { usageAllowed: true,
             allowedChanges.annotation: [ #ADD ],
             allowedChanges.value:      [ #NONE ] } }
       unitOfMeasureIsoCode   : ElementRef;
   };
```

  

**Semantics.systemDate.createdAt**

- 기본값은 default 뒤에 나오는 값
- Scope이 지정되지 않은 경우에는 상위 요소로 부터 상속
    - 여기서는 Semantcs로 \[#ELEMENT, #PARAMETER\]

```
   @CompatibilityContract: {
   c1: { usageAllowed: true,
         allowedChanges.annotation: [ #ANY ],
         allowedChanges.value: [ #ANY ] },
   c2: { usageAllowed: false } }
   @API.state: [ #RELEASED_FOR_SAP_CLOUD_PLATFORM, #RELEASED_FOR_KEY_USER_APPS ]
   systemDateTime
   {
       createdAt                  : Boolean default true;
       lastChangedAt              : Boolean default true;
       localInstanceLastChangedAt : Boolean default true;
   };
```

  

주석은 다음과 같이 사용할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Annotation'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE53
  as select from ZSD_I_SALESORDERITEM
{
  key SalesOrderItemId,

      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      OrderQuantity,

      OrderQuantityUnit,

      @Semantics.systemDate.createdAt: true
      CreatedAt
}
```

  

  

### Annotation Name

- 계층구조로 이루어져 있음
    - Semantics.quantity.unitOfMeasure는 도메인 Semantics에서 시작해서 중간에 quantity요소를 거쳐서 unitOfMeasure 요소로 이루어진 이름

  

다음은 많이 사용되는 Annotation 도메인이다.

|     |     |
| --- | --- |
| **Domain** | **Description** |
| ABAPCatalog | ABAP Runtime 환경 및 ABAP DDIC를 제어 |
| AccessControl | CDS 모델의 권한에 대한 정의 및 문서화에 사용 |
| Aggregation | Aggregation을 하는 Key figure로 사용되는 요소를 정의하는데 사용 |
| Analytics | 분석 데이터모델과 어플리케이션을 정의하는데 사용 |
| AnalyticsDetails | 분석쿼리의 표준 레이아웃 및 Aggregation시 적용해야하는 예외설정을 정의 |
| Consumption | CDS 모델을 사용하는 측에 해당 모델을 사용하여 구현할 때 고려해야하는 정보를 제공 |
| EndUserText | 다국어 라벨 텍스트를 정의 |
| Environment | 시스템변수를 가지고 파라미터에 기본 값 또는 로직을 제어하는데 사용 |
| Hierachy | 계층구조의 관계를 정의 |
| Metadata | CDS를 메타데이터 Extension 및 Annotation의 Propagation을 통해서 Extension 할 때 사용 |
| ObjectModel | 데이터모델의 기본적인 구조적 속성을 설명하는데 사용 |
| OData | CDS 모델을 OData 서비스로 제공할 때 사용 |
| Search | 검색 기능을 사용하고자 할 때 사용 |
| Semantics | 필드 및 파라미터의 기본적인 의미를 부여할 때 사용<br> |
| UI  | UI를 생성에 필요한 정보를 설정할 때 사용 |
| VDM | Virtual data model에 CDS모델을 분류할 때 사용 |

  

  

다음은 중요 Annotation에 대한 설명이다

- [참조 Help](https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP_752/cc0c305d2fab47bd808adcad3ca7ee9d/630ce9b386b84e80bfade96779fbaeec.html?version=7.52.3&locale=en-US "https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP_752/cc0c305d2fab47bd808adcad3ca7ee9d/630ce9b386b84e80bfade96779fbaeec.html?version=7.52.3&locale=en-US")

|     |     |     |
| --- | --- | --- |
| **Name** | **Description** | **Runtime**<br> |
| AccessControl.authorizationCheck | CDS뷰의 권한 검사 여부 | \-  |
| AccessControl.privilegedAssociations | Assocition 접근시 Access Control을 Skip하기 위해 필요한 설정. 일반적인 SELECT 문에서는 적용되지 않음 | SADL (OData) |
| Aggregation.default | 필드의 Aggregation 동작을 정의. KPI 관련 설정시 필요. DefaultAggregation을 대체.<br><br><br>- #AVG <br>- #COUNT\_DISTINCT <br>- #FORMULA <br>- #MAX <br>- #MIN <br>- #NONE <br>- #NOP <br>- #SUM | \-  |
| Analytics.dataCategory | CDS의 분석데이터 범주 정의<br><br><br>- #CUBE <br>- #DIMENSION <br>- #FACT | \-  |
| Analytics.query | CDS가 분석쿼리로 사용이 됨을 표시 | \-  |
| AnalyticsDetails.exceptionAggregationSteps | 집계 예외단계를 정의 | \-  |
| AnalyticsDetails.query.axis | 필드값이 행 또는 열로 표시될지 설정 | \-  |
| AnalyticsDetails.query.display | 필드가 분석쿼리에서 어떻게 표시될 것인지 유형을 정의<br><br><br>- #KEY <br>- #TEXT <br>- #KEY\_TEXT <br>- #TEXT\_KEY | \-  |
| AnalyticsDetails.query.formula | 계산식 정의 | \-  |
| AnalyticsDetails.query.hidden | 화면표시에서 제외 | \-  |
| AnalyticsDetails.query.sortDirection | 정렬기준을 정의 | \-  |
| AnalyticsDetails.query.totals | 필드의 부분합 표시를 제어 | \-  |
| Consumption.dbHints | 소비자가 전달할 데이터베이스 힌트를 지정 | \-  |
| Consumption.defaultValue | 파라미터에 대해서 기본값 정의 | \-  |
| Consumption.derivation | 필드 또는 파라미터에 대한 값의 derivation mechnisme을 정의 | \-  |
| Consumption.filter | 필드를 필터링하는 방법을 정의 | \-  |
| Consumption.hidden | 서비스에서 해당 필드를 노출할지 여부를 설정 | \-  |
| Consumption.valueHelpDefinition.association | 대상 Association을 이용해서 해당 필드의 Value help를 정의 | \-  |
| Consumption.valueHelpDefinition.additionalBinding.element | Value Help 모델의 필드에 링크 | \-  |
| Consumption.valueHelpDefinition.additionalBinding.localElement | Value Help 모델의 필드와 연결되는 Local 필드를 정의 | \-  |
| Consumption.valueHelpDefinition.entity.element | 필드와 연결되는 Value Help모델의 필드를 지정 | \-  |
| Consumption.valueHelpDefinition.entity.name | Value Help의 모델로 사용되는 CDS 뷰를 지정 | \-  |
| EndUserText.label | 매개변수, 필드에 대해서 설명을 정의 | \-  |
| EndUserText.quickInfo | 매개변수, 필드에 대해서 긴 설명을 정의 | \-  |
| Environnment.systemField | 매개변수에 ABAP의 시스템필드로 기본값을 넣고자 하는 경우 사용<br><br><br>- #CLIENT <br>- #SYSTEM\_DATE <br>- #SYSTEM\_TIME <br>- #SYSTEM\_LANGUAGE <br>- #USER <br>- #USER\_DATE <br>- #USER\_TIMEZONE | \-  |
| Hierarchy.parentChild | 부모-자식 계층구조 정의 | \-  |
| MappingRole | CDS 매핑 Role을 정의 | \-  |
| Metadata.allowExtensions | CDS에 메타데이터 Extension을 생성할수 있도록 설정 | \-  |
| Metadata.ignorePropagateAnnotations | 하위 엔터티에서의 Annotation을 가져오지 않도록 설정 | \-  |
| Metadata.layer | 메타데이터 확장의 계층을 할당<br><br><br>- #CORE <br>- #LOCALIZATION <br>- $INDUSTRY<br>- #PARTNER <br>- #CUSTOMER | \-  |
| MetadataExtension.usageAllowed | 메타데이터 확장에 사용가능한 Annotation 정의 | \-  |
| ObjectModel.alternativeKey.element | 대체키 필드를 정의 | \-  |
| ObjectModel.alternativeKey.id | 대체키 식별자를 정의 | \-  |
| ObjectModel.alternativeKey.uniqueness | 대체키의 유일성을 정의 | \-  |
| ObjectModel.dataCategory | CDS모델의 데이터 카탈로그를 정의<br><br><br>- #TEXT <br>- #HIERARCHY <br>- #VALUE\_HELP | \-  |
| ObjectModel.filter.enabled | 필터가능한 필드 정의 | \-  |
| ObjectModel.filter.transformedBy | 설정된 ABAP 클래스를 이용해서 해당 필드가 필터로 사용되는 경우 변환을 수행한다. | \-  |
| ObjectModel.foreignKey.association | 필드의 외래키 관계의 Association을 정의 | \-  |
| ObjectModel.hierarchy.association | 관련 계층 노드 뷰와의 Association을 정의 | \-  |
| ObjectModel.modelingPattern | CDS 모델 구현시 따라야하는 패턴을 정의 | \-  |
| ObjectModel.representativeKey | CDS 엔터티를 대표하는 키를 정의 | \-  |
| ObjectModel.sort.enabled | 정렬가능한 필드를 정의 | \-  |
| ObjectModel.sort.transformedBy | 정렬을 할 때 필드의 값을 ABAP클래스로 변경하는데 사용 | \-  |
| ObjectModel.supportedCapabilities | CDS 모델의 용도를 정의 | \-  |
| ObjectModel.text.association | #TEXT 데이터카탈로그를 가지는 CDS를 할당하여 필드의 텍스트로 사용 | \-  |
| ObjectModel.text.element | 필드의 텍스트로 쓰이는 하나이상의 필드를 할당 | \-  |
| ObjectModel.usageType.dataClass | CDS 엔터티의 데이터클래스를 정의 | \-  |
| ObjectModel.usageType.serviceQuality | CDS 엔터티의 성능에 대한 서비스 수준을 정의 | \-  |
| ObjectModel.usageType.sizeCategory | 기본 쿼리를 이용해서 다뤄야 하는 CDS 엔터티의 사이즈를 정의 | \-  |
| ObjectModel.virtualElement | 데이터베이스의 데이터를 사용하지 않고 로직을 통해서 데이터를 만들어야 하는 필드를 정의. Projection 엔터티의 필드에는 사용할 수 없다 | \-  |
| ObjectModel.virtualElementCalcualtedBy | 가상필드의 값을 만들어주는 클래스를 정의 | \-  |
| OData.publish | CDS모델을 자동으로 OData 서비스로 만들 때 사용 | \-  |
| OData.entityType.name | OData 엔터티타입의 이름을 SADL을 통한 OData 서비스를 제공할 때 설정할 때 사용 | \-  |
| Search.defaultSearchElement | 자유검색필드의 대상으로 필드를 설정시 사용 | \-  |
| Search.fuzzinessThreshold | fuzzy search시에 필드가 얼만큼 매칭이 되면 검색이 될지를 설정 | \-  |
| Search.ranking | 자유검색시 필드중에서 어떤 필드에 매칭의 우선순위를 둘지를 정의 | \-  |
| Search.searchable | CDS엔터티가 자유검색이 가능한지를 설정 | \-  |
| Semantics.amount.currencyCode | 필드가 금액인 경우 통화필드를 어떤걸 참조할지를 설정 | \-  |
| Semantics.booleanIndicator | 필드가 boolean 타입의 데이터를 가지는 경우 설정 | \-  |
| Semantics.businessDate.from | 필드의 데이터가 비지니스 관점의 유효시작일인 경우 설정 | \-  |
| Semantics.businessDate.to | 필드의 데이터가 비지니스 관점의 유효종료일인 경우 설정 | \-  |
| Semantics.currencyCode | 필드의 데이터가 통화인 경우에 사용. CDS View Entity에서는 더이상 사용하지 않는다. | \-  |
| Semantics.dateTime | 필드가 타임스탬프 유형의 필드인 경우에 설정 | \-  |
| Semantics.fiscal.period | 필드가 회계년도 데이터인 경우에 설정하며 PPP의 3자리 문자열로 구성된 회계 기간으로 인코딩 한다. | \-  |
| Semantics.fiscal.year | 필드를 회계년도를 나타내는 4자리의 문자열로 변환 | \-  |
| Semantics.fiscal.yearPeriod | 필드의 값을 YYYYPPP 패턴의 7자리 숫자문자열로 이루어진 회계 연도와 기간으로 인코딩한다. | \-  |
| Semantics.fiscal.yearVariant | 회계년도의 값을 가지는 필드를 회계년도의 회계기간과 매칭되는 달력년도로 인코딩하는데 사용 | \-  |
| Semantics.language | 필드가 언어값을 가지는 경우 설정 | \-  |
| Semantics.quantity.unitOfMeasure | 수량을 가지는 필드에 해당 단위를 가지는 필드를 지정할 때 사용 | \-  |
| Semantics.systemDate.createdAt | 데이터베이스에 데이터레코드의 생성일 정보를 가지는 필드에 사용 | \-  |
| Semantics.systemDate.lastChangedAt | 레코드의 최신 변경일을 가지는 필드에 사용 | \-  |
| Semantics.systemDateTime.createdAt | 레코드의 타임스탬프 형식의 생성일을 가지는 필드에 사용 | \-  |
| Semantics.systemDateTime.lastChangedAt | 레코드의 타임스탬프 형식의 최신변경일을 가지는 필드에 사용 | \-  |
| Semantics.systemTime.createdAt | 레코드가 생성된 시간을 가지는 필드에 사용 | \-  |
| Semantics.systemTime.lastChangedAt | 레코드의 최신 변경시간을 가지는 필드에 사용 | \-  |
| Semantics.text | 텍스트정보를 가지는 필드에 사용 | \-  |
| Semantics.unitOfMeasure | 단위 정보를 가지는 필드에 사용. CDS View Entity에서는 더이상 사용하지 않는다. | \-  |
| Semantics.user.createdBy | 레코드의 생성자 정보를 가지는 필드에 사용 | \-  |
| Semantics.user.lastChangedBy | 레코드의 최신 변경자를 가지는 필드에 사용 | \-  |
| UI.facet | 사용자 인터페이스의 프레젠테이션을 할때 사용자에게 필요한 정보를 제공 및 탐색, 필터링하기 위해 만드는 항목 | \-  |
| UI.fieldGroup | UI에 필드그룹을 표현할 때 사용 | \-  |
| UI.headerInfo | 엔터티의 정보중 화면에 반드시 출력해 줘야 하는 항목을 정의할 때 사용 | \-  |
| UI.hidden | 필드가 UI에 출력이 필요가 없는 경우에 설정 | \-  |
| UI.identification | 엔터티 하나를 화면에 출력할 때 기본적으로 나타내야 하는 필드를 정의할 때 사용 | \-  |
| UI.lineItem | 화면에 테이블의 필드로 출력이 필요한 필드에 대해서 설정 | \-  |
| UI.selectionField | 화면에 기본 검색필드로 사용해야 하는 필드에 설정 | \-  |
| VDM.private | 재사용하지 않는 CDS 엔터티에 대해서 설정 | \-  |
| VDM.usageType | CDS 엔터티의 사용가능한 사용법에 대해서 정의<br><br><br>- #ACTION\_PARAMETER\_STRUCTURE <br>- #ACTION\_RESULT\_STRUCTURE | \-  |
| VDM.viewExtension | CDS 엔터티가 다른 CDS의 확장 뷰임을 나타낼때 사용 | \-  |
| VDM.viewType | CDS 엔터티가 VDM 모델에서 어떤 타입으로 사용되는지 정의<br><br><br>- #BASIC <br>- #COMPOSITE <br>- #TRANSACTIONAL <br>- #CONSUMPTION <br>- #EXTENSION | \-  |

  

  

### Annotation 표기방법

  

#### \- 단일값 설정

```
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'ProductTextId'
```

  

#### \- 그룹핑 설정

```
@ObjectModel : {
  dataCategory: #TEXT,
  representativeKey: 'ProductTextId'
}
```

  

#### ⁠- 배열 설정

```
@ObjectModel.alternativeKey: [{
  id: 'ExternalID',
  element: ['_Product.Product', 'Language']
}]
```

  

  

### Annotation은 어떤 영향을 주는가?

- Artifacts 생성

- 런타임 실행에 영향
- 문서화

  

다음의 예제는 위의 3가지 형태의 예시를 보여준다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Annotation - Effect'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #B,
  sizeCategory: #XL,
  dataClass: #TRANSACTIONAL
}
@OData.entityType.name: 'ODataExposure_Type'
define view entity ZCM_EXAMPLE54
  with parameters
    @Consumption.hidden: true
    @Environment.systemField: #SYSTEM_DATE
    P_CreationDate : timestampl
  as select from ZSD_I_SALESORDERITEM
{
  key SalesOrderItemId,

      _Head.SalesOrder,

      SalesOrderItem
}
```

  

#### \- Artifacts 생성

@OData.entityType.name을 통해서 해당 CDS가 OData로 제공이 되는 경우 EntityType의 이름을 설정한 이름으로 생성해 준다.

  

#### \- 런타임 실행에 영향

@Environment.systemField를 통해서 ABAP코드에서 파라미터에 값을 넣지 않은 경우에 시스템변수의 값을 자동으로 할당해 준다.

  

#### \- 문서화

런타임 환경이나 Artifacts를 생성하는 것처럼 직접적인 영향을 주지는 않고 해당 CDS를 사용하는 측에서 알아야 하는 정보를 제공한다.

@ObjectModel.usageType.serviceQuality는 CDS 모델의 성능을 나타내며 품질값 B는 트랜잭션 어플리케이션에서 사용가능한 수준을 나타낸다.

@ObjectModel.usageType.sizeCategory는 요청시에 일반적으로 처리되는 레코드의 수를 나타낸다. XL은 1억건 미만을 의미한다.

@ObjectModel.usageType.dataClass는 해당 CDS뷰 모델의 데이터의 성격을 의미하며, 마스터이면 거의 변하는 데이터가 아니므로 Caching에 사용하는 등에 전략에 이용된다

  

  

## Propagation of Element Annotations

  

**Propagation** : 하위 CDS뷰의 Annotaion이 상위 CDS뷰에 전달되는 것을 의미. 즉 하위 CDS뷰에 선언된 Annotation은 상위 CDS뷰에서 재 정의하지 않아도 적용이 되는 것을 의미

  

기본원리

- 하위 CDS에 선언된 필드레벨의 Annotation은 상위 CDS뷰에 적용된다.
- 동일한 이름의 Annotation은 상위 CDS뷰에서 재정의해서 바꿀 수 있다.
- 매개변수와 CDS 헤더레벨의 Annotation은 Propagation되지 않는다

  

상속이 되지 않는 경우

- CAST 등의 계산 로직을 통해서 변경된 경우
- Annotation 값을 null로 지정하게 되면 하위 CDS뷰의 annotation을 더이상 상속되지 않게 만들어준다.
- Annotation을 재정의하여 다른 값을 할당한 경우
- CDS 헤더에 @Metadata.ignorePropagatedAnnotations를 설정한 경우 더이상 모든 Annotation이 상속되지 않는다

  

다음은 TABLE로 부터 CDS뷰를 선언한 경우 Annotation을 보여준다.

- 기본적인 필드의 라벨은 데이터엘리먼트에서 가져오게 된다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Source A'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE55
  as select from ztsd0011
{
  @Semantics.quantity.unitOfMeasure: 'Orderquantityunit'
  orderquantity     as OrderQuantity,

  orderquantityunit as OrderQuantityUnit,

  @Semantics.systemDate.createdAt: true
  createdat         as CreatedAt
}
```

Annotation을 보면 다음과 같다.

![](Files/image%2091.png)  

![](Files/image%2092.png)  

  

위의 CDS를 가지고 새로운 CDS 뷰를 생성할 때 필드를 cast를 하게 되면 기존 Annotation이 무효화가 되기때문에 unitOfMeasure annotation을 다시 설정해 줘야 한다. OrderQuantityUnit은 라벨을 변경하여서 해당 뷰를 사용하는경우 이제부터는 변경된 라벨을 상속받게 된다. CreatedAt은 별도의 처리를 하지 않아서 하위 CDS뷰의 annotation을 그대로 가져오게 된다.

**\[Cast로 인한 Annotation Propagation 무효화로 인한 오류\]**

![](Files/image%2093.png)  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Source B'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE56
  as select from ZCM_EXAMPLE55
{
  @Semantics.quantity.unitOfMeasure: 'Orderquantityunit'
  cast( OrderQuantity as meng15 preserving type ) as OrderQuantity,

  @EndUserText.label: 'Unit'
  OrderQuantityUnit,

  CreatedAt
}
```

![](Files/image%2094.png)  

![](Files/image%2095.png)  

![](Files/image%2096.png)다음은 annotation을 null로 설정을 해서 annotation을 초기화하는 예제이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Source C'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE57
  as select from ZCM_EXAMPLE56
{
  OrderQuantityUnit,

  @Semantics.systemDate.createdAt: null
  CreatedAt
}
```

![](Files/image%2097.png)  

![](Files/image%2098.png)  

  

Annotation @Metadata.ignorePropagatedAnnotations: true 를 이용하여 모든 상속되는 Annotation을 무효화 한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Source D'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE58
  as select from ZCM_EXAMPLE57
{
  OrderQuantityUnit
}
```

![](Files/image%2099.png)  

  

  

하위 CDS뷰에서 사용하던 참조필드들 상위뷰에서 동일한 이름으로 다음과 같이 재정의하는 경우에 기술적으로는 오류가 없지만, 데이터에 따라서는 해당 단위가 아닌 항목에 대해서는 데이터적으로 일관성이 없어지게 된다. 따라서 이런경우에는 참조필드와 다른 이름으로 필드를 재 정의 해서 작성하는 것이 좋다.

- 아래 예제에서는 원래는 EA 단위가 PC단위로 바뀌었는데 기술적으로는 아무런 오류가 발생하지 않는다.

  

**\[원래 CDS\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Source A'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE55
  as select from ztsd0011
{
  @Semantics.quantity.unitOfMeasure: 'Orderquantityunit'
  orderquantity     as OrderQuantity,

  orderquantityunit as OrderQuantityUnit,

  @Semantics.systemDate.createdAt: true
  createdat         as CreatedAt
}
```

  

![](Files/image%20100.png)  

  

**\[새로 작성한 CDS\]**

- 논리적으로 EA가 PC로 바뀌어서 오류가 있지만, 기술적으로는 아무런 오류가 발생하지 않는다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Consistency'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE59
  as select from ZCM_EXAMPLE55
{
  OrderQuantity,
  
  cast ('PC' as abap.unit(3)) as OrderQuantityUnit
}
```

  

![](Files/image%20101.png)  

  

**\[Recommendation\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Consistency'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE60
  as select from ZCM_EXAMPLE55
{
  OrderQuantity,

  OrderQuantityUnit,

  cast ('PC' as abap.unit(3)) as ConvertedQuantityUnit
}
```

  

  

만일 다음과 같이 필드를 Alias를 통해서 이름을 변경한 경우에 하위 CDS에서 그대로 Annoation이 상속이 되어서 오류는 나지 않지만, 존재하지 않는 필드를 참조하고 있기 때문에 문제가 발생할 수 있다.

- 이런 경우에는 annotation을 재정의하거나 annotation의 상속을 하지 않게 해서 새롭게 작성하는 것이 좋다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Propagation - Consistency'
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE61
  as select from ZCM_EXAMPLE55
{
  OrderQuantity     as Quantity,

  OrderQuantityUnit as QuantityUnit
}
```

![](Files/image%20102.png)  

  

  

### CDS Metadata Extensions

  

- DDLX 개발오브젝트
- Annotation을 별도의 파일로 작성
- Annotation이 CDS에 있는것 보다 훨씬 읽기 쉽게 해주며, 관련있는 Annotation을 모아서 관리할 수 있는 장점도 있다.

  

#### Layered Structure

- 하나의 CDS는 여러개의 Metadata Extension 파일로 확장이 가능
- 확장된 파일중에 동일한 Annotation을 선언해 놓은 경우 어떤 항목의 우선순위가 높은지를 판단하는 기준

![](Files/image%20103.png)  

  

#### 생성순서

- 먼저 대상 CDS에서 오른쪽 클릭을 해서 CONTEXT Menu에서 “New Metadata Extension”을 선택한다.

![](Files/image%20104.png)  

![](Files/image%20105.png)  

![](Files/image%20106.png)  

![](Files/image%20107.png)  

  

다음과 같은 오류가 발생하면 해당 CDS가 metadata extension이 가능하도록 설정이 되지 않은 것이므로 해당 설정을 한다. ctrl + 1을 눌러서 quick fix기능을 이용해서 처리한다.

![](Files/image%20108.png)  

![](Files/image%20109.png)  

  

Annotation을 작성한다. 주로 UI쪽을 작성하지만 여기서는 간단히 라벨을 고치는 것으로 한다.

![](Files/image%20110.png)  

  

CDS로 이동하여 Active Annotation을 확인한다.

![](Files/image%20111.png)  

  

만일 또다른 Metadata Extension을 생성해서 layer를 LOCALIZATION으로 변경하여 작성한 후 ANNOTATION이 어떤걸로 반영되었는지 확인한다.

![](Files/image%20112.png)  

![](Files/image%20113.png)  

  

  

SAP에서 생성한 Standard CDS는 메타데이터 Extension을 생성하지 않는것이 좋다.

- 해당 뷰가 사용된 모든 어플리케이션에 적용이 되기 때문에 영향도를 파악하기 힘들기 때문이다.
- 별도록 custom CDS를 만들어서 사용하는 것을 추천한다.