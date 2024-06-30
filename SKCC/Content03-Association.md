# Content03-Association

  

- CDS 모델간의 **관계를 설정**
- Association을 이용해서 해당 CDS뷰를 **필요한 데이터를 가져오는데 사용**
- 분석쿼리의 **관련 데이터 (dimension views)**를 화면에 나타나는데 사용
- 기술적으로는 join이 사용
    - 실제적으로 CDS내에서 Path 표현식으로 사용이 되어야지 Join 구문으로 변경
    - Association을 설정하고 다른 뷰에서 사용하지 않고 Propagate (전파)한다고 해서 Join 구문으로 바뀌지 않음
- CDS에서는 필드와 동일하게 취급된다

  

  

## Define Associations

  

Association명은 ‘\_’를 붙인 이름으로 선언하는걸 권장한다.

![](Files/image%2072.png)  

on은 소스와 타켓의 관계 (조건)를 설정한다.

![](Files/image%2073.png)  

  

Cardinalityy는 소스 CDS뷰 하나의 레코드에 대해서 타켓이 몇개가 연관되어 있는지를 나타낸다.

- 데이터가 중복 또는 누락을 방지하기 위해선 반드시 확인 필요
- 잘못된 정보는 성능에도 영향을 줄 수 있다

|     |     |     |
| --- | --- | --- |
| **Cardinality** | **Min** | **Max** |
| \[1\] | 0   | 1   |
| \[0..1\] | 0   | 1   |
| \[1..1\] | 1   | 1   |
| \[0..\*\] | 0   | Unlimited |
| \[1..\*\] | 1   | Unlimited |
| 지정하지 않음 | 0   | 1   |

  

타켓으로 사용할 수 있는 데이터 소스는 다음과 같다.

- CDS 뷰
- CDS 테이블 함수
- CDS Abstract 엔터티
- 테이블
    - 분석앱에서는 Association으로 사용못함

  

  

  

## Expose Associations

  

- 필드와 마찬가지로 필드로서 Projection 해야 한다.

  

다음과 같이 Association을 해당 CDS뷰를 사용하는 곳에서 사용가능하도록 할 수 있다.

![](Files/image%2074.png)  

...

![](Files/image%2075.png)  

  

CDS의 연관관계 주의점 

- 사용하지 않는 Association으로 인해서 불필요한 활성화 문제가 발생할 수 있으므로 불필요한 Association은 만들지 않는 것이 좋다

  

## Model Compositional Relations

  

- composition 관계
- 부모 - 자식 관계의 데이터소스에 사용
    - 판매오더헤더 - 판매오더아이템

  

  

### 문법

- 하나의 부모 Entity (Root)에 여러 자식 Entity로 관계가 설정
    - 부모만 root view entity로 선언
    - composition of 문법을 사용해서 자식 엔터티 연결

![](Files/image%2076.png)  

...

![](Files/image%2077.png)  

  

- 자식 Entity에서는 부모 Entity와의 관계를 다음과 같이 설정
    - association to parent로 부모 Entity와 연결
    - 부모에의 cardianality는 지정하지 않아도 자동으로 \[1\] 로 설정이 된다
    - on 조건에서는 부모의 모든 키에 대해서 조건이 들어가 줘야 한다.
        - 부모에서는 특별히 자식과의 on 조건을 지정안해도 되며, 자식의 on 조건을 가지고 판단하게 된다

![](Files/image%2078.png)  

...

![](Files/image%2079.png)  

  

- 자식의 자식의 관계에 대해서는 다음과 같이 자식 Entity에서 Root뷰와 바로 위의 부모뷰에 대해서 association 관계가 지정되어 있어야 한다.
    - Root View : association \[1..1\]
    - Parent View : association to parent 

  

다음은 **2중 composition** 관계를 정의하는 예시이다.

  

테이블 - Root

```
@EndUserText.label : 'Root'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zted0010 {
  key client  : abap.clnt not null;
  key rootkey : sysuuid_x16 not null;
  rootvalue01 : abap.char(10);

}
```

  

테이블 - Child

```
@EndUserText.label : 'Parent'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zted0011 {
  key client    : abap.clnt not null;
  key parentkey : sysuuid_x16 not null;
  rootkey       : sysuuid_x16;
  parentval01   : abap.char(10);

}
```

  

테이블 - Grand Child

```
@EndUserText.label : 'Child'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zted0012 {
  key client   : abap.clnt not null;
  key childkey : sysuuid_x16 not null;
  parentkey    : sysuuid_x16;
  rootkey      : sysuuid_x16;
  childval01   : abap.char(10);

}
```

  

다음으로 Composition 관계가 들어간 CDS 뷰 정의 부분이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'COMPOSITION - ROOT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define root view entity ZCM_EXAMPLE32
  as select from zted0010
  composition [0..*] of ZCM_EXAMPLE33 as _Parent
{
  key rootkey     as Rootkey,

      rootvalue01 as RootValue01,

      _Parent
}
```

  

```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'COMPOSITION - PARENT'
define view entity ZCM_EXAMPLE33
  as select from zted0011
  composition [0..*] of ZCM_EXAMPLE34 as _Child
  association to parent ZCM_EXAMPLE32 as _Root on $projection.RootKey = _Root.Rootkey
{
  key parentkey   as ParentKey,

      rootkey     as RootKey,

      parentval01 as ParentVal01,

      _Root,
      
      _Child
}
```

  

```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'COMPOSITION - CHILD'
define view entity ZCM_EXAMPLE34
  as select from zted0012
  association [1..1] to ZCM_EXAMPLE32        as _Root   on $projection.RootKey = _Root.Rootkey
  association        to parent ZCM_EXAMPLE33 as _Parent on $projection.ParentKey = _Parent.ParentKey
{
  key childkey   as ChildKey,

      parentkey  as ParentKey,

      rootkey    as RootKey,

      childval01 as ChildVal01,

      _Root,

      _Parent // Make association public
}
```

  

  

**Composition의 기술적 제한 사항**

- Association과 같이 Path 표현식을 사용 못함
- 키필드가 모두 on 조건에 매핑이 되어야 한다.
- Projection View는 Composition View를 가지고만 생성 가능

  

  

## M:N 관계 모델링

  

m:n 관계를 다룰 별도 (mapping)의 뷰가 필요

- 두개의 뷰의 하나의 레코드는 Mapping뷰의 데이터를 없거나 여러개와 연결된다 \[0..\*\]
- Mapping뷰 데이터는 두개의 뷰의 데이터를 하나를 없거나 하나의 데이터와 연결된다 \[0..1\]

  

다음은 M:N 관계를 나타내는 예시이다. 먼저 두개의 M:N 관계를 가지는 뷰 선언부이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'M:N - Source A'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE35
  as select distinct from t000
  association [0..*] to ZCM_EXAMPLE37 as _Mapping on $projection.KeyFieldA = _Mapping.KeyFieldA
{
  key abap.int4'1' as KeyFieldA,

      _Mapping
}
union select distinct from t000
association [0..*] to ZCM_EXAMPLE37 as _Mapping on $projection.KeyFieldA = _Mapping.KeyFieldA
{
  key abap.int4'2' as KeyFieldA,

      _Mapping
}
```

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'M:N - Source B'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE36
  as select distinct from t000
  association [0..*] to ZCM_EXAMPLE37 as _Mapping on $projection.KeyFieldB = _Mapping.KeyFieldB
{
  key abap.int4'3' as KeyFieldB,

      _Mapping
}
union select distinct from t000
association [0..*] to ZCM_EXAMPLE37 as _Mapping on $projection.KeyFieldB = _Mapping.KeyFieldB
{
  key abap.int4'4' as KeyFieldB,

      _Mapping
}
```

  

다음은 두개의 뷰와 M:N 관계에 대한 매핑 관계를 가지는 뷰이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'M:N - Mapping'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE37
  as select distinct from t000
  association [0..1] to ZCM_EXAMPLE35 as _Sample35 on $projection.KeyFieldA = _Sample35.KeyFieldA
  association [0..1] to ZCM_EXAMPLE36 as _Sample36 on $projection.KeyFieldB = _Sample36.KeyFieldB
{
  key abap.int4'1' as KeyFieldA,

  key abap.int4'3' as KeyFieldB,
  
  _Sample35,
  
  _Sample36
}
union select distinct from t000
  association [0..1] to ZCM_EXAMPLE35 as _Sample35 on $projection.KeyFieldA = _Sample35.KeyFieldA
  association [0..1] to ZCM_EXAMPLE36 as _Sample36 on $projection.KeyFieldB = _Sample36.KeyFieldB
{
  key abap.int4'1' as KeyFieldA,

  key abap.int4'4' as KeyFieldB,
  
  _Sample35,
  
  _Sample36
}
union select distinct from t000
  association [0..1] to ZCM_EXAMPLE35 as _Sample35 on $projection.KeyFieldA = _Sample35.KeyFieldA
  association [0..1] to ZCM_EXAMPLE36 as _Sample36 on $projection.KeyFieldB = _Sample36.KeyFieldB
{
  key abap.int4'2' as KeyFieldA,

  key abap.int4'4' as KeyFieldB,
  
  _Sample35,
  
  _Sample36
}
```

  

  

  

## Use Associations in CDS Views

  

### **\- Path Expression**

- Associaiton에 관계의 CDS의 필드를 가져와서 뷰를 구성할 때 사용되는 문법
- 암묵적으로 JOIN 구문을 생성
- 여러개의 Association을 Path 표현식을 통해서 사용하는 경우에 반드시 Association관계의 on조건의 필드들이 Projection 필드로 제공이 되어야 한다.

  

### **\- Path Expression 정의**

Path Expression에 대한 설명을 하기위해서 다음의 CDS를 사용한다.

  

**\[판매오더납품일정\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '판매오더 스케쥴라인'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZSD_I_SALESORDERSCHEDULE
  as select from ztsd0012
  association [1..1] to ZSD_I_SALESORDERITEM as _Item on $projection.SalesOrderItemId = _Item.SalesOrderItemId
{
      @EndUserText.label: '판매오더스케쥴키'
      @EndUserText.quickInfo: '판매오더스케쥴키'
  key salesorderscheduleid       as SalesOrderScheduleId,

      @EndUserText.label: '판매오더아이템키'
      @EndUserText.quickInfo: '판매오더아이템키'
      salesorderitemid           as SalesOrderItemId,

      @EndUserText.label: '아이템순번'
      @EndUserText.quickInfo: '아이템순번'
      salesorderscheduleline     as SalesOrderScheduleLine,

      @EndUserText.label: '수량'
      @EndUserText.quickInfo: '수량'
      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      orderquantity              as OrderQuantity,

      @EndUserText.label: '단위'
      @EndUserText.quickInfo: '단위'
      orderquantityunit          as OrderQuantityUnit,

      @EndUserText.label: '납품일'
      @EndUserText.quickInfo: '납품일'
      deliverydate               as DeliveryDate,

      @EndUserText.label: '스케쥴라인유형'
      @EndUserText.quickInfo: '스케쥴라인유형'
      salesorderschedulelinetype as Salesorderschedulelinetype,

      @Semantics.user.createdBy: true
      @EndUserText.label: '생성자'
      @EndUserText.quickInfo: '생성자'
      createdby                  as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      @EndUserText.label: '생성시간'
      @EndUserText.quickInfo: '생성시간'
      createdat                  as CreatedAt,

      @Semantics.user.localInstanceLastChangedBy: true
      @EndUserText.label: '임시변경자'
      @EndUserText.quickInfo: '임시변경자'
      locallastchangedby         as LocalLastchangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @EndUserText.label: '임시변경시간'
      @EndUserText.quickInfo: '임시변경시간'
      locallastchangedat         as LocalLastchangeDat,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: '변경자'
      @EndUserText.quickInfo: '변경자'
      lastchangedby              as LastChangedBy,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: '변경시간'
      @EndUserText.quickInfo: '변경시간'
      lastchangedat              as LastChangedAt,


      // Association
      _Item
}
```

  

**\[판매오더아이템\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '판매오더 아이템'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZSD_I_SALESORDERITEM
  as select from ztsd0011
  association [0..1] to ZSD_I_SALESORDER         as _Head         on $projection.SalesOrderId = _Head.SalesOrderId
  association [0..*] to ZSD_I_SALESORDERSCHEDULE as _ScheduleLine on $projection.SalesOrderId = _ScheduleLine.SalesOrderItemId
  association [0..1] to ZSD_I_PRODUCT            as _Product      on $projection.ProductId = _Product.ProductId
{
      @EndUserText.label: '판매오더아이템키'
      @EndUserText.quickInfo: '판매오더아이템키'
  key salesorderitemid    as SalesOrderItemId,

      @EndUserText.label: '판매오더키'
      @EndUserText.quickInfo: '판매오더키'
      salesorderid        as SalesOrderId,

      @EndUserText.label: '아이템순번'
      @EndUserText.quickInfo: '아이템순번'
      salesorderitem      as SalesOrderItem,

      @EndUserText.label: '제품키'
      @EndUserText.quickInfo: '제품키'
      productid           as ProductId,

      @EndUserText.label: '수량'
      @EndUserText.quickInfo: '수량'
      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      orderquantity       as OrderQuantity,

      @EndUserText.label: '단위'
      @EndUserText.quickInfo: '단위'
      orderquantityunit   as OrderQuantityUnit,

      @EndUserText.label: '가격'
      @EndUserText.quickInfo: '가격'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      netamount           as NetAmount,

      @EndUserText.label: '통화'
      @EndUserText.quickInfo: '통화'
      transactioncurrency as TransactionCurrency,

      @Semantics.user.createdBy: true
      @EndUserText.label: '생성자'
      @EndUserText.quickInfo: '생성자'
      createdby           as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      @EndUserText.label: '생성시간'
      @EndUserText.quickInfo: '생성시간'
      createdat           as CreatedAt,

      @Semantics.user.localInstanceLastChangedBy: true
      @EndUserText.label: '임시변경자'
      @EndUserText.quickInfo: '임시변경자'
      locallastchangedby  as LocalLastchangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @EndUserText.label: '임시변경시간'
      @EndUserText.quickInfo: '임시변경시간'
      locallastchangedat  as LocalLastchangeDat,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: '변경자'
      @EndUserText.quickInfo: '변경자'
      lastchangedby       as LastChangedBy,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: '변경시간'
      @EndUserText.quickInfo: '변경시간'
      lastchangedat       as LastChangedAt,

      // Associations
      _Head,

      _Product,

      _ScheduleLine
}
```

  

**\[제품\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '제품'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZSD_I_PRODUCT
  as select from ztsd0040
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text on $projection.ProductId = _Text.ProductId
{
      @EndUserText.label: '제품키'
      @EndUserText.quickInfo: '제품키'
  key productid          as ProductId,

      @EndUserText.label: '자재번호'
      @EndUserText.quickInfo: '자재번호'
      product            as Product,

      @EndUserText.label: '자재유형'
      @EndUserText.quickInfo: '자재유형'
      producttype        as ProductType,

      @EndUserText.label: '권한그룹'
      @EndUserText.quickInfo: '권한그룹'
      authorizationgroup as AuthorizationGroup,

      @Semantics.user.createdBy: true
      @EndUserText.label: '생성자'
      @EndUserText.quickInfo: '생성자'
      createdby          as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      @EndUserText.label: '생성시간'
      @EndUserText.quickInfo: '생성시간'
      createdat          as CreatedAt,

      @Semantics.user.localInstanceLastChangedBy: true
      @EndUserText.label: '임시변경자'
      @EndUserText.quickInfo: '임시변경자'
      locallastchangedby as LocalLastchangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @EndUserText.label: '임시변경시간'
      @EndUserText.quickInfo: '임시변경시간'
      locallastchangedat as LocalLastchangeDat,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: '변경자'
      @EndUserText.quickInfo: '변경자'
      lastchangedby      as LastChangedBy,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: '변경시간'
      @EndUserText.quickInfo: '변경시간'
      lastchangedat      as LastChangedAt,

      _Text
}
```

  

**\[제품명\]**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '제품명'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZSD_I_PRODUCTTEXT
  as select from ztsd0041
  association [1..1] to ZSD_I_PRODUCT as _Product on $projection.ProductId = _Product.ProductId
{
      @EndUserText.label: '제품명키'
      @EndUserText.quickInfo: '제품명키'
  key producttextid      as ProductTextId,

      @EndUserText.label: '제품키'
      @EndUserText.quickInfo: '제품키'
      productid          as ProductId,

      @EndUserText.label: '언어'
      @EndUserText.quickInfo: '언어'
      language           as Language,

      @EndUserText.label: '제품명'
      @EndUserText.quickInfo: '제품명'
      productname        as ProductName,

      @Semantics.user.createdBy: true
      @EndUserText.label: '생성자'
      @EndUserText.quickInfo: '생성자'
      createdby          as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      @EndUserText.label: '생성시간'
      @EndUserText.quickInfo: '생성시간'
      createdat          as CreatedAt,

      @Semantics.user.localInstanceLastChangedBy: true
      @EndUserText.label: '임시변경자'
      @EndUserText.quickInfo: '임시변경자'
      locallastchangedby as LocalLastchangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @EndUserText.label: '임시변경시간'
      @EndUserText.quickInfo: '임시변경시간'
      locallastchangedat as LocalLastchangeDat,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: '변경자'
      @EndUserText.quickInfo: '변경자'
      lastchangedby      as LastChangedBy,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: '변경시간'
      @EndUserText.quickInfo: '변경시간'
      lastchangedat      as LastChangedAt,

      // Association
      _Product
}
```

  

  

위의 CDS를 가지고 Path Expression이 들어간 CDS를 다음과 같이 생성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE38
  as select from ZSD_I_SALESORDERSCHEDULE
{
  key SalesOrderScheduleId,

      SalesOrderItemId,

      _Item._Head.SalesOrder,

      _Item.SalesOrderItem,

      SalesOrderScheduleLine,

      _Item._Product.Product as SalesOrderItemProduct,

      _Item,

      _Item.ProductId,

      _Item._Product
} 
```

  

**\_Item.\_Head.SalesOrder**

- ZSD\_I\_SALESORDERSCHEDULE CDS뷰의 Association인 \_Item의 \_Head Associaiton에 필드 SalesOrder를 가져오는 Path 표현식

  

\_Item Association을  Path 표현식에 사용하기 위해서는 ZSD\_I\_SALESORDERSCHEDULE과 \_Item Associaiton의 on 조건의 SalesOrderItemId가 화면의 Projection 필드로 제공되어야 한다.

![](Files/image%2080.png)  

  

동일하게 \_Item의 \_ Product Association을 사용하기 위해서는 \_Item Association 뷰에서 \_Product와의 on 조건인 ProductId가 화면의 Projection 필드로 제공되어야 한다.

![](Files/image%2081.png)  

  

위에서 만든 Path 표현식은 다음과 같이 **Join 구문**으로 생성됨을 확인할 수 있다.

![](Files/image%2082.png)  

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE38" AS SELECT 
  "ZSD_I_SALESORDERSCHEDULE"."MANDT" AS "MANDT", 
  "ZSD_I_SALESORDERSCHEDULE"."SALESORDERSCHEDULEID", 
  "ZSD_I_SALESORDERSCHEDULE"."SALESORDERITEMID", 
  "=A1"."SALESORDER", 
  "=A0"."SALESORDERITEM", 
  "ZSD_I_SALESORDERSCHEDULE"."SALESORDERSCHEDULELINE", 
  "=A2"."PRODUCT" AS "SALESORDERITEMPRODUCT", 
  "=A0"."PRODUCTID" 
FROM ( 
  ( 
    "ZSD_I_SALESORDERSCHEDULE" "ZSD_I_SALESORDERSCHEDULE" LEFT OUTER MANY TO ONE JOIN "ZSD_I_SALESORDERITEM" "=A0" ON ( 
      "ZSD_I_SALESORDERSCHEDULE"."MANDT" = "=A0"."MANDT" AND 
      "ZSD_I_SALESORDERSCHEDULE"."SALESORDERITEMID" = "=A0"."SALESORDERITEMID" 
    ) 
  ) LEFT OUTER MANY TO ONE JOIN "ZSD_I_SALESORDER" "=A1" ON ( 
    "ZSD_I_SALESORDERSCHEDULE"."MANDT" = "=A1"."MANDT" AND 
    "=A0"."SALESORDERID" = "=A1"."SALESORDERID" 
  ) 
) LEFT OUTER MANY TO ONE JOIN "ZSD_I_PRODUCT" "=A2" ON ( 
  "ZSD_I_SALESORDERSCHEDULE"."MANDT" = "=A2"."MANDT" AND 
  "=A0"."PRODUCTID" = "=A2"."PRODUCTID" 
) 
WHERE "ZSD_I_SALESORDERSCHEDULE"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

위의 구문을 Path 표현식을 사용하지 않고 작성하게 되면 다음과 같이 작성할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Express - without path expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE39
  as select from           ZSD_I_SALESORDERSCHEDULE as SL
    left outer to one join ZSD_I_SALESORDERITEM     as SI   on SI.SalesOrderItemId = SL.SalesOrderItemId
    left outer to one join ZSD_I_SALESORDER         as SO   on SO.SalesOrderId = SI.SalesOrderId
    left outer to one join ZSD_I_PRODUCT            as ITEM on ITEM.ProductId = SI.ProductId
{
  key SL.SalesOrderScheduleId,

      SL.SalesOrderItemId,

      SO.SalesOrder,

      SI.SalesOrderItem,

      SL.SalesOrderScheduleLine,

      ITEM.Product as SalesOrderItemProduct,

      SL._Item,

      SI.ProductId,

      SI._Product
}
```

  

  

### 참고 - 데이터베이스 정규화

  

설계시에 데이터의 중복을 최소화하고 데이터의 일관성을 유지하기 위해 **데이터를 구조화하는 방법**

- 테이블의 컬럼은 하나의 값만 가져야 한다 **(제1정규형 - 1NF)**
    - 수강과목이라는 컬럼이 있는 경우에 해당 컬럼의 값은 “수학"등의 하나의 값만 가져야하지, “수학,과학" 등의 두가지 이상의 값을 가지면 안된다
- 키값이 아닌 모든 속성은 기본키에 완전히 종속이 되어 있어야 한다. **(제2정규형 - 2NF)**
    - 만일 학생의 성적표를 저장하는 테이블인 경우에 해당 테이블의 기본키는 학생번호/과목번호 라고 한다면
        - “점수"라는 속성은 해당 값에 완전히 종속이 되므로 정규화 원리에 부합
        - “교수"라는 속성은 해당 값이 기본키에 대해서 완전히 종속되었다고 보기 어려우므로 비정규화된 속성이라고 볼 수 있다
    - 이런 경우에는 “교수"라는 속성은 다른 테이블인 과목별 교수 정보등을 통해서 저장해야 한다
- 기본키가 아닌 속성의 값에 따라서 기본키가 아닌 다른 속성이 결정되는 경우 이행적종속성을 가진다고 하는데, 이러한 이행적 종속성이 있는 경우 비정규화 된 데이터라고 한다. **(제3정규형 - 3NF)**
    - 만일 학생 정보 테이블에 키값이 학생ID라고 하고 해당 테이블에 학과하고 학과장까지 들어가 있는 경우
        - 학생 ID에 의해서 학과가 정해지고, 학과에 의해서 학과장이 정해지는 경우 학생 ID에 의해서 학과장이 정해지는 경우에 이행 종속성이 있다고 본다
    - 이행종속성이 있는 경우에는 이행종속성이 있는 속성을 별도의 테이블로 관리해야 한다.
        - 학생 ID에 대해서 학과 ID만 관리
        - 학과ID에 대해서 학과장을 관리하는 테이블 생성

- 모든 속성이 후보키가 될 수 있는 경우에 해당 속성을 따로 분리하여 다른 테이블로 관리하도록 하는 것. **(보이스-코드 정규형 - BCNF)**
    - 만일 강의정보에서 강의/시간/교수가 속성에 강의가 키값인 경우에 다음과 같이 시간이라는 속성이 기술적으로 봤을 때 교수를 결정할 수 있기 때문에 해당 속성을 따로 빼서 강의/교수, 시간/교수 라는 테이블 두개로 더욱 엄격하게 나누는 것을 의미

        - 강의는 시간을 결정
        - 강의는 교수를 결정
        - 시간은 교수를 결정

  

  

### Path Expression의 사용처에 따른 분류 

- Path Expression이 필드 레벨에서 사용되는 경우에는 Left Outer Join으로 변경
- Path Expression이 From이나 Association에서 사용되는 경우에는 Inner Join으로 변경

  

다음은 Path Expression을 From절에 사용한 경우에 해당 CREATE 문장을 보면 inner join이 사용되었음을 볼 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Association 사용'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE40
  as select from ZSD_I_SALESORDERITEM._Head
{
  key SalesOrderId,

      _Item as _SalesOrderItem
}
```

CREATE 구문을 보면 다음과 같다.

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE40" AS SELECT 
  "=A0"."MANDT" AS "MANDT", 
  "ZSD_I_SALESORDER"."SALESORDERID" 
FROM "ZSD_I_SALESORDERITEM" "=A0" INNER JOIN "ZSD_I_SALESORDER" "ZSD_I_SALESORDER" ON ( 
  "=A0"."MANDT" = "ZSD_I_SALESORDER"."MANDT" AND 
  "=A0"."SALESORDERID" = "ZSD_I_SALESORDER"."SALESORDERID" 
) 
WHERE "=A0"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

  

### **Path Expression에 따른 Cardinality 변경**

  

다음과 같이 CDS가 있고 CDS의 필드에 Path 표현식을 통해서 표현이 되는 경우에 CREATE 문을 보면 다음과 같다

  

**필드레벨에 명시적으로 INNER를 사용하고 Cardinality를 \*로 지정한 경우**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Cardinality'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE41
  as select from ZSD_I_PRODUCT
{
  key ProductId,

  key _Text[*:inner].ProductTextId,

      Product
}
```

CREATE 구문을 보면 다음과 같다

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE41" AS SELECT 
  "ZSD_I_PRODUCT"."MANDT" AS "MANDT", 
  "ZSD_I_PRODUCT"."PRODUCTID", 
  "=A0"."PRODUCTTEXTID", 
  "ZSD_I_PRODUCT"."PRODUCT" 
FROM "ZSD_I_PRODUCT" "ZSD_I_PRODUCT" INNER JOIN "ZSD_I_PRODUCTTEXT" "=A0" ON ( 
  "ZSD_I_PRODUCT"."MANDT" = "=A0"."MANDT" AND 
  "ZSD_I_PRODUCT"."PRODUCTID" = "=A0"."PRODUCTID" 
) 
WHERE "ZSD_I_PRODUCT"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

**필드레벨에 명시적으로 INNER를 사용하고 Cardinality를 1로 지정한 경우**

- **Cardinality를 1로 하나 \*로 하나 생성되는 CREATE SQL 구문은 동일하다. 즉 Path 표현식의 cardinality는 기술적인 구문에 영향을 주지 않고, 정보성이다**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Cardinality inner + 1'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE42
  as select from ZSD_I_PRODUCT
{
  key ProductId,

  key _Text[1:inner].ProductTextId,

      Product
}
```

CREATE 구문을 보면 다음과 같다

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE42" AS SELECT 
  "ZSD_I_PRODUCT"."MANDT" AS "MANDT", 
  "ZSD_I_PRODUCT"."PRODUCTID", 
  "=A0"."PRODUCTTEXTID", 
  "ZSD_I_PRODUCT"."PRODUCT" 
FROM "ZSD_I_PRODUCT" "ZSD_I_PRODUCT" INNER JOIN "ZSD_I_PRODUCTTEXT" "=A0" ON ( 
  "ZSD_I_PRODUCT"."MANDT" = "=A0"."MANDT" AND 
  "ZSD_I_PRODUCT"."PRODUCTID" = "=A0"."PRODUCTID" 
) 
WHERE "ZSD_I_PRODUCT"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

**필드레벨에 명시적인 INNER를 뺀 경우**

- **INNER가 없는 경우에는 LEFT OUTER JOIN으로 CREATE SQL 구문이 생성이 됨을 볼 수 있음** 
- **INNER 자체는 실제로 SQL 구문을 생성할 때 영향을 주고 있음**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE43
  as select from ZSD_I_PRODUCT
{
  key ProductId,

  key _Text.ProductTextId,

      Product
}
```

CREATE 구문을 보면 다음과 같다

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE43" AS SELECT 
  "ZSD_I_PRODUCT"."MANDT" AS "MANDT", 
  "ZSD_I_PRODUCT"."PRODUCTID", 
  "=A0"."PRODUCTTEXTID", 
  "ZSD_I_PRODUCT"."PRODUCT" 
FROM "ZSD_I_PRODUCT" "ZSD_I_PRODUCT" LEFT OUTER JOIN "ZSD_I_PRODUCTTEXT" "=A0" ON ( 
  "ZSD_I_PRODUCT"."MANDT" = "=A0"."MANDT" AND 
  "ZSD_I_PRODUCT"."PRODUCTID" = "=A0"."PRODUCTID" 
) 
WHERE "ZSD_I_PRODUCT"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

  

만일 Path Expression을 사용할 때 대상이 되는 Association에 특정한 조건을 줘야 하는 경우에는 다음과 같은 문법을 사용하면 된다.

- 특정언어에 대해서 텍스트를 가져오는 경우에 많이 사용이 된다
- Cardinality를 조정하는데 사용

  

다음은 Filter가 적용된 Path 표현식이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - condition'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE44 as select from ZSD_I_PRODUCT
{
  key ProductId,
  
      Product,
      
      _Text[1:Language='3'].ProductName as ProductNameKorean,
            
      _Text[1:Language='E'].ProductName as ProductNameEnglish
}
```

다음은 CREATE SQL 구문이다.

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE44" AS SELECT 
  "ZSD_I_PRODUCT"."MANDT" AS "MANDT", 
  "ZSD_I_PRODUCT"."PRODUCTID", 
  "ZSD_I_PRODUCT"."PRODUCT", 
  "=A0"."PRODUCTNAME" AS "PRODUCTNAMEKOREAN", 
  "=A1"."PRODUCTNAME" AS "PRODUCTNAMEENGLISH" 
FROM ( 
  "ZSD_I_PRODUCT" "ZSD_I_PRODUCT" LEFT OUTER MANY TO ONE JOIN "ZSD_I_PRODUCTTEXT" "=A0" ON ( 
    "ZSD_I_PRODUCT"."PRODUCTID" = "=A0"."PRODUCTID" AND 
    "=A0"."LANGUAGE" = N'3' AND 
    "ZSD_I_PRODUCT"."MANDT" = "=A0"."MANDT" 
  ) 
) LEFT OUTER MANY TO ONE JOIN "ZSD_I_PRODUCTTEXT" "=A1" ON ( 
  "ZSD_I_PRODUCT"."PRODUCTID" = "=A1"."PRODUCTID" AND 
  "=A1"."LANGUAGE" = N'E' AND 
  "ZSD_I_PRODUCT"."MANDT" = "=A1"."MANDT" 
) 
WHERE "ZSD_I_PRODUCT"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

동일한 FILTER 조건인 경우에는 여러개의 JOIN구문 대신에 하나의 JOIN 구문만이 생성이 된다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - condition'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE45
  as select from ZSD_I_PRODUCT
{
  key ProductId,

      _Text[1:Language='3'].ProductId   as ProductKorean,

      _Text[1:Language='3'].ProductName as ProductNameKorean
}
```

CREATE SQL 구문은 다음과 같다

```
CREATE OR REPLACE VIEW "ZCM_EXAMPLE45" AS SELECT 
  "ZSD_I_PRODUCT"."MANDT" AS "MANDT", 
  "ZSD_I_PRODUCT"."PRODUCTID", 
  "=A0"."PRODUCTID" AS "PRODUCTKOREAN", 
  "=A0"."PRODUCTNAME" AS "PRODUCTNAMEKOREAN" 
FROM "ZSD_I_PRODUCT" "ZSD_I_PRODUCT" LEFT OUTER MANY TO ONE JOIN "ZSD_I_PRODUCTTEXT" "=A0" ON ( 
  "ZSD_I_PRODUCT"."PRODUCTID" = "=A0"."PRODUCTID" AND 
  "=A0"."LANGUAGE" = N'3' AND 
  "ZSD_I_PRODUCT"."MANDT" = "=A0"."MANDT" 
) 
WHERE "ZSD_I_PRODUCT"."MANDT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

  

### Association의 기본 필터 적용 

- Path 표현식에 필터를 적용하지 않는 경우 기본적으로 적용이 된다.
- Path 표현식에 필터를 적용하게 되면 기본 필터 대신에 해당 필터를 사용하게 된다

  

먼저 Default Filter가 적용된 데이터를 가져오면 다음과 같다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE46
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,
  
  _TextDefaultFilter.Language,

  _TextDefaultFilter.ProductName as ProductNameKorean1

}
```

다음과 같이 Korean 텍스트를 중복없이 20개를 가져온다

![](Files/image%2083.png)  

  

다음으로 Default Fitler를 무시하고 Path 표현식에서 필터를 재정의한 경우의 데이터 이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE47
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,
  
  _TextDefaultFilter[1:Language='E'].Language,

  _TextDefaultFilter[1:Language='E'].ProductName as ProductNameEnglish
}
```

영어로 20개의 항목을 중복없이 가져온다.

![](Files/image%2084.png)  

  

만일 다음과 같이 Filter 자체를 없애는 경우에는 영어/한글로 40개의 데이터를 가져오게 된다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE48
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,
  
  _TextDefaultFilter[*:left outer].Language,

  _TextDefaultFilter[*:left outer].ProductName as ProductNameEnglish
}
```

![](Files/image%2085.png)  

  

여기서 주의할 점은 Path 표현식의 데이터는 lefter outer 조인을 통해서 컬럼에 적용된 순서대로 SQL 구문이 만들어지게 되므로 다음과 같이 필드의 순서를 바꾸게 되면 데이터의 개수가 변경이 될수 있다.

먼저 DEFAULT FILTER → FILTER를 제거한 순서대로 Path표현식을 쓰게 되면 20개의 데이터가 출력

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE49
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,

  _TextDefaultFilter.ProductName               as ProductNameKorean,

  _Text[1:Language='E'].ProductName            as ProductNameEnglish,
  
    _TextDefaultFilter[*:left outer].ProductName as ProductName
}
```

![](Files/image%2086.png)  

  

DEFAULT FILTER 제거 → DEFAULT FILTER 조건 변경 시

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE49
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,
  
  _TextDefaultFilter[*:left outer].ProductName as ProductName,

  _TextDefaultFilter.ProductName               as ProductNameKorean,

  _Text[1:Language='E'].ProductName            as ProductNameEnglish
}
```

![](Files/image%2087.png)  

  

**Recommendation**

- Default Filter를 사용하는 경우 데이터의 cardinality가 어떻게 변하는지 데이터를 사용하는 측에서 예측이 힘드므로 명시적으로 Association의 On 조건에 조건을 명시적으로 지정해 주는 것이 좋다.

  

### Path 표현식의 제한 사항

- Association의 on 조건에 Projection의 계산된 필드가 사용된 경우 해당 Association의 속성을 Projection 필드로 사용할 수 없다

  

**on 조건으로 사용한 필드가 기본 필드인 경우**

- Projection 필드로 해당 Association의 필드로 사용가능

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Restriction'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE52
  as select from ZCM_EXAMPLE50
  association [0..1] to ZCM_EXAMPLE51 as _Product      on $projection.Product = _Product.Product
{
  key SalesOrder,

  key SalesOrderItem,

      Product,

      _Product.ProductType
}
```

  

**on 조건으로 사용한 필드가 alias로 선언된 필드인 경우**

- Projection 필드로 해당 Association의 필드를 사용 가능

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Restriction'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE52
  as select from ZCM_EXAMPLE50
  association [0..1] to ZCM_EXAMPLE51 as _ProductAlias on $projection.ProductAlias = _ProductAlias.Product
{
  key SalesOrder,

  key SalesOrderItem,

      Product                   as ProductAlias,

      _ProductAlias.ProductType as ProductTypeAlias,

      _ProductAlias
}
```

  

**on 조건으로 사용한 필드가 계산된 필드인 경우**

- Projection 필드로 해당 Association의 필드를 사용 불가
    - 오류 내역 : The association \_ProductCast cannot be used locally in the view (see long text)

![](Files/image%2088.png)  

**on 조건으로 사용한 필드가 preserving type을 이용한 속성만 바꾼 필드인 경우**

- Projection필드로 해당 Association 필드를 사용 가능

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - Restriction'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE52
  as select from ZCM_EXAMPLE50
  association [0..1] to ZCM_EXAMPLE51 as _ProductPreserve on $projection.ProductPreserve = _ProductPreserve.Product
{
  key SalesOrder,

  key SalesOrderItem,

      cast(Product as matnr preserving type ) as ProductPreserve,

      _ProductPreserve.ProductType,

      _ProductPreserve
}
```

  

  

  

## ABAP코드에서 Path 표현식 사용하기

  

ABAP 코드내에서도 동일하게 Path 표현식을 사용하여 데이터를 검색해 올 수 있다.

- Association은 “\\” 로 접근
- Association의 필드는 “-” 로 접근 
- Filter는 \[ \] 안에 cardinality와 Where 문을 통해서 조건을 입력한다.

```
*&---------------------------------------------------------------------*
*& Report ZSD_UNIT03_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_unit03_01.

START-OF-SELECTION.

  SELECT \_item\_head-salesorder,
    \_item-salesorderitem,
    \_item\_product-product,
    \_item\_product\_text[ (1) INNER : WHERE language = '3' OR language = 'E' ]-productname
  FROM zsd_i_salesorderschedule
  WHERE \_item\_product-producttype = 'FERT'
  INTO TABLE @DATA(lt_data).
```

  

참고로 CDS에서도 FILTER에 여러 조건을 다음과 같이 설정할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Path Expression - default filter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE49
  as select from ZSD_I_PRODUCT
  association [0..*] to ZSD_I_PRODUCTTEXT as _TextDefaultFilter on  $projection.ProductId = _TextDefaultFilter.ProductId
  with default filter _TextDefaultFilter.Language = '3'
  association [0..*] to ZSD_I_PRODUCTTEXT as _Text              on  $projection.ProductId = _Text.ProductId
  association [0..1] to ZSD_I_PRODUCTTEXT as _TextKorean        on  $projection.ProductId = _TextKorean.ProductId
                                                                and _TextKorean.Language  = '3'
{
  ProductId,
  
  _TextDefaultFilter[*:left outer].ProductName as ProductName,

  _TextDefaultFilter.ProductName               as ProductNameKorean,

  _Text[1:Language='E' or Language='3'].ProductName            as ProductNameEnglish
}
```