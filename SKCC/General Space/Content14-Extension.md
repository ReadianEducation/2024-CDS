## Content14-Extension

  

Extension의 종류

- Key User App을 이용
- ABAP Development Tools (Eclipse)를 이용 

  

### ABAP Extension 예제

  

CDS에 추가적인 필드를 넣기 위해서 먼저 추가할 필드에 관한 개발 오브젝트를 생성한다. 여기서는 우선순위에 대한 필드를 하나 추가할 예정이며, 우선순위는 A,B,C로 상,중,하로 구분이 되며 해당 정보를 만들기 위해 먼저 도메인을 생성한다.

![](Files/image%20287.png)  

![](Files/image%20288.png)  

  

다음은 해당 도메인으로 데이터엘리먼트를 다음과 같이 생성한다.

![](Files/image%20289.png)  

도메인으로 만든 항목에 대해서 나중에 해당 필드에 대해서 Foreign Key 관계의 테이블이 필요하기 때문에 다음과 같이 CDS뷰를 생성한다.

- 도메인의 Fixed Value를 이용하는 경우 아래의 방법을 통해서 각각 도메인의 CDS뷰를 생성한다.

  

**CDS : ZRDS\_I\_PRIORITY / 우선순위**

- Fixed 값은 원래 데이터엘리먼트의 값으로 CAST 하여 사용한다.
- 원래 도메인 값은 나중에 값을 비교하기 위해 추가하지만 실제적으로 외부에서 사용하지 못하도록 어노테이션 처리를 한다
- 기본적으로 해당 종류의 CDS는 자율검색 필드에서 사용하기 위해서 @Search 어노테이션을 설정한다
- 코드에 대한 CDS만 다루며, 텍스트에 대한 CDS는 별도로 생성

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '우선순위'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity ZRDS_I_PRIORITY
  as select from dd07l
{
      
  key cast( domvalue_l as zrdsdepriority ) as Priority,
      
      @Consumption.hidden: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      domvalue_l                           as DomainValue
}
where
      domname  = 'ZRDSDOPRIORITY'
  and as4local = 'A'
  and as4vers  = '0000'
```

  

**CDS : ZRDS\_I\_PRIORITY\_TEXT / 우선순위내역**

- 텍스트 테이블 유형으로 설정
- 자율검색가능하도록 설정
- Foreign key 관계를 사용하기 위해서 대표키를 설정
- 텍스트에 대한 어느정도 텍스트가 일치하면 검색 결과로 나오게 하는 fuzzy 검색의 HANA 기능을 사용하도록 설정

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '우선순위'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'Priority'
@Search.searchable: true
define view entity ZRDS_I_PRIORITY_TEXT
  as select from dd07t
  association [1..1] to ZRDS_I_PRIORITY as _Priority on $projection.Priority = _Priority.Priority
  association [0..1] to I_Language      as _Language on $projection.Language = _Language.Language
{
      @ObjectModel.foreignKey.association: '_Language'
      @Semantics.language: true
  key cast( ddlanguage as spras preserving type ) as Language,

      @ObjectModel.foreignKey.association: '_Priority'
      @ObjectModel.text.element: [ 'PriorityText' ]
  key cast( domvalue_l as zrdsdepriority )        as Priority,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @Semantics.text: true
      ddtext                                      as PriorityText,

      _Priority,

      _Language
}
where
      domname  = 'ZRDSDOPRIORITY'
  and as4local = 'A'
  and as4vers  = '0000'
```

  

다음으로 해당 필드를 넣기 위해서는 테이블이 Extension이 가능하도록 설정이 필요하다. 현재는 cbo 테이블을 가지고 Extension을 해보도록 한다.

![](Files/image%20290.png)  

```
@EndUserText.label : '판매오더 아이템'
@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztsd0011 {
  key client           : abap.clnt not null;
  key salesorderitemid : sysuuid_x16 not null;
  salesorderid         : sysuuid_x16;
  salesorderitem       : posnr not null;
  productid            : sysuuid_x16;
  @Semantics.quantity.unitOfMeasure : 'ztsd0011.orderquantityunit'
  orderquantity        : kwmeng;
  orderquantityunit    : vrkme;
  @Semantics.amount.currencyCode : 'ztsd0011.transactioncurrency'
  netamount            : netwr_ap;
  transactioncurrency  : waerk;
  createdby            : abp_creation_user;
  createdat            : abp_creation_tstmpl;
  locallastchangedby   : abp_locinst_lastchange_user;
  locallastchangedat   : abp_locinst_lastchange_tstmpl;
  lastchangedby        : abp_lastchange_user;
  lastchangedat        : abp_lastchange_tstmpl;

}
```

  

먼저 테이블을 Extension 하기 위해서 “**append structure**”를 통해서 수행한다.

Structure : ZZRDSSALESITEM\_EXTENSION / Extension : 판매오더아이템

- 필드명은 Custom 추가 필드라는걸 확실히 표현하기 위해서 ZZ로 시작한다.

![](Files/image%20291.png)  

![](Files/image%20292.png)  

```
@EndUserText.label : 'Extension : 판매오더아이템'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
extend type ztsd0011 with zzrdssalesitem_extension {
  zzpriority : zrdsdepriority;

}
```

테이블을 검색해 보면 다음과 같이 필드가 추가가 되었음을 알 수 있다.

![](Files/image%20293.png)  

  

다음으로 CDS뷰를 Extension 한다. 기존 View를 건들이지 않고 extend view를 이용해서 추가를 한다.

CDS : ZSD\_X\_SALESORDERITEM - Extension : ZSD\_I\_SALESORDERITEM

- 필드와 Association을 추가한다

![](Files/image%20294.png)  

![](Files/image%20295.png)  

```
extend view entity ZSD_I_SALESORDERITEM with
association [0..1] to ZRDS_I_PRIORITY as _ZZPriority on $projection.ZZPriority = _ZZPriority.Priority
{
  ztsd0011.zzpriority as ZZPriority,

  _ZZPriority
}
```

  

활성화를 하게 되면 오류가 발생하게 되는데 아직 CDS뷰에 Extension 가능하도록 설정해야 한다.

![](Files/image%20296.png)  

![](Files/image%20297.png)  

  

**@AbapCatalog.viewEnhancementCategory**

- **#PROJECTION\_LIST**:
    - Extensions of the **SELECT list and additional associations** are allowed; extensions of CDS views whose SELECT lists contain aggregate expressions or have a UNION clause must be permitted using further values
- **#GROUP\_BY**:
    - Aggregated or non-aggregated elements are allowed to be added to a SELECT list with aggregated expressions and the associated extensions of the of the GROUP-BY clause are also permitted; can only be specified together with #PROJECTION\_LIST
- **#UNION**:
    - Extensions of the SELECT list of a CDS view with a UNION clause are allowed; can only be specified together with #PROJECTION\_LIST
- **#NONE**:
    - No extensions allowed; cannot be specified together with other values

  

CDS뷰의 데이터를 확인하면 다음과 같이 추가된 필드를 확인할 수 있다

![](Files/image%20298.png)  

  

CDS뷰를 Extension 하기 위해서 알아야 하는 사항은 다음과 같다.

- 하나의 CDS에 대해서 여러개의 Extension을 만들 수 있다
- 확장 필드의 데이터소스를 명시적으로 지정해야 한다.
- 업그레이드시의 이름충돌을 방지하기 위해서 반드시 ZZ 같은 네임스페이스 접두사를 만들어서 필드이름으로 추가해야 한다.
- 확장필드와 Cardinality가 최대 1로 Association을 설정할 수 있다
- 계산필드도 확장 필드로 정의할 수 있다
- Association만 추가도 가능하다
- 뷰를 여러군데에서 사용중이라면 그만큼 활성화하는데 오랜 시간이 걸리게 된다

  

  

## CDS View Stack 확장

  

- 기존에는 특정 테이블의 필드를 추가하면 해당 테이블로 부터 계속적으로 CDS뷰를 직접 Extension 하는 방법을 사용했지만, 해당 방법이 활성화 시에 오랜 시간이 걸리는 것과, 최상단 뷰에서 해당 필드를 사용하기 위해서 관련 하위 뷰를 모두 extesion 해야 한다는 단점이 존재
- 현재는 이렇게 직접적으로 CDS뷰를 확장하지 않고 간접적으로 확장하여 사용하는 방법으로 작업하고 있음

  

![](Files/image%20299.png)  

다음은 직접적으로 I\_SalesDocumentItem을 확장한 예시이다

![](Files/image%20300.png)  

  

## 간접 CDS 뷰 Extensions

  

- SAP S/4HANA Cloud는 Key User App을 통해서 확장하는 방법을 사용하고 있음
- 해당 App을 사용하지 않더라도 동일한 Extension 접근 방법이 설정된 CDS뷰나 CDS뷰엔터티는 확장이 가능하다

  

간접 CDS 뷰 확장 방법

- Extension Include view를 통해서 확장
    - \_Extension 명으로 만들어져 있음
- 해당 Association을 가지는 모든 뷰는 테이블의 필드가 추가되면 해당 뷰를 이용하여 확장 필드를 개별적으로 넣어서 확장이 가능하다
- 모든 View Stack을 확장하는 대신 확장이 필요한 CDS뷰만 확장하여 구현
    - Key User App은 이러한 방법을 사용한 뷰들의 확장을 수행한다

  

다음은 Standard View 예시이다

![](Files/image%20301.png)  

  

E\_SalesDocumentItemBasic

![](Files/image%20302.png)  

  

I\_SalesDocumentExtdItem

![](Files/image%20303.png)  

  

I\_SalesDocumentItemAnalytics

![](Files/image%20304.png)  

  

I\_SalesOrderItemCube

![](Files/image%20305.png)  

  

![](Files/image%20306.png)  

- 위의 그림과 같이 \_Extension Association을 구현한 뷰에서는 VBAP에 추가된 필드를 Extension 전용 뷰에 추가만 하면 해당 뷰를 이용하여 자체적으로 확장을 하면 된다. 모든 View Stack을 확장할 필요가 없다

  

### Indirect 구현 예제

  

먼저 VBAP의 확장뷰로 사용되는 E\_SalesDocumentItemBasic을 확인하면 다음의 항목이 구현되어 있다

- VBAP의 키 필드가 포함

![](Files/image%20307.png)  

  

VBAP 테이블에 PRIORITY 필드를 추가한다

![](Files/image%20308.png)  

![](Files/image%20309.png)  

다음과 같이 Structure를 작성하고 활성화를 한다.

```
@EndUserText.label : 'Extension : VBAP'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
extend type vbap with zzrds_vbap_ext {
  zzpriority : zrdsdepriority;

}
```

![](Files/image%20310.png)  

  

다음으로 Extension View를 Priority 필드를 추가하여 Extension 한다.

![](Files/image%20311.png)  

![](Files/image%20312.png)  

```
@AbapCatalog.sqlViewAppendName: 'ZRDSXSALES01'
@EndUserText.label: 'Extension - E_SalesDocumentItemBasic'
extend view E_SalesDocumentItemBasic with ZRDS_X_SALESDOCITBASIC01
  association [0..1] to ZRDS_I_PRIORITY as _ZZPrority on $projection.ZZPriority = _ZZPrority.Priority
{
  @ObjectModel.foreignKey.association: '_ZZPrority'
  Persistence.zzpriority as ZZPriority,

  _ZZPrority
}
```

- @AbapCatalog.sqlViewAppendName 은 DDIC 뷰가 생성되는 CDS View 유형에서 사용되며 CDS View Entity 유형은 사용되지 않는다
- 필드명과 마찬가지로 Association 명도 반드시 ZZ과 추가적인 Namespace를 넣어서 이름을 작성하여 업그래이드 시의 충돌을 피해야 한다
- 확장필드에도 필요한 경우 Annotation을 설정할 수 있다

  

활성화가 되면 다음과 같이 Extension 뷰에서 확장되었다는 아이콘이 표시가 되며 해당 아이콘에 커서를 올리면 관련 CDS뷰가 화면에 출력이 된다.

![](Files/image%20313.png)  

![](Files/image%20314.png)  

  

  

다음과 같이 Relation Explorer에서도 확인이 가능하다.

![](Files/image%20315.png)  

  

다음으로 추가된 필드가 필요한 뷰를 Extension 한다. 여기서는 I\_SalesOrderItemCube를 확장해 보도록 한다.

![](Files/image%20316.png)  

![](Files/image%20317.png)  

```
@AbapCatalog.sqlViewAppendName: 'ZRDSXSITCUBE'
@EndUserText.label: 'Extension - I_SalesOrderItemCube'
extend view I_SalesOrderItemCube with ZRDS_X_SALESORDERITEMCUBE
  association [0..1] to ZRDS_I_PRIORITY as _ZZPriority on $projection.zzpriority = _ZZPriority.Priority
{
  _Extension.ZZPriority,

  _ZZPriority
}
```

  

\_Extension을 이용한 확장은 E\_SalesDocumentItemBasic이던 I\_SalesOrderItemCube 둘다 VBAP 테이블을 기반으로 하기 때문에 테이블의 **Self-Join**이라 HANA 데이터베이스에서 최적화된 방식으로 실행이 가능

  

위의 뷰를 실행하면 추가된 필드를 확인할 수 있다.

![](Files/image%20318.png)  

  

Extension을 삭제하기 위해서는 위에서 만든 Extension View를 삭제하면 되며, 활성화 하는데 시간이 걸릴 수 있다

  

  

## Usage of CDS Entity Extensions

  

- Indirect Extension이 모든 뷰에 대해서 가능하지 않다

  

### Extension Association이 없는 경우

- \_ZZExtension과 같은 이름으로 Custom Extension 뷰를 만들어서 사용
    - 테이블의 키필드를 가지고 기본 Extension View에 사용
    - 나중에 \_Extension 뷰가 생기면 해당 뷰로 전환하여 사용 가능

  

### Extension Include View가 없는 경우

- Custom Extension Include View를 만들어서 사용

  

### Foreign Key 필드가 누락된 CDS 뷰 확장 

- 확장필드가 포함된 테이블을 사용하는 CDS뷰에 Foreign Key필드가 없는 경우에는 해당 뷰를 확장할 수 없다
- 외래키 필드를 가지고 있는 뷰스택에서 가장 가까운 뷰에 CDS뷰 확장을 하여 추가 시킨 후에 작업을 해야 한다.
    - Aggregating View or Union View
    - [참고 URL](http://s-prs.co/v529420 "http://s-prs.co/v529420") 

  

### 분석쿼리뷰 확장

- 분석쿼리로 사용되고 있는 Cube 또는 Dimension 뷰에 해당 필드를 추가하여 확장한다. 해당 필드가 Dimension 필드인 경우에는 반드시 Foreign Key 연관 관계를 추가해야 한다.
- 분석쿼리뷰를 확장하여 사용한다.
- Standard 분석쿼리 대신에 Custom 분석쿼리를 만들어서 사용하는 것도 하나의 방법

  

```
@AbapCatalog.sqlViewAppendName: 'ZRDSXSIT02'
@EndUserText.label: 'Extension - C_SalesOrderItemCube'
extend view C_SalesOrderItemQry with ZRDS_X_SALESORDERITEMCUBE01
  association [0..1] to ZRDS_I_PRIORITY as _Priority on $projection.zzpriority = _Priority.Priority
{
  @AnalyticsDetails.query.display: #KEY_TEXT
  @ObjectModel.foreignKey.association: '_Priority'
  I_SalesOrderItemCube.ZZPriority,

  _Priority
}
```

  

  

### Standard 필드를 이용한 Extension

- 주 데이터소스의 필드 뿐만 아니라 조인된 모든 데이터소스의 필드를 추가할 수 있다
- Risk
    - Cardinality가 \* 인 필드를 추가하게 되면 데이터 수에 영향을 줄 수 있음
    - Association을 사용하게 되면 실제로는 SQL의 Join을 의미하므로 뷰정의 자체의 복잡성을 증가시키고 실행 성능에 영향을 줄 수 있음
    - SAP가 표준 필드를 데이터보호등의 이유로 삭제하는 경우 해당 항목에 대한 처리가 필요할 수 있음
    - 안정성 계약이 C1으로 RELEASE된 뷰를 제외한 CDS에 대해서는 다음과 같은 변경이 있을 수 있기 때문에 주의가 필요하다
        - 데이터소스 변경
        - 필드이름변경
        - 필드유형변경
        - 계산필드추가
        - 필드제거
        - 데이터소스 조인방식 변경

  

  

### Standard Released된 CDS뷰의 확장

  

- C1으로 릴리즈된 뷰인 경우에도 표준데이터소스의 필드를 통한 Extension의 안정성까지 보장하지 않는다
- CDS Exxtend view에 Association은 다음과 같은 조건이 만족한다면 정의가 가능하다
    - Custom으로 만든 CDS뷰에만 정의되어야 한다
    - Association 대상이 되는 뷰도 반드시 Released 된 뷰를 사용해야 한다
    - Cardinality는 \[0..1\] 또는 \[1..1\]만 사용해야 한다.
    - 반드시 $projection을 통해서 노출된 필드만 사용하여 연관관계를 맺어야 한다
    - 확장할 뷰에 계산된 필드를 제외하고 Association에 사용이 가능하다

  

![](Files/image%20319.png)  

  

### 계산필드를 통한 확장

- 확장 뷰내에서 계산 필드를 선언하여 사용할 수 있다
- 반드시 $projection을 통해서 정의된 필드만을 사용해야 한다

  

![](Files/image%20320.png)