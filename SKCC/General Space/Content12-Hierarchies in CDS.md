## Content12-Hierarchies in CDS

  

- 계층구조의 데이터 
    - 조직
- 대량의 데이터를 조사하고 처리하는데 도움

  

CDS에서 제공하는 계층구조 방법

- 어노테이션기반 계층구조
    - 분석엔진이 필요하므로 분석 어플리케이션에서 사용

- CDS 계층구조
    - 계층구조에만 사용

  

## Hierarchy Categories and Basics

  

### 계층형구조 (Leveled hierarchy)

- 계층에 대한 정보를 모두 가지고 있는 데이터로 구성
    - 지역 데이터인 경우 나라/지역 값을 모두 가지고 있음 
- 별도의 함수가 필요가 없음

  

### 부모-자식 계층 ( Parent-Child hierarchy )

- 계층노드집합 + 자식노드에서 부모노드로의 관계 집합

  

![](Files/image%20256.png)  

  

- 루트노드
    - 부모노드를 가지지 않는 최상위 노드
    - 계층구조에서 여러개의 루트노드가 존재할 수 있음
- 리프노드
    - 자식노드를 가지지 않는 최하위 노드

  

계층구조의 예외상황

- 하나이상의 부모노드를 가지는 경우
- 사이클관계를 가지는 노드

  

**사이클관계**라는 것은 부모노드로 올라가다 보면 다시 내노드로 오는 것을 의미

  

### 엄격한계층구조 (Strict hierarchy)

- 계층구조의 예외상황이 없는 계층구조
- 계층구조의 처리와 화면표시가 용이하다

  

### 노드의 레벨

- 엄격한 계층구조에서 하나의 노드는 정확히 하나의 루트노드를 가진다
- 루트노드의 레벨은 1이고 하위노드의 레벨은 하나씩 내려갈때 마다 1씩 증가한다.

  

### 노드 모델링

- 노드는 자신노드와 부모노드에 대한 고유 식별아이디를 가지고 있다
- 부모노드와는 Association으로 모델링 한다

  

### 비즈니스 의미 부여

- 노드 모델 자체에는 특별한 의미를 부여하지 않는다
- 비즈니스의미가 있는 다른 엔터티와 association 관계를 맺어 사용한다

  

### 계층 디렉토리

- 동일한 엔터티에 대해서 여러개의 대체 계층구조를 관리하기 위해서 사용
    - 계획버전, 이력버전
- 동일한 Cost Center는 여러 계층버전 또는 버전에서 연결될 수 있다
    - 이런 연결관계를 관리하기 위해서 나온 엔터티를 계층 디렉토리라고 한다

  

### 계층 엔진

- 계층 구조를 처리하기 위해서는 루프 또는 재귀호출이 필요하지만 SQL에서는 해당 기능이 제공되지 않는다
- 계층 구조를 처리하기 위해서 제공된게 계층엔진이다
- CDS에서는 해당 계층엔진을 사용할 수 있도록 별도의 어노테이션을 제공한다.

  

## 어노테이션 기반의 부모-자식 계층구조

  

### 계층구조를 위한 CDS 뷰

- 비즈니스 의미를 가지는 **기본엔터티** 필요
- 계층노드 정보를 가지는 **계층노드엔터티**
- 계층노드엔터티에서 추가정보가 필요한 경우 해당 정보를 가지는 **노드****추가정보엔터티**와 연결하기 위해서는 외래키관계를 설정한다
- 계층노드엔터티는 여러개의 **계층디렉토리**와 연결
- 기본 엔티티와 연결된 **기본텍스트엔터티**
- 기본엔터티가 단순한 경우에는 계층노드엔터티와 동일하게 사용할 수 있다

  

![](Files/image%20257.png)  

  

### 계층구조 (Hierarchy Structure)

- **@ObjectModel.dataCategory: #HIERARACHY**  
    - 계층노드뷰 어노테이션
    - 기본엔터티와 분리된 계층노드엔터티에 사용
- **@Hierarchy.parentChild**
    - 계층구조를 정의하는데 사용

  

|     |     |
| --- | --- |
| 어노테이션 | 의미 (Semantics) |
| @Hierarchy.parentChild.name | 계층의 **기술적이름**을 지정. 계층디렉토리를 사용하지 않는 경우에는 필수 |
| @Hierarchy.parentChild.label<br> | 계층내역 (옵션) |
| @Hierarchy.parentChild.recurseBy<br> | 부모노드와의 관계가 동일필드의 association으로 사용하는 경우 해당 필드에 지정 |
| @Hierarchy.parentChild.recurse.parent<br>@Hierarchy.parentChild.recurse.child<br> | 부모노드와의 관계필드에 지정 |
| @Hierarchy.parentChild.siblingsOrder.by<br> | 형제노드의 순서를 정의하는 필드 지정 |
| @Hierarchy.parentChild.siblingsOrder.direction<br> | 형제노드의 정렬 순서. ‘ASC’ , ‘DESC’ 를 사용하면 ‘ASC’가 기본값 |
| @Hierarchy.parentChild.director<br> | 계층디렉토리 Association |

  

### 부모노드와의 관계

- @Hierarchy.parentChild.recurseBy를 사용하여 동일 필드를 지정하는 방법이 가장 좋음
- 0..1 카디넬러티를 가져야 함
- 대안
    - @Hierarchy.parentChild.recurse.parent에 부모노드를 식별하는 필드 지정
    - @Hierarchy.parentChild.recurse.child에 자식노드를 식별하는 필드 지정

  

## 계층구조예제

  

Cost Center 계층구조는 다음과 같이 정의할 수 있다.

  

먼저 Base Entity를 생성한다.

CDS - ZRDS\_I\_HIER\_BASE : 코스트센터

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '코스트센터'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.representativeKey: 'CostCenter'
define view entity ZRDS_I_HIER_BASE
  as select from I_CostCenter
  association [0..*] to I_CostCenterText as _Text     on  $projection.ControllingArea = _Text.ControllingArea
                                                      and $projection.CostCenter      = _Text.CostCenter
                                                      and $projection.ValidityEndDate = _Text.ValidityEndDate
  association [0..*] to ZRDS_I_HIER_NODE as _HierNode on  $projection.ControllingArea = _HierNode.ControllingArea
                                                      and $projection.CostCenter      = _HierNode.CostCenter
{
  key ControllingArea,

      @ObjectModel.text.association: '_Text'
      @ObjectModel.hierarchy.association: '_HierNode'
  key CostCenter,

      @Semantics.businessDate.to: true
  key ValidityEndDate,

      @Semantics.businessDate.from: true
      ValidityStartDate,

      /* Association */
      _Text,

      _HierNode
}
```

  

다음으로 계층노드 CDS를 생성한다.

CDS - ZRDS\_I\_HIER\_NODE : 코스트센터 - 계층노드

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '코스트센터 - 계층노드'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@Hierarchy.parentChild: [{
  recurse.parent: ['ParentNode' ],
  recurse.child: ['HierarchyNode'],
  siblingsOrder: [{by: 'SequenceNumber', direction: #ASC}],
  directory: '_HierDirectory'
 }]
define view entity ZRDS_I_HIER_NODE
  as select from I_CostCenterHierarchyNode
  association [0..*] to I_CostCenterHierarchyNodeT as _NodeText        on  $projection.ControllingArea     = _NodeText.ControllingArea
                                                                       and $projection.CostCenterHierarchy = _NodeText.CostCenterHierarchy
                                                                       and $projection.HierarchyNode       = _NodeText.HierarchyNode
                                                                       and $projection.CostCenter          = ''
  association [1..1] to ZRDS_I_HIER_DIRECTORY      as _HierDirectory   on  $projection.CostCenterHierarchy = _HierDirectory.CostCenterHierarchy
                                                                       and $projection.ControllingArea     = _HierDirectory.ControllingArea
                                                                       and $projection.ValidityEndDate     = _HierDirectory.ValidityEndDate
  association [0..1] to I_ControllingArea          as _ControllingArea on  $projection.ControllingArea = _ControllingArea.ControllingArea
  association [0..1] to I_CostCenter               as _CostCenter      on  $projection.ControllingArea = _CostCenter.ControllingArea
                                                                       and $projection.CostCenter      = _CostCenter.CostCenter
{
      @ObjectModel.foreignKey.association: '_ControllingArea'
  key ControllingArea,
      
      @ObjectModel.foreignKey.association: '_HierDirectory'
  key CostCenterHierarchy,

      @ObjectModel.text.association: '_NodeText'
  key HierarchyNode,

  key ValidityEndDate,

      ParentNode,

      HierarchyVersion,

      ValidityStartDate,

      @ObjectModel.foreignKey.association: '_CostCenter'
      CostCenter,

      SequenceNumber,

      HierarchyNodeSequence,

      HierarchyNodeLevel,

      NodeType,

      HierarchyNodeVal,

      /* Associations */
      _NodeText,
      
      _HierDirectory,

      _ControllingArea,

      _CostCenter
}
```

  

다음으로 계층디렉토리뷰를 생성한다.

CDS - ZRDS\_I\_HIER\_DIRECTORY : 코스트센터 - 계층디렉토리

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '코스트센터 - 계층디렉토리'
@Metadata.ignorePropagatedAnnotations: false
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_I_HIER_DIRECTORY
  as select from I_CostCenterHierarchy
{
  key ControllingArea,

  key CostCenterHierarchy,

      @Semantics.businessDate.to: true
  key ValidityEndDate,

      @Semantics.businessDate.from: true
      ValidityStartDate,

      LastChangedByUser,

      LastChangeDateTime,

      LastChangeTime,

      HierarchyShortID,

      /* Associations */
      _ControllingArea,

      _ControllingAreaText,

      _Text
}
```

  

  

## CDS Hierarchies

  

- 1809 버전 이후에 HANA 계층엔진을 이용한 새로운 CDS 계층을 사용 가능
- [관련정보](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenselect_hierarchy_data.htm "https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenselect_hierarchy_data.htm")

  

### 생성예제

  

먼저 비즈니스 데이터를 정의한다. 여기에서는 테스트이므로 CDS를 통해서 특정 값을 임의로 세팅하여 사용한다.

- 직원 정보
- 직원과 관리자간의 계층 정보

  

#### 기본정보 - 직원정보

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '직원정보'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_I_HIER_BASE01
  as select distinct from t000
{
  '0001'                      as Employee,
  cast ('A' as text10)        as EmployName,
  cast('F' as text01)         as Gender,
  cast(100 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0002'                     as Employee,
  cast ('B' as text10)       as EmployName,
  cast('M' as text01)        as Gender,
  cast(80 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0003'                      as Employee,
  cast ('C' as text10)        as EmployName,
  cast('F' as text01)         as Gender,
  cast(100 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0004'                     as Employee,
  cast ('D' as text10)       as EmployName,
  cast('M' as text01)        as Gender,
  cast(80 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0005'                     as Employee,
  cast ('E' as text10)       as EmployName,
  cast('F' as text01)        as Gender,
  cast(80 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0006'                      as Employee,
  cast ('F' as text10)        as EmployName,
  cast('M' as text01)         as Gender,
  cast(100 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0007'                     as Employee,
  cast ('G' as text10)       as EmployName,
  cast('F' as text01)        as Gender,
  cast(60 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0008'                     as Employee,
  cast ('H' as text10)       as EmployName,
  cast('M' as text01)        as Gender,
  cast(40 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0009'                      as Employee,
  cast ('I' as text10)        as EmployName,
  cast('F' as text01)         as Gender,
  cast(100 as abap.dec(15,2)) as PartTimePercent
}
union all select distinct from t000
{
  '0010'                      as Employee,
  cast ('J' as text10)        as EmployName,
  cast('M' as text01)         as Gender,
  cast(100 as abap.dec(15,2)) as PartTimePercent
}
```

![](Files/image%20258.png)  

  

#### 기본정보 - 직원과 관리자 매핑정보

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '직원-관리자정보'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_I_HIER_BASE02
  as select distinct from t000
{
  '0001' as Employee,

  '0000' as Manager
}
union all select distinct from t000
{
  '0002' as Employee,

  '0001' as Manager
}
union all select distinct from t000
{
  '0003' as Employee,

  '0001' as Manager
}
union all select distinct from t000
{
  '0004' as Employee,

  '0003' as Manager
}
union all select distinct from t000
{
  '0005' as Employee,

  '0001' as Manager
}
union all select distinct from t000
{
  '0006' as Employee,

  '0005' as Manager
}
union all select distinct from t000
{
  '0007' as Employee,

  '0005' as Manager
}
union all select distinct from t000
{
  '0008' as Employee,

  '0007' as Manager
}
union all select distinct from t000
{
  '0009' as Employee,

  '0000' as Manager
}
union all select distinct from t000
{
  '0010' as Employee,

  '0009' as Manager
}
```

  

다음으로 기본정보를 가지고 CDS뷰를 생성한다.

**CDS - ZRDS\_I\_HIER\_EMPLOYEE : 직원**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '직원'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@ObjectModel.representativeKey: 'Employee'
define view entity ZRDS_I_HIER_EMPLOYEE
  as select from ZRDS_I_HIER_BASE01
{
      @ObjectModel.text.element: [ 'EmployName' ]
      @EndUserText.label: '사번'
      @EndUserText.quickInfo: '사번'
  key Employee,

      @Semantics.text: true
      @EndUserText.label: '사원명'
      @EndUserText.quickInfo: '사원명'
      EmployName,

      @EndUserText.label: '성별'
      @EndUserText.quickInfo: '성별'
      Gender,

      @EndUserText.label: '파트타임 비율'
      @EndUserText.quickInfo: '파트타임 비율'
      PartTimePercent,

      @Aggregation.default: #SUM
      @EndUserText.label: '정규직환산인원'
      @EndUserText.quickInfo: '정규직환산인원'
      division(PartTimePercent, 100, 2) as FullTimeEquivalent
}
```

![](Files/image%20259.png)  

  

**CDS - ZRDS\_I\_HIER\_EMPLOYEE\_REL : 직원별 관리자**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '직원별 관리자'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZRDS_I_HIER_EMPLOYEE_REL
  as select from ZRDS_I_HIER_BASE02
  association [0..1] to ZRDS_I_HIER_EMPLOYEE_REL as _Manager  on $projection.Manager = _Manager.Employee
  association [0..1] to ZRDS_I_HIER_EMPLOYEE     as _Employee on $projection.Employee = _Employee.Employee
{
  Employee,

  Manager,

  _Manager,

  _Employee
}
```

  

  

다음으로 HANA의 계층함수를 이용해서 다음과 같이 CDS 계층뷰를 생성한다.

![](Files/image%20260.png)  

![](Files/image%20261.png)  

```
define hierarchy ZRDS_I_HIER_EMPLOYEE_INFO
  as parent child hierarchy(
    source ZRDS_I_HIER_EMPLOYEE_REL
    child to parent association _Manager
    start where
      Manager is initial
    siblings order by
      Employee
  )
{
      @ObjectModel.foreignKey.association: '_Employee'
  key Employee,

      Manager,

      _Employee,

      _Manager

}
```

  

#### hierarchy 함수 매개변수

- source <ParentRelationView>
    - 부모와의 관계 정보를 가지고 있는 뷰를 설정
- child to parent association <ParentAssociaiton>
    - self-association 중에 부모를 나타내는 Association 명
- start where 
    - 시작 노드의 조건
- sibling order by 
    - 동일 레벨의 노드의 정렬 순서

  

제약사항

- CDS 계층정의 내에 Association 정의 못함
- 계산된 필드를 사용 못함
- 위의 두가지가 필요하면 계층의 데이터소스뷰에서 추가를 해줘야 한다.

  

### Hierarchy Attributes

- 소스필드웨어도 추가적인 계층구조에 대한 예약된 이름의 필드가 생성이 된다.

  

해당 데이터를 확인하기 위해서 다음의 레포트를 작성하고 실행해 본다.

```
*&---------------------------------------------------------------------*
*& Report ZRDS_EDU_HIER01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrds_edu_hier01.

START-OF-SELECTION.

  SELECT FROM zrds_i_hier_employee_info
  FIELDS
      employee,
      \_employee-employname AS node_text,
      node_id,
      parent_id,
      hierarchy_level,
      hierarchy_tree_size,
      hierarchy_rank,
      hierarchy_parent_rank,
      hierarchy_is_orphan,
      hierarchy_is_cycle
    ORDER BY hierarchy_rank
    INTO TABLE @DATA(gt_hier).

  cl_demo_output=>write_data( gt_hier ).
  cl_demo_output=>display( ).
```

  

![](Files/image%20262.png)  

  

GT\_HIER

|     |     |     |     |     |     |     |     |     |     |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| EMPLOYEE | NODE\_TEXT | NODE\_ID | PARENT\_ID | HIERARCHY\_LEVEL | HIERARCHY\_TREE\_SIZE | HIERARCHY\_RANK | HIERARCHY\_PARENT\_RANK | HIERARCHY\_IS\_ORPHAN | HIERARCHY\_IS\_CYCLE |
| 0001 | A   | 0001 | 0000 | 1   | 8   | 1   | 0   | 0   | 0   |
| 0002 | B   | 0002 | 0001 | 2   | 1   | 2   | 1   | 0   | 0   |
| 0003 | C   | 0003 | 0001 | 2   | 2   | 3   | 1   | 0   | 0   |
| 0004 | D   | 0004 | 0003 | 3   | 1   | 4   | 3   | 0   | 0   |
| 0005 | E   | 0005 | 0001 | 2   | 4   | 5   | 1   | 0   | 0   |
| 0006 | F   | 0006 | 0005 | 3   | 1   | 6   | 5   | 0   | 0   |
| 0007 | G   | 0007 | 0005 | 3   | 2   | 7   | 5   | 0   | 0   |
| 0008 | H   | 0008 | 0007 | 4   | 1   | 8   | 7   | 0   | 0   |
| 0009 | I   | 0009 | 0000 | 1   | 2   | 9   | 0   | 0   | 0   |
| 0010 | J   | 0010 | 0009 | 2   | 1   | 10  | 9   | 0   | 0   |

  

필드설명 

- node\_id 
    - 계층의 노드의 기술적인 식별자
    - 위의 예제에서는 Employee
- parent\_id
    - 계층 노드의 Parent Node의 기술적 식별자
- hierarchy\_level
    - 계층구조에서 노드의 레벨
    - 루트노드는 레벨이 1
- hierarchy\_tree\_size
    - 해당 노드를 포함한 자식노드의 개수
    - 자식노드수 + 1 
- hierarchy\_rank
    - 계층순서번호
    - Root 부터 계층을 탐색하면서 1부터 순차적으로 번호가 생성
    - 계층의 데이터행에 대한 고유 식별자
- hierarchy\_parent\_rank
    - 부모노드의 계층순서번호
- hierarchy\_is\_orphan
    - 부모노드가 없으면서 루트노드가 아닌 값은 1
- hierarchy\_is\_cycle
    - cycle이 있는 노드인 경우에는 1

  

다음은 ALV Tree를 이용해서 해당 계층구조를 출력한 예시이다

```
*&---------------------------------------------------------------------*
*& Report ZRDS_EDU_HIER02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrds_edu_hier02.

TYPES: BEGIN OF ty_s_hier_display.
TYPES :
  employee              TYPE zrds_i_hier_employee_info-employee,
  node_text             TYPE text40,
  node_id               TYPE text20,
  parent_id             TYPE text20,
  hierarchy_level       TYPE i,
  hierarchy_tree_size   TYPE i,
  hierarchy_rank        TYPE i,
  hierarchy_parent_rank TYPE i,
  hierarchy_is_orphan   TYPE int1,
  hierarchy_is_cycle    TYPE int1,
  alv_node_key          TYPE salv_de_node_key.
TYPES : END OF ty_s_hier_display.
TYPES : ty_t_hier_display TYPE STANDARD TABLE OF ty_s_hier_display.

START-OF-SELECTION.

  "**********************************************************************
  " FIND TREE DATA
  "**********************************************************************
  DATA : gt_hierarchy_display TYPE ty_t_hier_display.

  SELECT FROM zrds_i_hier_employee_info
  FIELDS
    employee,
    \_employee-employname AS node_text,
    node_id,
    parent_id,
    hierarchy_level,
    hierarchy_tree_size,
    hierarchy_rank,
    hierarchy_parent_rank,
    hierarchy_is_orphan,
    hierarchy_is_cycle
  ORDER BY hierarchy_rank
  INTO CORRESPONDING FIELDS OF TABLE @gt_hierarchy_display[].

  "**********************************************************************
  " ALV
  "**********************************************************************
  DATA : gr_tree               TYPE REF TO cl_salv_tree,
         gt_hierarchy_display2 TYPE ty_t_hier_display,
         lr_alv_node           TYPE REF TO cl_salv_node,
         lv_tree_text          TYPE lvc_value.

  cl_salv_tree=>factory(
    IMPORTING
      r_salv_tree = gr_tree
    CHANGING
      t_table     = gt_hierarchy_display2 ).

  DATA(gr_alv_nodes) = gr_tree->get_nodes( ).

  LOOP AT gt_hierarchy_display ASSIGNING FIELD-SYMBOL(<fs_node_data>).

    IF <fs_node_data>-hierarchy_level = 1.
      lv_tree_text = <fs_node_data>-node_text.
      lr_alv_node  = gr_alv_nodes->add_node(
        related_node = space
        relationship = cl_gui_column_tree=>relat_last_child
        text         = lv_tree_text ).

      <fs_node_data>-alv_node_key = lr_alv_node->get_key( ).
      lr_alv_node->set_data_row( <fs_node_data> ).
    ELSE.
      READ TABLE gt_hierarchy_display
      WITH KEY
        hierarchy_rank = <fs_node_data>-hierarchy_parent_rank
      ASSIGNING FIELD-SYMBOL(<fs_parent_node>).

      lv_tree_text = <fs_node_data>-node_text.
      lr_alv_node  = gr_alv_nodes->add_node(
        related_node  = <fs_parent_node>-alv_node_key
        relationship  = cl_gui_column_tree=>relat_last_child
        text          = lv_tree_text ).
      <fs_node_data>-alv_node_key = lr_alv_node->get_key( ).
    ENDIF.
  ENDLOOP.

  DATA(lr_columns) = gr_tree->get_columns( ).
  lr_columns->set_optimize( 'X' ).
  DATA: lr_column TYPE REF TO cl_salv_column_tree.
  lr_column ?= lr_columns->get_column( 'NODE_TEXT' ).
  lr_column->set_short_text('Name').
  lr_column->set_technical(  ).
  lr_column ?= lr_columns->get_column( 'NODE_ID' ).
  lr_column->set_short_text('NodeID').
  lr_column ?= lr_columns->get_column( 'PARENT_ID' ).
  lr_column->set_short_text('ParentID').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_LEVEL' ).
  lr_column->set_short_text('Lvl').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_TREE_SIZE' ).
  lr_column->set_short_text('TrSz').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_RANK' ).
  lr_column->set_short_text('Rnk').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_PARENT_RANK' ).
  lr_column->set_short_text('PaRnk').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_IS_ORPHAN' ).
  lr_column->set_short_text('Orph').
  lr_column ?= lr_columns->get_column( 'HIERARCHY_IS_CYCLE' ).
  lr_column->set_short_text('Cycle').
  lr_column ?= lr_columns->get_column( 'EMPLOYEE' ).
  lr_column->set_short_text('Employee').
  lr_column ?= lr_columns->get_column( 'ALV_NODE_KEY' ).
  lr_column->set_short_text('ALVKey').
  lr_column->set_technical(  ).

  gr_tree->display( ).
```

  

다음과 같이 출력이 된다.

![](Files/image%20263.png)