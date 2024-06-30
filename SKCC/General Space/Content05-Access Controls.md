## Content05-Access Controls

  

### 최소권한의 원칙

- 비지니스 작업을 수행하는데 필요한 최소한의 데이터만 접근하는 권한만 가져야 한다.

  

### 권한제어

- **기능 수준**
    - ex) 영업팀은 영업관련 프로그램만 접근이 가능
- **개별 데이터 수준**
    - ex) 미국을 담당하는 영업팀은 해당 데이터만 조회 가능

  

CDS의 권한제어는 CDS 모델의 선택결과를 특정 데이터만으로 제어하는 “개별 데이터 수준"의 권한을 제어하는 기능을 제공한다.

  

## Access Control 기본

  

### 생성절차

  

신규 개발오브젝트 생성 메뉴를 실행한다.

![](Files/image%20114.png)  

  

Access Control을 검색하여 해당 항목을 선택한다.

![](Files/image%20115.png)  

  

생성오브젝트의 정보를 입력한다

- 생성되는 Access Control의 이름은 CDS명과 동일하게 한다

![](Files/image%20116.png)  

  

Request를 선택한다.

![](Files/image%20117.png)  

  

Access Control의 유형을 선택한다. 예제에서는 권한오브젝트를 가지고 제어하는 유형을 선택한다.

![](Files/image%20118.png)  

  

권한오브젝트와 해당 필드를 이용해서 다음과 같이 권한 Rule을 작성한다.

- CDS에 Where절을 추가하는 것과 동일하다
- 권한오브젝트, 권한필드... , ACTVT 순서로 입력하며 WHERE절의 필드(SalesOrderType)는 매핑되는 권한필드 (AUART)와 동일 순서로 입력해야 한다.

```
@EndUserText.label: 'AC - 판매오더아이템'
@MappingRole: true
define role ZSD_I_SALESORDERITEM {
  grant 
    select
      on
        ZSD_I_SALESORDERITEM
          where
            (_Head.SalesOrderType) = aspect pfcg_auth(V_VBAK_AAT, AUART, ACTVT = '03');
            
}
```

V\_VBAK\_AAT 권한오브젝트에 커서를 올리고 F2를 누르면 다음과 같이 Help 화면이 출력된다.

![](Files/image%20119.png)  

  

권한 조건은 여러개를 입력할 수 있다.

```
@EndUserText.label: 'AC - 판매오더아이템'
@MappingRole: true
define role ZSD_I_SALESORDERITEM {
  grant 
    select
      on
        ZSD_I_SALESORDERITEM
          where (_Head.SalesOrderType) = aspect pfcg_auth(V_VBAK_AAT, AUART, ACTVT = '03')
            and ( _Head.OrganizationDivision, _Head._SalesOrganization.SalesOrganization, _Head.DistributionChannel ) 
                = aspect pfcg_auth( V_VBAK_VKO, SPART, VKORG, VTWEG, ACTVT = '03');
            
}
```

  

다음으로 관련 CDS뷰로 이동하여 ACCESS CONTROL을 적용하도록 Annotation을 추가한다.

![](Files/image%20120.png)  

  

## Mode of Action of Access Control

  

- ABAP Runtime에서 Access Control이 적용된 CDS를 실행하게 되면 자동으로 권한설정이 적용된다.
    - 추가 필터로 적용

  

생성된 Access Control은 ABAP T-Code : **SACM**을 통해서 적용된 상태 및 Simulation을 할 수 있다.

  

T-Code : **SACM**을 실행한다.

![](Files/image%20121.png)  

  

현재 적용된 항목을 확인하기 위해서 “Access Controls (DCL Sources)”를 선택한다.

![](Files/image%20122.png)  

  

점검할 CDS뷰를 입력하고 조회한다.

![](Files/image%20123.png)  

  

검색된 항목을 더블클릭한다.

![](Files/image%20124.png)  

![](Files/image%20125.png)  

  

Runtime상에 적용되는 여부를 시뮬레이션하기 위해서 “Runtime Simulator”를 실행한다.

![](Files/image%20126.png)  

  

CDS Entity 및 사용자를 입력하고 실행한다.

![](Files/image%20127.png)  

  

권한 적용여부를 확인한다.

![](Files/image%20128.png)  

  

ABAP 프로그램을 실행할 때 Access Control의 Trace를 하기 위해서 “Runtime Trace Analyzer”를 실행한다.

![](Files/image%20129.png)  

  

“Start Tracing”을 실행한다.

![](Files/image%20130.png)  

  

프로그램을 실행한다.

![](Files/image%20131.png)  

  

“Stop Tracing”을 실행한다.

![](Files/image%20132.png)  

  

“Show Trace”를 눌러서 확인한다.

![](Files/image%20133.png)  

![](Files/image%20134.png)  

상세내역을 확인하기 위해서는 해당 라인을 더블클릭한다.

![](Files/image%20135.png)  

  

CDS뷰의 Access Control은 직접 로직에서 접근하는 CDS의 Access Control만 유효하며, 직접 사용하지 않는 하위 CDS뷰의 Access Control은 효과가 없다

  

![](Files/image%20136.png)  

![](Files/image%20137.png)  

- ViewA의 Access Control 적용

  

![](Files/image%20138.png)  

- ViewC의 Access Control이 적용

  

![](Files/image%20139.png)  

- ViewC 및 ViewD의 Access Control 적용

  

![](Files/image%20140.png)  

- ViewB와 ViewC의 Access Control 적용

  

  

### @AccessControl.authorizationCheck Annotation 설정값

|     |     |
| --- | --- |
| **Annotation 값** | **설명⁠** |
| CHECK | Access Control 설정을 적용 |
| NOT\_REQUIRED | Access Control 설정을 적용하지 않음 |
| NOT\_ALLOWED | Access Control 생성을 허용하지 않음. 런타임에 적용이 되는 설정 |
| PRIVILEGED\_ONLY | 데이터검색은 Previleged Access에 의해서만 가능 |

  

### 알아야할 사항

- 하나의 CDS에 여러개의 Access Control 파일을 설정할 수 있으며, 각각은 OR 조건으로 연결이 되게 된다.
- CDS의 이름과 Access Control의 이름은 동일하게 사용하는 것이 관리 차원에서 좋다.

  

  

## Implementation Patterns for Access Controls

  

### 경로표현식을 이용한 권한 제어

```
@EndUserText.label: 'AC - 판매오더아이템'
@MappingRole: true
define role ZSD_I_SALESORDERITEM {
  grant 
    select
      on
        ZSD_I_SALESORDERITEM
          where (_Head.SalesOrderType) = aspect pfcg_auth(V_VBAK_AAT, AUART, ACTVT = '03')
            and ( _Head.OrganizationDivision, _Head._SalesOrganization.SalesOrganization, _Head.DistributionChannel ) 
                = aspect pfcg_auth( V_VBAK_VKO, SPART, VKORG, VTWEG, ACTVT = '03');
            
}
```

  

  

### Adapt Evaluation Logic of Path Expressions

  

판매문서의 아이템의 모든 제품에 대해서 접근 권한이 있는 사용자가 판매문서도 접근할 수 있도록 하기 위해서 Access Control을 활용하는 방법

예제를 위해서 다음의 CDS를 선언한다.

  

#### \- 판매오더 헤더

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - Path Expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE63
  as select distinct from t000
  association [0..*] to ZCM_EXAMPLE64 as _Item on $projection.SalesOrder = _Item.SalesOrder
{
  key 'S1' as SalesOrder,

      _Item
}
union all select distinct from t000
association [0..*] to ZCM_EXAMPLE64 as _Item on $projection.SalesOrder = _Item.SalesOrder
{
  key 'S2' as SalesOrder,

      _Item
}
union all select distinct from t000
association [0..*] to ZCM_EXAMPLE64 as _Item on $projection.SalesOrder = _Item.SalesOrder
{
  key 'S3' as SalesOrder,

      _Item
}
```

![](Files/image%20141.png)  

#### \- 판매오더아이템

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Access Control - Path Expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE64
  as select distinct from t000
  association [0..1] to ZCM_EXAMPLE65 as _Product on $projection.Product = _Product.Product
{
  key 'S1' as SalesOrder,

  key 'I1' as SalesOrderItem,

      'P1' as Product,

      _Product
}
union select distinct from t000
association [0..1] to ZCM_EXAMPLE65 as _Product on $projection.Product = _Product.Product
{
  key 'S1' as SalesOrder,

  key 'I2' as SalesOrderItem,

      'P1' as Product,

      _Product
}
union select distinct from t000
association [0..1] to ZCM_EXAMPLE65 as _Product on $projection.Product = _Product.Product
{
  key 'S2' as SalesOrder,

  key 'I1' as SalesOrderItem,

      'P1' as Product,

      _Product
}
union select distinct from t000
association [0..1] to ZCM_EXAMPLE65 as _Product on $projection.Product = _Product.Product
{
  key 'S2' as SalesOrder,

  key 'I2' as SalesOrderItem,

      'P2' as Product,

      _Product
}
union select distinct from t000
association [0..1] to ZCM_EXAMPLE65 as _Product on $projection.Product = _Product.Product
{
  key 'S3' as SalesOrder,

  key 'I1' as SalesOrderItem,

      ''   as Product,

      _Product
}
```

![](Files/image%20142.png)  

  

#### \- 제품

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Access Control - Path Expression'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE65
  as select distinct from t000
{
  key 'P1' as Product,

      'A1' as AuthorizationGroup
}
union select distinct from t000
{
  key 'P2' as Product,

      'A2' as AuthorizationGroup
}
```

![](Files/image%20143.png)  

  

#### \- 판매오더 Access Control

- 오더아이템의 제품에 대해서 다음과 같이 권한 설정을 한다

  

아래는 판매오더를 조회하기 위해서는 판매오더아이템의 제품의 AuthorizationGroup의 값을 사용자가 적어도 하나를 가지고 있으면 가능한 Rule을 설정하였다

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE63 {
  grant 
    select
      on
        ZCM_EXAMPLE63
          where
            (_Item._Product.AuthorizationGroup) ?= aspect pfcg_auth(M_MATE_MAT, BEGRU, ACTVT = '03');
            
}
```

  

아래는 판매오더를 조회하기 위해서 판매오더아이템의 제품의 모든 AuthorizationGroup의 값을 사용자가 가지고 있어야 하는 Rule을 설정하였다.

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE63 {
  grant 
    select
      on
        ZCM_EXAMPLE63
          where
            all (_Item._Product.AuthorizationGroup) ?= aspect pfcg_auth(M_MATE_MAT, BEGRU, ACTVT = '03');
            
}
```

  

만일 제품을 가지지 않는 아이템이 있는 경우에는 위의 Rule에 의해서는 접근할 수 없기 때문에 없는 경우 skip할수 있도록 다음과 같이 Rule을 변경하여 설정할 수 있다.

- bypass when is null 구문을 사용

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE63 {
  grant 
    select
      on
        ZCM_EXAMPLE63
          where
            all (_Item._Product.AuthorizationGroup bypass when is null ) ?= aspect pfcg_auth(M_MATE_MAT, BEGRU, ACTVT = '03');
            
}
```

  

  

## Path 표현식의 파라미터 다루기

- Association 관계의 CDS에 파라미터가 있는 경우에는 Access Control을 생성할 때에도 해당 값에 값을 넘겨줘야 한다.

  

### 예제

  

#### 판매오더헤더

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - Parameter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE66
  with parameters
    P_Parameter1A : abap.char(1),
    P_Parameter1B : abap.char(1),
    P_Parameter1C : abap.char(1)
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

  

#### 판매오더아이템

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Access Control - Parameter'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE67
  with parameters
    P_Parameter2A : abap.char(1),
    P_Parameter2B : abap.char(1)
  as select from ZSD_I_SALESORDERITEM
  association [0..1] to ZCM_EXAMPLE66 as _Head on $projection.SalesOrderId = _Head.SalesOrderId
{
  key SalesOrderItemId,

      SalesOrderId,

      SalesOrderItem,
      
      _Head
}
```

  

판매오더아이템 - Access Control

- Path 표현식을 이용하기 위해서는 판매오더헤더의 파라미터의 값을 Rule에서 같이 넘겨줘야 한다.

```
@EndUserText.label: 'AC - 판매오더아이템'
@MappingRole: true
define role ZCM_EXAMPLE67 {
  grant 
    select
      on
        ZCM_EXAMPLE67
          where
            ( _Head( P_Parameter1A : $parameters.P_Parameter2A,
                     P_Parameter1B : $parameters.P_Parameter2B,
                     P_Parameter1C : 'X' ).SalesOrderType ) = aspect pfcg_auth( V_VBAK_AAT, AUART, ACTVT = '03');
            
}
```

  

  

  

## Access Control 상속

- 기존 하위 CDS에서 사용하던 Rule을 재사용할 수 있다.

  

#### 판매오더

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Access Control - 상속'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE68
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

  

#### 판매오더 - Access Control

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE68 {
  grant 
    select
      on
        ZCM_EXAMPLE68
          where
            (SalesOrderType) = aspect pfcg_auth(V_VBAK_AAT, AUART, ACTVT = '03');
}
```

  

다음은 위의 판매오더를 데이터소스로 해서 다음의 CDS를 생성하였고, Access Control은 상속을 하여 설정하였다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - 상속'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE69
  as select from ZCM_EXAMPLE68
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

##### 

##### **Access Control**

다음의 두가지 방법으로 권한 Rule을 상속받아서 사용할 수 있다.

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE69 {
  grant 
    select
      on
        ZCM_EXAMPLE69
          where inheriting conditions from entity ZCM_EXAMPLE68;
            
}
```

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE69 {
  grant 
    select
      on
        ZCM_EXAMPLE69
          where inherit ZCM_EXAMPLE68 for grant select on ZCM_EXAMPLE68;
            
}
```

바로 DCL 파일을 상속받아서 다음과 같이 사용도 가능하다

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE69 {
  grant 
    select
      on
        ZCM_EXAMPLE69
          inherit ZCM_EXAMPLE68;
            
}
```

  

Access Control 상속시에 고려 사항

- Rule에 사용된 필드는 상속하는 CDS에서도 필드로 포함되어 있어야 한다.
- Rule에 변경이 있는 경우나 사용되는 필드가 변경이 되는 경우에는 어떤 Side effect가 발생할지 확인하기 힘드므로 되도록이면 상속은 사용하지 않는 방법으로 종속성을 제거하는게 좋다

  

  

### 상속된 Access Control의 조건을 재정의 하기

  

상속된 Rule의 조건은 다음과 같이 변경이 가능하다.

  

#### \- 판매오더

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - 상속'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE70
  as select from ZCM_EXAMPLE68
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

  

다음과 같이 replacing ~ pfcg\_filter 구문을 통해서 재정의가 가능하다

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE70 {
  grant 
    select
      on
        ZCM_EXAMPLE70
         where inheriting conditions from entity ZCM_EXAMPLE68 
          replacing { pfcg_filter field actvt value '03' with 'F4' }
            OR inheriting conditions from entity ZCM_EXAMPLE68;
            
}
```

  

  

## Optional Elements

  

Access Control을 상속하는 경우 특정 요소가 상속하는 CDS에 존재하지 않아도 해당 권한 설정을 상속하기를 원하는 경우에, 해당 요소를 true 또는 false로 기본값을 설정하여 요소가 없는 경우에 기본값이 적용이 되어서 Rule을 실행할 수 있게 해준다.

  

먼저 다음과 같이 CDS를 선언한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - Optional'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE71
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType,

      OrganizationDivision,

      _SalesOrganization.SalesOrganization,

      DistributionChannel
}
```

  

위 CDS의 Access Control을 다음과 같이 작성한다.

- OrganizationDivision, SalesOrganization, DistributionChannel 등은 옵션으로 정의함

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE71 {
  grant 
    select
      on
        ZCM_EXAMPLE71
          with optional elements ( OrganizationDivision default true,
                                   SalesOrganization default true,
                                   DistributionChannel default true )
          where ( SalesOrderType ) = aspect pfcg_auth ( V_VBAK_AAT, AUART, ACTVT = '03' )
            and ( OrganizationDivision, SalesOrganization, DistributionChannel ) = 
                  aspect pfcg_auth ( V_VBAK_VKO, SPART, VKORG, VTWEG, ACTVT = '03' );
}
```

  

위에서 선언한 CDS를 상속받아서 다음의 CDS를 생성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - Optional'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE72
  as select from ZCM_EXAMPLE71
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

  

다음과 같이 Access Control을 생성한다.

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE72 {
  grant 
    select
      on 
        ZCM_EXAMPLE72
          where inheriting conditions from entity ZCM_EXAMPLE71;     
} 
```

옵션 필드에 대해서 다음과 같이 warning 및 info 정보가 표시가 된다.

![](Files/image%20144.png)  

  

  

## 이름이 변경된 필드에 대한 권한 처리

  

상위 CDS에서 필드의 이름이 변경이 되면 replacing 구문을 이용하여 변경된 필드에 권한이 적용되도록 설정한다.

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE73 {
  grant 
    select
      on
        ZCM_EXAMPLE73
          where
            inherit ZCM_EXAMPLE71 for grant select on ZCM_EXAMPLE71 
            replacing {  element SalesOrderType with RenamedSalesOrderType };
}
```

  

  

  

## 권한오브젝트를 사용하지 않고 Access Control 생성하기

  

### 현재 로그인한 사용자를 조건으로 사용

```
@EndUserText.label: 'AC - 판매오더'
@MappingRole: true
define role ZCM_EXAMPLE74 {
  grant 
    select
      on
        ZCM_EXAMPLE74
          where
            CreatedBy = aspect user;
            
}
```

  

  

### Custom aspect를 만들어서 Access Control에 사용하기

  

먼저 Aspect에 사용할 데이터 소스 CDS를 선언한다

- Custom aspect를 적용하기 위해서 두개의 추가 annotation을 설정한다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - custom aspect'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@AccessControl.auditing.specification: '...'
@AccessControl.auditing.type: #CUSTOM
define view entity ZCM_EXAMPLE75
  as select distinct from t000
{
  key cast( abap.char'USER_A' as vdm_userid ) as UserID,
      abap.char'TAF'                          as SalesOrderType
}
union select distinct from t000
{
  key abap.char'USER_B' as UserID,
      abap.char'OAF'    as SalesOrderType
}
```

  

위의 CDS를 이용하여 Custom Aspect를 생성한다. Access Control을 생성하는 화면을 실행한다.

![](Files/image%20145.png)  

![](Files/image%20146.png)  

![](Files/image%20147.png)  

다음과 같이 작성한다

```
define accesspolicy ZCM_EXAMPLE75_ASPECT {
  @EndUserText.label: 'Aspect'
  define aspect ZCM_EXAMPLE75_ASPECT as
    select from ZCM_EXAMPLE75 with user element UserID
      {
        SalesOrderType
      }
      
}
```

  

Custom aspect를 적용할 CDS뷰를 다음과 같이 선언한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - custom aspect'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE76
  as select distinct from t000
{
  key abap.char'S1'  as SalesOrder,
      abap.char'TAF' as SalesOrderType
}
union all select distinct from t000
{
  key abap.char'S2'  as SalesOrder,
      abap.char'TAF' as SalesOrderType
}
union all select distinct from t000
{
  key abap.char'S3'  as SalesOrder,
      abap.char'OAF' as SalesOrderType
}
```

  

위 CDS에 대해서 다음과 같이 Access Control을 작성한다.

```
@EndUserText.label: 'Aspect Test'
@MappingRole: true
define role ZCM_EXAMPLE76 {
  grant
    select
      on
        ZCM_EXAMPLE76
          where
            ( SalesOrderType ) = aspect ZCM_EXAMPLE75_ASPECT;
             
}
```

  

  

### 리터럴을 이용한 Access Control

  

문자열등의 상수값을 이용하여 Access Control의 Rule을 정의할 수 있다. 다음은 SalesOrderType이 초기값을 가지는 항목에 대해서는 권한오브젝트가 없어도 접근할 수 있도록 한다.

Ex)

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - Literal'
@Metadata.ignorePropagatedAnnotations: false
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE77
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

해당 CDS의 Access Control을 리터럴을 이용하여 선언하였다.

```
@EndUserText.label: 'AC - Literal'
@MappingRole: true
define role ZCM_EXAMPLE77 {
  grant 
    select
      on
        ZCM_EXAMPLE77
          where (SalesOrderType) = aspect pfcg_auth(V_VBAK_AAT, AUART, ACTVT = '03')
             or SalesOrderType = '';         
}
```

  

  

## ?= 을 통한 비교

- 초기값과 null 값을 동시에 비교하고자 하는 경우에 사용
- 성능에 영향을 미칠 수 있으므로 초기값 null 값 이외에는 사용하지 않는 것이 좋다

  

Ex)

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Access Control - ?='
@Metadata.ignorePropagatedAnnotations: false
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE78
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      SalesOrderType
}
```

Access Control에 ?=을 사용하면 is null or = ‘’ 의 비교와 동일하게 작동한다.

```
@EndUserText.label: 'AC - ?='
@MappingRole: true
define role ZCM_EXAMPLE78 {
  grant 
    select
      on
        ZCM_EXAMPLE78
          where
            SalesOrderType ?= 'TAF';
            
}
```

위의 Access Control Rule과 아래의 조건이 동일하다.

```
@EndUserText.label: 'AC - ?='
@MappingRole: true
define role ZCM_EXAMPLE78 {
  grant 
    select
      on
        ZCM_EXAMPLE78
          where SalesOrderType = ''
             or SalesOrderType is null 
             or SalesOrderType = 'TAF';
            
}
```

  

  

## 분석쿼리에 대한 Access Control 구현

  

분석쿼리뷰

- @Analytics.query: true 어노테이션으로 설정
- 분석엔진에 의해서 해석
- CDS뷰 자체로 검색할 수 없으며 단지 분석에 필요한 메타데이터만을 분석엔진으로 전달하는 역할을 수행
- 분석엔진은 쿼리뷰의 데이터소스에서 직접 데이터를 선택한다
- Access Control은 분석쿼리가 아닌 분석쿼리의 데이터소스에 설정이 되어야 한다.

  

![](Files/image%20148.png)  

  

권한체크

- **Primary 데이터소스**
    - Analytics.dataCategory:  #CUBE (큐브뷰)
    - Analytics.dataCategory: #DIMENSION (차원뷰)
    - 큐브 또는 차원뷰의 Access Control은 데이터를 선택하는데 영향을 준다
- **Display attributes**
    - 연관되어 있는 Dimension뷰의 속성이 분석쿼리의 결과 집합의 속성으로 포함되어 있더라도 해당뷰의 Access Control은 결과에 아무런 영향을 미치지 않는다
- **Value helps and lookup entities**
    - Dimension View가 분석쿼리에서 Value Help로 사용되는 경우에는 해당 뷰의 Access Control은 평가가 된다
    - @Consumption.valueHelpDefinion의 CDS와 @Consumption.derivation.lookupEntity의 CDS의 Access Control도 Access Control이 평가가 되어진다
- **Value helps on query results**
    - Value Help를 쿼리결과에서 사용하는 경우에는 먼저 해당 필드의 값을 가져오는 것은 Primary 데이터소스의 Access Control에 영향을 받지만 가져온 값을 이용해서 Value Help에서 값을 가져올 때는 Value Help의 Access Control은 영향을 미치지 않는다
- **Hierarchies and hierarchy directories**
    - @ObjectModel.dataCategory: #HIERARCHY 
        - 계층을 나타내는 View의 Access Control은 이를 사용하는 쿼리에는 영향을 미치지 않는다
    - @Hierarchy.parentChild.directory (계층 디렉터리)
        - 해당 Access Control은 효과가 있다

- **Texts**
    - 계층 디렉터리의 경우 할당된 텍스트 (@ObjectModel.dataCategory: #TEXT )의 Access Control이 영향을 미친다
- **Reused views**
    - 재사용되는 CDS 뷰를 분석엔진에서 접근하는 경우에 CDS뷰의 Access Control이 적용이 되지 않더라도 해당 뷰를 보호할 필요가 있다