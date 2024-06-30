# Content02-Fundamentals of CDS Data Modeling

  

## Overview of CDS Models

  

**CDS 유형별 특징**

|     |     |     |
| --- | --- | --- |
| **CDS 모델** | **정의** | **사용어플리케이션** |
| View | define view .. as select from ... | 데이터 검색 |
| View entity | define view entity ... as select from ... | 데이터 검색 |
| Projection view | define view entity ... as projection on ... | 데이터 검색 |
| Extend view | extend view ... with ... | CDS 뷰 Extension |
| Extend view entity | extend view entity ... with ... | CDS 뷰 엔터티 Extension |
| Table function | define table function ... | SAP HANA Native 함수를 가지고 데이터 검색 |
| Custom entity | define custom entity ... | OData 서비스 기반의 SADL Framework에서 ABAP 기반으로 데이터를 검색 |
| Abstract entity | define abstract entity ... | 구조를 모델링 |
| Hierarchy | define hierarchy entity ... | 계증구조를 모델링 |
| Metadata extension | annotate view ... with ... | CDS에 별도의 파일로 어노테이션을 추가하는 용도 |

  

**CDS 생성시 CDS 유형을 선택하는 화면**

![](Files/image%2048.png)  

  

|     |     |
| --- | --- |
| **Define View Entity** | Defines a simple CDS view entity with one data source.<br><br><br>```<br>@AbapCatalog.viewEnhancementCategory: [#NONE]<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>@Metadata.ignorePropagatedAnnotations: true<br>@ObjectModel.usageType:{<br>	serviceQuality: #X,<br>	sizeCategory: #S,<br>	dataClass: #MIXED<br>}<br>define view entity ${ddl_source_name} as select from ${data_source_name}<br>{<br>	${data_source_elements}${cursor}<br>}<br>``` |
| **Define Root View Entity** | Defines a root CDS view entity with a specialized association to a child CDS entity.<br><br>  <br><br>Root nodes, compositions and to-parent associations are used to define the structure of a business object which can be used in the ABAP RESTful programming model.<br><br><br><br><br>```<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define root view entity ${ddl_source_name} as select from ${data_source_name}<br>composition of ${target_data_source_name} as ${_association_name}<br>{<br>	${data_source_elements}${cursor}<br>	${_association_name} // Make association public<br>}<br>``` |
| **Define View Entity with To-Parent Association** | Defines a CDS view entity with a specialized association to its parent CDS entity.<br><br>  <br><br>Compositions and to-parent associations are used to define the structure of a business object which can be used in the ABAP RESTful programming model.<br><br>```<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define view entity ${ddl_source_name} as select from ${data_source_name}<br>association to parent ${target_data_source_name} as ${_association_name}<br>	on $$projection.${element_name} = ${_association_name}.${target_element_name}<br>{<br>	${data_source_elements}${cursor}<br>	${_association_name} // Make association public<br>}<br>``` |
| Define View (더이상 사용 안함) | Defines a simple CDS DDIC-based view with one data source.<br><br>  <br><br>```<br>@AbapCatalog.sqlViewName: '${sql_view_name}'<br>@AbapCatalog.compiler.compareFilter: true<br>@AbapCatalog.preserveKey: true<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define view ${ddl_source_name_editable} as select from ${data_source_name}<br>left outer join ${joined_data_source_name}<br>	on ${data_source_name}.${element_name} = ${joined_data_source_name}.${joined_element_name}<br>{<br>	${data_source_elements}${cursor}<br>}<br>``` |
| Define View with Join (더이상 사용 안함) | Defines a CDS DDIC-based view which combines two data sources using a left outer join.<br><br>  <br><br>The join conditions are specified in the on clause.<br><br>  <br><br>CDS DDIC-based views are obsolete as of Application Server ABAP 7.57. Use a template for a CDS view entity instead.<br><br>```<br>@AbapCatalog.sqlViewName: '${sql_view_name}'<br>@AbapCatalog.compiler.compareFilter: true<br>@AbapCatalog.preserveKey: true<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define view ${ddl_source_name_editable} as select from ${data_source_name}<br>association [${1}] to ${target_data_source_name} as ${_association_name}<br>	on $$projection.${element_name} = ${_association_name}.${target_element_name}<br>{<br>	${data_source_elements}${cursor}<br>	${_association_name} // Make association public<br>}<br>``` |
| Define View with To-Parent Association (더이상 사용 안함) | Defines a CDS DDIC-based view with a specialized association to its parent CDS entity.<br><br>  <br><br>Compositions and to-parent associations are used to define the structure of a business object that can be used in the ABAP RESTful Programming Model (RAP).<br><br>  <br><br>CDS DDIC-based views are obsolete as of Application Server ABAP 7.57. Use a template for a CDS view entity instead.<br><br>```<br>@AbapCatalog.sqlViewName: '${sql_view_name}'<br>@AbapCatalog.compiler.compareFilter: true<br>@AbapCatalog.preserveKey: true<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define view ${ddl_source_name_editable} as select from ${data_source_name}<br>association to parent ${target_data_source_name} as ${_association_name}<br>	on $$projection.${element_name} = ${_association_name}.${target_element_name}<br>{<br>	${data_source_elements}${cursor}<br>	${_association_name} // Make association public<br>}<br>``` |
| Define View with Parameters (더이상 사용 안함) | Defines a CDS DDIC-based view with a single input parameter.<br><br>  <br><br>The input parameter can be used as an element in the select list or as an operand in conditional or arithmetic expressions.<br><br>  <br><br>CDS DDIC-based views are obsolete as of Application Server ABAP 7.57. Use a template for a CDS view entity instead.<br><br><br><br><br>```<br>@AbapCatalog.sqlViewName: '${sql_view_name}'<br>@AbapCatalog.compiler.compareFilter: true<br>@AbapCatalog.preserveKey: true<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>@EndUserText.label: '${ddl_source_description}'<br>define view ${ddl_source_name_editable}<br>	with parameters ${parameter_name} : ${parameter_type}<br>as select from ${data_source_name}<br>{<br>	${data_source_elements}${cursor}<br>}<br>``` |
| **Define Projection View** | Defines a simple CDS projection view.<br><br><br>```<br>@EndUserText.label: '${ddl_source_description}'<br>@AccessControl.authorizationCheck: #NOT_REQUIRED<br>define view entity ${ddl_source_name} as projection on ${data_source_name}<br>{<br>	${data_source_elements}${cursor}<br>}<br>``` |
| **Extend View** | Extends an existing CDS view by adding the specified elements to the select list of the CDS view using a view enhancement.<br><br><br>```<br>@AbapCatalog.sqlViewAppendName: '${sql_view_append_name}'<br>@EndUserText.label: '${ddl_source_description}'<br>extend view ${view_name} with ${ddl_source_name_editable}<br>{<br>	${base_data_source_name}.${element_name}<br>}<br>``` |
| **Extend View Entity** | Extends an existing CDS projection view entity by adding the specified elements to its select list.<br><br><br>```<br>extend view entity ${view_name} with {<br>	${base_data_source_name}.${element_name}<br>}<br>``` |
| **Extend Abstract Entity** | Extends an abstract CDS entity by adding the specified elements to its select list.<br><br><br>```<br>extend abstract entity ${entity_name} with<br>{<br>    ${element_name} : ${element_type};<br>}<br>``` |
| **Extend Custom Entity** | Extends a custom CDS entity by adding the specified elements to its select list.<br><br><br>```<br>extend custom entity ${entity_name} with<br>{<br>    ${element_name} : ${element_type};<br>}<br>``` |
| **Define Table Function with Parameters** | Defines the type signature of a client dependent CDS table function with importing parameters. The CDS table function is implemented in the specified ABAP method.<br><br>  <br><br>The CDS table function can be used in Open SQL and as a data source in other CDS view definitions.<br><br><br><br><br>```<br>@EndUserText.label: '${ddl_source_description}'<br>define table function ${ddl_source_name_editable}<br>with parameters ${parameter_name} : ${parameter_type}<br>returns {<br>  ${client_element_name} : abap.clnt;<br>  ${element_name} : ${element_type};<br>  ${cursor}<br>}<br>implemented by method ${class_name}=>${method_name};<br>``` |
| **Define Abstract Entity with Parameters** | Defines an abstract CDS entity with a single input parameter.<br><br><br>```<br>@EndUserText.label: '${ddl_source_description}'<br>define abstract entity ${ddl_source_name}<br>  with parameters ${parameter_name} : ${parameter_type}<br>{<br>    ${element_name} : ${element_type};<br>    ${cursor}<br>}<br>``` |
| **Define Parent Child Hierarchy** | Defines a simple CDS parent child hierarchy.<br><br><br>```<br>define hierarchy ${ddl_source_name}<br>  as parent child hierarchy (<br>    source ${data_source_name}<br>    child to parent association ${_association_name}<br>    start where ${element_name} = ${value}<br>    siblings order by ${order_by_element_name}<br>  )<br>{<br>    ${element_name}<br>    ${cursor}<br>}<br>``` |
| **Define Custom Entity with Parameters** | Defines a custom CDS entity with a single input parameter.<br><br><br>```<br>@EndUserText.label: '${ddl_source_description}'<br>define custom entity ${ddl_source_name}<br> with parameters ${parameter_name} : ${parameter_type}<br>{<br>  key ${key_element_name} : ${key_element_type};<br>  ${element_name} : ${element_type};<br>  ${cursor}<br>}<br>``` |

  

  

**View(V1 View)와 View entity(V2 View)의 차이**

- View는 ABAP Dictionary View를 생성하며, View entity는 생성하지 않는다
- View entity가 좀 더 일관된 모델링을 강제하며, 여러가지 기술적인 이슈가 줄어듬
- View entity를 사용하는 것을 권장
- 마이그레이션을 하는 경우에는 레포트 RUTDDLSV2MIGRATION을 실행하여 수행 가능

  

**Projection View**

- 기존 View Entity의 모델링된 특성을 여러가지 용도로 필요한 부분만 노출하여 사용하고자 하는 경우에 생성
- 기능적으로 View Entity를 선언하여 사용하는 것보다 문법적으로 제약 사항이 많음

  

**View extensions**

- CDS 뷰 또는 CDS 뷰 엔터티에 필드/Association을 추가하여 확장

  

**Table functions**

- HANA SQLScript를 통해서 복잡한 데이터 로직 작성 가능
- CDS에서 지원하지 않는 여러가지 함수 사용 가능
- Table 함수를 사용하는 데 있어서 몇 가지 단점이 존재 하므로 요구사항에 맞게 취사선택 해야 함

  

**Custom entity**

- 데이터 모델의 선언부만을 사용하며 데이터 검색은 ABAP 로직을 통해서 이루어짐
- 데이터베이스레벨에서 수행이 불가능하며 ODATA를 통한 호출시에만 SADL 인프라를 이용해서 수행 가능

  

**Abstract entity**

- 구현없이 모델의 선언부만 정의하는데 사용
- RAP에서 Action과 Function의 매개변수로 주로 사용
- 외부서비스모델 엔터티에 대한 Proxy를 정의하는데 사용

  

**Hierarchies**

- SAP HANA의 계층 구조 기능을 사용할 수 있도록 해줌
- ABAP 로직에서도 사용 가능

  

**Metadata extensions**

- CDS의 Annotation을 CDS가 정의된 파일이 아닌 다른 파일에 놓고 작성 가능
- 주로 UI Annotation을 작성하는데 많이 사용

  

* * *

  

## Overview of CDS View Syntax

  

### CDS View 정의 예시

```
/* 주석사용법1 */
// 주석사용법2

// 어노테이션(Annotation) : @로 시작
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS 선언 예시'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_I_EXAMPLE01
  // 파라미터정의
  with parameters
    P_SalesOrderType : auart
  // 데이터소스 정의부. Alias를 통해서 소스내에서 다른 이름으로 사용 가능
  as select from           ZSD_I_SALESORDERITEM as ITEM
  // Join : Left outer, Inner Join
    left outer to one join ZSD_I_SALESORDER     as HEAD on HEAD.SalesOrderId = ITEM.SalesOrderId
  // Associaiton 정의부. $projection의 의미는 해당 뷰에서 최종 제공되는 뷰필드를 의미함
  association [0..1] to ZSD_I_PRODUCT as _Product on $projection.ProductId = _Product.ProductId
{
      // 키필드 선언부
  key ITEM.SalesOrderId,

      // Association 조건으로 사용된 필드
  key ITEM.ProductId,

      // 필드이름재정의
      HEAD.SalesOrder                                  as RenamedField,

      // 상수 선언
      abap.char'A'                                     as Constant1,
      cast(1 as abap.dec( 15, 2 ) )                    as Constant2,

      // 계산필드 (Calculated field)
      concat( ITEM._Product.Product, HEAD.SalesOrder ) as CalculatedField,

      // Aggregate
      count(*)                                         as AggregatedField,

      // Association되어 있는 항목에서 데이터추출 - Path 표현식
      ITEM._Product.Product,

      // 해당 뷰를 사용하는 곳에서 Association을 사용할 수 있도록 Projection 필드로 추가
      _Product
}
// 조건 - Join하는 데이터소스와 파라미터 사용
where
  HEAD.SalesOrderType = $parameters.P_SalesOrderType
// Aggregation Level
group by
  ITEM.SalesOrderId,
  HEAD.SalesOrder,
  ITEM.ProductId,
  ITEM._Product.Product
```

  

  

## **Key Fields**

- key 키워드 사용
- 여러개의 키를 선언 가능
- 필드리스트의 앞부분에 선언 해야 함
- 가능한한 짧은 단어를 사용하는 것이 좋음
- CDS 뷰 엔터티는 기본적으로 클라이언트 기준으로 자동 필터링 되므로 클라이언트는 키 필드에 포함할 필요가 없음
- 키필드를 통해서 cardinality 확인 및 access control등에서 사용이 된다.

![](Files/image%2049.png)  

  

## Cast Operation

  

- 계산필드의 유형 변환
- 기존필드의 유형을 데이터베이스 레벨에서 변경
- ABAP유형 및 데이터엘리먼트로 유형 변환 지원
    - 데이터엘리먼트로 변환한 경우에는 해당 데이터엘리먼트의 텍스트를 LABEL로 사용하게 됨
- PRESERVIING TYPE을 사용하면 데이터베이스레벨의 유형 변환은 일어나지 않고, 레이블텍스트 변경등에 사용
- 모든 유형이 변환을 지원하지 않으며, 때에 따라서 CDS 변환 함수등을 사용해서 변환할 필요가 있음
    - FLTP\_TO\_DEC 함수
- CDS Table 함수를 사용하는 경우에는 HANA 데이터베이스가 지원하는 CAST 기능을 사용할 수 있음
- 계산된 필드들은 반드시 명시적을 타입 캐스팅 필요
    - 계산된 데이터가 계산된 값과 유형이 불일치 할 수 있음 
    - 암묵적인 casting으로 인한 성능 저하
    - 예상치 못한 오류가 발생할 수 있음
- 변환가능한 타입에 대한 정보는 [SAP Help](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm "https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm")를 참조

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CAST 예제'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_I_EXAMPLE02
  as select from t000
{
      // Projected Field
  key logsys                                                      as LogicalSystem,

      '20240618'                                                  as CharacterFiled,

      cast ('20240618' as abap.dats)                              as DateField,

      cast( cast( 'E' as abap.lang ) as sylangu preserving type ) as LanguageField,

      1.2                                                         as FloatingPointField,

      fltp_to_dec( 1.2 as abap.dec(4,2) )                         as DecimalField

}
```

  

![](Files/image%2050.png)  

  

CDS가 어떤 타입으로 최종 생성되었는지는 F2를 눌러서 확인해 본다.

![](Files/image%2051.png)  

  

  

## Typed Literals

  

- 상수를 정의
- abap.유형'\[값\]’
- 상수는 항상 타입을 지정하여 선언해서 사용하는 것이 좋다

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '타입을 가지는 상수'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE03
  as select from t000
{
  // 암묵적인 문자 15자리 유형의 상수
  '      Char10   '                                            as CharacterValue,

  // 명시적으로 캐스팅하는 경우에는 길이가 짧은 경우에는 나머지 문자 제외
  cast( '      Char10   ' as abap.char(10) )                   as CastCharacterValue,

  // 타입이 지정된 상수를 선언하는 방법
  abap.char'      Char10   '                                   as TypedCharacterValue,

  // 타입이 지정된 상수에 텍스트 레이블을 제공하기 위해서는 preserving type 구문을 사용. 기술적으로 같은 유형이어야 함
  cast( abap.char'      Char10   ' as char15 preserving type ) as CastTypedCharacterValue,

  // 암묵적으로 부동소수점 유형으로 변환
  1234.56                                                      as FloatingPointValue,

  // 명시적으로 부동소스점 유형 지정
  abap.fltp'1234.56'                                           as TypedFloatingPointValue,

  // Float 유형을 Decimal 유형으로 변경 - 소수점이 원하는 값과 다를 수 있음 (1234.559999999 -> 1234.55)
  fltp_to_dec(1234.56 as abap.dec(6,2))                        as ConvertedDecimalValue,

  // Decimal 유형으로 선언 - 10진수 변환뒤에 소수점 적용 - 6자리의 2자리소수점
  abap.dec'1234.56'                                            as TypedDecimalValue,
  
  // Decimal 유형으로 선언 - 10자리의 4자리 소수점 
  abap.dec'001234.5600'                                        as TypeDecimalValue2

}
```

![](Files/image%2052.png)  

  

## Case 구문

  

- 조건에 따른 값을 계산하기 위해서 사용
- CASE - WHEN - ELSE 구문 사용
- 조건에 맞는 값이 없는 경우에 최종적으로 NULL 값이 될 수 있음
    - NULL 값은 데이터베이스 성능상에 안좋은 영향을 끼칠 수 있으므로 반드시 ELSE 구문을 통해서 null 값 대신에 알맞은 값으로 설정이 필요
- case (필드) 형태와 case when 조건들 형태의 두가지 case문을 작성할 수 있음

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CASE 구문'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE04
  as select from ZSD_I_SALESORDER
{
  key SalesOrderId,

      SalesOrder,

      // Cast 미적용 Case 문
      case (SalesOrderType)
        when 'TAF' then 'X'
        when 'OAF' then 'X'
        else ''
      end                   as IsStandardOrder,

      // Cast 적용 Case 문
      cast( case (SalesOrderType)
        when 'TAF' then 'X'
        when 'OAF' then 'X'
        else ''
      end as abap.char(3) ) as IsStandardOrderAsChar3,

      // 복잡한 조건으로 작성할 수 있는 Case 문
      case when SalesOrderType = 'TAF' or SalesOrderType = 'OAF' then 'X'
           when SalesOrderType like 'A%' then 'X'
         else ''
      end                   as IsStandardOrder2
}
```

  

  

## Session Variables

  

- ABAP의 SY 시스템 변수와 비슷
- CDS 구문내에서 바로 사용 가능

  

|     |     |     |
| --- | --- | --- |
| **CDS Session 변수** | **설명** | **관련 ABAP 코드** |
| $session.client | 현재 클라이언트 번호 | sy-mandt |
| $session.system\_date | 어플리케이션 서버의 시스템 날짜 | sy-datum<br> |
| $session.system\_language | 로그온 언어 | sy-langu |
| $session.user | 현재 사용자 | sy-uname |
| $session.user\_date | 사용자 설정 날짜 | sy-datlo |
| $session.user\_timezone | 사용자 타임존 | sy-zonlo |

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '$session 시스템변수'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE05
  as select from t000
{
  $session.client          as ClientField,

  $session.system_date     as SystemDateField,

  $session.system_language as SystemLanguageField,

  $session.user            as UserField,

  $session.user_date       as UserDateField,

  $session.user_timezone   as UserTimezoneField
}
```

![](Files/image%2053.png)  

  

  

## Client Handling

  

- 기본적으로 현재 로그인한 세션의 클라이언트로 자동 필터링
- 구문상에는 client를 명시적으로 선언하여 사용할 필요가 없음
- Table Function CDS뷰인 경우에는 클라이언트에 대한 처리가 필요하다 - [참조](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenddic_cds_table_functions.htm "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenddic_cds_table_functions.htm")

  

다음과 같이 CDS에서 “Show SQL Create Statement”를 실행해 보면 client가 조건에 적용되어 있는 것을 볼 수 있다.

  

 ![](Files/image%2054.png)

HANA에 생성되는 View 구문은 다음과 같으며, HANA DB의 SESSION\_CONTEXT 함수를 사용하여 기본적으로 클라이언트 설정을 하고 있음을 알 수 있다.

```
CREATE OR REPLACE VIEW "ZSD_I_PRODUCT" AS SELECT 
  "ZTSD0040"."CLIENT" AS "MANDT", 
  "ZTSD0040"."PRODUCTID" AS "PRODUCTID", 
  "ZTSD0040"."PRODUCT" AS "PRODUCT", 
  "ZTSD0040"."PRODUCTTYPE" AS "PRODUCTTYPE", 
  "ZTSD0040"."AUTHORIZATIONGROUP" AS "AUTHORIZATIONGROUP", 
  "ZTSD0040"."CREATEDBY" AS "CREATEDBY", 
  "ZTSD0040"."CREATEDAT" AS "CREATEDAT", 
  "ZTSD0040"."LOCALLASTCHANGEDBY" AS "LOCALLASTCHANGEDBY", 
  "ZTSD0040"."LOCALLASTCHANGEDAT" AS "LOCALLASTCHANGEDAT", 
  "ZTSD0040"."LASTCHANGEDBY" AS "LASTCHANGEDBY", 
  "ZTSD0040"."LASTCHANGEDAT" AS "LASTCHANGEDAT" 
FROM "ZTSD0040" "ZTSD0040" 
WHERE "ZTSD0040"."CLIENT" = SESSION_CONTEXT(
  'CDS_CLIENT'
)
```

  

  

  

## **Union Views**

  

- 여러개의 SELECT 문을 결합하는데 사용
    - 동일한 필드명
    - 동일한 필드 순서
    - 동일한 Association
    - 동일한 키정의
    - 첫번째 SELECT 구문에만 Annotation을 입력 가능

  

**SouceA**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE06
  as select distinct from t000
  association [0..1] to ZCM_EXAMPLE08 as _ViewC on $projection.FieldA3 = _ViewC.FieldC1
{
  key cast( 'A' as abap.char(1) ) as FieldA1,

      cast( 'B' as abap.char(1) ) as FieldA2,

      cast( 'C' as abap.char(2) ) as FieldA3,

      _ViewC
}
```

  

**SourceB**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union - Source B'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE07
  as select from t000
{
  key cast( 'B_X' as abap.char(3) ) as FieldB1,

      cast( 'A' as abap.char(1) )   as FieldB2
}
```

  

**SourceC**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union - Source C'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE08
  as select from t000
{
  key cast('C' as abap.char(2) ) as FieldC1,

      cast('C' as abap.char(2) ) as FieldC2
}
```

  

SourceA와 SourceB를 Union한 뷰는 다음과 같다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE09
  as select from ZCM_EXAMPLE06
  association [0..1] to ZCM_EXAMPLE08 as _ViewC on $projection.UnionField1 = _ViewC.FieldC1
{
      @EndUserText.label: '필드1'
  key FieldA1 as UnionField1,

  key FieldA2 as UnionField2,

  key FieldA3 as UnionField3,

      _ViewC
}
union select from ZCM_EXAMPLE07
association [0..1] to ZCM_EXAMPLE08 as _ViewC on $projection.UnionField1 = _ViewC.FieldC1
{
  key FieldB1 as UnionField1,

  key FieldB2 as UnionField2,

  key ''      as UnionField3,

      _ViewC
}
```

  

유니온시에는 가장 먼저 나오는 select 구문의 필드의 길이가 기본길이가 된다

- ZCM\_EXAMPLE06의 FieldA1은 2자리이고 ZCM\_EXAMPLE07의 FieldB1은 3자리인데 결과적으로 UNION된 필드 UnionField1은 2자리가 된다.
- 길이가 다른 데이터소스를 union하는 경우에는 CAST 구문을 통해서 길이를 명시적으로 설정하는 것이 좋다

  

유니온시에는 Annotation은 첫번째 데이터소스의 필드에 대해서만 설정해 주면 된다

  

유니온시에는 Association도 동일하게 설정이 되어야 한다. 이렇게 Assocaition을 설정하는 것 보다 union을 통해 만들어진 뷰는 Association을 넣지 않고 해당 뷰를 사용하는 뷰에서 Association을 설정해서 사용하는 방법이 좋다.

- 많은 Association이 필요한 경우에는 별도의 CDS로 만들어서 그 곳에 설정하는 것이 좋다

  

**Assocaition이 빠진 UNION 구문**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE10
  as select from ZCM_EXAMPLE06
{
      @EndUserText.label: '필드1'
  key FieldA1 as UnionField1,

  key FieldA2 as UnionField2,

  key FieldA3 as UnionField3
}
union select from ZCM_EXAMPLE07
{
  key FieldB1 as UnionField1,

  key FieldB2 as UnionField2,

  key ''      as UnionField3
}
```

  

**Assocaition이 빠진 Union 뷰를 사용하여 Assocaition을 정의한 뷰**

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Union 뷰 사용'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE11
  as select from ZCM_EXAMPLE10
  association [0..1] to ZCM_EXAMPLE08 as _ViewC on $projection.UnionField1 = _ViewC.FieldC1
{
  key UnionField1,

  key UnionField2,

  key UnionField3,

      _ViewC
}
```

  

union구문은 union 및 union all 구문이 있으며, union인 경우에는 중복되는 데이터를 제거하고 결과값을 만들고 union all인 경우에는 중복에 상관 없이 모든 데이터를 출력한다.

- 성능적으로 비교하는 로직이 들어가지 않은 UNION ALL 구문이 성능상에 좋기 때문에 적용가능한 경우 UNION ALL 구문을 적용하는 것이 좋다

  

UNION CDS인 경우에는 키값에 대해서 데이터소스의 키값이 다른 경우에 어떻게 KEY값을 정할지에 대해서 잘 고려하여 모델링을 수행해 줘야 한다.

- 중복되는 키값이 있는 경우에는 KEY를 설정하지 않은 UNION VIEW를 만들어야 한다.

  

  

### INTERSECT

  

동일한 레코드에 대해서 결과값을 가져오기 위해서 **INTERSECT**를 사용할 수 있다.

테스트를 하기 위해서 두개의 데이터소스를 다음과 같이 만든다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'INTERSECT - Source A'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE12
  as select distinct from t000
{
  key 'A' as Field1
}

union all select distinct from t000
{
  key 'B' as Field1
}
```

![](Files/image%2055.png)  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'INTERSECT - Source B'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE13
  as select distinct from t000
{
  key 'A' as Field01
}
union all select distinct from t000
{
  key 'C' as Field01
}
```

![](Files/image%2056.png)  

  

두 데이터 소스를 가지고 다음과 같이 intersect를 사용하여 CDS뷰를 작성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'INTERSECT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE14
  as select from ZCM_EXAMPLE12
{
  key Field1
}
intersect select from ZCM_EXAMPLE13
{
  key Field01 as Field1
}
```

![](Files/image%2057.png)  

  

  

### **EXCEPT**

- 앞의 데이터소스의 데이터중에서 뒤에 나오는 데이터소스의 데이터에 같은 KEY값의 데이터를 제외하고 출력

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'EXCEPT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPL15
  as select from ZCM_EXAMPLE12
{
  key Field1
}
except select from ZCM_EXAMPLE13
{
  key Field01 as Field1
}
```

![](Files/image%2058.png)  

  

  

## Joins

  

- 두개이상의 데이터소스를 연결하여 새로운 데이터소스를 만들 때 사용
- 종류
    - Left outer joins
        - Primary 데이터소스의 전체 데이터 출력
        - 조건에 맞는 데이터만 Secondary 데이터소스에서 가져와서 같이 출력
    - Right outer joins
        - Secondary 데이터소스의 전체 데이터 출력 
        - 조건에 맞는 데이터만 Primary 데이터소스에서 가져와서 같이 출력
    - Inner joins
        - Primary와 Secondary를 연결하는 조건에 동시에 만족하는 데이터만 화면에 출력
    - Cross joins
        - Primary \* Secondary 레코드수 만큼 결과값 생성

  

**Cardinality** 

- 조인을 하는 경우 생성되는 데이터의 레코드 수

  

테스트를 하기 위한 두개의 데이터 소스를 다음과 같이 선언한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'JOIN - Source A'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE15
  as select from t000
{
  key cast( 'A' as abap.char(1) ) as FieldD1,
      cast( 'D' as abap.char(1) ) as FieldD2
}
union select distinct from t000
{
  key cast( 'C' as abap.char(1) ) as FieldD1,
      cast( 'E' as abap.char(1) ) as FieldD2
}
```

![](Files/image%2059.png)  

  

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'JOIN - Source B'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE16
  as select from t000
{
  key cast( 'D' as abap.char(1) ) as FieldE1,
  key cast( 'H' as abap.char(1) ) as FieldE2
}
union select distinct from t000
{
  key cast( 'D' as abap.char(1) ) as FieldE1,
  key cast( 'I' as abap.char(1) ) as FieldE2
}
union select distinct from t000
{
  key cast( 'F' as abap.char(1) ) as FieldE1,
  key cast( 'I' as abap.char(1) ) as FieldE2
}
```

![](Files/image%2060.png)  

  

### **Left outer join**

  

다음은 Left outer join을 한 소스와 결과를 나타낸다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'JOIN - LEFT OUTER JOIN'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE17
  as select from            ZCM_EXAMPLE15
    left outer to many join ZCM_EXAMPLE16 on ZCM_EXAMPLE15.FieldD2 = ZCM_EXAMPLE16.FieldE1
{
  key ZCM_EXAMPLE15.FieldD1,
  
  key ZCM_EXAMPLE16.FieldE1,
  
  key ZCM_EXAMPLE16.FieldE2
}
```

![](Files/image%2061.png)  

- Join을 하는 경우에 대상레코드에 대한 **to one** / **to many**를 지정해 놓으면 CDS에 대해서 데이터베이스에서 데이터검색시에 검색 처리를 최적화하는데 도움을 준다.
- 검색이 되지 않는 필드에 대해서는 기본적으로 데이터베이스레벨에서는 null 값으로 설정이 된다.
    - 데이터베이스에서 값을 어플리케이션단으로 가져오게 되면 해당 null값은 관련 ABAP 필드의 타입에 따른 초기값으로 설정이 된다.

  

**데이터베이스레벨의 NULL 값 처리**

- null은 평가가 되지 않은 값을 의미
- JOIN시에 발생
- CDS뷰를 모델링 하는 경우에는 해당 NULL에 대해서 고려를 하고 WHERE절의 조건에서 값을 비교하는 경우 고려해 줘야 한다.

  

### Inner join

  

다음은 inner join을 사용한 예제를 보여준다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'JOIN - INNER JOIN'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE18
  as select from ZCM_EXAMPLE15
    inner join   ZCM_EXAMPLE16 on ZCM_EXAMPLE16.FieldE1 = ZCM_EXAMPLE15.FieldD2
{
  key ZCM_EXAMPLE15.FieldD1,

  key ZCM_EXAMPLE15.FieldD2,

  key ZCM_EXAMPLE16.FieldE2
}
```

결과는 다음과 같다.

![](Files/image%2062.png)  

  

JOIN을 하는 데이터소스는 다음과 같이 **alias**를 사용하여 작성할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'JOIN - ALIAS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE19
  as select from           ZCM_EXAMPLE15 as E1
    left outer to one join ZCM_EXAMPLE15 as E2 on E2.FieldD1 = E1.FieldD1
{
  key E1.FieldD1,
      E1.FieldD2
}
```

  

  

## Aggregation 함수

  

- Database 레벨의 Aggregation 사용
- Group by 절을 이용해서 Aggregation 기준 필드를 지정

  

Aggregation 함수를 사용하기 위한 데이터 소스는 다음과 같다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AGGREGATION - Source A'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE20
  as select distinct from t000
{
  key abap.char'A' as Field1,

  key abap.char'A' as Field2,

      abap.int1'1' as Field3
}
union all select distinct from t000
{
  key abap.char'A' as Field1,

  key abap.char'B' as Field2,

      abap.int1'2' as Field3
}
union all select distinct from t000
{
  key abap.char'A' as Field1,

  key abap.char'C' as Field2,

      abap.int1'3' as Field3
}
```

![](Files/image%2063.png)  

  

위의 데이터소스를 이용해서 다음과 같이 Aggregation 함수를 사용하여 작성하였다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AGGREGATION'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE21
  as select from ZCM_EXAMPLE20
{
  key Field1,

      min(Field3)                    as FieldWithMin,

      max(Field3)                    as FieldWithMax,

      avg(Field3 as abap.decfloat34) as FieldWithAvg,

      cast(sum(Field3) as abap.int4) as FieldWithSum,

      count(distinct Field1)         as FieldWithCountDistinct,

      count(*)                       as FieldWithCountAll
}
group by
  Field1
```

  

- min/max/avg/sum/count 등의 aggregation 함수 사용
- count(distinct .. )는 필드의 값이 중복된 항목을 제외한 개수

  

Aggregation을 하는 경우 유의할 점

- 반드시 결과값을 예상해서 overflow가 일어나지 않는 유형으로 설정해야 하며, 필요시에는 cast를 통해서 더 큰 데이터를 담을 수 있도록 변경 필요
- 계산된 필드를 기준으로 group by하는 것은 성능상에 영향을 주므로 지양해야 한다.
- 데이터양의 수 관점에서 Aggregation하는 CDS는 CDS Stack에서 되도록이면 가장 안쪽에 위치하게 해서 데이터의 양을 줄이는 것이 성능에 좋다
- 금액이나 수량인 경우에는 집계함수를 사용하기 전에 동일한 통화/단위로 변환후에 Aggregation을 수행해야 한다.
- 레코드가 많은 경우에 모든 레코드에 conversion 함수를 적용하는 것은 성능상에 안좋기 때문에 같은 단위로 aggregation후에 conversion함수를 사용하여 변환 하는 것이 성능상에 좋다

  

  

  

## Projection Fields

  

- CDS뷰의 필드
- CDS뷰 내에서 해당 필드에 접근하기 위해서는 $projection 키워드를 사용

  

다음은 Projection Fields에 대한 예시 CDS뷰이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PROJECTION FIELDS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE22
  as select distinct from t000
{
  abap.char'A'                                              as FieldA,

  abap.char'B'                                              as FieldB,

  concat( $projection.FieldA, $projection.FieldB )          as FieldC,

  concat( abap.char'A', abap.char'B' )                      as FieldD,

  concat( $projection.FieldC, abap.char'D')                 as FieldE,

  CONCAT( concat(abap.char'A', abap.char'B'), abap.char'D') as FieldF,

  mtext                                                     as ClientText,

  concat( 'A', $projection.ClientText )                     as FieldG
}
```

![](Files/image%2064.png)  

  

  

  

## Parameters

  

- CDS를 사용하는 호출측에서 제공하는 값
- where절, calculated 필드등을 만들 때 사용
- 데이터소스에 파라미터가 필요한 경우에도 해당 값으로 사용 가능
- WITH PARAMETERS 구문을 통해서 선언
    - 파라미터 이름은 CDS 뷰내에서 유니크한 필드명이어야 함
    - P\_ 등으로 파라미터의 이름에 대한 Naming Rule을 정하여 사용

  

Parameter의 예시를 만들기 위해서 기본 데이터소스를 다음과 같이 선언한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETER - Source A'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE23
  as select distinct from t000
{
  key abap.char'A'        as KeyField,

  key abap.lang'E'        as Language,

  key abap.dats'99991231' as ValidateEndDate,

      abap.dats'20140620' as ValidateStartDate
}
```

![](Files/image%2065.png)  

  

다음은 파라미터가 들어간 데이터 소스이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETER - Source B'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE24
  with parameters
    P_ValidateDate : abap.dats
  as select from ZCM_EXAMPLE23
{
  key KeyField,

  key Language,

      ValidateEndDate,

      ValidateStartDate
}
where
      ValidateEndDate   >= $parameters.P_ValidateDate
  and ValidateStartDate <= $parameters.P_ValidateDate
```

  

위의 데이터소스를 바탕으로 다음은 parameter가 들어간 CDS 예시이다.

- Where 조건에 사용 
- Association을 사용한 Path 표현식에 사용

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETER'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE25
  with parameters
    P_KEYDATE  : abap.dats,
    P_LANGUAGE : sylangu
  as select from ZCM_EXAMPLE23
  association [0..*] to ZCM_EXAMPLE24 as _Target         on  $projection.KeyField = _Target.KeyField
  association [0..1] to ZCM_EXAMPLE23 as _FilteredTarget on  $projection.KeyField = _FilteredTarget.KeyField
                                                         and $projection.Language = _FilteredTarget.Language
{
  key KeyField,

      ValidateEndDate,

      ValidateStartDate,

      $parameters.P_LANGUAGE                                                                       as Language,

      _Target(P_VALIDATEDATE: $parameters.P_KEYDATE)[1:Language = $parameters.P_LANGUAGE].KeyField as TargetKeyField,

      _FilteredTarget
}
where
      ValidateEndDate   >= $parameters.P_KEYDATE
  and ValidateStartDate <= $parameters.P_KEYDATE
  and Language          = $parameters.P_LANGUAGE
```

  

Association의 대상이 되는 데이터소스에 파라미터가 있는 경우 Association에서는 해당 Parameter에 명시적으로 값을 할당할 수 없다

- 해당 Association에서 값을 가져오는 경우에 파라미터를 매핑하여 사용할 수 있다.

  

CDS의 Primary CDS뷰가 파라미터를 가지는 경우에는 해당 파라미터에 값을 할당하여 사용할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMTER - Data Source'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE26
  as select from ZCM_EXAMPLE24( P_VALIDATEDATE: $session.system_date )
{
  key KeyField
}
```

  

  

ABAP 프로그램에서도 다음과 같이 파라미터가 있는 CDS뷰를 사용할 수 있다.

```
*&---------------------------------------------------------------------*
*& Report ZSD_UNIT02_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_unit02_01.

START-OF-SELECTION.

  SELECT keyfield
  FROM zcm_example24(
     p_validatedate = @sy-datum
  )
  INTO TABLE @DATA(lt_data).
```

  

기본적으로 파라미터의 값은 무조건 제공이 되어야 하지만, ABAP 환경내에서 실행되는 CDS인 경우에는 다음과 같이 Annotation을 설정을 하게 되면 해당 파라미터에 값을 입력하지 않아도 해당 CDS를 검색 할 수 있다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMTER - Annotation'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE27
  with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KEYDATE : abap.dats
  as select from ZCM_EXAMPLE23
{
  key KeyField
}
where
      ValidateEndDate   >= $parameters.P_KEYDATE
  and ValidateStartDate <= $parameters.P_KEYDATE
```

  

다음과 같이 ABAP 프로그램내에서 파라미터를 입력하거나 없이도 사용이 가능하다.

```
*&---------------------------------------------------------------------*
*& Report ZSD_UNIT02_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_unit02_02.

START-OF-SELECTION.

  SELECT *
  FROM zcm_example27(
   p_keydate = @sy-datum )
  INTO TABLE @DATA(lt_data1).

  SELECT *
  FROM zcm_example27
  INTO TABLE @DATA(lt_data2).
```

  

  

파라미터는 다음의 경우에만 선택적으로 사용한다

- 데이터 검색에 대한 필수 조건이 필요한 경우
- 데이터 검색시 성능 최적화가 필요한 경우
- 비지니스적으로 반드시 필요한 값이 제공되어서 사용되아어야 하는 경우

  

  

## Reference Fields

  

- 금액(CURR)/수량(QUAN) 필드에 대해서 각각 통화/단위를 참조해야 하는 경우 사용
- 금액 : Semantics.amount.currencyCode : ‘\[CUKY 타입 필드\]’
    - Amount 필드의 타입은 반드시 소수점 2자리로 해야 한다
- 수량 : Semantics.quantity.unitOfMeasure : ‘\[UNIT 타입 필드\]’

  

다음은 참조필드를 사용한 예시이다

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'REFERENCE FIELDS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE28
  as select from t000
{
  @Semantics.quantity.unitOfMeasure: 'QuantityUnit'
  abap.quan'1234.56' as Quantity,

  abap.unit'PC'      as QuantityUnit,

  @Semantics.amount.currencyCode: 'Currency'
  abap.curr'1234.56' as Amount,

  abap.cuky'USD'     as Currency
}
```

  

  

CURR/QUAN 필드를 계산에 포함하기 위해서는 ‘**GET\_NUMERIC\_VALUE**’를 사용하여 참조가 없는 단순한 값으로 변환해야 한다.

금액필드 유형인 CURR은 참조필드 특성을 유지하고 계산을 하기위해서는 ‘**CURR\_TO\_DECFLOAT\_AMOUNT**’ 함수를 사용해야 한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Currency/Amount Calculation'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE29
  as select from ZCM_EXAMPLE28
{
  @Semantics.quantity.unitOfMeasure: 'QuantityUnit'
  Quantity,

  QuantityUnit,

  @Semantics.amount.currencyCode: 'Currency'
  Amount,

  Currency,

  get_numeric_value(Amount)                                                 as AmountWithoutReference,

  get_numeric_value(Quantity)                                               as QuantityWithoutReference,

  @Semantics.amount.currencyCode: 'Currency'
  curr_to_decfloat_amount(Amount)                                           as DecFloatAmount,

  @Semantics.quantity.unitOfMeasure: 'CalculatedUnit'
  get_numeric_value(Amount) / $projection.quantity                          as AmountPerQuantity,

  cast(concat(Currency,concat('/', QuantityUnit))as dd_cds_calculated_unit) as CalculatedUnit
}
```

![](Files/image%2066.png)  

위의 CDS의 기술적인 특성은 다음과 같다.

![](Files/image%2067.png)  

  

  

  

## Conversion Functions for Currencies and Quantity Units

  

- 특정 통화 또는 단위로 변환이 필요한 경우 사용

  

다음은 단위에 대한 Conversion 함수를 사용한 예시이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CONVERSION FUNCTION'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE30
  with parameters
    P_DISPLAYUNIT : msehi
  as select from ZSD_I_SALESORDERITEM
{
  key SalesOrderItemId,

      SalesOrderId,

      SalesOrderItem,

      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      OrderQuantity,

      OrderQuantityUnit,

      @Semantics.quantity.unitOfMeasure: 'OrderQuantityDisplayUnit'
      unit_conversion( quantity => OrderQuantity, source_unit => OrderQuantityUnit, target_unit => $parameters.P_DISPLAYUNIT, error_handling => 'FAIL_ON_ERROR') as OrderQuantityInDisplayUnit,

      $parameters.P_DISPLAYUNIT                                                                                                                                  as OrderQuantityDisplayUnit
}
```

  

F8을 누르면 아래의 파라미터 입력화면이 나오고 해당 화면에 값을 입력하면 결과값을 확인할 수 있다.

![](Files/image%2068.png)  

![](Files/image%2069.png)  

  

다음은 통화에 대한 Conversion 함수를 사용한 예시이다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CONVERSION FUNCTION - Amount'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE31
  with parameters
    P_DISPLAYCURRENCY  : waers_curc,
    P_EXCHANGERATEDATE : sydatum
  as select from ZSD_I_SALESORDERITEM
{
  key SalesOrderId,

      _Head.SalesOrder,

      SalesOrderItem,

      @Semantics.amount.currencyCode: 'TransactionCurrency'
      NetAmount,

      TransactionCurrency,

      @Semantics.amount.currencyCode: 'DisplayCurrency'
      currency_conversion(
        amount              => NetAmount,
        source_currency     => TransactionCurrency,
        target_currency     => $parameters.P_DISPLAYCURRENCY,
        exchange_rate_date  => $parameters.P_EXCHANGERATEDATE) as NetAmountInDisplayCurrency,

      $parameters.P_DISPLAYCURRENCY                            as DisplayCurrency
}
```

  

F8을 눌러서 테스트를 하면 다음과 같이 파라미터 입력화면이 나오고, 값을 입력하면 결과값을 확인할 수 있다.

  

![](Files/image%2070.png)  

![](Files/image%2071.png)  

  

  

Conversion 오류가 발생한 경우에 Error 핸들링하는 옵션은 다음과 같다.

- **FAIL\_ON\_ERROR**
    - 변환이 안되는 경우 오류 발생 시킴
    - 프로그램이 중지가 되므로 사전에 입력 값에 대해서 사전 체크 로직을 작성하는게 좋다
- **KEEP\_UNCONVERTED**
    - 변환이 안되는 경우에 원래의 값을 유지
    - 에러는 발생하지 않지만 어떤게 문제가 있는지 알기 힘듬
- **SET\_TO\_NULL**
    - 변환이 안되는 경우에 NULL로 설정
    - 에러는 발생하지 않지만 어떤게 문제가 있는지 알기 힘듬

  

  

**분석 쿼리 뷰**

- @Analytics.query: true
- 통화 및 수량 변환함수가 레코드가 집계된 후 **분석엔진**에서 수행하므로 변환함수를 기본 데이터 소스에 적용하는 것보다 성능을 크게 향상시킬 수 있다
- 변환 실패시 원래 값과 단위가 보존된다.