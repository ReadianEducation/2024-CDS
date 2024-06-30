## Content07-Native SAP HANA Functions in CDS

  

### Table Function

- HANA의 SQLScript 언어를 사용하여 여러가지 제공되나느 고급 기능을 이용해서 데이터를 구성하여 CDS를 통해서 데이터를 제공하는 방법을 제공
    - SQLScript는 SQL 뿐만아니라 텍스트분석, 지리데이터, 그래프, 금융 수향, 예측분석, 기계학습과 같은 많은 영역의 도구 및 함수 제공
- ABAP에서는 AMDP (ABAP Managed Database Procedure)를 이용하여 SQLScript 언어를 가지고 개발 가능
- Table Function은 이러한 AMDP를 데이터 소스로 사용한다

  

  

### Table Function 생성절차

  

먼저 개발오브젝스트 생성 위저드에서 Data Defintion을 선택한다.

![](Files/image%20179.png)  

![](Files/image%20180.png)  

![](Files/image%20181.png)  

  

코드를 다음과 같이 작성한다

```
@EndUserText.label: 'Table Function'
@ClientHandling.type: #CLIENT_DEPENDENT
@ClientHandling.algorithm: #SESSION_VARIABLE
define table function ZF_EXAMP01
  with parameters
    @Environment.systemField: #CLIENT
    P_CLIENT : vdm_v_sap_client
returns
{
  mandt                     : vdm_v_sap_client;
  Country                   : land1_gp;
  CountryThreeLetterISOCode : intca3;
  CountryThreeDigitISOCode  : intcn3;
  CountryISOCode            : intca;
  CountryCurrency           : waers_005;
  IndexBasedCurrency        : curin;
  HardCurrency              : curha;
  TaxCalculationProcedure   : kalsm_d;
}
implemented by method
  ZCM_EXAMPLE01=>get_countries;
```

  

![](Files/image%20182.png)  

- 결과값이 ABAP Runtime에서 해당 클라이언트의 값으로 제한하는 설정
- HANA의 CDS\_CLIENT라는 세션변수를 기반으로 SQLScript에서 CDS뷰 또는 테이블함수를 호출해야 하는 경우에 필요

  

![](Files/image%20183.png)  

- 기본적으로 Table Function에는 파라미터로 Client 값을 넣는다
- ABAP Runtime에서 P\_CLIENT라는 파라미터의 값을 제공하지 않더라도 현재 CLIENT 번호를 기본값으로 제공한다.

  

![](Files/image%20184.png)

- AMDP에서 리턴되는 데이터의 형태를 정의
- 명시적으로 클라이언트에 관한 필드가 리턴값의 첫번째 부분에 포함되어야 한다.

  

![](Files/image%20185.png)  

- 결과값을 만들어주는 AMDP 로직을 가지고 있는 클래스와 메서드를 지정한다.
- 해당클래스가 없더라도 활성화는 가능하다

  

Table Function에서는 association 관계를 설정할 수 없으며, 반드시 Table Function을 데이터소스로 하는 CDS를 정의해서 사용해야 한다.

  

  

### SQLScript 작성을 위한 클래스와 메서드를 작성한다.

  

먼저 개발오브젝트 위저드를 실행하여 class를 선택한다.

![](Files/image%20186.png)  

인터페이스로 IF\_AMDP\_MARKER\_HDB 를 상속 받는다. 별다른 메서드가 없는 AMDP라는 걸 알려주는 MARKER 인터페이스이다

![](Files/image%20187.png)  

  

다음과 같이 클래스 로직을 작성한다.

```
CLASS zcm_class01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    CLASS-METHODS get_countries FOR TABLE FUNCTION zf_examp01.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcm_class01 IMPLEMENTATION.

  METHOD get_countries BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY
    USING i_country.

    RETURN
     SELECT
       :p_client AS mandt,
       country,
       countrythreeletterisocode,
       countrythreedigitisocode,
       countryisocode,
       countrycurrency,
       indexbasedcurrency,
       hardcurrency,
       taxcalculationprocedure
     FROM i_country
     WHERE mandt = :p_client;

  ENDMETHOD.

ENDCLASS.
```

  

![](Files/image%20188.png)  

- AMDP를 구현하는 클래스는 반드시 상속 받아야 함

  

![](Files/image%20189.png)  

- SAP HANA의 Table Function을 SQLScript로 구현한다는 것을 의미
- Function 뿐만아니라 Procedure도 작성이 가능

  

![](Files/image%20190.png)  

- Table Function은 데이터를 읽는 것만 가능하기 때문에 해당 구문을 삽입하여 변경 구문이 들어가면 오류가 발생하도록 함

  

![](Files/image%20191.png)  

- ABAP에서 생성되고 관리되는 객체를 SQLScript 상에서 사용하기 위해서는 반드시 해당 객체를 선언해 줘야 한다.
- ABAP을 제외한 다른 객체들은 선언 없이 스키마를 접근 가능하면 사용 가능하다
    - “MY\_SCHEMA”.”MY\_TABLE”
    - 물리적인 스키마 뿐만아니라 논리적인 스키마도 지원

  

![](Files/image%20192.png)  

- Return 문에 들어가는 필드는 Table Function의 Return 구문의 필드의 이름과 동일해야 한다.

  

  

### Table Function을 실행하여 데이터를 확인한다.

  

F8을 눌러서 실행하여 확인한다.

![](Files/image%20193.png)  

  

이렇게 생성된 항목은 HANA Studio를 이용해서 로그인해서 보면 SAPXXX 스키마의 Functions에 생성된다.

  

### CDS Wrapper 생성

  

Table Function 자체에 Assocaition 및 Annotation을 입력할 수 없기 때문에 해당 항목 입력을 위한 Wrapper CDS를 만든다.

![](Files/image%20194.png)  

![](Files/image%20195.png)  

  

CDS뷰를 다음과 같이 작성한다.

```
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Table Function'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZCM_EXAMPLE84
  as select from ZF_EXAMP01( P_CLIENT: $session.client )
  association [0..*] to I_CountryText as _Text     on $projection.Country = _Text.Country
  association [0..1] to I_Currency    as _Currency on $projection.CountryCurrency = _Currency.Currency
{
  key Country,

      CountryThreeLetterISOCode,

      CountryThreeDigitISOCode,

      CountryISOCode,

      @ObjectModel.foreignKey.association: '_Currency'
      CountryCurrency,

      IndexBasedCurrency,

      HardCurrency,

      TaxCalculationProcedure,

      _Text,

      _Currency
}
```

  

  

## Table Function 고려사항

  

- HANA 데이터베이스의 읽기 작업만 수행가능
- Table Function 자체는 CDS 뷰 Extension 불가
    - Wrapper CDS를 이용해서 확장 가능
- 적절한 필터를 잘 사용해서 데이터 검색 범위나 성능을 향상시키도록 해야 한다.