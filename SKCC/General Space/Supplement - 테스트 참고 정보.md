## Supplement - 테스트 참고 정보

  

테스트데이터 Prefix 관리 테이블

  

```
@EndUserText.label : '테스트데이터관리'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztsd0000 {
  key client : abap.clnt not null;
  key prefix : abap.char(2) not null;

}
```