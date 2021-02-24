class ZCL_TEST_CLASS definition
  public
  final
  create public .

public section.

  methods GET_FACTORIAL
    importing
      value(IV_NUM) type INT4
    exporting
      value(EV_FACT) type INT8 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TEST_CLASS IMPLEMENTATION.


  METHOD get_factorial.
    DATA: lv_num TYPE int4.
    CLEAR: lv_num.
    ev_fact = 1.
    IF iv_num GE 2.
      lv_num = iv_num.
      WHILE lv_num GT 0.
        ev_fact = ev_fact * lv_num.
        lv_num = lv_num - 1.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
