class ZCL_BOBAS_STVARV definition
  public
  final
  create public .

public section.

  class-methods GET_RANGE
    importing
      !IV_NAME type RVARI_VNAM
      !IV_CLIENT_SPECIFIC type BOOLE_D default 'X'
    exporting
      !ET_RANGE type TABLE .
  class-methods GET_PARAM
    importing
      !IV_NAME type RVARI_VNAM
      !IV_CLIENT_SPECIFIC type BOOLE_D default 'X'
    exporting
      !EV_PARAM type ANY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BOBAS_STVARV IMPLEMENTATION.


METHOD get_param.

  CONSTANTS: lc_parameter TYPE tvarv-type VALUE 'P'.

  DATA: lv_str    TYPE string,
        ls_tvarvc TYPE tvarvc,
        lv_table  TYPE dd02l-tabname,
        lo_root   TYPE REF TO cx_root.

  CLEAR: ev_param.

  IF iv_client_specific EQ abap_true.
    lv_table = 'TVARVC'.
  ELSE.
    lv_table = 'TVARV'.
  ENDIF.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_tvarvc
    FROM (lv_table)
    WHERE name = iv_name.
    IF sy-subrc <> 0.
      MESSAGE e690(db) WITH iv_name." RAISING no_data.
    ELSE.
      IF ls_tvarvc-type <> lc_parameter.
        MESSAGE e041(cg)." RAISING type_mismatch.
      ENDIF.

      TRY.
          MOVE ls_tvarvc-low TO ev_param.
        CATCH cx_root INTO lo_root.
          lv_str = lo_root->if_message~get_text( ).
          MESSAGE lv_str TYPE 'E'." RAISING conversion_error.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


METHOD get_range.

  CONSTANTS: lc_range  TYPE tvarvc-type VALUE 'S',
             lc_option TYPE abap_compname VALUE 'OPTION'.

  DATA: lv_str   TYPE string,
        lv_table TYPE dd02l-tabname,

        lt_tvarv TYPE STANDARD TABLE OF tvarvc,
        lr_range TYPE REF TO data,
        lo_root  TYPE REF TO cx_root.

  CLEAR: et_range[].

  IF iv_client_specific EQ abap_true.
    lv_table = 'TVARVC'.
  ELSE.
    lv_table = 'TVARV'.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_tvarv
    FROM (lv_table)
    WHERE name = iv_name.
  IF sy-subrc <> 0.
    MESSAGE e690(db) WITH iv_name." RAISING no_data.
  ELSE.
    CREATE DATA lr_range LIKE LINE OF et_range.
    ASSIGN lr_range->* TO FIELD-SYMBOL(<ls_range>).

    LOOP AT lt_tvarv TRANSPORTING NO FIELDS
                     WHERE type NE lc_range.
      MESSAGE e041(cg)." RAISING type_mismatch.
    ENDLOOP.

    LOOP AT lt_tvarv ASSIGNING FIELD-SYMBOL(<ls_tvarv>)
                                       WHERE sign IS NOT INITIAL
                       AND opti IS NOT INITIAL.
      TRY.
          APPEND INITIAL LINE TO et_range ASSIGNING <ls_range>.
          MOVE-CORRESPONDING <ls_tvarv> TO <ls_range>.

          ASSIGN COMPONENT lc_option OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_value>).
          CHECK <lv_value> IS ASSIGNED.

          MOVE <ls_tvarv>-opti TO <lv_value>.
          UNASSIGN <lv_value>.

        CATCH cx_root INTO lo_root.
          lv_str = lo_root->if_message~get_text( ).
          MESSAGE lv_str TYPE 'E'." RAISING conversion_error.
      ENDTRY.

    ENDLOOP.
  ENDIF.

ENDMETHOD.
ENDCLASS.
