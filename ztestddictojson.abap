*&---------------------------------------------------------------------*
*& Report ztestddictojson
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztestddictojson.


TYPES:BEGIN OF ts_s_level,
        level      TYPE int2,
        position   TYPE tabfdpos,
        filedname  TYPE fieldname,
        inttype    TYPE inttype,
        rollname   TYPE rollname,
        ddtext     TYPE ddtext,

        rollnameup TYPE rollname,
      END OF ts_s_level.

TYPES:tt_t_level TYPE TABLE OF ts_s_level.

DATA:gt_level TYPE tt_t_level.

TYPES:tt_t_rollname TYPE RANGE OF rollname.

DATA:gt_rollname TYPE RANGE OF rollname.

PARAMETERS iv_data TYPE rollname.


START-OF-SELECTION.
  DATA:lv_level TYPE i.
  DATA:lv_json TYPE string.

  CALL METHOD cl_abap_typedescr=>describe_by_name
    EXPORTING
      p_name         = iv_data
    RECEIVING
      p_descr_ref    = DATA(lr_typedescrnew)
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*  Implement suitable error handling here

    MESSAGE '您输入的参数不是ddic定义' TYPE 'E'.
  ENDIF.

  gt_rollname = VALUE #( BASE gt_rollname ( sign = 'I' option = 'EQ' low = iv_data ) ).
  PERFORM frm_get_gt TABLES gt_rollname USING  lv_level.



*  DATA(lr_typedescrnew) = cl_abap_typedescr=>describe_by_name( iv_data ) .

  CASE lr_typedescrnew->kind.

    WHEN cl_abap_typedescr=>kind_struct.
      MOVE '{' TO lv_json.
    WHEN cl_abap_typedescr=>kind_table.
      MOVE '[{' TO lv_json.
  ENDCASE.



  DATA:ls_level TYPE ts_s_level.

  DATA:gt_json TYPE string.

  LOOP AT gt_level ASSIGNING FIELD-SYMBOL(<ls_level>) WHERE level = 1.


    IF sy-tabix NE 1.
      CONCATENATE lv_json ',' INTO lv_json.
    ENDIF.

    IF <ls_level>-inttype = 'u' OR <ls_level>-inttype = 'h' OR <ls_level>-inttype = 'v'.
      MOVE-CORRESPONDING <ls_level> TO ls_level.
      PERFORM frm_table_json USING ls_level lv_json.

    ELSE.
      CONCATENATE lv_json '"' <ls_level>-filedname '"' ':' '"' <ls_level>-ddtext '"' INTO lv_json.
    ENDIF.

  ENDLOOP.


  CASE lr_typedescrnew->kind.

    WHEN cl_abap_typedescr=>kind_struct.
      CONCATENATE lv_json '}' INTO lv_json.
    WHEN cl_abap_typedescr=>kind_table.
      CONCATENATE lv_json '}]' INTO lv_json.
  ENDCASE.

*  cl_demo_output=>display_json( lv_json ).
*

  PERFORM frm_show_alv.

  cl_demo_output=>display_json( lv_json ).
*DATA(lr_typedescr) = cl_abap_typedescr=>describe_by_name( iv_data ) .
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_GT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LEVEL  text
*      -->P_LEVEL  text
*      -->P_GT_ROLLNAME  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_GT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LEVEL  text
*      -->P_GT_ROLLNAME  text
*      -->P_LEVEL  text
*      -->P_GT_ROLLNAME  text
*----------------------------------------------------------------------*
FORM frm_get_gt  TABLES
                          pt_rollname TYPE tt_t_rollname
                 USING    VALUE(pv_level) TYPE i
                          .

  DATA:lt_dfies     TYPE icl_dfies_tab,
       lt_key_dfies TYPE icl_dfies_tab.
*  DATA:lv_levelnew TYPE i.
  DATA:lv_tabname TYPE tabname.
  DATA:lv_tabname1 TYPE tabname.

  DATA:ls_dd40v TYPE dd40v.

  IF pt_rollname[] IS INITIAL.
    RETURN.
  ENDIF.

  pv_level = pv_level + 1.
*
  DATA:lt_rollname TYPE RANGE OF rollname.


  APPEND LINES OF pt_rollname TO lt_rollname.

  CLEAR:gt_rollname.
  REFRESH:gt_rollname.


  LOOP AT lt_rollname ASSIGNING FIELD-SYMBOL(<ls_rollname>).

    DATA(lr_typedescr) = cl_abap_typedescr=>describe_by_name( <ls_rollname>-low ) .
*    CLEAR:lv_tabname.
*    clear:
    lv_tabname = <ls_rollname>-low.
    CASE lr_typedescr->kind.

      WHEN cl_abap_typedescr=>kind_struct.

        CLEAR:lt_dfies,
              lt_key_dfies.
        CALL FUNCTION 'ICL_GET_DDIC_INFO'
          EXPORTING
            iv_tabname            = lv_tabname
*           IV_RESOLVE_SUBSTRUCTURES       = ' '
*           IV_LANGUAGE           = SY-LANGU
          IMPORTING
            et_dfies              = lt_dfies
            et_dfies_key_fields   = lt_key_dfies
          EXCEPTIONS
            wrong_call            = 1
            erroneous_input_data  = 2
            ddic_data_read_failed = 3
            error_occurred        = 4
            OTHERS                = 5.





        LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
          APPEND INITIAL LINE TO gt_level ASSIGNING FIELD-SYMBOL(<ls_level>).
          <ls_level>-level = pv_level. "层次
          <ls_level>-position = <ls_dfies>-position. "字段定位
          <ls_level>-inttype = <ls_dfies>-inttype.     "关键得类型指标
          <ls_level>-filedname = <ls_dfies>-fieldname. "字段描述

          <ls_level>-rollnameup = <ls_rollname>-low.

          CASE <ls_dfies>-inttype.
            WHEN 'u' OR 'v'.
              SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd02t
                WHERE tabname = <ls_dfies>-rollname
                  AND ddlanguage = sy-langu.

              <ls_level>-ddtext = |struct:| && <ls_level>-ddtext.
              <ls_level>-rollname = <ls_dfies>-rollname.
              gt_rollname = VALUE #( BASE gt_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
            WHEN 'h'.
              SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd40t
                WHERE typename = <ls_dfies>-rollname
                  AND ddlanguage = sy-langu.
              <ls_level>-rollname = <ls_dfies>-rollname.
              <ls_level>-ddtext = |type table:| && <ls_level>-ddtext.
              gt_rollname = VALUE #( BASE gt_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
            WHEN OTHERS.
              <ls_level>-ddtext = |DATATYPE: |    &&  <ls_dfies>-datatype &&
                                  | LENG:|        &&  <ls_dfies>-leng     &&
                                  | FIELDTEXT:|   &&  <ls_dfies>-fieldtext.
          ENDCASE.

          DATA(lv_text) =   escape( val = <ls_level>-ddtext format = cl_abap_format=>e_json_string ).
          <ls_level>-ddtext = lv_text.
          CLEAR:lv_text.
          IF gt_rollname[] IS NOT INITIAL.
            PERFORM frm_get_gt TABLES  gt_rollname USING pv_level.
          ENDIF.


        ENDLOOP.

      WHEN cl_abap_typedescr=>kind_table.
        CALL FUNCTION 'DD_TTYP_GET'
          EXPORTING
            ttyp_name     = lv_tabname
          IMPORTING
            dd40v_wa_a    = ls_dd40v
          EXCEPTIONS
            illegal_value = 1
            op_failure    = 2
            OTHERS        = 3.

        CLEAR:lv_tabname.
        lv_tabname = ls_dd40v-rowtype.


        CLEAR:lt_dfies,
              lt_key_dfies.
        CALL FUNCTION 'ICL_GET_DDIC_INFO'
          EXPORTING
            iv_tabname            = lv_tabname
*           IV_RESOLVE_SUBSTRUCTURES       = ' '
*           IV_LANGUAGE           = SY-LANGU
          IMPORTING
            et_dfies              = lt_dfies
            et_dfies_key_fields   = lt_key_dfies
          EXCEPTIONS
            wrong_call            = 1
            erroneous_input_data  = 2
            ddic_data_read_failed = 3
            error_occurred        = 4
            OTHERS                = 5.


        LOOP AT lt_dfies ASSIGNING <ls_dfies>.
          APPEND INITIAL LINE TO gt_level ASSIGNING <ls_level>.
          <ls_level>-level = pv_level. "层次
          <ls_level>-position = <ls_dfies>-position. "字段定位
          <ls_level>-inttype = <ls_dfies>-inttype.     "关键得类型指标
          <ls_level>-filedname = <ls_dfies>-fieldname. "字段描述
          <ls_level>-rollnameup = <ls_rollname>-low.
          CASE <ls_dfies>-inttype.
            WHEN 'u' OR 'v'.
              SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd02t
                WHERE tabname = <ls_dfies>-rollname
                  AND ddlanguage = sy-langu.
              <ls_level>-rollname = <ls_dfies>-rollname.
              gt_rollname = VALUE #( BASE gt_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
            WHEN 'h'.
              SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd40t
                WHERE typename = <ls_dfies>-rollname
                  AND ddlanguage = sy-langu.
              <ls_level>-rollname = <ls_dfies>-rollname.
              gt_rollname = VALUE #( BASE gt_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
            WHEN OTHERS.
              <ls_level>-ddtext = |DATATYPE: |    &&  <ls_dfies>-datatype &&
                                  | LENG:|        &&  <ls_dfies>-leng     &&
                                  | FIELDTEXT:|   &&  <ls_dfies>-fieldtext.
          ENDCASE.

          lv_text =   escape( val = <ls_level>-ddtext format = cl_abap_format=>e_json_string ).
          <ls_level>-ddtext = lv_text.
          CLEAR:lv_text.
          IF gt_rollname[] IS NOT INITIAL.
            PERFORM frm_get_gt TABLES  gt_rollname USING pv_level.
          ENDIF.

          IF gt_rollname[] IS NOT INITIAL.

            PERFORM frm_get_gt TABLES  gt_rollname USING pv_level.
          ENDIF.

        ENDLOOP.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_alv .
  DATA: lr_table TYPE REF TO cl_salv_table.
  DATA: lr_columns TYPE REF TO cl_salv_columns_table.
  DATA: lr_column TYPE REF TO cl_salv_column_table.

  DATA:
    ls_color          TYPE lvc_s_colo,
    lt_comps          TYPE abap_compdescr_tab,           "data define
    lr_struc          TYPE REF TO cl_abap_structdescr,   "struc object define
    lr_salv_msg       TYPE REF TO cx_salv_msg,           "data alv abnormal
    lr_salv_not_found TYPE REF TO cx_salv_not_found,     "data alv abnormal
    lv_msg            TYPE string,                       "data msg
    lv_scrtext_s      TYPE scrtext_s,                    "data alv abnormal short
    lv_scrtext_m      TYPE scrtext_m,                    "data alv abnormal medium
    lv_scrtext_l      TYPE scrtext_l.                    "data alv abnormal long

  TRY.
      cl_salv_table=>factory(                                "ALV creation
          IMPORTING r_salv_table = lr_table                  "ALV creation
          CHANGING t_table = gt_level[] ).                    "ALV creation
    CATCH cx_salv_msg INTO lr_salv_msg.                      "Catch exception
      lv_msg = lr_salv_msg->get_text( ).                     "Catch exception
      MESSAGE lv_msg TYPE 'E'.
  ENDTRY.


  DATA: lr_functions TYPE REF TO cl_salv_functions_list.            "set alv funtions
  lr_functions = lr_table->get_functions( ).                        "set alv funtions
  lr_functions->set_all( abap_true ).                               "set alv funtions
  lr_table->set_screen_status( pfstatus      = 'STANDARD'
                                  report        = 'SAPLKKBL' ).

  lr_table->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_TABLE_JSON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LEVEL  text
*      -->P_LV_JSON  text
*----------------------------------------------------------------------*
FORM frm_table_json  USING    VALUE(ps_level) TYPE ts_s_level
                              pv_json.

  DATA:lv_lines TYPE i.
  IF ps_level-inttype = 'h'.
    CONCATENATE  lv_json '"' ls_level-filedname '"' ':' '[' '{' INTO lv_json.
  ELSEIF ps_level-inttype = 'u' OR ps_level-inttype = 'v'.
    CONCATENATE  lv_json '"' ls_level-filedname '"' ':'  '{' INTO lv_json.
  ENDIF.

  CLEAR:lv_lines.

  DATA:lv_levelnew TYPE i.
  CLEAR:lv_levelnew.
  lv_levelnew = ps_level-level + 1.

*  排除同级别有重复数据
  TYPES: BEGIN OF ts_s_field,
           fieldname TYPE fieldname,
           rollname  TYPE rollname,
         END OF ts_s_field.

  DATA:lt_field TYPE TABLE OF ts_s_field.

  CLEAR:lt_field.
  LOOP AT gt_level ASSIGNING FIELD-SYMBOL(<ls_level>) WHERE rollnameup = ps_level-rollname AND
                                                            level      = lv_levelnew.
    APPEND INITIAL LINE TO lt_field ASSIGNING FIELD-SYMBOL(<ls_field>).
    <ls_field>-fieldname = <ls_level>-filedname.
    <ls_field>-rollname  = ps_level-rollname.

  ENDLOOP.


  SORT lt_field BY fieldname rollname.
  DELETE ADJACENT DUPLICATES FROM lt_field COMPARING fieldname rollname.

  LOOP AT gt_level ASSIGNING <ls_level> WHERE rollnameup = ps_level-rollname AND
                                                            level      = lv_levelnew.
*    去除重复 begin
    READ TABLE lt_field INTO DATA(ls_field) WITH KEY fieldname = <ls_level>-filedname BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE TABLE lt_field FROM ls_field.
    ELSE.
      EXIT.
    ENDIF.
*    去除重复 end
    lv_lines = lv_lines + 1.
    IF lv_lines GT 1 .
      CONCATENATE lv_json ',' INTO lv_json.
    ENDIF.
    IF <ls_level>-inttype = 'u' OR <ls_level>-inttype = 'h' OR <ls_level>-inttype = 'v'.
      CLEAR:ls_level.
      MOVE-CORRESPONDING <ls_level> TO ls_level.
      PERFORM frm_table_json USING ls_level lv_json.
    ELSE.
      CONCATENATE lv_json '"' <ls_level>-filedname '"' ':' '"' <ls_level>-ddtext '"' INTO lv_json.
    ENDIF.
  ENDLOOP.

  IF ps_level-inttype = 'h'.
    CONCATENATE lv_json '}' ']' INTO lv_json.
  ELSEIF ps_level-inttype = 'u' OR ps_level-inttype = 'v'.
    CONCATENATE lv_json '}'  INTO lv_json.
  ENDIF.

ENDFORM.
