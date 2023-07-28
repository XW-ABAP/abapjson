class ZCL_DDIC_TO_JSON definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_s_level,
        level      TYPE i, " ZLEVERL type i fieldtext level
        position   TYPE tabfdpos,
        filedname  TYPE fieldname,
        inttype    TYPE inttype,
        rollname   TYPE rollname,
        ddtext     TYPE ddtext,
        rollnameup TYPE rollname,
      END OF ts_s_level .
  types:
    tt_t_level TYPE TABLE OF ts_s_level .
  types:
    tt_t_rollname TYPE RANGE OF rollname .

  class-data GT_LEVEL type TT_T_LEVEL .

  methods SHOW_ALV
    importing
      !IT_TAB type TT_T_LEVEL .
  class-methods DDIC_TO_JSON
    importing
      !IV_DATA type ROLLNAME
      value(IV_FIELDNAME) type FIELDNAME
    returning
      value(EV_JSON) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_table
      IMPORTING
        VALUE(it_rollname) TYPE tt_t_rollname
        VALUE(iv_level)    TYPE int4 .
    CLASS-METHODS convert_to_json
      IMPORTING
        !it_tab        TYPE tt_t_level OPTIONAL
        !is_level      TYPE ts_s_level OPTIONAL
        !iv_data       TYPE rollname OPTIONAL
      CHANGING
        VALUE(iv_json) TYPE string OPTIONAL .
ENDCLASS.



CLASS ZCL_DDIC_TO_JSON IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_DDIC_TO_JSON=>CONVERT_TO_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TAB                         TYPE        TT_T_LEVEL(optional)
* | [--->] IS_LEVEL                       TYPE        TS_S_LEVEL(optional)
* | [--->] IV_DATA                        TYPE        ROLLNAME(optional)
* | [<-->] IV_JSON                        TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_to_json.
    DATA:ls_level TYPE ts_s_level.


    IF is_level IS INITIAL.
      DATA(lr_typedescrnew) = cl_abap_typedescr=>describe_by_name( iv_data ) .

      CASE lr_typedescrnew->kind.
        WHEN cl_abap_typedescr=>kind_struct.
          MOVE '{' TO iv_json.
        WHEN cl_abap_typedescr=>kind_table.
          MOVE '[{' TO iv_json.
      ENDCASE.


      LOOP AT it_tab ASSIGNING FIELD-SYMBOL(<ls_level>) WHERE level = 1.
        IF sy-tabix NE 1.
          CONCATENATE iv_json ',' INTO iv_json.
        ENDIF.

        IF <ls_level>-inttype = 'u' OR <ls_level>-inttype = 'h' OR <ls_level>-inttype = 'v'.
          MOVE-CORRESPONDING <ls_level> TO ls_level.
*        PERFORM frm_table_json USING ls_level iv_json.

          CALL METHOD zcl_ddic_to_json=>convert_to_json
            EXPORTING
*             it_tab   =
              is_level = ls_level
*             iv_data  =
            CHANGING
              iv_json  = iv_json.



        ELSE.
          CONCATENATE iv_json '"' <ls_level>-filedname '"' ':' '"' <ls_level>-ddtext '"' INTO iv_json.
        ENDIF.

      ENDLOOP.

      CASE lr_typedescrnew->kind.

        WHEN cl_abap_typedescr=>kind_struct.
          CONCATENATE iv_json '}' INTO iv_json.
        WHEN cl_abap_typedescr=>kind_table.
          CONCATENATE iv_json '}]' INTO iv_json.
      ENDCASE.

*      ev_json = iv_json.
    ELSE.
      DATA:lv_lines TYPE i.
      IF is_level-inttype = 'h'.
        CONCATENATE  iv_json '"' is_level-filedname '"' ':' '[' '{' INTO iv_json.
      ELSEIF is_level-inttype = 'u' OR is_level-inttype = 'v'.
        CONCATENATE  iv_json '"' is_level-filedname '"' ':'  '{' INTO iv_json.
      ENDIF.

      CLEAR:lv_lines.

      DATA:lv_levelnew TYPE i.
      CLEAR:lv_levelnew.
      lv_levelnew = is_level-level + 1.

*  排除同级别有重复数据
      TYPES: BEGIN OF ts_s_field,
               fieldname TYPE fieldname,
               rollname  TYPE rollname,
             END OF ts_s_field.

      DATA:lt_field TYPE TABLE OF ts_s_field.

      CLEAR:lt_field.
      LOOP AT gt_level ASSIGNING <ls_level> WHERE rollnameup = is_level-rollname AND
                                                                level      = lv_levelnew.
        APPEND INITIAL LINE TO lt_field ASSIGNING FIELD-SYMBOL(<ls_field>).
        <ls_field>-fieldname = <ls_level>-filedname.
        <ls_field>-rollname  = is_level-rollname.

      ENDLOOP.


      SORT lt_field BY fieldname rollname.
      DELETE ADJACENT DUPLICATES FROM lt_field COMPARING fieldname rollname.

      LOOP AT gt_level ASSIGNING <ls_level> WHERE rollnameup = is_level-rollname AND
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
          CONCATENATE iv_json ',' INTO iv_json.
        ENDIF.
        IF <ls_level>-inttype = 'u' OR <ls_level>-inttype = 'h' OR <ls_level>-inttype = 'v'.
          CLEAR:ls_level.
          MOVE-CORRESPONDING <ls_level> TO ls_level.
*          PERFORM frm_table_json USING ls_level iv_json.

*          CALL METHOD zcl_ddic_to_json=>convert_to_json
*            EXPORTING
**             it_tab   =
*              is_level = ls_level
**             iv_data  =
*            RECEIVING
*              iv_json  = iv_json.
          CALL METHOD zcl_ddic_to_json=>convert_to_json
            EXPORTING
*             it_tab   =
              is_level = ls_level
*             iv_data  =
            CHANGING
              iv_json  = iv_json.
        ELSE.
          CONCATENATE iv_json '"' <ls_level>-filedname '"' ':' '"' <ls_level>-ddtext '"' INTO iv_json.
        ENDIF.
      ENDLOOP.

      IF is_level-inttype = 'h'.
        CONCATENATE iv_json '}' ']' INTO iv_json.
      ELSEIF is_level-inttype = 'u' OR is_level-inttype = 'v'.
        CONCATENATE iv_json '}'  INTO iv_json.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DDIC_TO_JSON=>DDIC_TO_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        ROLLNAME
* | [--->] IV_FIELDNAME                   TYPE        FIELDNAME
* | [<-()] EV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ddic_to_json.
    DATA:lo_elem_descr TYPE REF TO cl_abap_elemdescr.
*    DATA(lr_typedesc) = cl_abap_typedescr=>describe_by_name( iv_data ) .


    CALL METHOD cl_abap_typedescr=>describe_by_name
      EXPORTING
        p_name         = iv_data
      RECEIVING
        p_descr_ref    = DATA(lr_typedesc)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
*  Implement suitable error handling here

*      MESSAGE '您输入的参数不是ddic定义' TYPE 'E'.
      TYPES:BEGIN OF ts_s_msg,
              message TYPE string,
            END OF ts_s_msg.

      DATA:ls_msg TYPE ts_s_msg.
*      DATA(lv_message) = '您输入的参数不是ddic定义'.

      ls_msg-message = '您输入的参数不是ddic定义'.
      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = ls_msg
*         compress         = C_BOOL-FALSE
*         name   =
*         pretty_name      = PRETTY_MODE-NONE
*         type_descr       =
*         assoc_arrays     = C_BOOL-FALSE
*         ts_as_iso8601    = C_BOOL-FALSE
*         expand_includes  = C_BOOL-TRUE
*         assoc_arrays_opt = C_BOOL-FALSE
*         numc_as_string   = C_BOOL-FALSE
*         name_mappings    =
*         conversion_exits = C_BOOL-FALSE
        RECEIVING
          r_json = ev_json.


      RETURN.
    ENDIF.
    DATA:lt_rollname TYPE tt_t_rollname.
    DATA:lv_level TYPE i.
    CLEAR:gt_level.
    REFRESH:gt_level.

    IF iv_fieldname IS INITIAL.
      iv_fieldname =  'ROOT'.
    ENDIF.

    CASE lr_typedesc->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        lo_elem_descr   ?= lr_typedesc.

        SELECT  FROM dd04l AS a
          INNER JOIN dd04t AS b
          ON a~rollname = b~rollname AND
             a~as4local = b~as4local AND
             a~as4vers  = b~as4vers
          FIELDS
          a~datatype,
          a~leng,
          b~ddtext
          WHERE a~as4local = 'A'
            AND a~as4vers  = '0000'
            AND b~ddlanguage = @sy-langu
            AND a~rollname = @iv_data
          INTO TABLE @DATA(lt_data).

        READ TABLE lt_data INTO DATA(ls_data) INDEX 1.



        DATA(lv_ddtext) = |DATATYPE: |    &&  ls_data-datatype &&
                                          | LENG:|        &&  ls_data-leng     &&
                                          | FIELDTEXT:|   &&  ls_data-ddtext.

        CONCATENATE '{' '"' iv_fieldname '"' ':' '"' lv_ddtext '"' '}' INTO ev_json.
        CLEAR:lt_data,ls_data,lv_ddtext.
      WHEN cl_abap_typedescr=>kind_struct OR cl_abap_typedescr=>kind_table.
        lt_rollname = VALUE #( BASE lt_rollname ( sign = 'I' option = 'EQ' low = iv_data ) ).
        CLEAR:lv_level.

        CALL METHOD zcl_ddic_to_json=>get_table
          EXPORTING
            it_rollname = lt_rollname
            iv_level    = lv_level.

        CALL METHOD zcl_ddic_to_json=>convert_to_json
          EXPORTING
            it_tab  = gt_level
*           is_level =
            iv_data = iv_data
          CHANGING
            iv_json = ev_json.
        .


        CONCATENATE '{' '"' iv_fieldname '"' ':' ev_json '}' INTO ev_json.
    ENDCASE.
    cl_demo_output=>display_json( ev_json ).  "If you want to make an interface, please comment out this code
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_DDIC_TO_JSON=>GET_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ROLLNAME                    TYPE        TT_T_ROLLNAME
* | [--->] IV_LEVEL                       TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_table.

    DATA:lt_dfies     TYPE icl_dfies_tab,
         lt_key_dfies TYPE icl_dfies_tab.
    DATA:lv_tabname TYPE tabname.

    DATA:ls_dd40v TYPE dd40v.

    IF it_rollname[] IS INITIAL.
      RETURN.
    ENDIF.

    iv_level = iv_level + 1.
*
    DATA:lt_rollname  TYPE RANGE OF rollname,
         lt_rollname1 TYPE RANGE OF rollname.


    APPEND LINES OF it_rollname TO lt_rollname.

    CLEAR:it_rollname.
    REFRESH:it_rollname.


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
*             IV_RESOLVE_SUBSTRUCTURES       = ' '
*             IV_LANGUAGE           = SY-LANGU
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
            CLEAR:it_rollname.

            APPEND INITIAL LINE TO gt_level ASSIGNING FIELD-SYMBOL(<ls_level>).
            <ls_level>-level = iv_level. "层次
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
                it_rollname = VALUE #( BASE it_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
              WHEN 'h'.
                SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd40t
                  WHERE typename = <ls_dfies>-rollname
                    AND ddlanguage = sy-langu.
                <ls_level>-rollname = <ls_dfies>-rollname.
                <ls_level>-ddtext = |type table:| && <ls_level>-ddtext.
                it_rollname = VALUE #( BASE it_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
              WHEN OTHERS.
                <ls_level>-ddtext = |DATATYPE: |    &&  <ls_dfies>-datatype &&
                                    | LENG:|        &&  <ls_dfies>-leng     &&
                                    | FIELDTEXT:|   &&  <ls_dfies>-fieldtext.
            ENDCASE.

            DATA(lv_text) =   escape( val = <ls_level>-ddtext format = cl_abap_format=>e_json_string ).
            <ls_level>-ddtext = lv_text.
            CLEAR:lv_text.

            IF it_rollname[] IS NOT INITIAL.
*              PERFORM frm_get_gt TABLES  it_rollname USING iv_level.
              CALL METHOD zcl_ddic_to_json=>get_table
                EXPORTING
                  it_rollname = it_rollname
                  iv_level    = iv_level.
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
*             IV_RESOLVE_SUBSTRUCTURES       = ' '
*             IV_LANGUAGE           = SY-LANGU
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
            <ls_level>-level = iv_level. "层次
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
                it_rollname = VALUE #( BASE it_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
              WHEN 'h'.
                SELECT SINGLE ddtext INTO <ls_level>-ddtext FROM dd40t
                  WHERE typename = <ls_dfies>-rollname
                    AND ddlanguage = sy-langu.
                <ls_level>-rollname = <ls_dfies>-rollname.
                it_rollname = VALUE #( BASE it_rollname ( sign = 'I' option = 'EQ' low = <ls_dfies>-rollname ) ).
              WHEN OTHERS.
                <ls_level>-ddtext = |DATATYPE: |    &&  <ls_dfies>-datatype &&
                                    | LENG:|        &&  <ls_dfies>-leng     &&
                                    | FIELDTEXT:|   &&  <ls_dfies>-fieldtext.
            ENDCASE.

            lv_text =   escape( val = <ls_level>-ddtext format = cl_abap_format=>e_json_string ).
            <ls_level>-ddtext = lv_text.
            CLEAR:lv_text.



            IF it_rollname[] IS NOT INITIAL.

*              PERFORM frm_get_gt TABLES  it_rollname USING iv_level.
              CALL METHOD zcl_ddic_to_json=>get_table
                EXPORTING
                  it_rollname = it_rollname
                  iv_level    = iv_level.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DDIC_TO_JSON->SHOW_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TAB                         TYPE        TT_T_LEVEL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_alv.

    DATA: lr_table TYPE REF TO cl_salv_table.

    DATA:
      lr_salv_msg       TYPE REF TO cx_salv_msg,           "data alv abnormal
      lr_salv_not_found TYPE REF TO cx_salv_not_found,     "data alv abnormal
      lv_msg            TYPE string.                       "data msg
    TRY.
        cl_salv_table=>factory(                                "ALV creation
            IMPORTING r_salv_table = lr_table                  "ALV creation
            CHANGING t_table = gt_level[] ).                    "ALV creation
      CATCH cx_salv_msg INTO lr_salv_msg.                      "Catch exception
        lv_msg = lr_salv_msg->get_text( ).                     "Catch exception
        MESSAGE  lv_msg TYPE 'E'.
    ENDTRY.


    DATA: lr_functions TYPE REF TO cl_salv_functions_list.            "set alv funtions
    lr_functions = lr_table->get_functions( ).                        "set alv funtions
    lr_functions->set_all( abap_true ).                               "set alv funtions
    lr_table->set_screen_status( pfstatus      = 'STANDARD'
                                    report        = 'SAPLKKBL' ).

    lr_table->display( ).

  ENDMETHOD.
ENDCLASS.
