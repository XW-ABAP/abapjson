FUNCTION zXXX_hr_pafm_pannnn_up.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_PRELP) TYPE  PRELP OPTIONAL
*"     VALUE(IV_MOLGA) TYPE  VIEKN DEFAULT '28'
*"     VALUE(IV_NOCOMMIT) TYPE  BAPI_STAND-NO_COMMIT OPTIONAL
*"     VALUE(IV_OPERATION) TYPE  PSPAR-ACTIO DEFAULT 'MOD'
*"----------------------------------------------------------------------
  DATA:lr_wa    TYPE REF TO data.
  DATA:lr_descr    TYPE REF TO cl_abap_structdescr.
  DATA: ls_returninfo TYPE bapireturn1.    "Return Parameter
  DATA:ls_pskey TYPE pskey.
  FIELD-SYMBOLS <ls_primary> TYPE any.
  FIELD-SYMBOLS <ls_secondary> TYPE any.
  DATA lr_primary_ref TYPE REF TO data.
  DATA lr_secondary_ref TYPE REF TO data.
  *FIELD-SYMBOLS:<lt_table>       TYPE STANDARD  TABLE,
  *              <ls_wa>          TYPE any,
  *              <lt_tablesecond> TYPE STANDARD  TABLE,
  *              <ls_wasecond>    TYPE any.

  DATA(lv_sssname) = 'PSKEY'.
  lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_sssname ).
  DATA(lt_fields) = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                            p_including_substructres = abap_true ).
  SELECT SINGLE
    molga,
    infty,
    vinft,
    dname,
    repid,
    dynnr
    FROM t582v
    WHERE molga = @iv_molga
      AND infty = @is_prelp-infty
  INTO @DATA(ls_t582v).
  IF sy-subrc = 0.
    DATA(ls_t582w) = cl_hr_t582w=>read( vinft = ls_t582v-vinft seqnr = 2 ).
    DATA(lv_secondary_infty) = ls_t582w-infty.
    DATA(ls_t777d_primary) = cl_hr_t777d=>read( infty = is_prelp-infty ).
    CREATE DATA lr_primary_ref TYPE (ls_t777d_primary-ppnnn).
    ASSIGN lr_primary_ref->* TO <ls_primary>.

    DATA(ls_t777d_secondary) = cl_hr_t777d=>read( infty = lv_secondary_infty ).
    CREATE DATA lr_secondary_ref TYPE (ls_t777d_secondary-ppnnn).
    ASSIGN lr_secondary_ref->* TO <ls_secondary>.

    CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_view
      EXPORTING
        prelp            = is_prelp
        secondary_infty  = lv_secondary_infty
      IMPORTING
        primary_record   = <ls_primary>
        secondary_record = <ls_secondary>.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
      ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE <ls_primary> TO FIELD-SYMBOL(<lv_source>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE ls_pskey TO FIELD-SYMBOL(<lv_target>).
        IF sy-subrc = 0.
          <lv_target> = <lv_source>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DO 100 TIMES.
      CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = ls_pskey-pernr
        IMPORTING
          return = ls_returninfo.

      IF ls_returninfo-type IS NOT INITIAL.
        WAIT UP TO 5 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty            = ls_pskey-infty
        number           = ls_pskey-pernr
        subtype          = ls_pskey-subty
        objectid         = ls_pskey-objps
        lockindicator    = ls_pskey-sprps
        validityend      = ls_pskey-endda
        validitybegin    = ls_pskey-begda
        recordnumber     = ls_pskey-seqnr
        record           = <ls_primary>
        secondary_record = <ls_secondary>
        operation        = iv_operation
        tclas            = 'A'
        nocommit         = iv_nocommit
        view_identifier  = iv_molga
        dialog_mode      = '0'
      IMPORTING
        return           = ls_returninfo.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = ls_pskey-pernr
      IMPORTING
        return = ls_returninfo.
  ELSE.

    ls_t777d_primary = cl_hr_t777d=>read( infty = is_prelp-infty ).
    CREATE DATA lr_primary_ref TYPE (ls_t777d_primary-ppnnn).
    ASSIGN lr_primary_ref->* TO <ls_primary>.

    CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
      EXPORTING
        prelp = is_prelp
      IMPORTING
        pnnnn = <ls_primary>.

    LOOP AT lt_fields ASSIGNING <ls_fields>.
      ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE <ls_primary> TO <lv_source>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE ls_pskey TO <lv_target>.
        IF sy-subrc = 0.
          <lv_target> = <lv_source>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DO 100 TIMES.
      CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = ls_pskey-pernr
        IMPORTING
          return = ls_returninfo.

      IF ls_returninfo-type IS NOT INITIAL.
        WAIT UP TO 5 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

*    *INFOTYPE COPY
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = ls_pskey-infty
        number        = ls_pskey-pernr
        subtype       = ls_pskey-subty
        objectid      = ls_pskey-objps
        lockindicator = ls_pskey-sprps
        validityend   = ls_pskey-endda
        validitybegin = ls_pskey-begda
        recordnumber  = ls_pskey-seqnr
        record        = <ls_primary>
        operation     = iv_operation
        tclas         = 'A'
        nocommit      = iv_nocommit
        dialog_mode   = '0'
      IMPORTING
        return        = ls_returninfo.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = ls_pskey-pernr
      IMPORTING
        return = ls_returninfo.

  ENDIF.

ENDFUNCTION.
