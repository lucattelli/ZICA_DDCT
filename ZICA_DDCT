*&---------------------------------------------------------------------*
*& Z Instant Comprehensive ABAP - Data Dictionary Table Comparison Tool
*& Copyright (C) 2015 Bruno Lucattelli - lucattelli.com
*& This work is licensed under CC ShareAlike 4.0 International
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* This program compares data for an ABAP Data Dictionary table from
* two different systems (eg: DEV and PRD) and exports a DIFF.
* How to use it:
*  - First, you'll need to extract data from your systems by using
*    FM/method GUI_DOWNLOAD with WRITE_FIELD_SEPARATOR = 'X'.
*  - Name the files as <TABLE_NAME>.TXT. (eg: for MARA, use MARA.TXT)
*  - Run the program and fill in the parameters.
*  - Open the DIFF file in Excel and have fun! :)
*----------------------------------------------------------------------*

REPORT zica_ddct.

TYPE-POOLS : abap.

DATA : a TYPE REF TO data,
       b TYPE REF TO data,
       t TYPE REF TO data.

TYPES : BEGIN OF type_ikey,
          position TYPE dd03l-position,
          fieldname TYPE dd03l-fieldname,
          rollname TYPE dd03l-rollname,
        END OF type_ikey.

DATA cached_tkey TYPE TABLE OF type_ikey WITH DEFAULT KEY.

PARAMETERS table TYPE dd03l-tabname DEFAULT 'T16FS'.
PARAMETERS apath TYPE rlgrap-filename DEFAULT 'C:\TEMP\QAS\'.
PARAMETERS bpath TYPE rlgrap-filename DEFAULT 'C:\TEMP\PRD\'.
PARAMETERS dpath TYPE rlgrap-filename DEFAULT 'C:\TEMP\DIFF\'.

START-OF-SELECTION.
  DATA : ap TYPE string,
         bp TYPE string,
         dp TYPE string.
  CONCATENATE apath table '.TXT' INTO ap.
  CONCATENATE bpath table '.TXT' INTO bp.
  CONCATENATE dpath table '_DIFF.TXT' INTO dp.
  PERFORM get_datatab USING table ap CHANGING a.
  PERFORM get_datatab USING table bp CHANGING b.
  PERFORM talloc_abtab USING table CHANGING t.
  PERFORM compare_abtab USING table CHANGING a b t.
  PERFORM download_abtab USING dp t.

*&---------------------------------------------------------------------*
*&      Form  get_key_comp
*&---------------------------------------------------------------------*
FORM get_key_comp USING tname TYPE dd03l-tabname
                  CHANGING tcomp TYPE abap_component_tab.

  DATA : icomp TYPE LINE OF abap_component_tab.

  DATA tkey TYPE TABLE OF type_ikey WITH DEFAULT KEY.
  IF cached_tkey[] IS NOT INITIAL.
    tkey = cached_tkey.
  ELSE.
    SELECT position fieldname rollname
      FROM dd03l
      INTO TABLE tkey
      WHERE tabname EQ tname
        AND keyflag EQ 'X'.
    SORT tkey BY position.
    cached_tkey = tkey.
  ENDIF.

  DATA ikey TYPE type_ikey.
  LOOP AT tkey INTO ikey.
    icomp-name = ikey-fieldname.
    icomp-type ?= cl_abap_elemdescr=>describe_by_name( ikey-rollname ).
    APPEND icomp TO tcomp.
  ENDLOOP.

ENDFORM.                    "get_key_comp

*&---------------------------------------------------------------------*
*&      Form  talloc_abtab
*&---------------------------------------------------------------------*
FORM talloc_abtab USING tname TYPE dd03l-tabname
                  CHANGING dref_t TYPE REF TO data.

  DATA : icomp TYPE LINE OF abap_component_tab,
         tcomp TYPE abap_component_tab.

  PERFORM get_key_comp USING tname CHANGING tcomp.

  icomp-name = 'A'.
  icomp-type ?= cl_abap_elemdescr=>describe_by_name( tname ).
  APPEND icomp TO tcomp.

  icomp-name = 'B'.
  icomp-type ?= cl_abap_elemdescr=>describe_by_name( tname ).
  APPEND icomp TO tcomp.

  icomp-name = 'DIFF'.
  icomp-type ?= cl_abap_elemdescr=>describe_by_name( 'FLAG' ).
  APPEND icomp TO tcomp.

  DATA : sref TYPE REF TO cl_abap_structdescr.
  sref ?= cl_abap_structdescr=>create( tcomp ).

  DATA : tref TYPE REF TO cl_abap_tabledescr.
  tref = cl_abap_tabledescr=>create( sref ).

  CREATE DATA dref_t TYPE HANDLE tref.

ENDFORM.                    "talloc_abtab

*&---------------------------------------------------------------------*
*&      Form  get_datatab
*&---------------------------------------------------------------------*
FORM get_datatab USING tname TYPE dd03l-tabname
                       fpath TYPE string
                 CHANGING dload TYPE REF TO data.

  DATA : sload TYPE REF TO cl_abap_structdescr.
  sload ?= cl_abap_typedescr=>describe_by_name( tname ).

  DATA : tload TYPE REF TO cl_abap_tabledescr.
  tload = cl_abap_tabledescr=>create( sload ).

  CREATE DATA dload TYPE HANDLE tload.

  FIELD-SYMBOLS <dload> TYPE ANY TABLE.
  ASSIGN dload->* TO <dload>.

  cl_gui_frontend_services=>gui_upload( EXPORTING filename = fpath
                                                  has_field_separator = 'X'
                                        CHANGING data_tab = <dload> ).

ENDFORM.                    "get_datatab

*&---------------------------------------------------------------------*
*&      Form  fill_abtab
*&---------------------------------------------------------------------*
FORM fill_abtab USING dload TYPE REF TO data
                CHANGING dabtab TYPE REF TO data.

  FIELD-SYMBOLS : <dload> TYPE STANDARD TABLE,
                  <dabtab> TYPE STANDARD TABLE.

  ASSIGN : dload->* TO <dload>,
           dabtab->* TO <dabtab>.

  DATA : tabline TYPE REF TO cl_abap_tabledescr.
  tabline ?= cl_abap_tabledescr=>describe_by_data( <dabtab> ).

  DATA : sabline TYPE REF TO cl_abap_structdescr.
  sabline ?= tabline->get_table_line_type( ).

  DATA : dabline TYPE REF TO data.
  CREATE DATA dabline TYPE HANDLE sabline.

  FIELD-SYMBOLS : <dabline> TYPE ANY.
  ASSIGN dabline->* TO <dabline>.

  FIELD-SYMBOLS <iload> TYPE ANY.
  LOOP AT <dload> ASSIGNING <iload>.
    MOVE-CORRESPONDING <iload> TO <dabline>.
    APPEND <dabline> TO <dabtab>.
  ENDLOOP.

  SORT <dabtab>.
  DELETE ADJACENT DUPLICATES FROM <dabtab>.

ENDFORM.                    "fill_abtab

*&---------------------------------------------------------------------*
*&      Form  key_is_equal
*&---------------------------------------------------------------------*
FORM key_is_equal USING tname a t CHANGING equal.

  DATA : tcomp TYPE abap_component_tab.
  PERFORM get_key_comp USING tname CHANGING tcomp.

  DATA : srefa TYPE REF TO cl_abap_structdescr,
         sreft TYPE REF TO cl_abap_structdescr.

  srefa ?= cl_abap_structdescr=>create( tcomp ).
  sreft ?= cl_abap_structdescr=>create( tcomp ).

  DATA : drefa TYPE REF TO data,
         dreft TYPE REF TO data.

  CREATE DATA : drefa TYPE HANDLE srefa,
                dreft TYPE HANDLE sreft.

  FIELD-SYMBOLS : <drefa> TYPE ANY,
                  <dreft> TYPE ANY.

  ASSIGN : drefa->* TO <drefa>,
           dreft->* TO <dreft>.

  MOVE-CORRESPONDING : a TO <drefa>,
                       t TO <dreft>.

  IF <drefa> EQ <dreft>.
    equal = 'X'.
  ELSEIF <drefa> GT <dreft>.
    equal = 'G'.
  ENDIF.

ENDFORM.                    "key_is_equal

*&---------------------------------------------------------------------*
*&      Form  compare_abtab
*&---------------------------------------------------------------------*
FORM compare_abtab USING tname TYPE dd03l-tabname
                   CHANGING drefa TYPE REF TO data
                            drefb TYPE REF TO data
                            dreft TYPE REF TO data.

  PERFORM fill_abtab USING drefa CHANGING dreft.
  PERFORM fill_abtab USING drefb CHANGING dreft.

  FIELD-SYMBOLS : <drefa> TYPE STANDARD TABLE,
                  <drefb> TYPE STANDARD TABLE,
                  <dreft> TYPE STANDARD TABLE.

  ASSIGN : drefa->* TO <drefa>,
           drefb->* TO <drefb>,
           dreft->* TO <dreft>.

  DATA : trefa TYPE REF TO cl_abap_tabledescr,
         trefb TYPE REF TO cl_abap_tabledescr.

  trefa ?= cl_abap_tabledescr=>describe_by_data( <drefa> ).
  trefb ?= cl_abap_tabledescr=>describe_by_data( <drefb> ).

  DATA : slrefa TYPE REF TO cl_abap_structdescr,
         slrefb TYPE REF TO cl_abap_structdescr.

  slrefa ?= trefa->get_table_line_type( ).
  slrefb ?= trefb->get_table_line_type( ).

  DATA : dlrefa TYPE REF TO data,
         dlrefb TYPE REF TO data.

  CREATE DATA dlrefa TYPE HANDLE slrefa.
  CREATE DATA dlrefb TYPE HANDLE slrefb.

  FIELD-SYMBOLS : <dlrefa> TYPE ANY,
                  <dlrefb> TYPE ANY,
                  <dlreft> TYPE ANY.

  ASSIGN : dlrefa->* TO <dlrefa>,
           dlrefb->* TO <dlrefb>.

  SORT : <drefa>, <drefb>, <dreft>.

  DATA : total TYPE i,
         current TYPE i,
         equal TYPE flag,
         tabix_a TYPE i,
         tabix_b TYPE i.
  FIELD-SYMBOLS : <a> TYPE ANY,
                  <b> TYPE ANY,
                  <diff> TYPE ANY.
  DESCRIBE TABLE <dreft> LINES total.
  PERFORM progress USING 1 total.
  LOOP AT <dreft> ASSIGNING <dlreft>.
    ASSIGN : ('<DLREFT>-A') TO <a>,
             ('<DLREFT>-B') TO <b>,
             ('<DLREFT>-DIFF') TO <diff>.
    current = sy-tabix.
    PERFORM progress USING current total.
    LOOP AT <drefa> INTO <dlrefa>.
      tabix_a = sy-tabix.
      CLEAR equal.
      PERFORM key_is_equal USING tname <dlrefa> <dlreft> CHANGING equal.
      IF equal EQ 'X'.
        MOVE <dlrefa> TO <a>.
        DELETE <drefa> INDEX tabix_a.
        EXIT.
      ELSEIF equal EQ 'G'.
        EXIT.
      ENDIF.
    ENDLOOP.
    LOOP AT <drefb> INTO <dlrefb>.
      tabix_b = sy-tabix.
      CLEAR equal.
      PERFORM key_is_equal USING tname <dlrefb> <dlreft> CHANGING equal.
      IF equal EQ 'X'.
        MOVE <dlrefb> TO <b>.
        DELETE <drefb> INDEX tabix_b.
        EXIT.
      ELSEIF equal EQ 'G'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF <a> NE <b>.
      <diff> = 'X'.
    ENDIF.
    UNASSIGN : <a>, <b>, <diff>.
  ENDLOOP.

ENDFORM.                    "compare_abtab

*&---------------------------------------------------------------------*
*&      Form  download_abtab
*&---------------------------------------------------------------------*
FORM download_abtab USING fpath TYPE string
                          d TYPE REF TO data.

  FIELD-SYMBOLS : <d> TYPE STANDARD TABLE.

  ASSIGN : d->* TO <d>.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename              = fpath
      write_field_separator = 'X'
    CHANGING
      data_tab              = <d>.

ENDFORM.                    "download_abtab

*&---------------------------------------------------------------------*
*&      Form  progress
*&---------------------------------------------------------------------*
FORM progress USING current total.

  DATA : modi TYPE i,
         text TYPE c LENGTH 40,
         cn TYPE n LENGTH 10,
         tn TYPE n LENGTH 10.

  modi = current MOD 100.
  cn = current.
  tn = total.
  IF modi = 0.
    CONCATENATE '[' cn '/' tn ']' INTO text.
    CALL FUNCTION 'PROGRESS_INDICATOR'
      EXPORTING
        i_text      = text
        i_processed = current
        i_total     = total.
  ENDIF.

ENDFORM.                    "progress
