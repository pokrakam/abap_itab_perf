REPORT ztest_itab_search_perf.

*-----------------------------------------------------------------------*
CLASS lcl_profiler DEFINITION CREATE PRIVATE.
*-----------------------------------------------------------------------*

  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_struct,
             id  TYPE i,
             val TYPE i,
           END OF ty_struct.
    TYPES ty_itab TYPE SORTED TABLE OF ty_struct WITH UNIQUE KEY id.

    CONSTANTS c_repeat TYPE i VALUE 1000000.

    DATA my_default TYPE ty_struct.
    DATA itab TYPE SORTED TABLE OF ty_struct WITH UNIQUE KEY id.

    DATA find_id TYPE i.

    METHODS constructor.
    METHODS test IMPORTING iv_key TYPE i.

ENDCLASS.

*-----------------------------------------------------------------------*
CLASS lcl_profiler IMPLEMENTATION.
*-----------------------------------------------------------------------*
  METHOD constructor.
    DO 10 TIMES.
      INSERT VALUE #( id = sy-index ) INTO TABLE itab.
    ENDDO.
    my_default = VALUE #( id = 55 val = 9999 ).
  ENDMETHOD.

  METHOD run.
    DATA(lo_access) = NEW lcl_profiler( ).

    WRITE: / 'Lookup nonexisting value:'.
    lo_access->test( 0 ).

    ULINE.

    WRITE: / 'Lookup existing value:'.
    lo_access->test( 5 ).
  ENDMETHOD.

  DEFINE _start_timer.
    GET RUN TIME FIELD start.
    DO c_repeat TIMES.
  END-OF-DEFINITION.

  DEFINE _stop_timer.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / &1 && |: { stop - start }|.
  END-OF-DEFINITION.

  METHOD test.
    DATA start TYPE i.
    DATA stop TYPE i.
    DATA row TYPE ty_struct.

    find_id = iv_key.

    _start_timer.
      IF line_exists( itab[ id = find_id ] ).
      ENDIF.
    _stop_timer `line_exists( )                         `.

    _start_timer.
      READ TABLE itab WITH KEY id = find_id TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        "Empty IF-block, just to provide equivalence to catch block
      ENDIF.
    _stop_timer `READ TABLE ... TRANSPORTING NO FIELDS  `.

    _start_timer.
      READ TABLE itab INTO row WITH KEY id = find_id.
      IF sy-subrc <> 0.
      ENDIF.
    _stop_timer `READ TABLE                             `.

    _start_timer.
      DATA(idx) = line_index( itab[ id = find_id ] ).
      IF idx GT 0.
        row = itab[ idx ].
      ENDIF.
    _stop_timer `line_index( )                          `.

    _start_timer.
      ASSIGN itab[ id = find_id ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc <> 0.
      ENDIF.
    _stop_timer `ASSIGN itab[ ... ]                     `.

    _start_timer.
      DATA(row_ref) = REF #( itab[ id = find_id ] OPTIONAL ) .
      IF sy-subrc <> 0.
      ENDIF.
    _stop_timer `REF #( itab[ ... ] OPTIONAL )          `.

    _start_timer.
      row = VALUE #( itab[ id = find_id ] OPTIONAL ).
      IF row IS INITIAL.
      ENDIF.
    _stop_timer `VALUE #( itabl[ ... ] OPTIONAL )       `.

    _start_timer.
      row = VALUE #( itab[ id = find_id ] OPTIONAL ).
    _stop_timer `VALUE #( itabl[ ... ] OPTIONAL ) w/o IF`.

    _start_timer.
      row = VALUE #( itab[ id = find_id ] OPTIONAL ).
    _stop_timer `VALUE #( itabl[ ... ] OPTIONAL w/o IF )`.

    _start_timer.
      row = VALUE #( itab[ id = find_id ] DEFAULT my_default ).
    _stop_timer `VALUE #( itabl[ ... ] DEFAULT w/o IF ) `.

    _start_timer.
      TRY.
          row = itab[ id = find_id ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    _stop_timer `TRY ... itab[ ... ] CATCH              `.

  ENDMETHOD.

ENDCLASS.


*-----------------------------------------------------------------------*
START-OF-SELECTION.
*-----------------------------------------------------------------------*
  lcl_profiler=>run( ).
