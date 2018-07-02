REPORT ztest_itab_search_perf.

PARAMETERS: p_repeat TYPE i DEFAULT 1000000,
            p_size   TYPE i DEFAULT 10235.

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

    DATA repeat TYPE i.
    DATA my_default TYPE ty_struct.
    DATA itab TYPE ty_itab.

    METHODS constructor IMPORTING size   TYPE i DEFAULT p_size
                                  repeat TYPE i DEFAULT p_repeat.
    METHODS execute IMPORTING find_id TYPE i
                              user    TYPE syuname.
ENDCLASS.

*-----------------------------------------------------------------------*
CLASS lcl_profiler IMPLEMENTATION.
*-----------------------------------------------------------------------*
  METHOD constructor.
    DATA rnd TYPE REF TO if_random_number.
    rnd ?= NEW cl_random_number( ).
    rnd->init( i_seed = 78348 ).
    me->repeat = repeat.
    DO size TIMES.
      INSERT VALUE #( id = sy-index val = rnd->get_random_int( 9999 ) ) INTO TABLE itab.
    ENDDO.
    my_default = VALUE #( id = 55 val = 9999 ).
  ENDMETHOD.

  METHOD run.
    DATA(access) = NEW lcl_profiler( size = p_size ).

    WRITE: / 'Lookup nonexisting value:'.
    access->execute( find_id = 0
                     user = 'XXXS' ).
    ULINE.

    WRITE: / 'Lookup existing value:'.
    access->execute( find_id = 5
                     user = sy-uname ).
  ENDMETHOD.

  DEFINE _start_timer.
    GET RUN TIME FIELD start.
    DO repeat TIMES.
  END-OF-DEFINITION.

  DEFINE _stop_timer.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / &1 && |: { stop - start }|.
  END-OF-DEFINITION.

  METHOD execute.
    DATA start TYPE i.
    DATA stop TYPE i.
    DATA row TYPE ty_struct.

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
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
    _stop_timer `TRY ... itab[ ... ] CATCH              `.

    _start_timer.
    SELECT SINGLE bname FROM usr01 INTO @DATA(lv_name) WHERE bname = @user.
    _stop_timer `SELECT SINGLE FROM usr01 WHERE ...     `.

  ENDMETHOD.

ENDCLASS.


*-----------------------------------------------------------------------*
START-OF-SELECTION.
*-----------------------------------------------------------------------*
  lcl_profiler=>run( ).
