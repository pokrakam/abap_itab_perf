REPORT ztest_itab_search_perf.

*-----------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
*-----------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS start.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_struct,
             id  TYPE i,
             val TYPE i,
           END OF ty_struct.
    TYPES ty_itab TYPE SORTED TABLE OF ty_struct WITH UNIQUE KEY id.

    CONSTANTS repeat TYPE i VALUE 1000000.
    DATA itab TYPE SORTED TABLE OF ty_struct WITH UNIQUE KEY id.
    DATA find_id TYPE i.

    METHODS run_tests.
ENDCLASS.

*-----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
*-----------------------------------------------------------------------*

  METHOD start.

    DO 10 TIMES.
      INSERT VALUE #( id = sy-index ) INTO TABLE itab.
    ENDDO.

    WRITE: / 'Lookup nonexisting value:'.
    run_tests( ).
    ULINE.

    WRITE: / 'Lookup existing value:'.
    find_id = 5.
    run_tests( ).

  ENDMETHOD.

  METHOD run_tests.
    DATA row TYPE ty_struct.


    GET RUN TIME FIELD DATA(start).
    DO repeat TIMES.
      IF line_exists( itab[ id = find_id ] ).
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD DATA(stop).
    WRITE: / |line_exists( )                         : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      READ TABLE itab WITH KEY id = find_id TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        "Empty IF-block, just to provide equivalence to catch block
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |READ TABLE ... TRANSPORTING NO FIELDS  : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      READ TABLE itab INTO row WITH KEY id = find_id.
      IF sy-subrc <> 0.
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |READ TABLE                             : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      ASSIGN itab[ id = find_id ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc <> 0.
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |ASSIGN itab[ ... ]                     : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      DATA(row_ref) = REF #( itab[ id = find_id ] OPTIONAL ) .
      IF sy-subrc <> 0.
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |REF #( itab[ ... ] OPTIONAL )          : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      row = VALUE #( itab[ id = find_id ] OPTIONAL ).
      IF row IS INITIAL.
      ENDIF.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |VALUE #( itabl[ ... ] OPTIONAL )       : { stop - start }|.


    GET RUN TIME FIELD start.
    DO repeat TIMES.
      TRY.
          row = itab[ id = find_id ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDDO.
    GET RUN TIME FIELD stop.
    WRITE: / |TRY ... itab[ ... ] CATCH              : { stop - start }|.

  ENDMETHOD.

ENDCLASS.


*-----------------------------------------------------------------------*
START-OF-SELECTION.
*-----------------------------------------------------------------------*
  NEW lcl_main( )->start( ).
