CLASS lcl_alv DEFINITION.
  "" LOCAL UTILS Class to display any table to an ALV.

  PUBLIC SECTION.
    CLASS-METHODS display
      IMPORTING
        ir_table TYPE REF TO data
      RAISING
        cx_salv_msg.
ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD display.
    "" Display any table in an ALV
    ""
    "" Input  : it_table    TYPE REF TO DATA
    DATA: lr_alv       TYPE REF TO cl_salv_table,
          lr_functions TYPE REF TO cl_salv_functions_list.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN ir_table->* TO <lt_table>.
    cl_salv_table=>factory(
        IMPORTING
            r_salv_table = lr_alv
        CHANGING
            t_table = <lt_table>
    ).

    lr_functions = lr_alv->get_functions( ).
    lr_functions->set_all( abap_true ).
    lr_alv->display( ).

  ENDMETHOD.
ENDCLASS. "" lcl_alv
