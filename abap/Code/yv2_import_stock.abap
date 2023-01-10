*&---------------------------------------------------------------------*
*& Report yv2_import_stock
*&---------------------------------------------------------------------*
*& 10/01/2023
*&---------------------------------------------------------------------*
REPORT yv2_import_stock.

**********************************************************************
** Local classes
**
**********************************************************************

CLASS lcl_csv DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      export IMPORTING data TYPE REF TO data
                       file TYPE string OPTIONAL.
ENDCLASS.

CLASS lcl_csv IMPLEMENTATION.
  "" LOCAL UTILS Class to export any table to a CSV file.

  METHOD export.
    "" Export any table to a CSV file
    ""
    "" Input  : data TYPE REF TO DATA
    ""          file TYPE string

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.
    DATA: table_description TYPE REF TO cl_abap_tabledescr,
          line_structure    TYPE REF TO cl_abap_structdescr.

    ASSIGN data->* TO <fs_table>.

    table_description ?= cl_abap_structdescr=>describe_by_data_ref( data ).
    line_structure ?= table_description->get_table_line_type( ).
    DATA(fields) = line_structure->components.

    DATA content_tab TYPE string_t.
    DATA(line_content) = VALUE string( ).

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<fs_field>).
      IF strlen( line_content ) > 0.
        line_content = |{ line_content },|.
      ENDIF.

      DATA(fieldname) = |{ <fs_field>-name }|.
      SELECT SINGLE scrtext_l
        FROM dd04t
       WHERE ddlanguage = @sy-langu
         AND as4local = 'A'
         AND rollname = @<fs_field>-name
         INTO @fieldname.

      line_content = |{ line_content }"{ fieldname }"|.
    ENDLOOP.
    APPEND line_content TO content_tab.

    LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row>).
      line_content = ''.
      LOOP AT fields ASSIGNING <fs_field>.
        ASSIGN COMPONENT <fs_field>-name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_value>).

        IF strlen( line_content ) > 0.
          line_content = |{ line_content },|.
        ENDIF.
        line_content = |{ line_content }"{ <fs_value> }"|.

      ENDLOOP.

      APPEND line_content TO content_tab.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = file
        write_field_separator = 'X'
      TABLES
        data_tab              = content_tab.
    MESSAGE |Le fichier a été enregistré avec succès| TYPE 'S'.
  ENDMETHOD.
ENDCLASS. "" lcl_csv

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


CLASS lcl_material_importer DEFINITION.

  PUBLIC SECTION.


    METHODS:
      constructor
        IMPORTING
          VALUE(file_path)             TYPE localfile
          VALUE(movement_posting_date) TYPE datum DEFAULT sy-datum
          VALUE(test_mode)             TYPE abap_bool DEFAULT abap_true
          VALUE(import_id)             TYPE string OPTIONAL,

      import.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF s_movement_result,
             import_id            TYPE string,
             move_type            TYPE string,
             material             TYPE string,
             plant                TYPE string,
             stge_loc             TYPE string,
             batch                TYPE string,
             entry_qnt            TYPE string,
             entry_uom            TYPE string,
             item_text            TYPE string,
             serial_number_profil TYPE string,
             movement_status      TYPE string,
             movement_result      TYPE string,
           END OF s_movement_result.

    TYPES: BEGIN OF s_file_structure,
             material                 TYPE matnr,
             plant                    TYPE werks_d,
             storage_location         TYPE lgort_d,
             batch                    TYPE charg_d,
             "unit                     TYPE meins,
             quantity                 TYPE labst,
             quantity_in_transf_plant TYPE umlmc2,
             quantity_in_transf_stloc TYPE umlmd,
             "serial_number_profil     TYPE serail,
           END OF s_file_structure,
           t_file_content TYPE STANDARD TABLE OF s_file_structure WITH DEFAULT KEY.

    DATA: alv TYPE REF TO lcl_alv,
          csv TYPE REF TO lcl_csv.

    DATA: movement_logs TYPE STANDARD TABLE OF s_movement_result.

    DATA: file_path             TYPE localfile,
          movement_posting_date TYPE datum,
          test_mode             TYPE abap_bool,
          import_id             TYPE string.

    DATA: importing_stock         TYPE t_file_content,
          storage_stock_in_transf TYPE t_file_content,
          plant_stock_in_transf   TYPE t_file_content.

    METHODS:

      generate_random_import_id RETURNING VALUE(r_result) TYPE string,

      load_stock_file,
      import_stock,

      import_all_stock,
      _import_stock
        IMPORTING VALUE(stock)    TYPE s_file_structure
        RETURNING VALUE(r_result) TYPE s_movement_result,

      move_plant_stock_in_transf,
      _move_plant_stock_in_transf
        IMPORTING VALUE(stock)    TYPE s_file_structure
        RETURNING VALUE(r_result) TYPE s_movement_result,

      move_storage_stock_in_transf,
      _move_storage_stock_in_transf
        IMPORTING VALUE(stock)    TYPE s_file_structure
        RETURNING VALUE(r_result) TYPE s_movement_result,

      export_movement_logs,



      read_file IMPORTING VALUE(file)     TYPE string
                RETURNING VALUE(r_result) TYPE string_t,

      convert_file_content_to_table IMPORTING VALUE(file_content) TYPE string_t
                                    RETURNING VALUE(r_result)     TYPE t_file_content.


ENDCLASS.

CLASS lcl_material_importer IMPLEMENTATION.


  METHOD constructor.

    me->alv = NEW lcl_alv( ).
    me->csv = NEW lcl_csv( ).

    me->file_path = file_path.
    me->movement_posting_date = movement_posting_date.
    me->test_mode = test_mode.
    me->import_id = import_id.

    IF strlen( me->import_id ) = 0.
      me->import_id = me->generate_random_import_id( ).
    ENDIF.

  ENDMETHOD.

  METHOD import.

    me->load_stock_file( ).
    me->import_stock( ).

  ENDMETHOD.

**********************************************************************
** PRIVATE
**********************************************************************

  METHOD generate_random_import_id.
    "" Generate a random export id
    IF me->test_mode = abap_true.
      r_result = 'TEST_MODE'.
    ELSE.
      TRY.
          r_result = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          r_result = '0000000000'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD load_stock_file.

    CLEAR me->importing_stock.

    MESSAGE |Lecture du fichier d'import| TYPE 'S'.
    DATA(file_content) = read_file( CONV #( me->file_path ) ).

    MESSAGE |Conversion du fichier d'import| TYPE 'S'.
    me->importing_stock = convert_file_content_to_table( file_content ).

  ENDMETHOD.

  METHOD import_stock.

    MESSAGE |Import du stock| TYPE 'S'.
    me->import_all_stock( ).
    me->move_plant_stock_in_transf( ).
    me->move_storage_stock_in_transf( ).

    me->export_movement_logs( ).
  ENDMETHOD.

  METHOD import_all_stock.

    LOOP AT me->importing_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

      cl_progress_indicator=>progress_indicate(
        i_text = |Import du stock: { sy-tabix }/{ lines( me->importing_stock ) }|
        i_output_immediately = abap_true ).

      IF <fs_stock>-quantity_in_transf_plant > 0.
        APPEND <fs_stock> TO me->plant_stock_in_transf.
      ENDIF.

      IF <fs_stock>-quantity_in_transf_stloc > 0.
        APPEND <fs_stock> TO me->storage_stock_in_transf.
      ENDIF.

      DATA(result) = me->_import_stock( <fs_stock> ).
      APPEND result TO me->movement_logs.

    ENDLOOP.

  ENDMETHOD.

  METHOD _import_stock.

    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( pstng_date = me->movement_posting_date doc_date = sy-datum ).

    movement-move_type  = '561'.
    movement-material = stock-material.
    movement-plant      = stock-plant.
    movement-stge_loc   = stock-storage_location.
    movement-entry_qnt  = stock-quantity + stock-quantity_in_transf_plant + stock-quantity_in_transf_stloc.
    movement-item_text  = me->import_id.
    movement-batch      = stock-batch.

    DATA(movements) = VALUE bapi2017_gm_item_create_t( ( movement ) ).
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = movement_header
        goodsmvt_code   = '05'
        testrun         = me->test_mode
      TABLES
        goodsmvt_item   = movements
        return          = movements_result.

    r_result = CORRESPONDING #( movement ).
    LOOP AT movements_result ASSIGNING FIELD-SYMBOL(<movement_result>).
      r_result-movement_result = |{ r_result-movement_result }{ <movement_result>-message }|.
      r_result-movement_status = |{ r_result-movement_status }{ <movement_result>-type }|.
    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD move_plant_stock_in_transf.

    LOOP AT me->plant_stock_in_transf ASSIGNING FIELD-SYMBOL(<fs_stock>).
      cl_progress_indicator=>progress_indicate(
        i_text = |Déplacement du stock en transf sur division { sy-tabix }/{ lines( me->plant_stock_in_transf ) }|
        i_output_immediately = abap_true ).


      DATA(result) = me->_move_plant_stock_in_transf( <fs_stock> ).
      APPEND result TO me->movement_logs.
    ENDLOOP.

  ENDMETHOD.

  METHOD _move_plant_stock_in_transf.
    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( pstng_date = me->movement_posting_date doc_date = sy-datum ).

    movement-move_type  = '303'.
    movement-material   = stock-material.
    movement-plant      = stock-plant.
    movement-stge_loc   = stock-storage_location.
    movement-entry_qnt  = stock-quantity_in_transf_plant.
    movement-item_text  = me->import_id.
    movement-batch      = stock-batch.

    DATA(movements) = VALUE bapi2017_gm_item_create_t( ( movement ) ).
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = movement_header
        goodsmvt_code   = '04'
        testrun         = me->test_mode
      TABLES
        goodsmvt_item   = movements
        return          = movements_result.

    r_result = CORRESPONDING #( movement ).
    LOOP AT movements_result ASSIGNING FIELD-SYMBOL(<movement_result>).
      r_result-movement_result = |{ r_result-movement_result }{ <movement_result>-message }|.
      r_result-movement_status = |{ r_result-movement_status }{ <movement_result>-type }|.
    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD move_storage_stock_in_transf.

    LOOP AT me->storage_stock_in_transf ASSIGNING FIELD-SYMBOL(<fs_stock>).
      cl_progress_indicator=>progress_indicate(
        i_text = |Déplacement du stock en transf sur storage loc { sy-tabix }/{ lines( me->storage_stock_in_transf ) }|
        i_output_immediately = abap_true ).

      DATA(result) = me->_move_storage_stock_in_transf( <fs_stock> ).
      APPEND result TO me->movement_logs.
    ENDLOOP.

  ENDMETHOD.

  METHOD _move_storage_stock_in_transf.
    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( pstng_date = me->movement_posting_date doc_date = sy-datum ).

    movement-move_type  = '313'.
    movement-material   = stock-material.
    movement-plant      = stock-plant.
    movement-stge_loc   = stock-storage_location.
    movement-entry_qnt  = stock-quantity_in_transf_stloc.
    movement-item_text  = me->import_id.
    movement-batch      = stock-batch.

    DATA(movements) = VALUE bapi2017_gm_item_create_t( ( movement ) ).
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = movement_header
        goodsmvt_code   = '04'
        testrun         = me->test_mode
      TABLES
        goodsmvt_item   = movements
        return          = movements_result.

    r_result = CORRESPONDING #( movement ).
    LOOP AT movements_result ASSIGNING FIELD-SYMBOL(<movement_result>).
      r_result-movement_result = |{ r_result-movement_result }{ <movement_result>-message }|.
      r_result-movement_status = |{ r_result-movement_status }{ <movement_result>-type }|.
    ENDLOOP.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD export_movement_logs.

    DATA: filename TYPE string,
          path     TYPE string,
          fullpath TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = 'Sauvegarde du fichier de log'
        default_extension         = 'csv'
        default_file_name         = 'movement_import_logs'
        file_filter               = '*.csv'
      CHANGING
        filename                  = filename
        path                      = path
        fullpath                  = fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "" Format movement_logs
    DATA(logs) = me->movement_logs.
    LOOP AT logs ASSIGNING FIELD-SYMBOL(<movement>).
      <movement>-import_id = me->import_id.
      REPLACE ALL OCCURRENCES OF '.' IN <movement>-entry_qnt WITH ','.
      <movement>-movement_status = COND #( WHEN <movement>-movement_status = '' THEN 'Success'
                                                                                ELSE <movement>-movement_status ).
    ENDLOOP.
    me->csv->export( file = fullpath
                     data = REF #( logs ) ).

  ENDMETHOD.

  METHOD read_file.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = file
      TABLES
        data_tab                = r_result
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD convert_file_content_to_table.
    DATA: csv_content_pattern TYPE string VALUE '"(.*?)"',
          table_description   TYPE REF TO cl_abap_tabledescr,
          line_structure      TYPE REF TO cl_abap_structdescr.

    table_description ?= cl_abap_structdescr=>describe_by_data( r_result ).
    line_structure ?= table_description->get_table_line_type( ).
    DATA(fields) = line_structure->components.

    LOOP AT file_content ASSIGNING FIELD-SYMBOL(<fs_content>).

      IF sy-tabix = 1.
        "" Skip header line
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO r_result ASSIGNING FIELD-SYMBOL(<stock>).
      FIND ALL OCCURRENCES OF PCRE csv_content_pattern IN <fs_content> RESULTS DATA(csv_data).

      DATA(index) = 0.
      LOOP AT csv_data ASSIGNING FIELD-SYMBOL(<fs_data>).
        index = index + 1.
        DATA(value) = substring( val = <fs_content>
                                 off = <fs_data>-submatches[ 1 ]-offset
                                 len = <fs_data>-submatches[ 1 ]-length ).
        REPLACE ALL OCCURRENCES OF ',' IN value WITH '.'.

        ASSIGN COMPONENT fields[ index ]-name OF STRUCTURE <stock> TO FIELD-SYMBOL(<fs_value>).
        <fs_value> = value.
        UNASSIGN <fs_value>.

        "" Format values
        SHIFT <stock>-material LEFT DELETING LEADING '0'.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <stock>-material
          IMPORTING
            output = <stock>-material.

        IF strlen( <stock>-plant ) = 1.
          <stock>-plant = |0{ <stock>-plant }|.
        ENDIF.

        IF strlen( <stock>-storage_location ) = 1.
          <stock>-storage_location = |0{ <stock>-storage_location }|.
        ENDIF.

        IF <stock>-batch IS NOT INITIAL.
          SHIFT <stock>-batch LEFT DELETING LEADING '0'.
          CALL FUNCTION 'CONVERSION_EXIT_CHARG_INPUT'
            EXPORTING
              input  = <stock>-batch
            IMPORTING
              output = <stock>-batch.
        ENDIF.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

PARAMETERS     p_source   TYPE localfile  OBLIGATORY.
PARAMETERS     p_datcom   TYPE bsim-budat DEFAULT sy-datum OBLIGATORY.
PARAMETERS     p_impid    TYPE c LENGTH 18.
PARAMETERS     p_test     TYPE abap_bool  AS CHECKBOX DEFAULT 'X'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_source.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'p_source'
    IMPORTING
      file_name  = p_source.


START-OF-SELECTION.

  DATA(material_importer) = NEW lcl_material_importer(
    file_path             = p_source
    movement_posting_date = p_datcom
    import_id             = |{ p_impid }|
    test_mode             = p_test
  ).

  material_importer->import( ).