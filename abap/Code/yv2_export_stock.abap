*&---------------------------------------------------------------------*
*& Report yv2_export_stock
*&---------------------------------------------------------------------*
*& 10/01/2023
*&---------------------------------------------------------------------*
REPORT yv2_export_stock.

**********************************************************************
** Local classes
** - lcl_csv
** - lcl_alv
** - lcl_material_exporter
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

CLASS lcl_material_exporter DEFINITION.

  PUBLIC SECTION.
    METHODS:

      constructor
        IMPORTING
          VALUE(movement_posting_date) TYPE datum DEFAULT sy-datum
          VALUE(export_id)             TYPE string DEFAULT ''
          VALUE(test_mode)             TYPE abap_bool DEFAULT abap_true
          VALUE(display_alv)           TYPE abap_bool DEFAULT abap_true,

      export.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF s_material_in_storage_loc.
             INCLUDE TYPE mard.
    TYPES:   charg TYPE charg_d,
             sobkz TYPE sobkz,
             lifnr TYPE lifnr,
             meins TYPE meins,
             sernp TYPE serail,
           END OF s_material_in_storage_loc,
           t_material_storage_loc TYPE STANDARD TABLE OF s_material_in_storage_loc.

    TYPES: BEGIN OF s_material_plant.
             INCLUDE TYPE  marc.
    TYPES:   meins TYPE meins,
           END OF s_material_plant,
           t_material_plant TYPE STANDARD TABLE OF s_material_plant.

    TYPES: BEGIN OF s_movement_result,
             export_id            TYPE string,
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

    TYPES: BEGIN OF s_stock,
             material                 TYPE matnr,
             plant                    TYPE werks_d,
             storage_location         TYPE lgort_d,
             batch                    TYPE charg_d,
             unit                     TYPE meins,
             quantity                 TYPE labst,
             quantity_in_transf_plant TYPE umlmc2,
             quantity_in_transf_stloc TYPE umlmd,
             serial_number_profil     TYPE serail,
           END OF s_stock.

    DATA: movement_logs TYPE STANDARD TABLE OF s_movement_result.

    DATA: alv TYPE REF TO lcl_alv,
          csv TYPE REF TO lcl_csv.

    DATA:
      movement_posting_date TYPE datum,
      export_id             TYPE string,
      test_mode             TYPE abap_bool,
      display_alv           TYPE abap_bool.

    DATA:
      storage_stock           TYPE t_material_storage_loc,
      storage_stock_in_transf TYPE t_material_storage_loc,
      plant_stock_in_transf   TYPE t_material_plant,
      merged_stock            TYPE TABLE OF s_stock.

    METHODS:

      generate_random_export_id RETURNING VALUE(r_result) TYPE string,

      check_invalid_stock,
      dispay_invalid_stock_error_msg IMPORTING VALUE(invalid_stock) TYPE t_material_storage_loc,
      export_invalid_stock_to_csv IMPORTING VALUE(invalid_stock) TYPE t_material_storage_loc,

      fetch_stocks,
      display_stocks,

      export_stock_to_csv,
      merge_all_stocks,
      merge_plant_stock_in_transf,
      merge_stloc_stock_in_transf,
      merge_stloc_stock,

      refresh_storage_stock,
      refresh_plant_stock_in_transf,
      refresh_storage_stock_in_trans,

      release_stocks,
      move_plant_stock_in_transf,
      _move_plant_stock_in_transf
        IMPORTING VALUE(material) TYPE s_stock
        RETURNING VALUE(r_result) TYPE s_movement_result,

      move_storage_stock_in_transf,
      _move_storage_stock_in_transf
        IMPORTING VALUE(material) TYPE s_stock
        RETURNING VALUE(r_result) TYPE s_movement_result,

      release_merged_stock,
      _release_merged_stock
        IMPORTING VALUE(material) TYPE s_stock
        RETURNING VALUE(r_result) TYPE s_movement_result,

      export_movement_logs,

      find_material_storage_loc
        IMPORTING VALUE(material) TYPE matnr
                  VALUE(plant)    TYPE werks_d
        RETURNING VALUE(r_result) TYPE lgort,

      raise_error,
      display_message IMPORTING VALUE(message) TYPE string.

ENDCLASS.

CLASS lcl_material_exporter IMPLEMENTATION.

  METHOD constructor.
    "" Initialize the class
    ""
    "" Input  : movement_posting_date TYPE DATUM
    ""          export_id             TYPE STRING
    ""          test_mode             TYPE ABAP_BOOL
    ""          display_alv           TYPE ABAP_BOOL

    me->alv = NEW lcl_alv( ).
    me->csv = NEW lcl_csv( ).

    me->movement_posting_date = movement_posting_date.
    me->export_id = export_id.
    me->test_mode = test_mode.
    me->display_alv = display_alv.

    IF strlen( me->export_id ) = 0.
      me->export_id = me->generate_random_export_id( ).
    ENDIF.

  ENDMETHOD.

  METHOD export.
    "" Export actual materials.

    me->check_invalid_stock( ).
    me->fetch_stocks( ).
    IF me->display_alv = abap_true.
      me->display_stocks( ).
    ENDIF.
    me->merge_all_stocks( ).
    me->export_stock_to_csv( ).
    me->release_stocks( ).
  ENDMETHOD.


  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "" PRIVATE METHODS
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  METHOD generate_random_export_id.
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

  METHOD check_invalid_stock.
    "" Check if there is any invalid stock
    DATA: stock_with_error TYPE t_material_storage_loc.

    SELECT *
      FROM mard
     WHERE insme <> 0
        OR einme <> 0
        OR speme <> 0
        OR retme <> 0
     ORDER BY matnr ASCENDING, werks ASCENDING, lgort ASCENDING
      INTO CORRESPONDING FIELDS OF TABLE @stock_with_error .

    IF lines( stock_with_error ) > 0.
      me->dispay_invalid_stock_error_msg( invalid_stock = stock_with_error ).
      me->export_invalid_stock_to_csv( invalid_stock = stock_with_error ).
      me->raise_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD dispay_invalid_stock_error_msg.
    "" Display error message with number of errors.
    ""
    "" Input  : invalid_stock TYPE MARD_TAB

    DATA(error_message) = |{ lines( invalid_stock ) } erreurs trouvé.|.
    MESSAGE error_message TYPE 'I'.
  ENDMETHOD.

  METHOD export_invalid_stock_to_csv.
    "" Export invalid stock to CSV
    ""
    "" Input  : invalid_stock TYPE MARD_TAB

    DATA: filename TYPE string,
          path     TYPE string,
          fullpath TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = 'Sauvegarde du fichier'
        default_extension         = 'csv'
        default_file_name         = 'invalid_stock'
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

    me->csv->export( file = fullpath
                         data = REF #( invalid_stock ) ).

  ENDMETHOD.

  METHOD fetch_stocks.
    "" Fetch stock from MARC and MARD

    me->refresh_storage_stock( ).
    me->refresh_storage_stock_in_trans( ).
    me->refresh_plant_stock_in_transf( ).

    DATA(message) = |Résultat:|.
    message = |{ message } { lines( me->plant_stock_in_transf ) } materials avec du stock en transfert (division).|.
    message = |{ message } { lines( me->storage_stock_in_transf ) } materials avec du stock en transfert (magasin).|.
    message = |{ message } { lines( me->storage_stock  ) } materials avec du stock.|.
    me->display_message( message ).

  ENDMETHOD.

  METHOD display_stocks.
    "" Display stock in ALV.

    TYPES: BEGIN OF s_stock,
             matnr TYPE matnr,
             werks TYPE werks_d,
             lgort TYPE lgort_d,
             charg TYPE charg_d,
             labst TYPE labst,
             umlme TYPE umlmd,
             umlmc TYPE umlmc2,
           END OF s_stock.
    DATA: merged_data TYPE STANDARD TABLE OF s_stock,
          stock_line  TYPE s_stock.

    MOVE-CORRESPONDING me->plant_stock_in_transf TO merged_data.
    MOVE-CORRESPONDING me->storage_stock TO merged_data KEEPING TARGET LINES.

    FIELD-SYMBOLS: <fs_stock> LIKE LINE OF merged_data.
    LOOP AT me->storage_stock_in_transf ASSIGNING FIELD-SYMBOL(<fs_material>).
      UNASSIGN <fs_stock>.
      ASSIGN merged_data[ matnr = <fs_material>-matnr
                          werks = <fs_material>-werks
                          lgort = <fs_material>-lgort
                          charg = <fs_material>-charg ] TO <fs_stock>.
      IF <fs_stock> IS NOT ASSIGNED.
        MOVE-CORRESPONDING <fs_material> TO stock_line.
        APPEND stock_line TO merged_data.
      ELSE.
        <fs_stock>-umlme = <fs_material>-umlme.
      ENDIF.

    ENDLOOP.

    me->alv->display( ir_table = REF #( merged_data ) ).

  ENDMETHOD.

  METHOD export_stock_to_csv.
    "" Export current stock to a CSV file to import it later

    TYPES: BEGIN OF s_formatted_stock,
             material                 TYPE string,
             plant                    TYPE string,
             storage_location         TYPE string,
             batch                    TYPE string,
             quantity                 TYPE string,
             quantity_in_transf_plant TYPE string,
             quantity_in_transf_stloc TYPE string,
           END OF s_formatted_stock.

    DATA: filename        TYPE string,
          path            TYPE string,
          fullpath        TYPE string,
          formatted_stock TYPE STANDARD TABLE OF s_formatted_stock.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = 'Sauvegarde du fichier'
        default_extension         = 'csv'
        default_file_name         = |stock_export_{ sy-datum }|
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

    MOVE-CORRESPONDING me->merged_stock TO formatted_stock.
    LOOP AT formatted_stock ASSIGNING FIELD-SYMBOL(<stock>).
      REPLACE ALL OCCURRENCES OF '.' IN <stock>-quantity WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN <stock>-quantity_in_transf_plant WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN <stock>-quantity_in_transf_stloc WITH ','.
    ENDLOOP.

    me->csv->export( file = fullpath
                     data = REF #( formatted_stock ) ).

  ENDMETHOD.

  METHOD merge_all_stocks.
    "" Convert all stocks tables to one movement table

    me->merge_plant_stock_in_transf( ).
    me->merge_stloc_stock_in_transf( ).
    me->merge_stloc_stock( ).
  ENDMETHOD.

  METHOD merge_plant_stock_in_transf.

    "" Move stock in transfer from plant to quantity in storage location.
    LOOP AT me->plant_stock_in_transf ASSIGNING FIELD-SYMBOL(<fs_plant_stock>).
      APPEND INITIAL LINE TO me->merged_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

      <fs_stock>-material = <fs_plant_stock>-matnr.
      <fs_stock>-plant = <fs_plant_stock>-werks.
      <fs_stock>-storage_location = me->find_material_storage_loc( material = <fs_plant_stock>-matnr
                                                                   plant    = <fs_plant_stock>-werks ).
      <fs_stock>-quantity_in_transf_plant = <fs_plant_stock>-umlmc.
      <fs_stock>-unit = <fs_plant_stock>-meins.
    ENDLOOP.

  ENDMETHOD.


  METHOD merge_stloc_stock_in_transf.
    "" Merge stock in transfer from storage location with existing data
    FIELD-SYMBOLS: <fs_stock> LIKE LINE OF merged_stock.
    LOOP AT me->storage_stock_in_transf ASSIGNING FIELD-SYMBOL(<fs_storage_stock>).
      UNASSIGN <fs_stock>.
      ASSIGN me->merged_stock[ material         = <fs_storage_stock>-matnr
                               plant            = <fs_storage_stock>-werks
                               storage_location = <fs_storage_stock>-lgort
                               batch            = <fs_storage_stock>-charg
                               ] TO <fs_stock>.
      IF <fs_stock> IS NOT ASSIGNED.
        APPEND INITIAL LINE TO me->merged_stock ASSIGNING <fs_stock>.
      ENDIF.

      <fs_stock>-material = <fs_storage_stock>-matnr.
      <fs_stock>-plant = <fs_storage_stock>-werks.
      <fs_stock>-storage_location = <fs_storage_stock>-lgort.
      <fs_stock>-batch = <fs_storage_stock>-charg.
      <fs_stock>-quantity_in_transf_stloc = <fs_storage_stock>-umlme.
      <fs_stock>-unit = <fs_storage_stock>-meins.
      <fs_stock>-serial_number_profil = <fs_storage_stock>-sernp.
    ENDLOOP.

  ENDMETHOD.

  METHOD merge_stloc_stock.
    "" Merge stock with existing data
    FIELD-SYMBOLS: <fs_stock> LIKE LINE OF merged_stock.
    LOOP AT me->storage_stock ASSIGNING FIELD-SYMBOL(<fs_storage_stock>).
      UNASSIGN <fs_stock>.
      ASSIGN me->merged_stock[ material         = <fs_storage_stock>-matnr
                               plant            = <fs_storage_stock>-werks
                               storage_location = <fs_storage_stock>-lgort
                               batch            = <fs_storage_stock>-charg ] TO <fs_stock>.
      IF <fs_stock> IS NOT ASSIGNED.
        APPEND INITIAL LINE TO me->merged_stock ASSIGNING <fs_stock>.
      ENDIF.

      <fs_stock>-material = <fs_storage_stock>-matnr.
      <fs_stock>-plant = <fs_storage_stock>-werks.
      <fs_stock>-storage_location = <fs_storage_stock>-lgort.
      <fs_stock>-batch = <fs_storage_stock>-charg.
      <fs_stock>-quantity = <fs_storage_stock>-labst.
      <fs_stock>-unit = <fs_storage_stock>-meins.
      <fs_stock>-serial_number_profil = <fs_storage_stock>-sernp.
    ENDLOOP.

  ENDMETHOD.

  METHOD refresh_plant_stock_in_transf.

    CLEAR me->plant_stock_in_transf.

    SELECT
        marc~matnr,
        marc~werks,
        marc~umlmc,
        mara~meins,
        marc~sernp
      FROM marc
        INNER JOIN mara ON mara~matnr = marc~matnr
      WHERE umlmc <> 0
      INTO CORRESPONDING FIELDS OF TABLE @me->plant_stock_in_transf.

  ENDMETHOD.

  METHOD refresh_storage_stock.

    CLEAR me->storage_stock.

    SELECT
        mard~matnr,
        mard~werks,
        mard~lgort,
        mchb~charg,
        CASE
            WHEN mchb~charg IS NULL THEN mard~labst
            ELSE mchb~clabs
        END AS labst,
        mara~meins,
        marc~sernp
      FROM mard
        INNER JOIN mara ON mara~matnr = mard~matnr
        INNER JOIN marc ON marc~matnr = mard~matnr AND marc~werks = mard~werks
        LEFT OUTER JOIN mchb ON mchb~matnr = mard~matnr AND mchb~werks = mard~werks AND mchb~lgort = mard~lgort
      WHERE ( mchb~charg IS NULL AND mard~labst <> 0 ) OR ( mchb~charg IS NOT NULL AND mchb~clabs <> 0 )
      INTO CORRESPONDING FIELDS OF TABLE @me->storage_stock.

  ENDMETHOD.

  METHOD refresh_storage_stock_in_trans.

    CLEAR me->storage_stock_in_transf.

    SELECT mard~matnr,
           mard~werks,
           mard~lgort,
           mard~umlme,
           mara~meins,
           marc~sernp
      FROM mard
        INNER JOIN mara ON mara~matnr = mard~matnr
        INNER JOIN marc ON marc~matnr = mard~matnr AND marc~werks = mard~werks
      WHERE umlme <> 0
      INTO CORRESPONDING FIELDS OF TABLE @me->storage_stock_in_transf.

  ENDMETHOD.

  METHOD release_stocks.

    me->move_plant_stock_in_transf( ).
    me->move_storage_stock_in_transf( ).
    me->release_merged_stock( ).

    me->export_movement_logs( ).

  ENDMETHOD.

  METHOD move_plant_stock_in_transf.
    " Move stock in transfer from plant to storage location.

    LOOP AT me->merged_stock ASSIGNING FIELD-SYMBOL(<fs_stock>) WHERE quantity_in_transf_plant > 0.

      cl_progress_indicator=>progress_indicate(
        i_text = |Processing plant tansf: { sy-tabix }/{ lines( me->plant_stock_in_transf ) }|
        i_output_immediately = abap_true ).
      DATA(result) = me->_move_plant_stock_in_transf( <fs_stock> ).
      APPEND result TO me->movement_logs.
    ENDLOOP.
  ENDMETHOD.

  METHOD _move_plant_stock_in_transf.
    " Create movement 304 to place material in storage location

    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( doc_date = sy-datum
                               pstng_date = me->movement_posting_date ).

    "" Add general data
    movement-move_type = '304'.
    movement-material  = material-material.
    movement-plant     = material-plant.
    movement-stge_loc  = material-storage_location.
    movement-entry_qnt = material-quantity_in_transf_plant.
    movement-entry_uom = material-unit.

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
    " Move storage from MARD stock in transfer to unrestricted stock

    LOOP AT me->merged_stock ASSIGNING FIELD-SYMBOL(<fs_stock>) WHERE quantity_in_transf_stloc > 0.
      cl_progress_indicator=>progress_indicate(
        i_text = |Processing stloc tansf: { sy-tabix }/{ lines( me->storage_stock_in_transf ) }|
        i_output_immediately = abap_true ).
      DATA(result) = me->_move_storage_stock_in_transf( <fs_stock> ).
      APPEND result TO me->movement_logs.
    ENDLOOP.

  ENDMETHOD.

  METHOD _move_storage_stock_in_transf.
    "" Create movement 315 to move stock in transfer to unrestricted stock

    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( doc_date = sy-datum
                               pstng_date = me->movement_posting_date ).

    "" Add general data
    movement-move_type = '315'.
    movement-material  = material-material.
    movement-plant     = material-plant.
    movement-stge_loc  = material-storage_location.
    movement-entry_qnt = material-quantity_in_transf_stloc.
    movement-entry_uom = material-unit.

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

  METHOD release_merged_stock.
    " Release all stock with movement 562

    LOOP AT me->merged_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).
      cl_progress_indicator=>progress_indicate(
        i_text = |Processing release: { sy-tabix }/{ lines( me->merged_stock ) }|
        i_output_immediately = abap_true ).

      DATA(result) = me->_release_merged_stock( <fs_stock> ).
      APPEND result TO me->movement_logs.
    ENDLOOP.

  ENDMETHOD.

  METHOD _release_merged_stock.
    "" Make 562 movement to release stock

    DATA: movement         TYPE bapi2017_gm_item_create,
          movement_header  TYPE bapi2017_gm_head_01,
          movements_result TYPE bapiret2_t.

    movement_header = VALUE #( doc_date = sy-datum
                               pstng_date = me->movement_posting_date ).

    "" Add general data
    movement-move_type = '562'.
    movement-material  = material-material.
    movement-plant     = material-plant.
    movement-stge_loc  = material-storage_location.
    movement-entry_qnt = material-quantity + material-quantity_in_transf_stloc + material-quantity_in_transf_plant.
    movement-entry_uom = material-unit.
    movement-item_text = |Export { me->export_id }|.
    movement-batch = material-batch.

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
    r_result-serial_number_profil = material-serial_number_profil.
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
        default_file_name         = 'movement_logs'
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
    LOOP AT me->movement_logs ASSIGNING FIELD-SYMBOL(<movement>).
      <movement>-export_id = me->export_id.
      REPLACE ALL OCCURRENCES OF '.' IN <movement>-entry_qnt WITH ','.
      <movement>-movement_status = COND #( WHEN <movement>-movement_status = '' THEN 'Success'
                                                                                ELSE <movement>-movement_status ).
    ENDLOOP.

    me->csv->export( file = fullpath
                     data = REF #( me->movement_logs ) ).
  ENDMETHOD.

  METHOD find_material_storage_loc.

    SELECT lgort
      FROM mard
      INTO TABLE @DATA(storages)
     WHERE werks = @plant
       AND matnr = @material
     ORDER BY lgort DESCENDING.

    r_result = VALUE #( storages[ 1 ] OPTIONAL ).

  ENDMETHOD.

  METHOD raise_error.
    "" Raise an error

    DATA(action_id) = c_utils=>popup_to_confirm(
      EXPORTING
        id_caption               = |Erreur(s) trouvé|
        id_message               = |Des erreurs ont été trouvé. Souhaitez vous continuer l'export du stock ?|
        id_display_cancel_button = ' '
        id_text_button1          = 'Oui'
        id_text_button2          = 'Non'
    ).

    IF action_id = 1. " OK
    ELSEIF action_id = 2. " NOK
      DATA(message) = |Arret du programme.|.
      MESSAGE message TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD display_message.
    "" Display message

    MESSAGE message TYPE 'I' DISPLAY LIKE 'S'.

  ENDMETHOD.

ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK params WITH FRAME TITLE title.

  PARAMETERS: iv_mvdat    TYPE datum DEFAULT sy-datum,
              iv_expid(8) TYPE c,
              iv_test     TYPE abap_bool AS CHECKBOX DEFAULT abap_true,
              iv_alv      TYPE abap_bool AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN END OF BLOCK params.

INITIALIZATION.
  title = 'Parametres d''export'.


START-OF-SELECTION.

  DATA(movement_posting_date) = iv_mvdat.
  DATA(export_id) = |{ iv_expid }|.
  DATA(test_mode) = iv_test.
  DATA(display_alv) = iv_alv.

  DATA(material_exporter) = NEW lcl_material_exporter(
    movement_posting_date = movement_posting_date
    export_id             = export_id
    test_mode             = iv_test
    display_alv           = display_alv
  ).

  material_exporter->export( ).