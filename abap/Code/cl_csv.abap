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