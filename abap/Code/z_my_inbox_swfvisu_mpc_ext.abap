"! Here is a snippest for MPC_EXT extension of standard services to use in My Inbox with SWFVISU and CDS
"!
"! Step to create extension.
"! 1. Search your service in segw (for example C_PurchaseOrderFs)
"! 2. Copy the project (ex: ZC_EXT_PurchaseOrderFs)
"! 3. Activate and generate the project
"! 4. Copy the content of MPC_EXT and DPC_EXT to the new service
"! 5. Update MPC_EXT to extend the extension and or update annotation
"!
"! Usefull links:
"! - MPC_EXT Class: https://answers.sap.com/questions/13395831/how-to-create-in-the-mpc-ext-class.html
"! - Chear sheet CDS: https://www.brandeis.de/en/blog/cheat-sheet-cds-abap
"! - Dynamic annotation: https://blogs.sap.com/2020/07/23/dynamic-annotation-values-in-abap-cds-view/comment-page-1/

  
"! MPC_EXT of C_ContractFs with field added and with dynamic UI.Hidden


class ZCL_ZC_EXT_CONTRACT_FS_MPC_EXT definition
  public
  inheriting from ZCL_ZC_EXT_CONTRACT_FS_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZC_EXT_CONTRACT_FS_MPC_EXT IMPLEMENTATION.


  method DEFINE.

  super->define( ).

  "Here starts the Annotation for Object Explorer App

  DATA lo_ann_target       TYPE REF TO /iwbep/if_mgw_vocan_ann_target.
  DATA lo_record1          TYPE REF TO /iwbep/if_mgw_vocan_record.
  DATA lo_record2          TYPE REF TO /iwbep/if_mgw_vocan_record.
  DATA lo_record3          TYPE REF TO /iwbep/if_mgw_vocan_record.
  DATA lo_record4          TYPE REF TO /iwbep/if_mgw_vocan_record.
  DATA lo_record5          TYPE REF TO /iwbep/if_mgw_vocan_record.
  DATA lo_property_value1  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_property_value2  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_property_value3  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_property_value4  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_property_value5  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_property_value6  TYPE REF TO /iwbep/if_mgw_vocan_property.
  DATA lo_collection1      TYPE REF TO /iwbep/if_mgw_vocan_collection.
  DATA lo_collection3      TYPE REF TO /iwbep/if_mgw_vocan_collection.
  DATA lo_collection4      TYPE REF TO /iwbep/if_mgw_vocan_collection.
  DATA lo_function         TYPE REF TO /iwbep/if_mgw_vocan_function.
  DATA lo_labeled_element  TYPE REF TO /iwbep/if_mgw_vocan_label_elem.
  DATA lo_annotation       TYPE REF TO /iwbep/if_mgw_vocan_annotation.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = 'vocabularies.UI.v1'
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'com.sap.vocabularies.UI.v1'
  iv_alias = 'UI' )   ##NO_TEXT.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = 'vocabularies.Communication.v1'
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'com.sap.vocabularies.Communication.v1'
  iv_alias = 'vCard' )   ##NO_TEXT.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = 'Measures.V1'
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'Org.OData.Measures.V1'
  iv_alias = 'CQP' )   ##NO_TEXT.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = 'Core.V1'
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'Org.OData.Core.V1'
  iv_alias = 'Core' )   ##NO_TEXT.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = 'vocabularies.Common.v1'
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'com.sap.vocabularies.Common.v1'
  iv_alias = 'Common' )   ##NO_TEXT.

  vocab_anno_model->create_vocabulary_reference(
  iv_vocab_id = ''
  iv_vocab_version = '0001'
  )->create_include(
  iv_namespace = 'C_CONTRACT_FS_SRV'
  iv_alias = 'C_CONTRACT_FS_SRV' )   ##NO_TEXT.


  DATA(lv_obje_service_name) = CAST /iwbep/if_mgw_odata_re_model( model )->get_schema_namespace( ).

  IF lv_obje_service_name IS INITIAL.
    lv_obje_service_name = 'ZC_EXT_CONTRACT_FS_SRV'.
  ENDIF.


  lo_annotation = lo_ann_target->create_annotation( iv_term = 'UI.Facets' )   ##NO_TEXT.

  lo_collection1 = lo_annotation->create_collection( ).
  lo_record2 = lo_collection1->create_record( iv_record_type = 'UI.CollectionFacet')   ##NO_TEXT.
  lo_record2->create_annotation( iv_term = 'UI.IsSummary' )   ##NO_TEXT.
  lo_property_value3 = lo_record2->create_property( 'Label' )    ##NO_TEXT.
  lo_property_value3->create_simple_value( )->set_string_from_otr( '005056AC4BF11ED58CC0769EF8561BF9' ).
  lo_property_value3 = lo_record2->create_property( 'ID' )    ##NO_TEXT.
  lo_property_value3->create_simple_value( )->set_string( 'GeneralInformation' )   ##NO_TEXT.
  lo_property_value3 = lo_record2->create_property( 'Facets' )    ##NO_TEXT.
  lo_collection4 = lo_property_value3->create_collection( ).
  lo_record5 = lo_collection4->create_record( iv_record_type = 'UI.ReferenceFacet')   ##NO_TEXT.
  lo_property_value6 = lo_record5->create_property( 'Label' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_string_from_otr( '40F2E93067D51EE5849EEA72742FC41D' ).
  lo_property_value6 = lo_record5->create_property( 'Target' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_annotation_path( '@UI.Identification' )   ##NO_TEXT.
  lo_record5 = lo_collection4->create_record( iv_record_type = 'UI.ReferenceFacet')   ##NO_TEXT.
  lo_property_value6 = lo_record5->create_property( 'Label' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_string_from_otr( '40F2E93067D51EE5849F988EA19FC670' ).
  lo_property_value6 = lo_record5->create_property( 'Target' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_annotation_path( '@UI.FieldGroup#Detail2' )   ##NO_TEXT.
  lo_record5 = lo_collection4->create_record( iv_record_type = 'UI.ReferenceFacet')   ##NO_TEXT.
  lo_property_value6 = lo_record5->create_property( 'Label' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_string_from_otr( '40F2E93067D51EE5849F03BA94FC846B' ).
  lo_property_value6 = lo_record5->create_property( 'Target' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_annotation_path( '@UI.FieldGroup#Detail3' )   ##NO_TEXT.
  cl_contactcard_util=>define_contactcard_facet( EXPORTING iv_collection_of_facets = lo_collection1 iv_navigation_name = 'to_ContactCard' iv_facet_otrtext = '40F2E93067D51EE5849F0A45581FC473' ).
  lo_record2 = lo_collection1->create_record( iv_record_type = 'UI.ReferenceFacet')   ##NO_TEXT.
  lo_property_value3 = lo_record2->create_property( 'Label' )    ##NO_TEXT.
  lo_property_value3->create_simple_value( )->set_string_from_otr( '40F2E93067D51EE5849F27F534B504F5' ).
  lo_property_value3 = lo_record2->create_property( 'Target' )    ##NO_TEXT.
  lo_property_value3->create_simple_value( )->set_annotation_path( 'to_PurchaseContractItem/@UI.LineItem' )   ##NO_TEXT.

*** EXTENSION

** NEW SUB
  data(record) = lo_collection4->create_record( iv_record_type = 'UI.ReferenceFacet')   ##NO_TEXT.
  lo_property_value6 = lo_record5->create_property( 'Label' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_string( 'EXTENSION' ).
  lo_property_value6 = lo_record5->create_property( 'Target' )   ##NO_TEXT.
  lo_property_value6->create_simple_value( )->set_annotation_path( '@UI.FieldGroup#DetailEXT' )   ##NO_TEXT.

** DYNAMIC UI.Hidden
  lo_ann_target = vocab_anno_model->create_annotations_target( iv_target = lv_obje_service_name && '.' && 'C_ContractFsType/document_type' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'UI.Hidden' ).
  lo_annotation->create_simple_value( )->set_path( 'visible' ).

*** CDS EXAMPLE
**   @AbapCatalog.sqlViewAppendName: 'ZC_EXT_CONTRACT'
** extend view C_ContractFs with ZC_EXT_CONTRACTFS

**   association [1..1] to ekko on ekko.ebeln = $projection.purchasecontract

** {
**   @UI.fieldGroup: [{
**        qualifier: 'DetailEXT'
**    }]
**   @UI.lineItem: [{
**        position: 20,
**        importance: #HIGH }]
**   ekko.bstyp as document_type,
**   // Can be a virtual or a case but must be casted as boolean
**   cast('X' as boolean)          as visible

** }



  endmethod.
ENDCLASS.
