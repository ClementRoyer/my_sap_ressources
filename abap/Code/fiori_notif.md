# Dev of Fiori Notifications

> Resources :
> - https://community.sap.com/t5/technology-blogs-by-members/fiori-launchpad-notification-configuration/ba-p/13556686
> - /IWNGW/R_BEP_DEMO_CREATE_NOTIF
> - /IWNGW/CL_DEMO_NOTIF_PROVIDER
> - ZCL_PS_INVESTMENT_HUB_NOTIF

## Result
![image](https://github.com/user-attachments/assets/ab4f4895-9919-46cf-b0c7-00000705705d)



## 1. Create a provider

- Create class `ZCL*NOTIF*` (using interface `/iwngw/if_notif_provider`)
- SPRO, "Register Notification Providers"
- SPRO, "Manage Notification Providers" (Activate)

## 2. Implement Class

``` ABAP
CLASS zcl_ps_investment_hub_notif DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /iwngw/if_notif_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ps_investment_hub_notif IMPLEMENTATION.


  METHOD /iwngw/if_notif_provider~get_notification_parameters.
    DATA base_param TYPE /iwngw/if_notif_provider=>ty_s_notification_parameter.

    DATA(lv_lang) = sy-langu.

    SET LANGUAGE lv_lang.

    APPEND VALUE #( BASE base_param
                        name = 'fix_param'
                        value = '"fix value"'
                        type = /iwngw/if_notif_provider=>gcs_parameter_types-type_string
                        is_sensitive = abap_true ) TO et_parameter.

  ENDMETHOD.


  METHOD /iwngw/if_notif_provider~get_notification_type.

    es_notification_type = VALUE #( version        = iv_type_version
                                    type_key       = iv_type_key
                                    is_groupable   = abap_false ).

    et_notification_action = VALUE #( ( action_key = 'Accept' nature =  /iwngw/if_notif_provider=>gcs_action_natures-positive )
                                      ( action_key = 'Reject' nature =  /iwngw/if_notif_provider=>gcs_action_natures-negative ) ).

  ENDMETHOD.


  METHOD /iwngw/if_notif_provider~get_notification_type_text.


    CASE iv_type_key.
      WHEN 'DEMO'.
        CASE iv_type_version.
          WHEN OTHERS.
            "" Demo Version * ""

            et_action_text = VALUE #(
               (
                   action_key           = 'Accept'
                   display_text         = 'OK'
                   display_text_grouped = 'OK ALL'
               ) (
                   action_key           = 'Reject'
                   display_text         = 'KO'
                   display_text_grouped = 'KO ALL'
               )
           ).


            es_type_text = VALUE #(
                  template_public = 'This is an example | public | {param1}'
                  template_sensitive = 'This is an example | sensitive text | {param1}'
                  template_grouped = 'This is an example | grouped | {param1}'
                  description = 'This is an example | description | {param1}'
                  subtitle = 'This is an example | subtitle | {param1}'
            ).


        ENDCASE.
    ENDCASE.


  ENDMETHOD.


  METHOD /iwngw/if_notif_provider~handle_action.
    es_result = VALUE #( success = abap_true action_msg_txt = 'Bien ouéj' delete_on_return = abap_true ).
  ENDMETHOD.


  METHOD /iwngw/if_notif_provider~handle_bulk_action.

    et_notif_result = VALUE #( FOR notif IN it_bulk_notif
                                ( id = notif-id
                                  type_key = notif-type_key
                                  type_version = notif-type_version
                                  success = abap_true
                                  delete_on_return = abap_true ) ).
  ENDMETHOD.
ENDCLASS.
```

## Extra. Program to test your class

> If your changes aren't applied during the developpment, try using `/iwngw/cl_notification_api=>clear_cache( provider_id ).`


``` ABAP
REPORT yclroy_test.

DATA: notif TYPE /iwngw/if_notif_provider=>ty_t_notification.

DATA(provider_id) = CONV /iwngw/notif_provider_id( 'ZINVESTMENT_HUB')."/IWNGW/DEMO' ).

DATA(system_uuid) = cl_uuid_factory=>create_system_uuid( ).
DATA(notif_id) = system_uuid->create_uuid_x16( ).


notif = VALUE #(
    (
        id                       = notif_id
        type_key                 = 'DEMO'
        type_version             = '1'
        priority                 = /iwngw/if_notif_provider=>gcs_priorities-low
        actor_id                 = sy-uname
        actor_type               = ''
        actor_display_text       = sy-uname
        actor_image_url          = 'https://scn.sap.com/people/guest/avatar/ROYECLE43.png'
        parameters               = VALUE #(
                                    ( language = 'E'
                                      parameters = VALUE #( ( name = 'param1'
                                                              value = '"Value Param 1"'
                                                              type = /iwngw/if_notif_provider=>gcs_parameter_types-type_string
                                                              is_sensitive = abap_false ) ) ) )
        recipients               = VALUE #( ( id = sy-uname ) )
        navigation_target_object = 'Action'
        navigation_target_action = 'toappstatesample'
        navigation_parameters    = VALUE #( ( name = 'PurchaseOrderId' value = '236400' )
                                            ( name = 'PurchaseOrderVendor' value = 'PARTNER_137' ) )
    )
).

TRY.
    /iwngw/cl_notification_api=>clear_cache( provider_id ).
    /iwngw/cl_notification_api=>create_notifications( iv_provider_id = provider_id
                                                      it_notification = notif ).

  CATCH /iwngw/cx_notification_api INTO DATA(lrx_api).
    " lt_invalid_recip = lrx_api->get_invalid_recip_list( ).
    lrx_api->get_invalid_recip_list(
      IMPORTING
        et_invalid_recip = DATA(invalid_recipients)
    ).
    IF invalid_recipients IS NOT INITIAL.
      DATA(invalid_recipient) = VALUE #( invalid_recipients[ 1 ] ).
      DATA(type_key) = invalid_recipient-type_key.
      DATA(nb_invalid_recipients) = lines( invalid_recipients ).

      WRITE: / lrx_api->if_message~get_text( ).
      RETURN.
    ENDIF.
ENDTRY.


COMMIT WORK.

```
