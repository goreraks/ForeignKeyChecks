CLASS zcl_foreign_key_checks DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_foreign_key_checks.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_foreign_key_checks) TYPE REF TO zif_foreign_key_checks.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_foreign_key_info,
        check_table       TYPE tabname,
        check_field       TYPE abp_field_name,
        foreign_key_field TYPE abp_field_name,
      END OF ty_foreign_key_info.

    CLASS-DATA mo_foreign_key_checks TYPE REF TO zcl_foreign_key_checks.

    DATA mt_foreign_key_info TYPE STANDARD TABLE OF ty_foreign_key_info.

    METHODS get_foreign_key_info
      IMPORTING
        iv_table_name TYPE string.

ENDCLASS.



CLASS zcl_foreign_key_checks IMPLEMENTATION.

  METHOD zif_foreign_key_checks~execute.

    TYPES: BEGIN OF lty_check_table,
             check_table TYPE tabname,
             check_field TYPE abp_field_name,
           END OF lty_check_table.

    DATA: lt_check_table TYPE STANDARD TABLE OF lty_check_table.

    DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.

    DATA: lv_where_clause TYPE string.

    lo_strucdescr ?= cl_abap_structdescr=>describe_by_data( is_data_to_be_validated ).

    DATA(lv_table_name) = lo_strucdescr->get_relative_name( ).

    get_foreign_key_info( lv_table_name ).

    LOOP AT mt_foreign_key_info[] ASSIGNING FIELD-SYMBOL(<ls_foreign_key_info>)
                                  GROUP BY ( check_table = <ls_foreign_key_info>-check_table
                                             size        = GROUP SIZE
                                             index       = GROUP INDEX
                                           ) ASSIGNING FIELD-SYMBOL(<ls_foreign_key_info_group>).

      LOOP AT GROUP <ls_foreign_key_info_group> ASSIGNING FIELD-SYMBOL(<ls_group_memebers>).

        ASSIGN COMPONENT <ls_group_memebers>-foreign_key_field OF STRUCTURE is_data_to_be_validated
        TO FIELD-SYMBOL(<lv_value>).

        DATA(lv_value) = cl_abap_dyn_prg=>quote( CONV string( <lv_value> ) ).

        DATA(lv_check_field) = cl_abap_dyn_prg=>check_column_name( <ls_group_memebers>-check_field ).

        DATA(lo_database_table) = xco_cp_abap_dictionary=>database_table( CONV #( <ls_group_memebers>-check_table ) ).

        DATA(lo_table_field) = lo_database_table->field( <ls_group_memebers>-check_field ).

        DATA(lo_field_type) = lo_table_field->content( )->get_type( ).

        IF lo_field_type->is_built_in_type( ) = abap_true.

          IF lo_field_type->get_built_in_type( )->type = 'CLNT'.
            CONTINUE.
          ENDIF.

        ELSE.

          DATA(lo_data_type) = lo_field_type->get_data_element( ).

          IF lo_data_type->if_xco_ad_object~name = 'MANDT'.
            CONTINUE.
          ENDIF.

        ENDIF.

        IF lv_where_clause IS INITIAL.

          CONCATENATE lv_check_field
                      '='
                      lv_value
          INTO lv_where_clause
          SEPARATED BY space.

          CONTINUE.

        ENDIF.

        CONCATENATE lv_where_clause
                    'AND'
                    lv_check_field
                    '='
                    lv_value
        INTO lv_where_clause
        SEPARATED BY space.

      ENDLOOP.

      SELECT SINGLE @abap_true
      FROM (<ls_foreign_key_info_group>-check_table)
      WHERE (lv_where_clause)
      INTO @DATA(lv_result).
      IF sy-subrc NE 0.

      ENDIF.

      CLEAR lv_where_clause.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance.

    IF mo_foreign_key_checks IS NOT BOUND.

      mo_foreign_key_checks = NEW #( ).

    ENDIF.

    ro_foreign_key_checks ?= mo_foreign_key_checks.

  ENDMETHOD.


  METHOD get_foreign_key_info.

    DATA(lt_table_fields) = xco_cp_abap_dictionary=>database_table( CONV #( iv_table_name ) )->fields->all->get( ).

    LOOP AT lt_table_fields[] ASSIGNING FIELD-SYMBOL(<ls_table_field>).

      CHECK <ls_table_field>->foreign_key->exists( ) = abap_true.

      DATA(lo_content) = <ls_table_field>->foreign_key->content( ).

      DATA(lv_check_table) = lo_content->get_check_table( )->name.

      DATA(lt_field_assignments) = lo_content->get_field_assignments( ).

      LOOP AT lt_field_assignments[] ASSIGNING FIELD-SYMBOL(<ls_field_assignment>).

        APPEND VALUE #( check_table       = lv_check_table
                        check_field       = <ls_field_assignment>-check_field
                        foreign_key_field = <ls_field_assignment>-foreign_key_field )
        TO mt_foreign_key_info[].

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
