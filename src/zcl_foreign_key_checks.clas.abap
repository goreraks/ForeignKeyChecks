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

    CLASS-DATA mo_foreign_key_checks TYPE REF TO zcl_foreign_key_checks.

ENDCLASS.



CLASS zcl_foreign_key_checks IMPLEMENTATION.

  METHOD zif_foreign_key_checks~execute.

    DATA: lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lo_strucdescr TYPE REF TO cl_abap_structdescr.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( it_data_to_be_validated[] ).

    lo_strucdescr ?= lo_tabledescr->get_table_line_type( ).

    DATA(lv_table_name) = lo_strucdescr->get_relative_name( ).

  ENDMETHOD.

  METHOD get_instance.

    IF mo_foreign_key_checks IS NOT BOUND.

      mo_foreign_key_checks = NEW #( ).

    ENDIF.

    ro_foreign_key_checks ?= mo_foreign_key_checks.

  ENDMETHOD.

ENDCLASS.
