INTERFACE zif_foreign_key_checks
  PUBLIC .

  METHODS execute
    IMPORTING
      it_data_to_be_validated TYPE ANY TABLE.

ENDINTERFACE.
