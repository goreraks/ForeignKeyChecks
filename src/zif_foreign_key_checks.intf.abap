INTERFACE zif_foreign_key_checks
  PUBLIC .

  METHODS execute
    IMPORTING
      is_data_to_be_validated TYPE any.

ENDINTERFACE.
