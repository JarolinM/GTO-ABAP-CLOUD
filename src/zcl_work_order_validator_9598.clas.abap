CLASS zcl_work_order_validator_9598 DEFINITION
" Propósito: Validar las operaciones de creación, actualización y eliminación de órdenes de trabajo.
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Valida si se puede crear una orden de trabajo
    METHODS:
      validate_create_order
        IMPORTING
          iv_customer_id   TYPE zde_customer_id_9598
          iv_technician_id TYPE zde_technician_id_9598
          iv_priority      TYPE zde_priority_9598
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      " Valida si se puede actualizar una orden de trabajo existente
      validate_update_order
        IMPORTING
          iv_work_order_id TYPE zde_order_id_9598
          iv_status        TYPE zde_status_9598
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      " Valida si se puede eliminar una orden de trabajo
      validate_delete_order
        IMPORTING
          iv_work_order_id TYPE zde_order_id_9598
          iv_status        TYPE zde_status_9598
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      " Valida si el estado y la prioridad son correctos
      validate_status_and_priority
        IMPORTING
          iv_status   TYPE zde_status_9598
          iv_priority TYPE zde_priority_9598
        RETURNING VALUE(rv_valid) TYPE abap_bool.

  PRIVATE SECTION.

    " Verifica si el cliente existe
    METHODS:
      check_customer_exists
        IMPORTING iv_customer_id TYPE zde_customer_id_9598
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      " Verifica si el técnico existe
      check_technician_exists
        IMPORTING iv_technician_id TYPE zde_technician_id_9598
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      " Verifica si la orden de trabajo existe
      check_order_exists
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      " Verifica si la orden de trabajo tiene historial asociado
      check_order_history
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.


CLASS zcl_work_order_validator_9598 IMPLEMENTATION.

  METHOD validate_create_order.
    " Verifica existencia de cliente y técnico, y si la prioridad es válida
    DATA(lv_customer_exists)   = check_customer_exists( iv_customer_id ).
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    DATA(lv_valid_priority)    = validate_status_and_priority( iv_status = 'PE' iv_priority = iv_priority ).

    " Solo es válida si todo existe y la prioridad es correcta
    IF lv_customer_exists = abap_true AND
       lv_technician_exists = abap_true AND
       lv_valid_priority = abap_true.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD validate_update_order.
    " Verifica si la orden existe y si el nuevo estado es 'PE' (Pendiente)
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).

    IF lv_order_exists = abap_true AND iv_status = 'PE'.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD validate_delete_order.
    " Verifica si la orden no tiene historial y si está en estado 'PE'
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).

    IF iv_status = 'PE' AND lv_has_history = abap_false.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD validate_status_and_priority.
    " Define listas válidas de estados y prioridades
    TYPES: ty_status   TYPE c LENGTH 2,
           ty_priority TYPE c LENGTH 1.

    DATA: lt_status   TYPE STANDARD TABLE OF ty_status WITH EMPTY KEY,
          lt_priority TYPE STANDARD TABLE OF ty_priority WITH EMPTY KEY.

    lt_status   = VALUE #( ( 'PE' ) ( 'CO' ) ). " PE: Pendiente, CO: Completada
    lt_priority = VALUE #( ( 'A' ) ( 'B' ) ).   " A: Alta, B: Baja

    " Verifica si el estado y la prioridad están en las listas válidas
    IF line_exists( lt_status[ table_line = iv_status ] ) AND
       line_exists( lt_priority[ table_line = iv_priority ] ).
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_customer_exists.
    " Consulta si existe el cliente
    SELECT SINGLE customer_id FROM zcustomer_9598
      WHERE customer_id = @iv_customer_id
      INTO @DATA(dummy).
    rv_exists = xsdbool( sy-subrc = 0 ). " Verdadero si encontró resultado
  ENDMETHOD.

  METHOD check_technician_exists.
    " Consulta si existe el técnico
    SELECT SINGLE technician_id FROM ztechnician_9598
      WHERE technician_id = @iv_technician_id
      INTO @DATA(dummy).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_order_exists.
    " Consulta si existe la orden de trabajo
    SELECT SINGLE work_order_id FROM zwork_order_5598
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(dummy).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_order_history.
    " Consulta si la orden de trabajo tiene historial
    SELECT SINGLE work_order_id FROM zworkorderhist
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(dummy).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.

