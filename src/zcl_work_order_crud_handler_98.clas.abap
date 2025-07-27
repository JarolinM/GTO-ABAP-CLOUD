
" Descripción  : Clase para manejar operaciones CRUD de órdenes de trabajo
" Autor        : Jarolin Matos Martínez
" Fecha        : 11/07/2025
" Proyecto     : Gestión de órdenes de trabajo
"----------------------------------------------------------------------
CLASS zcl_work_order_crud_handler_98 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA rv_result TYPE string.
    METHODS:

      " Crear una nueva orden de trabajo
      create_work_order
        IMPORTING iv_customer_id   TYPE zde_customer_id_9598
                  iv_technician_id TYPE zde_technician_id_9598
                  iv_priority      TYPE zde_priority_9598
        RETURNING VALUE(rv_result) TYPE string,

      " Leer los datos de una orden de trabajo por ID
      read_work_order
        IMPORTING iv_work_order_id     TYPE zde_order_id_9598
        RETURNING VALUE(rs_work_order) TYPE zwork_order_5598,

      " Actualizar el estado de una orden de trabajo
      update_work_order
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
                  iv_status        TYPE zde_status_9598
        RETURNING VALUE(rv_result) TYPE string,
      " Eliminar una orden de trabajo (lógica o física)
      delete_work_order
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
                  iv_status        TYPE zde_status_9598
        RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
   " Referencia al validador de órdenes
    DATA: mo_validator TYPE REF TO zcl_work_order_validator_9598.

    METHODS:
    " Generar nuevo ID para orden de trabajo
      generate_order_id
        RETURNING VALUE(rv_id) TYPE zde_order_id_9598,
      " Insertar nueva orden en la base de datos
      insert_into_db
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
                  iv_customer_id   TYPE zde_customer_id_9598
                  iv_technician_id TYPE zde_technician_id_9598
                  iv_priority      TYPE zde_priority_9598,
       " Consultar una orden por ID desde la base de datos
      select_from_db
        IMPORTING iv_work_order_id     TYPE zde_order_id_9598
        RETURNING VALUE(rs_work_order) TYPE zwork_order_5598,

        " Actualizar el estado de una orden en BD
      update_db_record
        IMPORTING iv_work_order_id TYPE zde_order_id_9598
                  iv_status        TYPE zde_status_9598,

        " Eliminar orden de trabajo en BD
      delete_db_record
        IMPORTING iv_work_order_id TYPE zde_order_id_9598.
ENDCLASS.


CLASS zcl_work_order_crud_handler_98 IMPLEMENTATION.

  METHOD create_work_order.
    CREATE OBJECT mo_validator.
    " Validar entrada
    DATA(lv_valid) = mo_validator->validate_create_order(
                      iv_customer_id   = iv_customer_id
                      iv_technician_id = iv_technician_id
                      iv_priority      = iv_priority ).

    IF lv_valid = abap_true.
    " Generar nuevo ID y guardar orden
      DATA(lv_new_id) = generate_order_id( ).
      insert_into_db(
        iv_work_order_id = lv_new_id
        iv_customer_id   = iv_customer_id
        iv_technician_id = iv_technician_id
        iv_priority      = iv_priority ).
      rv_result = |Orden { lv_new_id } creada con exito|.
    ELSE.
      rv_result = 'Error de validacion: no se puede crear la orden'.
    ENDIF.
  ENDMETHOD.

  METHOD read_work_order.
   " Leer los datos de una orden de trabajo
    rs_work_order = select_from_db( iv_work_order_id ).
  ENDMETHOD.

  METHOD update_work_order.
    CREATE OBJECT mo_validator.

    DATA(lv_valid) = mo_validator->validate_update_order(
                      iv_work_order_id = iv_work_order_id
                      iv_status        = iv_status ).

    IF lv_valid = abap_true.
      update_db_record(
        iv_work_order_id = iv_work_order_id
        iv_status        = iv_status ).
      rv_result = 'Orden actualizada con exito'.
    ELSE.
        rv_result = 'Error de validacion: no se pudo actualizar la orden'.
    ENDIF.
  ENDMETHOD.

  METHOD delete_work_order.
  " Eliminar una orden de trabajo luego de validación
    CREATE OBJECT mo_validator.

    DATA(lv_valid) = mo_validator->validate_delete_order(
                       iv_work_order_id = iv_work_order_id
                       iv_status        = iv_status ).

    IF lv_valid = abap_true.
      delete_db_record( iv_work_order_id ).
      rv_result = 'Orden eliminada con exito'.
    ELSE.
      rv_result = 'Error de validacion: no se puede eliminar la orden'.
    ENDIF.
  ENDMETHOD.

  METHOD generate_order_id.
   " Generar nuevo ID basado en el mayor ID actual en la tabla
    DATA lv_max TYPE zde_order_id_9598.

    SELECT SINGLE MAX( work_order_id )
      FROM zwork_order_5598
      INTO @lv_max.

    rv_id = lv_max + 1.
  ENDMETHOD.


  METHOD insert_into_db.
   " Insertar orden en tabla ZWORK_ORDER_5598
    DATA(ls_order) = VALUE zwork_order_5598(
                       client        = sy-mandt
                       work_order_id = iv_work_order_id
                       customer_id   = iv_customer_id
                       technician_id = iv_technician_id
                       priority      = iv_priority
                       status        = 'CO'
                       creation_date = sy-datum
                       descripcion   = 'Creado automáticamente' ).

    INSERT zwork_order_5598 FROM @ls_order.

    IF sy-subrc <> 0.
      rv_result = 'Error al insertar la orden en la base de datos'.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD select_from_db.
    "Buscar orden de trabajo por ID
    SELECT SINGLE * FROM zwork_order_5598
      WHERE work_order_id = @iv_work_order_id
      INTO @rs_work_order.

    IF sy-subrc <> 0.
      CLEAR rs_work_order.
    ENDIF.
  ENDMETHOD.

  METHOD update_db_record.
    UPDATE zwork_order_5598
      SET status = @iv_status
      WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc <> 0.
      rv_result = 'Error al actualizar la orden TYPE E'.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD delete_db_record.
    DELETE FROM zwork_order_5598
      WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc <> 0.
      rv_result = 'Error al eliminar la orden TYPE E'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

