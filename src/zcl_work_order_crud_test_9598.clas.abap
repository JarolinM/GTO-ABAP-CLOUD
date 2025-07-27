CLASS zcl_work_order_crud_test_9598 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    DATA mo_handler TYPE REF TO zcl_work_order_crud_handler_98.
    DATA mt_logs     TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS:
      log_message IMPORTING iv_text TYPE string,
      test_create_work_order,
      test_read_work_order,
      test_update_work_order,
      test_delete_work_order.
ENDCLASS.


CLASS zcl_work_order_crud_test_9598 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Borrar datos de prueba previos
    DELETE FROM zcustomer_9598 WHERE customer_id = '00000001'.
    DELETE FROM ztechnician_9598 WHERE technician_id = 'TECH001'.

    DATA ls_customer TYPE zcustomer_9598.
    DATA ls_technician TYPE ztechnician_9598.

    " Insertar cliente
    CLEAR ls_customer.
    ls_customer-client      = sy-mandt.
    ls_customer-customer_id = '00000002'.
    ls_customer-name        = 'Cliente Test2'.


    TRY.
        INSERT zcustomer_9598 FROM @ls_customer.
        log_message( 'Cliente insertado correctamente.' ).
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql1).
        log_message( |Error al insertar cliente: { lx_sql1->get_text( ) }| ).
    ENDTRY.

    " Insertar técnico
    CLEAR ls_technician.
    ls_technician-client        = sy-mandt.
    ls_technician-technician_id = 'TECH002'.
    ls_technician-name          = 'Técnico Test2'.
    ls_technician-specialty     = 'General'.



    TRY.
        INSERT ztechnician_9598 FROM @ls_technician.
        log_message( 'Técnico insertado correctamente.' ).
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql2).
        log_message( |Error al insertar técnico: { lx_sql2->get_text( ) }| ).
    ENDTRY.


    CREATE OBJECT mo_handler.

    log_message( '--- INICIO PRUEBAS CRUD ---' ).

    test_create_work_order( ).
    test_read_work_order( ).
    test_update_work_order( ).
    test_delete_work_order( ).

    log_message( '--- FIN PRUEBAS CRUD ---' ).

    " Mostrar logs al final
    out->write( mt_logs ).
  ENDMETHOD.

  METHOD log_message.
    APPEND iv_text TO mt_logs.
  ENDMETHOD.


  METHOD test_create_work_order.
    DATA(lv_result) = mo_handler->create_work_order(
      iv_customer_id   = '00000002'
      iv_technician_id = 'TECH002'
      iv_priority      = 'B' ).

    log_message( |Test Create: { lv_result }| ).
  ENDMETHOD.

  METHOD test_read_work_order.
    DATA(ls_order) = mo_handler->read_work_order( iv_work_order_id = '00000002' ).

    log_message( |Test Read: ID={ ls_order-work_order_id }, Customer={ ls_order-customer_id }, Tech={ ls_order-technician_id }, Status={ ls_order-status }| ).
  ENDMETHOD.

  METHOD test_update_work_order.
    DATA(lv_result) = mo_handler->update_work_order(
      iv_work_order_id = '000000012'
      iv_status        = 'PE' ).

    log_message( |Test Update: { lv_result }| ).
  ENDMETHOD.

  METHOD test_delete_work_order.
    DATA(lv_result) = mo_handler->delete_work_order(
      iv_work_order_id = '0000000013'
      iv_status        = 'PE' ).

    log_message( |Test Delete: { lv_result }| ).
  ENDMETHOD.

ENDCLASS.

