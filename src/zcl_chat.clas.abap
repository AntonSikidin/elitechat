class ZCL_CHAT definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  data MR_SERVER type ref to IF_HTTP_SERVER .
  data MS_DATA type ZCHAT_ST_REQUEST .
  data MS_RESPONCE type ZCHAT_ST_RESPONCE .

  methods POST .
  methods GET .
  methods PING .
  methods POST_MESSAGE .
  methods REGISTER .
  methods CHECK_ACCES
    returning
      value(RV_DATA) type XFELD .
  methods RESPOND .
  methods GET_DATA .
  methods SAVE_MESSAGE .
  methods GET_MESSAGE
    importing
      !IV_GUID type GUID
    returning
      value(RV_MESSAGE) type ZCHAT_STRING .
  methods GET_PUBLIC_KEY
    importing
      !IV_LOGIN type TEXT20
    returning
      value(RV_PUBLIC) type ZCHAT_STRING .
  methods GET_LAST
    importing
      !IV_TIME type TZNTSTMPL
    returning
      value(RV_DATA) type ZCHAT_STRING .
  methods FORMAT_TIME
    importing
      !IV_TIME type TZNTSTMPL
    returning
      value(RV_DATA) type ZCHAT_STRING .
  methods TSTMP2SECS
    importing
      !TSTMP type P
    returning
      value(SECS) type TZNTSTMPL .
  methods HISTORY .
  methods HISTORY2 .
  methods LIKED .
  methods SAVE_AVATAR .
  methods GET_IMAGE
    importing
      !ID type STRING .
  methods GET_DEFAULT_IMAGE .
protected section.
private section.
ENDCLASS.



CLASS zcl_chat IMPLEMENTATION.


  METHOD check_acces.

    SELECT SINGLE clnt INTO sy-mandt
      FROM zchat_login
      WHERE login = ms_data-login.

    CHECK sy-subrc = 0.

    DATA
          : lt_key TYPE TABLE OF zchat_publik_key
          .

    SELECT  login
            npp
            value
      INTO CORRESPONDING FIELDS OF TABLE lt_key
      FROM zchat_publik_key
      WHERE login = ms_data-login
      ORDER BY npp.

    CHECK sy-subrc = 0.

    FIELD-SYMBOLS <fs_key> TYPE zchat_publik_key.

    DATA lv_key TYPE string.


    LOOP AT lt_key ASSIGNING <fs_key>.
      lv_key = |{ lv_key }{ <fs_key>-value }|.
    ENDLOOP.

    CONDENSE lv_key .

    IF lv_key = ms_data-publickey.
      rv_data = 'X'.
    ENDIF.


  ENDMETHOD.


  method FORMAT_TIME.
     RV_DATA  = IV_TIME.
     rv_data = |{ rv_data(4) }-{ rv_data+4(2) }-{ rv_data+6(2) } { rv_data+8(2) }:{ rv_data+10(2) }:{ rv_data+12(2) }|.
  endmethod.


  method GET.

     DATA
          : lv_templ_xstr TYPE xstring
          , lt_mime TYPE TABLE OF w3mime
          .

    DATA(ls_key) = VALUE wwwdatatab(
    relid = 'MI'
    objid = 'ZCHAT' ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ls_key
      TABLES
        mime   = lt_mime
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        lv_templ_xstr = cl_bcs_convert=>xtab_to_xstring( lt_mime ).
      CATCH cx_bcs.
        RETURN.
    ENDTRY.

     mr_server->response->SET_DATA( data = lv_templ_xstr ).
  endmethod.


  METHOD get_data.
    DATA
          : lt_login TYPE TABLE OF zchat_login
          , lv_tstmp_tmp TYPE tzntstmpl
          , lv_tstmp_start TYPE tzntstmpl
          , lv_tstmp1 TYPE tzntstmpl
          , lv_last_visit TYPE tzntstmpl
          , lv_tstmp2 TYPE tzntstmpl
          .

    FIELD-SYMBOLS
                   : <fs_login> TYPE zchat_login
                   , <fs_user>  TYPE zchat_st_users
                   .

    SELECT * INTO TABLE lt_login FROM zchat_login.

    SELECT SINGLE id INTO ms_responce-id
      FROM zchat_login
      WHERE login = ms_data-login.

    GET TIME STAMP FIELD lv_tstmp_tmp .

    DATA
      : lv_xz_stmmp TYPE tzntstmpl
      .

    lv_xz_stmmp = lv_tstmp_tmp.

    CALL METHOD cl_abap_tstmp=>add
      EXPORTING
        tstmp   = lv_xz_stmmp
        secs    = 10800
      RECEIVING
        r_tstmp = lv_tstmp_tmp.




*TRY.
    CALL METHOD cl_abap_tstmp=>normalize
      EXPORTING
        tstmp_in  = lv_tstmp_tmp
      RECEIVING
        tstmp_out = lv_tstmp1.
* CATCH cx_parameter_invalid_range .
* CATCH cx_parameter_invalid_type .
*ENDTRY.
    lv_tstmp_start = lv_tstmp1.
*TRY.
    CALL METHOD cl_abap_tstmp=>subtractsecs
      EXPORTING
        tstmp   = lv_tstmp1
        secs    = 300
      RECEIVING
        r_tstmp = lv_tstmp2.
* CATCH cx_parameter_invalid_range .
* CATCH cx_parameter_invalid_type .
*ENDTRY.


    LOOP AT lt_login ASSIGNING <fs_login>.
      APPEND INITIAL LINE TO ms_responce-users ASSIGNING <fs_user>.

      <fs_user>-login = <fs_login>-login.
      <fs_user>-id = <fs_login>-id.
      <fs_user>-public = get_public_key( <fs_login>-login ).

      SELECT MAX( time ) INTO <fs_user>-time
        FROM zchat_convers
        WHERE login = ms_data-login
        AND receiver = <fs_login>-login.

      IF <fs_user>-time IS INITIAL.
        <fs_user>-time = 1.
      ENDIF.


      <fs_user>-lastvisit = get_last( <fs_login>-last_visit ).

      IF <fs_login>-last_visit > lv_tstmp2 .
        <fs_user>-active = 'X'.
      ENDIF.

    ENDLOOP.

    SORT ms_responce-users BY time DESCENDING.



    SELECT SINGLE last_visit INTO
      lv_tstmp1
      FROM zchat_login
      WHERE login = ms_data-login.

    lv_last_visit = lv_tstmp1.


    DATA
          : lt_convers TYPE TABLE OF zchat_convers
          .

    FIELD-SYMBOLS
                   : <fs_convers> TYPE zchat_convers
                   , <fs_message> TYPE zchat_st_messages
                   .

    IF ms_data-all IS NOT INITIAL.

      DATA
            : lv_tmp(8) TYPE n
            , lv_yestarday TYPE datum
            .

      lv_tmp = sy-datum.

      lv_yestarday = sy-datum - 1 .

      lv_tstmp1 = lv_tmp - 1.
      lv_tstmp1 = lv_tstmp1 * 1000000.


      SELECT * INTO TABLE lt_convers
        FROM zchat_convers
        WHERE login = ms_data-login
        AND date1 >= lv_yestarday
        ORDER BY time
  .

    ELSE.
      SELECT * INTO TABLE lt_convers
        FROM zchat_convers
        WHERE login = ms_data-login
        AND time > lv_tstmp1
        ORDER BY time
.

    ENDIF.


    LOOP AT lt_convers ASSIGNING <fs_convers>.
      APPEND INITIAL LINE TO ms_responce-messages ASSIGNING <fs_message>.

      <fs_message>-login = <fs_convers>-receiver.

      SELECT SINGLE id INTO <fs_message>-id
        FROM zchat_login
        WHERE login = <fs_message>-login.


      <fs_message>-direction = <fs_convers>-direction.
      <fs_message>-messageid = <fs_convers>-message.

      IF <fs_convers>-likes IS INITIAL.
        <fs_message>-likes = '0'.
      ELSE.
        <fs_message>-likes = '1'.
      ENDIF.

      <fs_message>-time = format_time( <fs_convers>-time ).
      <fs_message>-message = get_message( <fs_convers>-message ).

      IF ms_data-all IS INITIAL.
        <fs_message>-notify = 'X'.
      ELSE.
        IF <fs_convers>-time > lv_last_visit .
          <fs_message>-notify = 'X'.
        ENDIF.

      ENDIF.


    ENDLOOP.

    DATA
          : lt_zchat_tmp_like TYPE TABLE OF zchat_tmp_like
          .

    FIELD-SYMBOLS
                   :<fs_like> TYPE zchat_tmp_like
                   , <fs_any> TYPE string
                   .

    SELECT * INTO TABLE lt_zchat_tmp_like FROM
      zchat_tmp_like WHERE login = ms_data-login.

    DELETE zchat_tmp_like FROM TABLE lt_zchat_tmp_like.

    LOOP AT lt_zchat_tmp_like ASSIGNING <fs_like>.
      APPEND INITIAL LINE TO ms_responce-likes ASSIGNING <fs_any>.
      <fs_any> = <fs_like>-message.
    ENDLOOP.

    UPDATE zchat_login SET last_visit = lv_tstmp_start
    WHERE login = ms_data-login.


  ENDMETHOD.


  METHOD get_default_image.


   DATA
          : lv_templ_xstr TYPE xstring
          , lt_mime TYPE TABLE OF w3mime
          .

    DATA(ls_key) = VALUE wwwdatatab(
    relid = 'MI'
    objid = 'ZCHAT_DEFAULT_USER' ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ls_key
      TABLES
        mime   = lt_mime
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        lv_templ_xstr = cl_bcs_convert=>xtab_to_xstring( lt_mime ).
      CATCH cx_bcs.
        RETURN.
    ENDTRY.

     mr_server->response->SET_DATA( data = lv_templ_xstr ).

  ENDMETHOD.


  METHOD get_image.
    DATA
          : lt_avatar TYPE TABLE OF zchat_avatar
          , lt_solix TYPE  solix_tab
          , lv_xstring TYPE xstring
          .

    FIELD-SYMBOLS
                   : <fs_avatar> TYPE zchat_avatar
                   , <fs_solix> TYPE solix
                   .

    SELECT *
      INTO TABLE lt_avatar
      FROM zchat_avatar
      WHERE id = id
      ORDER BY npp.

    IF sy-subrc = 0.
      LOOP AT lt_avatar ASSIGNING <fs_avatar>  .
        APPEND INITIAL LINE TO lt_solix ASSIGNING <fs_solix>.
        <fs_solix>-line = <fs_avatar>-value.

      ENDLOOP.

      lv_xstring =  cl_bcs_convert=>solix_to_xstring( lt_solix ).

      mr_server->response->set_data( data = lv_xstring ).
    ELSE.

      get_default_image( ).

    ENDIF.


  ENDMETHOD.


  METHOD get_last.

    DATA
          : lv_tstmp_start 	TYPE tzntstmpl
          , lv_tstmp1   TYPE tzntstmpl
          , lv_sec1 TYPE tzntstmpl
          , lv_sec2 TYPE tzntstmpl

          .



    GET TIME STAMP FIELD lv_tstmp_start.

    DATA
      : lv_xz_stmmp TYPE tzntstmpl
      .

    lv_xz_stmmp = lv_tstmp_start.

    CALL METHOD cl_abap_tstmp=>add
      EXPORTING
        tstmp   = lv_xz_stmmp
        secs    = 10800
      RECEIVING
        r_tstmp = lv_tstmp_start.


*TRY.
    CALL METHOD cl_abap_tstmp=>normalize
      EXPORTING
        tstmp_in  = lv_tstmp_start
      RECEIVING
        tstmp_out = lv_tstmp1.

*    TRY.
    CALL METHOD tstmp2secs
      EXPORTING
        tstmp = lv_tstmp1
      RECEIVING
        secs  = lv_sec1.

    IF iv_time IS NOT INITIAL.
      CALL METHOD tstmp2secs
        EXPORTING
          tstmp = iv_time
        RECEIVING
          secs  = lv_sec2.
    ENDIF.

*     CATCH cx_parameter_invalid_range .
*     CATCH cx_parameter_invalid_type .
*    ENDTRY.




    lv_tstmp1 = lv_sec1 - lv_sec2.
*TRY.

    DATA
          : lv_date TYPE datum
          , lv_time TYPE  sy-uzeit
          , lv_tmp TYPE timestamp
          , lv_days TYPE i
          , lv_hrs TYPE i
          , lv_mins TYPE i
          .

    TRY.

        lv_days = lv_tstmp1 / ( 3600 * 24 ).
        lv_hrs =  lv_tstmp1 / ( 3600  ).
        lv_mins =  lv_tstmp1 / ( 60  ).

      CATCH cx_sy_arithmetic_overflow.

    ENDTRY.




    IF lv_days IS NOT INITIAL .
      rv_data =  |was { lv_days } day ago|.
    ELSEIF lv_hrs IS NOT INITIAL .
      rv_data =  |was { lv_hrs } hrs ago|.
    ELSEIF lv_mins IS NOT INITIAL AND lv_mins > 5.
      rv_data =  |was { lv_mins } min ago|.
    ELSE.
      rv_data =  |online|.
    ENDIF.




  ENDMETHOD.


  METHOD get_message.

    DATA lt_message TYPE TABLE OF zchat_message .

    FIELD-SYMBOLS
                   : <fs_message> TYPE  zchat_message
                   .

    SELECT * INTO TABLE  lt_message
      FROM zchat_message
      WHERE  MESSAGE = iv_guid
      ORDER  BY npp.


    LOOP AT  lt_message ASSIGNING <fs_message>.
      rv_message = |{ rv_message }{ <fs_message>-value }|.
    ENDLOOP.

    CONDENSE rv_message.

  ENDMETHOD.


  METHOD get_public_key.

    DATA
          : lt_public TYPE TABLE OF zchat_publik_key
          .

    FIELD-SYMBOLS
                   : <fs_public> TYPE zchat_publik_key
                   .
    SELECT * INTO TABLE lt_public
      FROM zchat_publik_key
      WHERE login = iv_login
      ORDER BY npp.


    LOOP AT lt_public ASSIGNING <fs_public>.
      rv_public  = |{ rv_public }{ <fs_public>-value }|.
    ENDLOOP.

    CONDENSE rv_public.
  ENDMETHOD.


  method HISTORY.

    SELECT DISTINCT RECEIVER as login
      INTO CORRESPONDING FIELDS OF TABLE ms_responce-HISTORY
      from ZCHAT_CONVERS
      where login = ms_data-login.

      FIELD-SYMBOLS
                     : <fs_hist> TYPE ZCHAT_ST_HISTORY
                     .


      LOOP AT ms_responce-HISTORY ASSIGNING  <fs_hist> .

        SELECT DISTINCT DATE1 as dates INTO TABLE <fs_hist>-dates
          from ZCHAT_CONVERS where login = ms_data-login
          and receiver = <fs_hist>-login.

          sort <fs_hist>-dates DESCENDING.


      ENDLOOP.

    respond( ).
  endmethod.


  method HISTORY2.

    data
          : lv_date TYPE datum
          , lt_convers TYPE TABLE OF zchat_convers
          .

    FIELD-SYMBOLS
                   : <fs_convers> TYPE zchat_convers
                   , <fs_message> like LINE OF ms_responce-messages
                   .

    lv_date = ms_data-MESSAGE_ME(4) && ms_data-MESSAGE_ME+5(2) && ms_data-MESSAGE_ME+8(2).

      SELECT * INTO TABLE lt_convers
        FROM zchat_convers
        WHERE login = ms_data-login
        and RECEIVER = ms_data-receiver
        AND DATE1 = lv_date
        ORDER BY time
.


    LOOP AT lt_convers ASSIGNING <fs_convers>.
      APPEND INITIAL LINE TO ms_responce-messages ASSIGNING <fs_message>.

      <fs_message>-login = <fs_convers>-receiver.

      SELECT SINGLE id INTO <fs_message>-id
        FROM zchat_login
        WHERE login = <fs_message>-login.


      <fs_message>-direction = <fs_convers>-direction.

      <fs_message>-time = format_time( <fs_convers>-time ).
      <fs_message>-message = get_message( <fs_convers>-message ).


    ENDLOOP.
    respond( ).
  endmethod.


METHOD if_http_extension~handle_request.

  DATA
      : lv_request_method   TYPE string
      .

  mr_server = server.

* get request method
  lv_request_method =
  server->request->get_header_field( '~REQUEST_METHOD' ).

  data
        : lv_id TYPE string
        .



  IF lv_request_method EQ 'POST'.
    post( ).
  ELSE.
     lv_id = server->request->get_form_field( name = 'id' ).
     IF lv_id is INITIAL.
       get( ).
       else.
         get_image( lv_id ).
     ENDIF.

  ENDIF.

ENDMETHOD.


  METHOD liked.
    IF check_acces( ) = 'X'.

      DATA
            : ls_zchat_convers TYPE zchat_convers
            , lt_zchat_convers TYPE TABLE OF zchat_convers
            .

      FIELD-SYMBOLS
                     : <fs_convers> TYPE zchat_convers
                     .
      ls_zchat_convers-message = ms_data-message_me.

      SELECT SINGLE  * INTO ls_zchat_convers
        FROM zchat_convers
        WHERE message = ls_zchat_convers-message.

      SELECT * INTO TABLE lt_zchat_convers
        FROM zchat_convers WHERE time = ls_zchat_convers-time.

      LOOP AT lt_zchat_convers ASSIGNING <fs_convers>.
        <fs_convers>-likes = 'X'.
      ENDLOOP.

      MODIFY zchat_convers FROM TABLE lt_zchat_convers.

      DATA
            : ls_zchat_tmp_like TYPE zchat_tmp_like
            .

      LOOP AT lt_zchat_convers ASSIGNING <fs_convers> WHERE
      message  NE ls_zchat_convers-message.



        ls_zchat_tmp_like-login = <fs_convers>-login.
        ls_zchat_tmp_like-message = <fs_convers>-message.

        MODIFY zchat_tmp_like FROM ls_zchat_tmp_like.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD ping.

    IF check_acces( ) = 'X'.
      get_data( ).
    ELSE.
      ms_responce-error = 'Wrong user or pass'.
    ENDIF.

    respond( ).
  ENDMETHOD.


  METHOD post.


    DATA
        :   lv_body_x TYPE xstring
        ,   lv_body_str TYPE string
        ,   lr_parser TYPE REF TO /ui5/cl_json_parser
        ,   lr_conv TYPE REF TO cl_abap_conv_in_ce
        .

    mr_server->request->get_data( RECEIVING data   =  lv_body_x ).

    CHECK lv_body_x IS NOT INITIAL.
    CLEAR ms_responce.

    lr_conv = cl_abap_conv_in_ce=>create(
              encoding = 'UTF-8' ).

    TRY .
        lr_conv->convert(
              EXPORTING input = lv_body_x
              IMPORTING data = lv_body_str ).

      CATCH cx_sy_conversion_codepage.

    ENDTRY.

    TRY .
        CREATE OBJECT lr_parser.
        CALL METHOD lr_parser->parse
          EXPORTING
            json = lv_body_str.

      CATCH /ui5/cx_vfs_error .

      CATCH cx_sy_conversion_codepage.

    ENDTRY.


    CLEAR ms_data.


    IF lr_parser IS BOUND .
      LOOP AT lr_parser->m_entries ASSIGNING FIELD-SYMBOL(<fs_entry>) WHERE type = 1 AND parent = ''.
        CASE <fs_entry>-name.
          WHEN 'login'.
            ms_data-login = <fs_entry>-value.
          WHEN 'publickey'.
            ms_data-publickey = <fs_entry>-value.
          WHEN 'action'.
            ms_data-action = <fs_entry>-value.
          WHEN 'receiver'.
            ms_data-receiver = <fs_entry>-value.
          WHEN 'messageme'.
            ms_data-message_me = <fs_entry>-value.
          WHEN 'messageother'.
            ms_data-message_other = <fs_entry>-value.
          WHEN 'all'.
            ms_data-all = <fs_entry>-value.
        ENDCASE.
      ENDLOOP.
    ENDIF.


    CASE ms_data-action.
      WHEN 'PING'.
        ping( ).
      WHEN 'POST'.
        post_message( ).
      WHEN 'REGISTER'.
        register( ).
      WHEN 'HISTORY'.
        history( ).
      WHEN 'HISTORY2'.
        history2( ).
      WHEN 'AVATAR'.
        save_avatar( ).
      WHEN 'LIKED'.
        liked( ).
      WHEN OTHERS.
        mr_server->response->set_cdata( data = '' ).
    ENDCASE.



  ENDMETHOD.


  METHOD post_message.

    IF check_acces( ) = 'X'.
      save_message( ).
      get_data( ).
    ELSE.
      ms_responce-error = 'Wrong user or pass'.
    ENDIF.

    respond( ).

  ENDMETHOD.


  METHOD register.

    DATA
          : ls_login TYPE zchat_login
          , lt_public TYPE TABLE OF zchat_publik_key
          , lv_npp TYPE numc3
          , lv_i TYPE i
          .

    FIELD-SYMBOLS
                   : <fs_public> TYPE zchat_publik_key
                   .

    ls_login-login = ms_data-login.
    ls_login-uname = sy-uname.

    SELECT SINGLE clnt INTO sy-mandt
      FROM zchat_login
      WHERE login = ms_data-login.

    IF sy-subrc = 0.
      ms_responce-error = 'Уже зареган'.
    ELSE.

      select max( id ) into
        ls_login-id
        from zchat_login.

        add 1 to ls_login-id.

      MODIFY zchat_login FROM ls_login.

      lv_npp  = 0.
      DO  .

        ADD 1 TO lv_npp .
*MS_DATA-PUBLICKEY

        APPEND INITIAL LINE TO lt_public ASSIGNING <fs_public>.

        <fs_public>-login = ms_data-login.
        <fs_public>-npp = lv_npp.

        <fs_public>-value = ms_data-publickey.


        lv_i = strlen( ms_data-publickey ).
        IF lv_i > 255.
          ms_data-publickey = ms_data-publickey+255.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      MODIFY zchat_publik_key FROM TABLE lt_public.

      get_data( ).
    ENDIF.


    respond( ).
  ENDMETHOD.


  METHOD respond.

    DATA
                : lv_char_type        TYPE string
                , lv_string_charset   TYPE string
                , lv_echo_string TYPE string
                .


*возвращаем ответ

    lv_char_type = 'utf-8'.

    CONCATENATE 'application/json' '; charset =' lv_char_type
    INTO lv_string_charset.

***set CharSet into response
    mr_server->response->set_header_field(
        name  = 'Content-Type'
        value = lv_string_charset ).

    mr_server->response->set_header_field(
      name  = 'Expires'                                     "#EC NOTEXT
      value = '0' ).

    lv_echo_string = /ui2/cl_json=>serialize( data = ms_responce
                                          compress = abap_true
                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                          assoc_arrays_opt = abap_true  ).

    DATA
          : lr_conv_out TYPE REF TO cl_abap_conv_out_ce
          , lv_echo_xstring TYPE xstring
          .

    lr_conv_out = cl_abap_conv_out_ce=>create(
      encoding    = 'UTF-8'               " Кодировка в которую будем преобразовывать
    ).


    lr_conv_out->convert( EXPORTING data = lv_echo_string IMPORTING buffer = lv_echo_xstring ).
*отправляем
*ответ
    mr_server->response->set_data( data = lv_echo_xstring ).


  ENDMETHOD.


  METHOD save_avatar.

    IF check_acces( ) = 'X'.

      DATA
            : lv_x_str TYPE xstring
            , lt_string TYPE TABLE OF string
            , lv_string TYPE  string
            .

      SPLIT ms_data-message_me AT ',' INTO TABLE lt_string.

      LOOP AT lt_string INTO lv_string.

      ENDLOOP.

      CALL METHOD cl_http_utility=>decode_x_base64
        EXPORTING
          encoded = lv_string
        RECEIVING
          decoded = lv_x_str.


      DATA
      : lt_file_tab  TYPE solix_tab
      , lv_bytecount TYPE i

      .

      lt_file_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lv_x_str ).
      lv_bytecount = xstrlen( lv_x_str ).

      DATA
            : ls_login TYPE zchat_login
            .

      SELECT SINGLE * INTO ls_login
        FROM zchat_login
        WHERE login =  ms_data-login.

      ls_login-avatar_len = lv_bytecount.

      MODIFY zchat_login FROM ls_login.

      DATA
            : lt_avatar TYPE TABLE OF zchat_avatar
            .

      FIELD-SYMBOLS
                     : <fs_solix> TYPE solix
                     , <fs_avatar> TYPE zchat_avatar
                     .

      LOOP AT lt_file_tab ASSIGNING <fs_solix>.
        APPEND INITIAL LINE TO lt_avatar ASSIGNING  <fs_avatar>.


        <fs_avatar>-id = ls_login-id.
        <fs_avatar>-npp = sy-tabix.
        <fs_avatar>-value = <fs_solix>-line.

      ENDLOOP.

      DELETE FROM zchat_avatar WHERE id = ls_login-id.

      MODIFY zchat_avatar FROM TABLE lt_avatar.

    ENDIF.
  ENDMETHOD.


  METHOD save_message.

    DATA
          : lv_tstmp_start TYPE tzntstmpl
          , lv_tstmp1 TYPE tzntstmpl
           , lt_convers TYPE TABLE OF zchat_convers
           , lt_message TYPE TABLE OF zchat_message
           , lv_i TYPE i
           , lv_npp TYPE numc3
          .

    FIELD-SYMBOLS
                   : <fs_convers> TYPE zchat_convers
                   , <fs_message> TYPE zchat_message
                   .



    GET TIME STAMP FIELD lv_tstmp_start.


        data
          : lv_xz_stmmp TYPE tzntstmpl
          .

    lv_xz_stmmp = lv_tstmp_start.

    CALL METHOD cl_abap_tstmp=>add
      EXPORTING
        tstmp   = lv_xz_stmmp
        secs    = 10800
      RECEIVING
        r_tstmp = lv_tstmp_start
        .
*TRY.
    CALL METHOD cl_abap_tstmp=>normalize
      EXPORTING
        tstmp_in  = lv_tstmp_start
      RECEIVING
        tstmp_out = lv_tstmp1.


    APPEND INITIAL LINE TO lt_convers ASSIGNING <fs_convers>.
    <fs_convers>-date1 = sy-datum.


    <fs_convers>-login = ms_data-login.
    <fs_convers>-receiver = ms_data-receiver.
    <fs_convers>-direction = '0'.
    <fs_convers>-time = lv_tstmp1.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = <fs_convers>-message.

    CLEAR lv_npp.
    DO .
      ADD 1 TO lv_npp.
      APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message> .
      <fs_message>-message = <fs_convers>-message.
      <fs_message>-npp = lv_npp.
      <fs_message>-value =  ms_data-message_me.

      lv_i = strlen( ms_data-message_me ).

      IF lv_i > 255.
        ms_data-message_me = ms_data-message_me+255.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.


    APPEND INITIAL LINE TO lt_convers ASSIGNING <fs_convers>.
    <fs_convers>-date1 = sy-datum.


    <fs_convers>-login = ms_data-receiver .
    <fs_convers>-receiver = ms_data-login.
    <fs_convers>-direction = '1'.
    <fs_convers>-time = lv_tstmp1.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = <fs_convers>-message.

    CLEAR lv_npp.
    DO .
      ADD 1 TO lv_npp.
      APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message> .
      <fs_message>-message = <fs_convers>-message.
      <fs_message>-npp = lv_npp.
      <fs_message>-value =  ms_data-message_other.

      lv_i = strlen( ms_data-message_other ).

      IF lv_i > 255.
        ms_data-message_other = ms_data-message_other+255.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

    MODIFY zchat_convers FROM TABLE lt_convers.
    MODIFY zchat_message FROM TABLE lt_message.


  ENDMETHOD.


  METHOD tstmp2secs.


    DATA: date      TYPE d,      " date part of incoming time stamp
          time      TYPE t,      " time part of incoming time stamp
          nano_secs TYPE p
                    DECIMALS 7,  " decimals fraction of Tstmp Secs
          p         TYPE p.

    DATA: d_type  TYPE string,
          d_len   TYPE string,
          d_decs  TYPE string,
          p_value TYPE string.



*** input type check
    DESCRIBE FIELD tstmp
      TYPE     d_type
      LENGTH   d_len IN BYTE MODE
      DECIMALS d_decs.
    IF NOT ( d_type = 'P' AND
             ( d_len = 8  AND d_decs = 0 ) OR
             ( d_len = 11 AND d_decs = 7 )
            ).
      CONCATENATE d_type `(` d_len `) DECIMALS ` d_decs INTO d_type.
      RAISE EXCEPTION TYPE cx_parameter_invalid_type
        EXPORTING
          parameter = 'TSTMP'
          type      = d_type.
    ENDIF.


* consistency check
    CONVERT TIME STAMP tstmp TIME ZONE '      ' INTO DATE date TIME time.
    IF sy-subrc <> 4.
      p_value = tstmp.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'TSTMP'
          value     = p_value.

    ENDIF.

* get nano_secs
    p = trunc( tstmp ).
    nano_secs = tstmp - p.


* calculate returning diff
    secs = nano_secs.
    secs = secs + time.
    secs = secs + date * 86400.

    RETURN.

  ENDMETHOD.
ENDCLASS.
