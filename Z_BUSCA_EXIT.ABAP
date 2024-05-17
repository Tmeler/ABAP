report Z_BUSCA_EXIT  no standard page heading.
*------------------------------------------------------------------*
*                                                                  *
*                                                                  *
* Este programa pesquisa todas as ampliações e suas respectivas    *
* funções para modificações de acordo com sua  necessidade.        *
* Eventualmente, poderá haver funções dentro destas  ampliações    *
* que não atenda o interessado,   pois a pesquisa será efetuada    *
* levando-se  em  conta a  classe de  desenvolvimento.  Sabe-se,   *
* entretanto, que o R3 prima por considerar cada processo dentro   *
* uma unica classe de desenvolvimento.                             *
*                                                                  *
*------------------------------------------------------------------*

* Tabelas para pequisar ampliações MODSAP MOSAPA

tables :
  tstc,    " Códigos transação SAP
  tstct,   " Textos das transações
  tadir,   " Catálogo de objetos repository
  trdir,   " Programas com seus atributos
  tfdir,   " Módulos de funções
  modsapt, " Textos breves ampliações SAP
  modsap,
  tftit,
  enlfdir. " Atributos adicionais das funções

data: begin of mylist occurs 50.       " Internal table hierarchy
        include structure snodetext.
data: end of mylist.

data:
   begin of tp occurs 10,
     id(01),
     nr(08),
     text(255),
     length like textpool-length,
   end of tp.

data: txt_report like dokhl-object.    "Report name for documentation

data: f15 type c, w_campo(30).

data :
  t_objetos like tadir occurs 0 with header line,
  v_classe  like tadir-devclass,
  cursor(30).

parameters :
  p_tcode like tstc-tcode obligatory default 'VA01',
  p_langu like sy-langu obligatory default 'PT'.

start-of-selection.

select single * from tstc where tcode eq p_tcode.

  get cursor field cursor.

  read textpool sy-repid into tp language sy-langu.
  loop at tp where id = 'R'.
    set titlebar '001' with tp-text.
    exit.
  endloop.

  perform monta_tabela.
  perform build_tree.
  perform draw_tree.

at user-command.
case sy-ucomm.
 when 'BACK' or 'EXIT' or 'CANC'.
 set screen 0.
  leave program.
when others.
endcase.

top-of-page.
  format color col_positive intensified off.
  write:/(19) 'Codigo Transação - ',
       20(20) p_tcode,
       45(50) tstct-ttext.
  skip.

at line-selection.
  check cursor(9) eq 'T_OBJETOS'.
  set parameter id 'MON' field sy-lisel+1(10).
  call transaction 'SMOD' and skip first screen.

*---------------------------------------------------------------------*
*       FORM build_tree                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form build_tree.

  call function 'RS_TREE_CONSTRUCT'
    TABLES
      nodetab      = mylist
    EXCEPTIONS
      tree_failure = 1.

endform.                    "build_tree

*---------------------------------------------------------------------*
*       FORM draw_tree                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form draw_tree.

  sy-lsind = 0.
  call function 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program      = 'Z_BUSCA_EXIT'
      callback_user_command = 'USER_COMMAND'  "'NODE_SELECT'
    IMPORTING
      f15                   = f15.

endform.                    "draw_tree

*&---------------------------------------------------------------------*
*&      Form  monta_tabela
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_tabela.
  if sy-subrc eq 0.
    select single * from tadir
      where pgmid   = 'R3TR'
      and  object   = 'PROG'
      and  obj_name = tstc-pgmna.

    move  tadir-devclass to v_classe.

    if sy-subrc ne 0.
      select single * from trdir where name = tstc-pgmna.
      if trdir-subc eq 'F'.
        select single * from tfdir
          where pname = tstc-pgmna.

        select single * from enlfdir
          where funcname = tfdir-funcname.

        select single * from tadir
          where pgmid    = 'R3TR'
          and   object   = 'FUGR'
          and   obj_name = enlfdir-area.

        move : tadir-devclass to v_classe.
      endif.
    endif.

    select * from tadir into table t_objetos
      where pgmid  = 'R3TR'
      and object   = 'SMOD'
      and devclass = v_classe.

    select single *
      from tstct
      where sprsl = p_langu
      and   tcode = p_tcode.


    if not t_objetos[] is initial.
      mylist-name = 'Ampliações'.
      mylist-color = 6.
      mylist-intensiv = '1'.
      mylist-text = 'Raiz'.
      mylist-tlength = 10.
      mylist-tlevel = 1.
      mylist-tcolor = 6.
      mylist-tintensiv = '1'.
      mylist-text1 = 'Ampliações'.
      mylist-tlength1 = 30.
      mylist-tcolor1 = 6.
      mylist-tintensiv1 = '2'.
      append mylist.

      loop at t_objetos.
        select single * from modsapt
          where sprsl = p_langu
          and   name  = t_objetos-obj_name.
        mylist-name = 'Ampliações'.
        mylist-color = 4.
        mylist-intensiv = '1'.
        mylist-text = t_objetos-obj_name.
        mylist-tlength = 10.
        mylist-tlevel = 2.
        mylist-tcolor = 4.
        mylist-tintensiv = '1'.
        mylist-text1 = modsapt-modtext.
        mylist-tlength1 = 50.
        mylist-tcolor1 = 4.
        mylist-tintensiv1 = '2'.
        append mylist.

        select * from modsap
          where name = t_objetos-obj_name
          and   typ  = 'E'.
          select single * from  tftit
                 where  spras     = 'PT'
                 and    funcname  = modsap-member.
          if sy-subrc ne 0.
            tftit-stext = space.
          endif.

          mylist-name = 'Funções'.
          mylist-color = 7.
          mylist-intensiv = '1'.
          mylist-text = modsap-member.
          mylist-tlength = 30.
          mylist-tlevel = 3.
          mylist-tcolor = 7.
          mylist-tintensiv = '1'.
          mylist-text1 = tftit-stext.
          mylist-tlength1 = 50.
          mylist-tcolor1 = 7.
          mylist-tintensiv1 = '2'.
          append mylist.
        endselect.

      endloop.

    else.
      format color col_negative intensified on.
      write:/(95) 'Não existe ampliações para esta transação'.
    endif.
  else.
    format color col_negative intensified on.
    write:/(95) 'Transação não existe'.
  endif.

endform.                    " monta_tabela

*---------------------------------------------------------------------*
*       FORM node_select                                              *
*---------------------------------------------------------------------*
form user_command tables knoten structure seucomm
  using command
  changing exit
           list_refresh.

  case command.
    when 'TRSL'.                       "F2 wurde gedrückt
      if knoten-name = 'Ampliações'.
        set parameter id 'MON' field knoten-text.
        call transaction 'SMOD' and skip first screen.
        exit = ' '.
      endif.

      if knoten-name = 'Funções'.
        set parameter id 'LIB' field knoten-text.
        call transaction 'SE37'.
        exit = ' '.
      endif.


  endcase.

  list_refresh = 'X'.
endform.                    "user_command
