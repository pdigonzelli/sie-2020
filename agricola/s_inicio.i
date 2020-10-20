/*--- SISTEMAS: (contacto, gessi, posmaster, nudelman, core) ---*/
{../sistema.i}

/*--- VARIABLES GLOBALES DE TODOS LOS SISTEMAS ---*/
define {1} shared variable vlc_host          as character format "x(60)".
define {1} shared variable vlc_host_app      as character format "x(60)".
define {1} shared variable vlc_misc          as character format "x(60)" no-undo initial ?.
define {1} shared variable vlc_file_server   as character format "x(60)".
define {1} shared variable vlc_print_server  as character format "x(60)".
define {1} shared variable vlc_dir_fuentes   as character format "x(60)".
define {1} shared variable vlc_dir_objetos   as character format "x(60)".
define {1} shared variable vlc_dir_spool     as character format "x(60)".
define {1} shared variable vlc_dir_temp      as character format "x(60)".
define {1} shared variable vlc_dir_i-o       as character format "x(60)".
define {1} shared variable vlc_dir_bkp       as character format "x(60)".
define {1} shared variable vlc_comando       as character format "x(60)".
define {1} shared variable vlc_web           as character format "x(60)".
define {1} shared variable vlx_web           as integer   format ">>>9".
define {1} shared variable vln_web           as character format "x(60)".
define {1} shared variable vlc_email         as character format "x(60)".
define {1} shared variable vlx_email         as integer   format ">>>9".
define {1} shared variable vln_email         as character format "x(60)".
define {1} shared variable vlc_doc           as character format "x(60)".
define {1} shared variable vlx_doc           as integer   format ">>>9".
define {1} shared variable vln_doc           as character format "x(60)".
define {1} shared variable vlc_calc          as character format "x(60)".
define {1} shared variable vlx_calc          as integer   format ">>>9".
define {1} shared variable vln_calc          as character format "x(60)".
define {1} shared variable vlc_html          as character format "x(60)".
define {1} shared variable vlx_html          as integer   format ">>>9".
define {1} shared variable vln_html          as character format "x(60)".
define {1} shared variable vlc_send_email    as character format "x(60)".
define {1} shared variable vlx_send_email    as integer   format ">>>9".
define {1} shared variable vln_send_email    as character format "x(60)".
define {1} shared variable vlc_pdf           as character format "x(60)".
define {1} shared variable vlc_printer       as logical   initial no.
define {1} shared variable hserver           as handle.
define {1} shared variable x_dlc             as character format "x(60)" initial ?.
define {1} shared variable h_sesion          as HANDLE.

/*--- VARIABLES GLOBALES DE MODULOS ADMINISTRATIVOS/CONTABLES ---*/
&IF "{&SISTEMA}" = "gessi" OR "{&SISTEMA}" = "posmaster" OR "{&SISTEMA}" = "contacto" &THEN
  define {1} shared variable v_dia_de_pago as integer.
&ENDIF
