define NEW shared variable vlc_host          as character format "x(40)".
define NEW shared variable vlc_file_server   as character format "x(40)".
define NEW shared variable vlc_print_server  as character format "x(40)".
define NEW shared variable vlc_dir_fuentes   as character format "x(40)".
define NEW shared variable vlc_dir_objetos   as character format "x(40)".
define NEW shared variable vlc_dir_spool     as character format "x(40)".
define NEW shared variable vlc_dir_temp      as character format "x(40)".
define NEW shared variable vlc_dir_i-o       as character format "x(40)".
define NEW shared variable vlc_comando       as character format "x(40)".

/*RUN wliqimpplanilla.w.*/

/*RUN wliqselsucursal.w.*/

/*RUN wliqresplanilla.w.*/

/*RUN d_liq_control_tot_fincas01.w.  */
/*RUN wliqseltarja.w. */
/*RUN wrcatperstareas.w.*/
/*RUN wliqtareas.w. */ 
/*RUN wautorizahs.w.*/
/*RUN wliqgencomp.w. */
/*RUN wliqtarjaslegajo.w. */
/*RUN wliqplanillatarja.w. */

/*RUN wliqgenpl.w.*/
/*RUN wr-analisis01.w. */
/*RUN wliqtareaslegajo3.w. */
/*RUN wconsliqtareas.w. */
/*RUN wpartediario.w.*/

/*RUN wliqselauto.w.*/ 
/*RUN wliqplanillatarja.w.*/
/*RUN wliqtarjas01.w. */
/*RUN wliqtareaslegajo.w. */
/*RUN wliqtarjascertifica.w. */
/*RUN wreslluvia.w.*/

/*RUN wliqselsucursal.w. */
/*RUN wliqgenliqrhpro.w. */
RUN wliqtareas.w.  
/*RUN wliqseltarja.w. */
/*RUN wliqtarjasfincas.w.*/  
/*RUN wliqoperativo.w.   */

/*RUN p_remitos_usa.p.*/
