/* TEMPORIZADORES MEDIDOS EN SEGUNDOS */
&GLOBAL-DEFINE TEMPO_PAUSA_MENU 2700
&GLOBAL-DEFINE TEMPO_PAUSA_PROGRAMA 1800
&GLOBAL-DEFINE TEMPO_PAUSA_AGREGAR 900
&GLOBAL-DEFINE TEMPO_PAUSA_EDITAR 600

/* COMPORTAMIENTO DE LA SENTENCIA DELETE */
&GLOBAL-DEFINE VERSION_DELETE 2

/* NOTAS

VERSION 1:
---------

En Librer¡a se borra un registro de la tabla con la sentencia:
   delete {&archivo}.

En el diccionario de datos la validaci¢n contra el borrado de datos en la tabla debe estar puesta en la funci¢n validate:
   validate(not can-find(first tabla of {&archivo}), "No se puede eliminar registro. Hay datos relacionados")


VERSION 2:
---------

En Librer¡a se borra un registro de la tabla con la sentencia:
   delete {&archivo} no-error.
   if error-status:error then
     do:
       message return-value view-as alert-box error.
       undo _sale, leave _sale.
     end.

En el diccionario de datos la validaci¢n contra el borrado de datos en la tabla debe estar puesta en el trigger for detele:

   TRIGGER PROCEDURE FOR DELETE OF personas.

   define variable mensaje as character.

   if can-find(first r_personas_actividad of personas) then
     mensaje = mensaje + "Actividades, ".
   if can-find(first r_personas_idiomas of personas) then
     mensaje = mensaje + "Idiomas, ".
   if can-find(curriculums of personas) then
     mensaje = mensaje + "Curr¡culums, ".
   if can-find(first r_personas_companias of personas) then
     mensaje = mensaje + "Compa¤¡as, ".
   if can-find(first r_espec_comp_pers of personas) then
     mensaje = mensaje + "Espcializaciones, ".
   if can-find(first encuentros where encuentros.id_persona_resp_interno = personas.id_persona) then
     mensaje = mensaje + "Encuentros, ".
   if can-find(first suscriptores of personas) then
     mensaje = mensaje + "Suscripciones, ".
   if can-find(first publicaciones where publicaciones.id_persona_resp_interno = personas.id_persona) then
     mensaje = mensaje + "Publicaciones, ".
   if can-find(first r_personas_estudios of personas) then
     mensaje = mensaje + "Estudios, ".
   if can-find(first familia_hijos of personas) or 
      can-find(first familia_hijos where familia_hijos.id_persona_hijo = personas.id_persona) then
     mensaje = mensaje + "Familia - Hijos, ".
   if can-find(first familia_hermanos of personas) or 
      can-find(first familia_hermanos where familia_hermanos.id_persona_hermano = personas.id_persona) then
     mensaje = mensaje + "Familia - Hermanos, ".

   if mensaje <> "" then
     return error "No se puede eliminar la Persona seleccionada. Tiene v¡nculos con: " + mensaje + "elim¡nelos primero.".

*/

