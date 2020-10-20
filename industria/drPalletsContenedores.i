  FIELD id_tipo_contenedor LIKE r_pallets_contenedores.id_tipo_contenedor VALIDATE ~
  FIELD id_tipo_pallet LIKE r_pallets_contenedores.id_tipo_pallet VALIDATE ~
  FIELD palletsxcontenedor LIKE r_pallets_contenedores.palletsxcontenedor VALIDATE ~
  FIELD contenedor LIKE tipo_contenedor.descripcion VALIDATE ~
  FIELD tipopallet LIKE tipo_pallets.descripcion VALIDATE  LABEL "TipoPallet"
