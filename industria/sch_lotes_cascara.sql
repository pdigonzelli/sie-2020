DROP TABLE IF EXISTS lotes_cascara;
CREATE TABLE lotes_cascara(
	PROGRESS_RECID bigint NOT NULL,
	anio	smallint DEFAULT 0,
	anio_contrato	smallint DEFAULT 2001,
	control_calidad	tinyint DEFAULT 1,
	c_fecha	date,
	c_hora	varchar(5) DEFAULT "",
	c_usuario	varchar(5) DEFAULT "",
	fecha	date,
	id_articulo	integer DEFAULT 0,
	id_calidad	integer DEFAULT 0 NOT NULL,
	id_contrato	varchar(5) DEFAULT "",
	id_empresa	integer DEFAULT 0,
	id_envase	integer DEFAULT 0,
	id_lote	integer DEFAULT 0 NOT NULL,
	id_orden_entrega	mediumint DEFAULT 0,
	id_sucursal	integer DEFAULT 0 NOT NULL,
	id_sucursal_remito	integer DEFAULT 0 NOT NULL,
	id_tipotambor	mediumint DEFAULT 0,
	id_tipo_contrato	smallint DEFAULT 0,
	id_tipo_movsto	mediumint DEFAULT 0,
	item_contrato	integer DEFAULT 0,
	item_oe	mediumint DEFAULT 0,
	nromov	integer DEFAULT 0,
	nro_remito	integer DEFAULT 0,
	observaciones	varchar(6) DEFAULT "",
	usuario_control_calidad	varchar(5) DEFAULT "",
	primary key	 CONV_PRIMARY (PROGRESS_RECID),
	key	lote (
			id_lote,
			anio,
			id_articulo),
## The PROGRESS database primary index
	key	lotes_cascara (
			id_empresa,
			id_sucursal,
			id_tipotambor,
			nromov)
)
	comment = "";
