	��Vz��X�;  - �                                              �d 3B8800EFutf-8 MAIN D:\desarrollos\webservices\dItemPalletSap.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,bultos integer 0 0,calibre character 1 0,camara integer 2 0,certificado character 3 0,cert_china character 4 0,codigo_trazabilidad character 5 0,cod_prod character 6 0,cod_trazabilidad character 7 0,contramarca character 8 0,con_testigo logical 9 0,c_fecha date 10 0,c_hora character 11 0,c_usuario character 12 0,fecha_ingreso date 13 0,fecha_operativa date 14 0,id_articulo integer 15 0,id_calidad integer 16 0,id_caract integer 17 0,id_categoria integer 18 0,id_color integer 19 0,id_empresa integer 20 0,id_envase integer 21 0,id_finca_senasa integer 22 0,id_lote integer 23 0,id_lote_senasa integer 24 0,id_marca integer 25 0,id_orden integer 26 0,id_origen integer 27 0,id_packing integer 28 0,id_pallet integer 29 0,id_proveedor integer 30 0,id_punto_emisor integer 31 0,id_sucursal_remito integer 32 0,id_suc_trabajo integer 33 0,id_tipo_movsto integer 34 0,id_turno_packing integer 35 0,id_variedad integer 36 0,item integer 37 0,item_factura integer 38 0,item_pallet integer 39 0,item_remito integer 40 0,nro integer 41 0,nro_certificado integer 42 0,nro_partida_general decimal 43 0,renspa character 44 0,tipo_fruta logical 45 0,tipo_proceso character 46 0,ubicacion character 47 0,unidad_productora character 48 0,zona_up character 49 0,RowNum integer 50 0,RowIdent character 51 0,RowMod character 52 0,RowIdentIdx character 53 0,RowUserProp character 54 0,ChangedFields character 55 0       �L              0;             � �L  D�              č              LH     +   �� �  W   �� h  X   � <  Y   D�   [   X�   \   t� @  ]   �� $  ^   �� 4  `   ? � �#  ISO8859-1                                                                        $  ,L    �                                      �                   �               lL  �    �   -7   ��              ��  �   �L      �L          �                                             PROGRESS                         �           
        
                    �              �                                                                                                     
                   produccion                       PROGRESS                         ,        �                                �֕S               �>                              �  t                      �  �  2     ID_EMPRESAID_SUC_TRABAJOID_SUCURSAL_REMITOID_TIPO_MOVSTONROITEM_REMITOCAMARAUBICACIONID_PALLETID_ORDENITEMTIPO_FRUTAID_ARTICULOID_VARIEDADID_COLORID_MARCAID_ENVASEID_CALIDADID_CATEGORIAID_CARACTCALIBRECONTRAMARCABULTOSID_PROVEEDORID_ORIGENID_LOTECODIGO_TRAZABILIDADCOD_TRAZABILIDADTIPO_PROCESOUNIDAD_PRODUCTORAID_FINCA_SENASARENSPAID_LOTE_SENASAC_USUARIOC_FECHAC_HORAITEM_FACTURACOD_PRODFECHA_INGRESOITEM_PALLETFECHA_OPERATIVANRO_PARTIDA_GENERALNRO_CERTIFICADOID_PUNTO_EMISORCERTIFICADOID_TURNO_PACKINGID_PACKINGCERT_CHINACON_TESTIGOZONA_UP                                                                      	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          �        $  
        
                    �             �                                                                                                    
      `         �  
        
                  �  �             H                                                                                                     
      	  2      �  
        
                  x  H	             �                                                                                          2          
      �	  ?      @	  
        
                  ,	  �	             �	                                                                                          ?          
      |
  R      �	  
        
                  �	  �
             d
                                                                                          R          
      0  d      �
  
        
                  �
  d  	                                                                                                     d          
      �  y      \  
        
                  H    
           �                                                                                          y          
      �  �        
        
                  �  �             �                                                                                          �          
      L  �      �                             �  �             4                                                                                          �                   �      x                            d  4             �                                                                                          �                �  �      ,  
        
                    �             �                                                                                          �          
      h  �      �  
        
                  �  �             P                                                                                          �          
        �      �  
        
                  �  P                                                                                                       �          
      �  �      H                            4               �                                                                                          �                �  �      �                            �  �             l                                                                                          �                8  �      �                            �  l                                                                                                        �                          d                            P  �             �                                                                                                          T         �       �  X  @/  R   �/  �  ��      �/  7       �             �          <      �              �       �  X  �J  S   �J  �  rN      LK  8       �         �    �0          @5      �                 `�                                               d�          D  �  L l$                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                                 �#  �#  �#  �#  �#                          $  $  $   $  $                         $$  ,$  0$  @$  8$                         D$  P$  X$  p$  d$                         t$  �$  �$  �$  �$                         �$  �$  �$  �$  �$                         �$  �$  �$  %  �$                         %  %  $%  D%  8%                         H%  T%  \%  l%  d%                         p%  |%  �%  �%  �%                         �%  �%  �%  �%  �%          �%             �%  �%   &  &  &          &             @&  L&  T&  d&  \&          h&             �&  �&  �&  �&  �&          �&             �&  �&  '  $'  '          ('             8'  D'  P'  `'  X'          d'             |'  �'  �'  �'  �'          �'             �'  �'  �'  �'  �'          �'             (  (  (  0(  $(                         4(  @(  H(  `(  X(          d(             t(  �(  �(  �(  �(                         �(  �(  �(  �(  �(          �(             �(  �(  �(  )  �(          )              )  ()  ,)  <)  4)                         @)  P)  X)  l)  d)                         p)  |)  �)  �)  �)          �)             �)  �)  �)  �)  �)                         �)  �)  �)  �)  �)                          *  *  *  (*   *                         ,*  8*  D*  \*  P*                         `*  p*  x*  �*  �*                         �*  �*  �*  �*  �*                         �*  �*  �*  �*  �*                         �*  +  +   +  +                         $+  4+  8+  P+  D+          T+             t+  �+  �+  �+  �+                         �+  �+  �+  �+  �+          �+             �+  �+  �+  ,  ,  4,  ,  @,             T,  d,  h,  �,  t,                         �,  �,  �,  �,  �,                         �,  �,  �,  �,  �,                         �,  �,  �,  �,  �,                         �,  -  -  0-  $-                         4-  H-  T-  t-  d-                         x-  �-  �-  �-  �-          �-             �-  �-  �-  �-  �-                         �-  �-  �-  .   .                         .  .   .  4.  ,.                         8.  L.  T.  p.  d.                         t.  |.  �.  �.  �.                         �.  �.  �.  �.                             �.  �.  �.  �.                              �.  �.  �.  �.                             �.  /  /  /                             /  (/  0/  </                                                                          bultos  >>,>>9  Bultos  Bultos  0   calibre XXX/XX  Calibre Calib.      camara  9   Camara  Cam.    0   certificado X(8)    Cert.Sen.   Cert.Senasa     cert_china  X(8)    Cert.China  Cert.China      codigo_trazabilidad X(7)    Cod.Traz.   C.Traz.     cod_prod    X(2)    Cod.Prod.   Cod.Prod.       cod_trazabilidad    X(12)   C�d.Trazabilidad    C�d.Traz.       contramarca X(4)    Letra   Letra       con_testigo SI/NO   Con Testigo CT  NO  c_fecha 99/99/99    Fecha   Fecha   today    Fecha de �ltima modificaci�n del registro  c_hora  x(8)    Hora    Hora    ?    Hora de �ltima modificaci�n del registro   c_usuario   x(12)   Usuario Usuario ?    Nombre del usuario que modific� el registro    fecha_ingreso   99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  fecha_operativa 99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  id_articulo >>>>>>>>9   C�digo  C�digo  0    C�digo del producto    id_calidad  >>9 Calidad Calidad 0   C�digo de calidad del producto  id_caract   >>9 C�d.caracter�stica  C�digo  0   C�d.caracter�stica  id_categoria    >9  Cod.Categ.  Cod.Cat.    0   id_color    >>,>>9  C�d.de color    Color   0   C�d.de color    id_empresa  >>9 Cod.Empresa C.Emp.  0   id_envase   >>9 Envase  Envase  0   Codigo de envase    id_finca_senasa >>>9    Finca Senasa    F.S.    0   C�digo de finca senasa  id_lote >>9 Lote    Lote    0   id_lote_senasa  >>>9    Lote Senasa L.S.    0   id_marca    >>9 Marca   Marca   0   Marca del producto  id_orden    >,>>>,>>9   Nro. Orden  Orden   0   id_origen   >>,>>9  Origen  Origen  0   id_packing  >>9 Packing Real    P.Real  0   id_pallet   >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_proveedor    >>>,>>9 Productor   Produc. 0   id_punto_emisor >9  Punto Emisor    P.E.    0   id_sucursal_remito  >>9 Sucursal Remito Suc.Remito  0   id_suc_trabajo  >>9 Sucursal    Suc.    0   id_tipo_movsto  >>9 T/Movsto    T/Movsto    0    Tipo de movimineto de stock    id_turno_packing    >>9 Cod.Packing C.Pack. 0   id_variedad >>9 variedad    variedad    0   C�digo de variedad del producto item    >>9 Item    Item    0    Debe ingresar un n�mero de �tem    item > 0 .   Item de pedidos    item_factura    >>9 Item Remito Item Rem.   0   item_pallet >>9 Item Pallet Item Pallet 0   item_remito >>9 Item Remito Item Rem.   0   nro >>>,>>9 Nro Nro 0   nro_certificado >>>,>>>,>>9 Certificado Senasa  Cert.Senasa 0   nro_partida_general >>>,>>>,>>9 Partida General Partida General 0   renspa  X(21)   Renspa  Renspa      Renspa  tipo_fruta  PROPIA/TERCEROS Tipo Fruta  Tipo Fruta  PROPIA  tipo_proceso    X(7)    T.Proc. T.Proc.     ubicacion   X(3)    Ubicacion   Ubic.       unidad_productora   X(3)    Unidad Prod.    Un.Prod.        zona_up x(4)    Zona UP ZonaUP      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �   / @�  ���8������          �����                                             %"        5"        <"                �     i     i     i    4 	6 	7 	    �  �  �  �  �        !   2   >   J   R   Y   c   q   �   �   �   �   �   �   �   �   �   �   �   �   !  !  !  %!  2!  B!  U!  d!  s!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  "  "  "  %"  ,"  5"  <"  H"                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                                 ?  ?  ?  ,?  $?                         0?  8?  @?  P?  H?                         T?  \?  `?  p?  h?                         t?  �?  �?  �?  �?                         �?  �?  �?  �?  �?                         �?  �?  �?  @  �?                         @  @  @  4@  (@                         8@  L@  T@  t@  h@                         x@  �@  �@  �@  �@                         �@  �@  �@  �@  �@                         �@  �@  �@  �@  �@          �@              A  (A  0A  @A  8A          DA             pA  |A  �A  �A  �A          �A             �A  �A  �A  B  �A          B             B  (B  4B  TB  DB          XB             hB  tB  �B  �B  �B          �B             �B  �B  �B  �B  �B          �B             �B  �B   C  C  C           C             4C  DC  HC  `C  TC                         dC  pC  xC  �C  �C          �C             �C  �C  �C  �C  �C                         �C  �C  �C  �C  �C          �C             D  D  D  4D  ,D          8D             PD  XD  \D  lD  dD                         pD  �D  �D  �D  �D                         �D  �D  �D  �D  �D          �D             �D  �D  �D  E  �D                         E  E  E  ,E  $E                         0E  <E  @E  XE  PE                         \E  hE  tE  �E  �E                         �E  �E  �E  �E  �E                         �E  �E  �E  �E  �E                         �E  F  F  $F  F                         (F  8F  <F  PF  HF                         TF  dF  hF  �F  tF          �F             �F  �F  �F  �F  �F                         �F  �F  �F  �F  �F           G              G  (G  ,G  <G  4G  dG  @G  pG             �G  �G  �G  �G  �G                         �G  �G  �G  �G  �G                         �G  �G  �G  H  �G                         H  H  H   H  H                         $H  4H  @H  `H  TH                         dH  xH  �H  �H  �H                         �H  �H  �H  �H  �H          �H             �H  �H  �H  I  �H                         I   I  (I  8I  0I                         <I  HI  PI  dI  \I                         hI  |I  �I  �I  �I                         �I  �I  �I  �I  �I                         �I  �I  �I  �I                             �I  �I  �I  J                              J  J  J  $J                             (J  4J  <J  HJ                             LJ  XJ  `J  lJ                              pJ  �J  �J  �J                                                                          bultos  >>,>>9  Bultos  Bultos  0   calibre XXX/XX  Calibre Calib.      camara  9   Camara  Cam.    0   certificado X(8)    Cert.Sen.   Cert.Senasa     cert_china  X(8)    Cert.China  Cert.China      codigo_trazabilidad X(7)    Cod.Traz.   C.Traz.     cod_prod    X(2)    Cod.Prod.   Cod.Prod.       cod_trazabilidad    X(12)   C�d.Trazabilidad    C�d.Traz.       contramarca X(4)    Letra   Letra       con_testigo SI/NO   Con Testigo CT  NO  c_fecha 99/99/99    Fecha   Fecha   today    Fecha de �ltima modificaci�n del registro  c_hora  x(8)    Hora    Hora    ?    Hora de �ltima modificaci�n del registro   c_usuario   x(12)   Usuario Usuario ?    Nombre del usuario que modific� el registro    fecha_ingreso   99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  fecha_operativa 99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  id_articulo >>>>>>>>9   C�digo  C�digo  0    C�digo del producto    id_calidad  >>9 Calidad Calidad 0   C�digo de calidad del producto  id_caract   >>9 C�d.caracter�stica  C�digo  0   C�d.caracter�stica  id_categoria    >9  Cod.Categ.  Cod.Cat.    0   id_color    >>,>>9  C�d.de color    Color   0   C�d.de color    id_empresa  >>9 Cod.Empresa C.Emp.  0   id_envase   >>9 Envase  Envase  0   Codigo de envase    id_finca_senasa >>>9    Finca Senasa    F.S.    0   C�digo de finca senasa  id_lote >>9 Lote    Lote    0   id_lote_senasa  >>>9    Lote Senasa L.S.    0   id_marca    >>9 Marca   Marca   0   Marca del producto  id_orden    >,>>>,>>9   Nro. Orden  Orden   0   id_origen   >>,>>9  Origen  Origen  0   id_packing  >>9 Packing Real    P.Real  0   id_pallet   >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_proveedor    >>>,>>9 Productor   Produc. 0   id_punto_emisor >9  Punto Emisor    P.E.    0   id_sucursal_remito  >>9 Sucursal Remito Suc.Remito  0   id_suc_trabajo  >>9 Sucursal    Suc.    0   id_tipo_movsto  >>9 T/Movsto    T/Movsto    0    Tipo de movimineto de stock    id_turno_packing    >>9 Cod.Packing C.Pack. 0   id_variedad >>9 variedad    variedad    0   C�digo de variedad del producto item    >>9 Item    Item    0    Debe ingresar un n�mero de �tem    item > 0 .   Item de pedidos    item_factura    >>9 Item Remito Item Rem.   0   item_pallet >>9 Item Pallet Item Pallet 0   item_remito >>9 Item Remito Item Rem.   0   nro >>>,>>9 Nro Nro 0   nro_certificado >>>,>>>,>>9 Certificado Senasa  Cert.Senasa 0   nro_partida_general >>>,>>>,>>9 Partida General Partida General 0   renspa  X(21)   Renspa  Renspa      Renspa  tipo_fruta  PROPIA/TERCEROS Tipo Fruta  Tipo Fruta  PROPIA  tipo_proceso    X(7)    T.Proc. T.Proc.     ubicacion   X(3)    Ubicacion   Ubic.       unidad_productora   X(3)    Unidad Prod.    Un.Prod.        zona_up x(4)    Zona UP ZonaUP      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �   / @�  ���9������          �����                                             %"        5"        <"                �     i     i     i    4 	6 	7 	    �  �  �  �  �        !   2   >   J   R   Y   c   q   �   �   �   �   �   �   �   �   �   �   �   �   !  !  !  %!  2!  B!  U!  d!  s!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  "  "  "  %"  ,"  5"  <"  H"  T"    ��                            ����                            ]    ��                    A�    �#   ��                    ��    undefined                                                               �       ��  �   p   ��  ��                    �����               8�f                        O   ����    e�          O   ����    R�          O   ����    ��      x       �   �              4   ����      /                                    3   ����       $     L  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  <      d       4   ����d                 L                      ��                  �   �                   Hg                           �   �  �  	  �   �                                        3   ����|       O   �   ��  ��  �   batchServices                               @  (      ��                  p  s  X              �g                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             p               ��                  �           ��                            ����                            clientSendRows                              �  �      ��                  u  {  �              �6g                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   $             �               ��   L                            ��   t             @               ��                  h           ��                            ����                            commitTransaction                               l  T      ��                  }  ~  �              �g                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             p  X      ��                  �  �  �              Hg                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �               Kg                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  
              �Kg                        O   ����    e�          O   ����    R�          O   ����    ��            ��   \
             (
               �� 
          �       P
  
         ��                            ����                            destroyServerObject                             T  <      ��                  �  �  l              �Mg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                d  L      ��                  �  �  |              �lg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              h  P      ��                  �  �  �              �og                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  |      ��                  �  �  �               pg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  �  �  �               sg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              xsg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              tg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              {g                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              0Wg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              �Wg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �                �Zg                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �                l[g                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $           ��                            ����                            printToCrystal                              $        ��                  �  �  <              `�g                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             T               ��   �             |               ��                  �           ��                            ����                            refreshRow                              �  �      ��                  �  �  �              ��g                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              �  �      ��                  �  �  �              ��g                        O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��   4                             ��   \             (               ��   �             P               ��   �             x               ��   �             �               �� 
  �      �       �  
             ��                  �           ��                            ����                            restartServerObject                             �  �      ��                  �  �                �g                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �                �9h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                                �      ��                  �  �                �Hh                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            saveContextAndDestroy                               <   $       ��                  �  �  T               �(h                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  l            ��                            ����                            serverSendRows                              l!  T!      ��                  �  �  �!              ,=h                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �!             �!               ��   �!             �!               ��    "             �!               ��   H"             "               ��   p"             <"               �� 
          �       d"  
         ��                            ����                            serverFetchRowObjUpdTable                               p#  X#      ��                  �  �  �#              h                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �#  
         ��                            ����                            setPropertyList                             �$  �$      ��                  �  �  �$              �Lh                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �$           ��                            ����                            serverSendRows                              �%  �%      ��                  �     �%              ,Qh                        O   ����    e�          O   ����    R�          O   ����    ��            ��   4&              &               ��   \&             (&               ��   �&             P&               ��   �&             x&               ��   �&             �&               �� 
          �       �&  
         ��                            ����                            startServerObject                               �'  �'      ��                      �'              X0h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �(  �(      ��                      �(              1h                        O   ����    e�          O   ����    R�          O   ����    ��            ��   8)             )               ��                  ,)           ��                            ����                            submitForeignKey                                0*  *      ��                  
    H*              �eh                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �*             `*               ��   �*             �*               ��                  �*           ��                            ����                            submitValidation                                �+  �+      ��                      �+              �mh                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ,             �+               ��                  ,           ��                            ����                            synchronizeProperties                               -  �,      ��                      ,-              th                        O   ����    e�          O   ����    R�          O   ����    ��            ��   x-             D-               ��                  l-           ��                            ����                            transferToExcel                             l.  T.      ��                  "  '  �.              T~h                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �.             �.               ��   �.             �.               ��    /             �.               ��                  /           ��                            ����                            undoTransaction                             0  �/      ��                  )  *  ,0              @�h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                              1  1      ��                  ,  /  81              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �1             P1               ��                  x1           ��                            ����                            updateQueryPosition                             |2  d2      ��                  1  2  �2              Ԉh                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �3  h3      ��                  4  6  �3              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �3           ��                            ����                            addRow          4      @4     w       CHARACTER,INPUT pcViewColList CHARACTER cancelRow    4      h4      �4   	 ~       CHARACTER,  canNavigate t4      �4      �4    �       LOGICAL,    closeQuery  �4      �4      5   
 �       LOGICAL,    columnProps �4      5      <5    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   5      |5      �5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �5      �5      �5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   �5       6      L6   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   ,6      p6      �6   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    |6      �6      �6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   �6      (7      X7  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow 87      �7      �7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    �7      �7      8    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds �7      p8      �8    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  |8      �8      �8    	      CHARACTER,  hasForeignKeyChanged    �8      9      @9           LOGICAL,    openDataQuery    9      L9      |9    5      LOGICAL,INPUT pcPosition CHARACTER  openQuery   \9      �9      �9   	 C      LOGICAL,    prepareQuery    �9      �9      :    M      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    �9      (:      X:    Z      LOGICAL,INPUT pcDirection CHARACTER rowValues   8:      |:      �:   	 g      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �:      ;      0;   	 q      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   ;      p;      �;   	 {      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   |;      �;      <    �      CHARACTER,  assignDBRow                             �<  �<      ��                      �<              d�h                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �<  
         ��                            ����                            bufferCopyDBToRO                                �=  �=      ��                     %  �=              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D>             >  
             �� 
  l>             8>  
             ��   �>             `>               ��                  �>           ��                            ����                            compareDBRow                                �?  p?      ��                  '  (  �?              ��h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �@  x@      ��                  *  ,  �@              �h                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �@           ��                            ����                            dataAvailable                               �A  �A      ��                  .  0  �A              P�h                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            fetchDBRowForUpdate                             �B  �B      ��                  2  3  C               �h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �C  �C      ��                  5  6  D              ȍh                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �D  �D      ��                  8  9  E              ��h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                                F  �E      ��                  ;  <  F              ��h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               G  �F      ��                  >  ?  G              ��h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              H  �G      ��                  A  C  ,H              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 DH  
         ��                            ����                            initializeObject                                HI  0I      ��                  E  F  `I              ��h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                PJ  8J      ��                  H  J  hJ              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �J  
         ��                            ����                            releaseDBRow                                �K  hK      ��                  L  M  �K              0�h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �L  lL      ��                  O  P  �L               �h                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �M  tM      ��                  R  U  �M              ��h                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �M             �M               ��                  �M           ��                            ����                            addQueryWhere   �;      LN      |N    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    \N      �N      O    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO �N      `O      �O    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   tO       P      4P    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  P      pP      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �P      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �P      Q      PQ          CHARACTER,INPUT pcColumn CHARACTER  columnTable 0Q      tQ      �Q    "      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �Q      �Q      �Q     .      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �Q      R      LR  !  ;      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  ,R      tR      �R  "  L      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �R      �R      �R  #  [      CHARACTER,INPUT iTable INTEGER  getDataColumns  �R      S      DS  $  j      CHARACTER,  getForeignValues    $S      PS      �S  %  y      CHARACTER,  getQueryPosition    dS      �S      �S  &  �      CHARACTER,  getQuerySort    �S      �S       T  '  �      CHARACTER,  getQueryString  �S      T      <T  (  �      CHARACTER,  getQueryWhere   T      HT      xT  )  �      CHARACTER,  getTargetProcedure  XT      �T      �T  *  �      HANDLE, indexInformation    �T      �T      �T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �T      PU      �U  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  dU      �U      V  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    �U      �V      �V  .  	      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �V      HW      xW  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  XW      �W      �W  0  (      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �W      <X      lX  1  7      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    LX      �X      �X  2  G      LOGICAL,    removeQuerySelection    �X      �X      Y  3  X      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   �X      LY      |Y  4  m      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  \Y      �Y      �Y  5 
 {      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �Y      �Y       Z  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition     Z      |Z      �Z  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Z      �Z      [  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString  �Z      $[      T[  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   4[      |[      �[  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �[      �[       \  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �\  �\      ��                  �  �  �\              �Ae                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �]  �]      ��                  �  �  �]              xBe                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �^  �^      ��                  �  �  �^              0e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �_  �_      ��                  �  �  �_              e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �`  �`      ��                  �  �   a              �Fe                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �a  �a      ��                  �  �  b              �Ge                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �b  �b      ��                      c              �&e                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,c  
         ��                            ����                            startServerObject                               0d  d      ��                      Hd              �
e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                8e   e      ��                    
  Pe              `e                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  he           ��                            ����                            getAppService   �[      �e       f  <  �      CHARACTER,  getASBound  �e      f      8f  = 
 �      LOGICAL,    getAsDivision   f      Df      tf  >  �      CHARACTER,  getASHandle Tf      �f      �f  ?  	      HANDLE, getASHasStarted �f      �f      �f  @        LOGICAL,    getASInfo   �f      �f      g  A 	 %      CHARACTER,  getASInitializeOnRun    �f      (g      `g  B  /      LOGICAL,    getASUsePrompt  @g      lg      �g  C  D      LOGICAL,    getServerFileName   |g      �g      �g  D  S      CHARACTER,  getServerOperatingMode  �g      �g       h  E  e      CHARACTER,  runServerProcedure   h      ,h      `h  F  |      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   @h      �h      �h  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �h      �h      ,i  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle i      Pi      |i  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   \i      �i      �i  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �i      �i       j  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt   j      Dj      tj  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Tj      �j      �j  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �j      �j      $k  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �k  �k      ��                  �  �  �k              Xme                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Hl             l  
             ��   pl             <l               �� 
                 dl  
         ��                            ����                            addMessage                              `m  Hm      ��                  �  �  xm              ,ze                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �m             �m               ��   �m             �m               ��                  �m           ��                            ����                            adjustTabOrder                              �n  �n      ��                  �  �  �n              Hae                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Do             o  
             �� 
  lo             8o  
             ��                  `o           ��                            ����                            applyEntry                              \p  Dp      ��                  �  �  tp              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            changeCursor                                �q  tq      ��                  �  �  �q              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �q           ��                            ����                            createControls                              �r  �r      ��                  �  �  �r              t�e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �s  �s      ��                  �  �  �s              ��e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �t  �t      ��                  �  �  �t              L�e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �u  �u      ��                  �  �  �u              ��e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �v  �v      ��                  �  �  �v              Գe                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �w  �w      ��                  �  �  �w              ��e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �x  �x      ��                  �  �  y              h�e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �y  �y      ��                  �    z              X�e                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `z             ,z  
             ��   �z             Tz               ��   �z             |z               ��                  �z           ��                            ����                            modifyUserLinks                             �{  �{      ��                      �{              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��   |             �{               ��   0|             �{               �� 
                 $|  
         ��                            ����                            removeAllLinks                              $}  }      ��                  	  
  <}              ؿe                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              (~  ~      ��                      @~              x�e                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �~             X~  
             ��   �~             �~               �� 
                 �~  
         ��                            ����                            repositionObject                                �  �      ��                      �              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            returnFocus                              �  �      ��                      �              ,�e                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 0�  
         ��                            ����                            showMessageProcedure                                8�   �      ��                      P�              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             h�               ��                  ��           ��                            ����                            toggleData                              ��  t�      ��                     "  ��              ��e                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ��           ��                            ����                            viewObject                              ��  ��      ��                  $  %  Є              H�e                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  k      (�      T�  O 
 \      LOGICAL,    assignLinkProperty  4�      `�      ��  P  g      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   t�      �      �  Q  z      CHARACTER,  getChildDataKey ��      (�      X�  R  �      CHARACTER,  getContainerHandle  8�      d�      ��  S  �      HANDLE, getContainerHidden  x�      ��      Ԇ  T  �      LOGICAL,    getContainerSource  ��      ��      �  U  �      HANDLE, getContainerSourceEvents    �      �      X�  V  �      CHARACTER,  getContainerType    8�      d�      ��  W  �      CHARACTER,  getDataLinksEnabled x�      ��      ؇  X  �      LOGICAL,    getDataSource   ��      �      �  Y        HANDLE, getDataSourceEvents �      �      P�  Z        CHARACTER,  getDataSourceNames  0�      \�      ��  [  1      CHARACTER,  getDataTarget   p�      ��      ̈  \  D      CHARACTER,  getDataTargetEvents ��      ؈      �  ]  R      CHARACTER,  getDBAware  �      �      D�  ^ 
 f      LOGICAL,    getDesignDataObject $�      P�      ��  _  q      CHARACTER,  getDynamicObject    d�      ��      ĉ  `  �      LOGICAL,    getInstanceProperties   ��      Љ      �  a  �      CHARACTER,  getLogicalObjectName    �      �      L�  b  �      CHARACTER,  getLogicalVersion   ,�      X�      ��  c  �      CHARACTER,  getObjectHidden l�      ��      Ȋ  d  �      LOGICAL,    getObjectInitialized    ��      Ԋ      �  e  �      LOGICAL,    getObjectName   �      �      H�  f  �      CHARACTER,  getObjectPage   (�      T�      ��  g        INTEGER,    getObjectParent d�      ��      ��  h        HANDLE, getObjectVersion    ��      ȋ      ��  i  $      CHARACTER,  getObjectVersionNumber  ܋      �      @�  j  5      CHARACTER,  getParentDataKey     �      L�      ��  k  L      CHARACTER,  getPassThroughLinks `�      ��      ��  l  ]      CHARACTER,  getPhysicalObjectName   ��      ̌      �  m  q      CHARACTER,  getPhysicalVersion  �      �      D�  n  �      CHARACTER,  getPropertyDialog   $�      P�      ��  o  �      CHARACTER,  getQueryObject  d�      ��      ��  p  �      LOGICAL,    getRunAttribute ��      ̍      ��  q  �      CHARACTER,  getSupportedLinks   ܍      �      <�  r  �      CHARACTER,  getTranslatableProperties   �      H�      ��  s  �      CHARACTER,  getUIBMode  d�      ��      ��  t 
 �      CHARACTER,  getUserProperty ��      Ȏ      ��  u  	      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ؎       �      X�  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles 8�      ��      ��  w  '	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      Џ       �  x  3	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      <�      h�  y  @	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   H�      Ԑ      �  z  L	      CHARACTER,INPUT piMessage INTEGER   propertyType    �      (�      X�  {  Z	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  8�      ��      ��  |  g	      CHARACTER,  setChildDataKey ��      ��      �  }  v	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ̑      �      H�  ~  �	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  (�      h�      ��    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    |�      ��      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ؒ      �      P�  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   0�      x�      ��  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ȓ      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ܓ      $�      X�  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   8�      ��      ��  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      Ԕ      �  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      ,�      X�  � 
 0
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject 8�      x�      ��  �  ;
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      ԕ      �  �  O
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      $�      \�  �  `
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    <�      ��      ��  �  v
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      Ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      ,�      \�  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent <�      |�      ��  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      ̗       �  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      (�      \�  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks <�      ��      ��  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ؘ      �  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      0�      d�  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute D�      ��      ��  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      ��      �  �  *      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      8�      t�  �  <      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  T�      ��      Ě  � 
 V      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  a      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      T�      ��  �  q      LOGICAL,INPUT pcMessage CHARACTER   Signature   `�      ��      Л  � 	 }      CHARACTER,INPUT pcName CHARACTER    Ԟ    ;  �  ��      �       4   �����                 ��                      ��                  <  i                  �.f                           <   �        =  ��  <�      �       4   �����                 L�                      ��                  >  h                   /f                           >  ̜  P�    U  h�  �      �       4   �����                 ��                      ��                  a  c                  �/f                           a  x�         b                                  ,     
                    � ߱        |�  $  e  $�  ���                           $  g  ��  ���                       x                         � ߱        �    m  �  p�      �      4   �����                ��                      ��                  n  2	                  X0f                           n   �  ��  o   q      ,                                 �  $   r  ��  ���                       �  @         �              � ߱         �  �   s        4�  �   t  �      H�  �   v        \�  �   x  x      p�  �   z  �      ��  �   |  `      ��  �   }  �      ��  �   ~        ��  �   �  �      Ԡ  �   �         �  �   �  |      ��  �   �  �      �  �   �  t      $�  �   �  �      8�  �   �  ,      L�  �   �  �      `�  �   �  �      t�  �   �  P	      ��  �   �  �	      ��  �   �   
      ��  �   �  t
      ġ  �   �  �
      ء  �   �  l      �  �   �  �       �  �   �  \      �  �   �  �      (�  �   �  D      <�  �   �  �      P�  �   �  �      d�  �   �  0      x�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  X      Ȣ  �   �  �      ܢ  �   �        �  �   �  L      �  �   �  �      �  �   �  �      ,�  �   �         @�  �   �  <      T�  �   �  x      h�  �   �  �      |�  �   �  �          �   �  ,                      ��          �   �      ��                  Y	  �	  0�              f                        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        ؤ  $ m	  H�  ���                           O   �	  ��  ��  h               D�          4�  <�    $�                                             ��                            ����                                �;      ��      �     V     L�                       H�  �                     ��    �	  �  ��      t      4   ����t                ��                      ��                  �	  .
                  \9f                           �	  �  ��  �   �	  �      ��  �   �	  H      Ц  �   �	  �      �  �   �	  @      ��  �   �	  �      �  �   �	  8       �  �   �	  �      4�  �   �	  (      H�  �   �	  �      \�  �   �	         p�  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �    ^
  ȧ  H�      x      4   ����x                X�                      ��                  _
  �
                  4{f                           _
  ا  l�  �   a
  �      ��  �   b
  T      ��  �   c
  �      ��  �   d
  D      ��  �   e
  �      Ш  �   f
  �      �  �   h
  p      ��  �   i
  �      �  �   j
  X       �  �   k
  �      4�  �   l
  �      H�  �   m
  D       \�  �   n
  �       p�  �   o
  �       ��  �   p
  x!      ��  �   q
  �!      ��  �   r
  h"      ��  �   s
  �"      ԩ  �   t
  `#      �  �   u
  �#      ��  �   v
  X$      �  �   w
  �$      $�  �   x
  �$      8�  �   y
  L%      L�  �   z
  �%      `�  �   {
  <&      t�  �   |
  �&      ��  �   }
  4'      ��  �   ~
  �'      ��  �   
  ,(      Ī  �   �
  h(      ت  �   �
  �(      �  �   �
  X)       �  �   �
  �)      �  �   �
  *      (�  �   �
  �*      <�  �   �
  �*      P�  �   �
  l+      d�  �   �
  �+      x�  �   �
  \,      ��  �   �
  �,      ��  �   �
  L-      ��  �   �
  �-      ȫ  �   �
  <.      ܫ  �   �
  �.      �  �   �
  4/      �  �   �
  �/          �   �
  $0      ��      4�  ��      T0      4   ����T0                Ĭ                      ��                    �                  0~f                             D�  ج  �     �0      �  �     (1       �  �     �1      �  �     2      (�  �     �2      <�  �     3      P�  �     |3      d�  �     �3      x�  �     t4      ��  �     �4      ��  �     l5      ��  �     �5      ȭ  �     d6      ܭ  �     �6      �  �     L7      �  �     �7      �  �      <8      ,�  �   !  �8      @�  �   "  ,9      T�  �   #  �9      h�  �   $  :      |�  �   %  X:      ��  �   &  �:      ��  �   '  H;      ��  �   (  �;      ̮  �   )  8<      �  �   *  �<          �   +  (=      �    �  �  ��      �=      4   �����=  	              ��                      ��             	     �  L                  |Lf                           �   �  ��  �   �  �=      ȯ  �   �  t>      ܯ  �   �  �>      �  �   �  l?      �  �   �  �?      �  �   �  \@      ,�  �   �  �@      @�  �   �  TA      T�  �   �  �A      h�  �   �  DB      |�  �   �  �B      ��  �   �  <C      ��  �   �  �C      ��  �   �  ,D      ̰  �   �  �D      �  �   �  $E      ��  �   �  �E      �  �   �  F      �  �   �  �F      0�  �   �  G      D�  �   �  �G      X�  �   �  �G      l�  �   �  8H      ��  �   �  �H      ��  �   �  0I      ��  �   �  �I      ��  �   �  (J      б  �   �  �J          �   �  K      getRowObjUpdStatic  deleteRecordStatic  ��      (�  8�      �K      4   �����K      /     d�     t�                          3   �����K            ��                      3   �����K  p�      ��  @�  ��  �K      4   �����K  
              P�                      ��             
       n                  �Pf                             в  d�  �     4L      ��  $    ��  ���                       `L     
                    � ߱        г  �     �L      (�  $     ��  ���                       �L  @         �L              � ߱        �  $    T�  ���                       �L       	       	           � ߱        N     
                �N                     �O  @        
 �O              � ߱        t�  V   !  ��  ���                        �O       	       	       P       
       
       TP       	       	           � ߱        �  $  =  �  ���                       Q     
                �Q                     �R  @        
 �R              � ߱            V   O  ��  ���                                      h�                      ��                  p                    �Rf                           p  0�  �R     
                hS                     �T  @        
 xT           U  @        
 �T          �U  @        
 @U          �U  @        
 �U              � ߱            V   �  ��  ���                        adm-clone-props ��  ��              �     W     l                          h  �                     start-super-proc    ��   �  �           �     X     (                          $  �                     �    %  ��  ��      lY      4   ����lY      /   &  ȸ     ظ                          3   ����|Y            ��                      3   �����Y  `�  $   @  4�  ���                       �Y                         � ߱         �    P  |�  ��  ��  �Y      4   �����Y                p�                      ��                  Q  U                  Ęb                           Q  ��  �Y                      Z                     Z                         � ߱            $  R  �  ���                             V  ��  ��      ,Z      4   ����,Z  LZ                         � ߱            $  W  Ⱥ  ���                       �    ^  <�  L�  ��  `Z      4   ����`Z      $  _  x�  ���                       �Z                         � ߱            �   |  �Z      �Z     
                P[                     �\  @        
 `\              � ߱        H�  V   �  ��  ���                        \�  �   �  �\      X�    B  x�  ��      �\      4   �����\      /   C  ��     ļ                          3   �����\            �                      3   ����]  <]     
                �]                     _  @        
 �^              � ߱        �  V   O  ��  ���                        T_     
                �_                      a  @        
 �`              � ߱        �  V   s  ��  ���                        ��    �  0�  ��      4a      4   ����4a                ��                      ��                  �  �                  <�b                           �  @�  ,�  /   �  �     ��                          3   ����Da            �                      3   ����da      /   �  X�     h�                          3   �����a            ��                      3   �����a  ��  /  `  Ŀ         �a                      3   �����a  initProps   �  Կ              4     Y     �                       �  h  	                                   �          ��  ��      ��                �  �  ��              �oa                        O   ����    e�          O   ����    R�          O   ����    ��      r                      ��          ��  p   �  �|  8�      �  8�  ��     �|                ��                      ��                  �  �                  dqa                           �  H�  ��  :  �                 $  �  �  ���                       �|                         � ߱        ��  ��     �|                                        ��                  �                    ,�a                           �  H�  X�  H�      }                                        ��                    $                   �a                             ��  ��  ��     }                                        ��                  %  A                  Ԅa                           %  h�  x�  h�     (}                                        ��                  B  ^                  ��a                           B  ��  �  ��     <}                                        ��                  _  {                  �~a                           _  ��  ��  ��     P}                                        ��                  |  �                  xa                           |  �  (�  �     d}                                        ��                  �  �                  L�a                           �  ��  ��  ��     x}  	                                      ��             	     �  �                   �a                           �  8�  H�  8�     �}  
                                      ��             
     �  �                  �a                           �  ��  ��  ��     �}                                        ��                  �                    �a                           �  X�  h�  X�     �}                                        ��                    )                  ��a                             ��  ��  ��     �}                                        ��                  *  F                  ��a                           *  x�  ��  x�     �}                                        ��                  G  c                  ��a                           G  �  �  �     �}                                        ��                  d  �                  ��a                           d  ��  ��  ��     ~                                        ��                  �  �                  h�a                           �  (�  8�  (�     ~                                        ��                  �  �                  <�a                           �  ��      ��     ,~                                        ��                  �  �                  �a                           �  H�      O   �  ��  ��  @~               L�          4�  @�   , �                                                       �     ��                            ����                            �  ��   �  <�      ��     Z     T�                      � P�  �                     ��    �  �  ��      L~      4   ����L~                ��                      ��                  �                    ��a                           �  �  �  /   �  ��     ��                          3   ����\~            ��                      3   ����|~  t�  /   �  4�     D�                          3   �����~            d�                      3   �����~  ��  /   �  ��     ��                          3   �����~            ��                      3   �����~      /   �  �     �                          3   ����            <�                      3   ����0  P     
                �                     �  @        
 ܀              � ߱        ��  V   �  L�  ���                        ��  $  �  �  ���                       0�                         � ߱        T�     
                Ё                      �  @        
 ��              � ߱        ��  V   �  4�  ���                        ��  $  �  ��  ���                       ,�     
                    � ߱        @�     
                ��                     �  @        
 ̄              � ߱        ��  V   �  �  ���                        h�  $  �  ��  ���                       �     
                    � ߱        ,�     
                ��                     ��  @        
 ��              � ߱        ��  V   �  �  ���                        P�  $    ��  ���                       �                         � ߱        8�     
                ��                     �  @        
 Ĉ              � ߱        |�  V     ��  ���                        ��  �   ,  �      L�  $  -  ��  ���                       <�     
                    � ߱        P�     
                ̉                     �  @        
 ܊              � ߱        x�  V   7  ��  ���                        ��  $  Q  ��  ���                       (�     
                    � ߱        ��  �   k  <�      <�  $  u  �  ���                       |�     
                    � ߱        P�  �   �  ��      ��  $   �  |�  ���                       Ћ                         � ߱              �  ��  ��      �      4   �����      /   �   �     �                          3   �����  @�     
   0�                      3   ����,�  p�        `�                      3   ����4�  ��        ��                      3   ����H�            ��                      3   ����d�  pushRowObjUpdTable  ��  ��  �                   [      �                               �"                     pushTableAndValidate    ��  @�  �           �     \     �                          �  
#                     remoteCommit    X�  ��  �           t     ]                                �  U#                     serverCommit    ��   �  �           p     ^     �                          �  b#                                     D�          �  ��      ��                  �  �  ,�              ( '                        O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  ��    ��                            ����                            0�  �      ��              _      \�                      
�     o#                     disable_UI  ��  ��                      `      �                               �#  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����       ��  �      viewObject  ,   ��  �  $�      toggleData  ,INPUT plEnabled LOGICAL    �  P�  h�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  @�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  0�  <�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE  �  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �  0�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��   �      editInstanceProperties  ,   ��  �  $�      displayLinks    ,   �  8�  H�      createControls  ,   (�  \�  l�      changeCursor    ,INPUT pcCursor CHARACTER   L�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  8�  D�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER (�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  �      unbindServer    ,INPUT pcMode CHARACTER ��  0�  @�      runServerObject ,INPUT phAppService HANDLE   �  l�  ��      disconnectObject    ,   \�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ,�  8�      startFilter ,   �  L�  \�      releaseDBRow    ,   <�  p�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   `�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  �  ,�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  \�  l�      compareDBRow    ,   L�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   p�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  @�  L�      updateState ,INPUT pcState CHARACTER    0�  x�  ��      updateQueryPosition ,   h�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��  ��  �      undoTransaction ,   ��  �  ,�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  $�  8�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  t�  �  �      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  `�  t�      startServerObject   ,   P�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   x�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  �  (�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  $�  4�      rowObjectState  ,INPUT pcState CHARACTER    �  `�  p�      retrieveFilter  ,   P�  ��  ��      restartServerObject ,   t�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  H�  X�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  8�  ��  ��      initializeServerObject  ,   x�  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��  ��  �      genContextList  ,OUTPUT pcContext CHARACTER ��  4�  @�      fetchPrev   ,   $�  T�  `�      fetchNext   ,   D�  t�  ��      fetchLast   ,   d�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��  �  ,�      destroyServerObject ,   �  @�  P�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    0�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ,�  @�      commitTransaction   ,   �  T�  d�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    D�  ��  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
   %     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� ,   J   %               � 
" 	   
   %              h �P  \         (          
�                          
�            � �    
" 	   
   
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
   �               1� �  
   � �   �%               o%   o           � �    #
"   
   �           �    1� �     � �   �%               o%   o           � �   #
"   
   �           �    1� �  
   � �   �%               o%   o           � �   #
"   
   �           l    1� �     � �   �%               o%   o           � �    #
"   
   �           �    1� �     � �   �%               o%   o           � �   #
"   
   �           T    1� 
     �    �%               o%   o           %               
"   
   �          �    1�      � .     
"   
   �               1� 5     � �   �%               o%   o           � H  #
"   
   �           �    1� J     � �   �%               o%   o           � Y  S #
"   
   �           �    1� �     �    �%               o%   o           %               
"   
   �           p    1� �     �    �%               o%   o           %               
"   
   �           �    1� �     �    �%               o%   o           %              
"   
   �          h    1� �     �      
"   
   �           �    1� �  
   �    �%               o%   o           %               
"   
   �                1� �     � �   �%               o%   o           � �    #
"   
   �          �    1� �     � .     
"   
   �           �    1�      � �   �%               o%   o           � $  t #
"   
   �          D	    1� �  
   � .     
"   
   �           �	    1� �     � �   �%               o%   o           � �  � #
"   
   �           �	    1� B     � �   �%               o%   o           � �    #
"   
   �           h
    1� Y  
   � d   �%               o%   o           %               
"   
   �           �
    1� h     �    �%               o%   o           %              
"   
   �           `    1� p     � �   �%               o%   o           � �    f
"   
   �           �    1� �     � �   �%               o%   o           o%   o           
"   
   �           P    1� �  
   � �   �%               o%   o           � �    f
"   
   �           �    1� �     � �  	 �%               o%   o           � �  / f
"   
   �          8    1� �     � �  	   
"   
   �           t    1� �     � �  	 �o%   o           o%   o           � �    f
"   
   �          �    1�      � �  	   
"   
   �           $    1�      � �  	 �o%   o           o%   o           � �    f
"   
   �          �    1� +     �      
"   
   �          �    1� 9     � �  	   
"   
   �              1� F     � �  	   
"   
   �          L    1� S     � �  	   
"   
   �           �    1� a     �    �o%   o           o%   o           %              
"   
   �              1� r     � �  	   
"   
   �          @    1� �  
   � �     
"   
   �          |    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          0    1� �     � �  	   
"   
   �          l    1� �  	   � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �                1�      � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            �� "     p�               �L
�    %              � 8          � $         � )          
�    � C     
"   
   � @  , 
�           �� �  
   p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� F  
   � �   �%               o%   o           � �    f
"   
   �           <    1� Q  
   � �   �%               o%   o           o%   o           
"   
   �           �    1� \     � .   �%               o%   o           o%   o           
"   
   �           4    1� e     �    �%               o%   o           %               
"   
   �           �    1� t     �    �%               o%   o           %               
"   
   �           ,    1� �     � �   �%               o%   o           � �    f
"   
   �           �    1� �     �    �%               o%   o           %              
"   
   �               1� �     �    �%               o%   o           o%   o           
"   
   �           �    1� �     � �   �%               o%   o           o%   o           
"   
   �               1� �  	   � �   �%               o%   o           � �    f
"   
   �           �    1� �     � �   �%               o%   o           o%   o           
"   
   �               1� �     � �   �%               o%   o           o%   o           
"   
   �           �    1� �     �    �%               o%   o           %               
"   
   �           �    1� �     �    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� �  
   �    �%               o%   o           %              
"   
   �           H    1�      � �   �%               o%   o           o%   o           
"   
   �           �    1�      � �   �%               o%   o           � �    f
"   
   �           8    1� "     � �   �%               o%   o           o%   o           
"   
   �          �    1� .     � .     
"   
   �           �    1� ;     � �   �%               o%   o           � N  ! f
"   
   �           d    1� p     � �   �%               o%   o           � �    f
"   
   �           �    1� }     � �   �%               o%   o           � �   f
"   
   �          L    1� �     � �     
"   
   �          �    1� �     � .     
"   
   �           �    1� �     � �   �%               o%   o           � �    f
"   
   �          8     1� �  
   � .     
"   
   �           t     1� �     �    �%               o%   o           o%   o           
"   
   �           �     1� �     �    �%               o%   o           %               
"   
   �           l!    1� �     �    �%               o%   o           %               
"   
   �           �!    1� 	     � �   �%               o%   o           � �    f
"   
   �           \"    1�      � �   �%               o%   o           o%   o           
"   
   �           �"    1� %     �    �%               o%   o           %              
"   
   �           T#    1� 6     �    �%               o%   o           %               
"   
   �           �#    1� C     �    �%               o%   o           %               
"   
   �          L$    1� S     � .     
"   
   �          �$    1� `     � �     
"   
   �           �$    1� m     � d   �%               o%   o           o%   o           
"   
   �           @%    1� y     � �   �%               o%   o           � �    f
"   
   �           �%    1� �     � �   �%               o%   o           o%   o           
"   
   �           0&    1� �     �    �o%   o           o%   o           o%   o           
"   
   �           �&    1� �     � �  	 �%               o%   o           o%   o           
"   
   �           ('    1� �     � �   �%               o%   o           o%   o           
"   
   �           �'    1� �  
   � d   �%               o%   o           o%   o           
"   
   �           (    1� �     � �     
"   
   �           \(    1� �     � �   �%               o%   o           � �  4 f
"   
   �           �(    1� 0  
   �    �%               o%   o           %              
"   
   �          L)    1� ;     � .     
"   
   �           �)    1� L     � �   �%               o%   o           � �    f
"   
   �           �)    1� Z     �    �%               o%   o           %              
"   
   �           x*    1� i     � �   �%               o%   o           � �    f
"   
   �           �*    1� v     � �   �%               o%   o           � �    f
"   
   �           `+    1� �     � �   �%               o%   o           � �    f
"   
   �           �+    1� �     �    �%               o%   o           %               
"   
   �           P,    1� �  	   � .   �%               o%   o           o%   o           
"   
   �           �,    1� �     � �   �%               o%   o           � �  	 f
"   
   �           @-    1� �     � d   �%               o%   o           %       �       
"   
   �           �-    1� �     � �   �%               o%   o           � �    f
"   
   �           0.    1� �     �    �o%   o           o%   o           %              
"   
   �           �.    1� �     �    �%               o%   o           %               
"   
   �           (/    1� �     � �   �%               o%   o           o%   o           
"   
   �           �/    1�      � �  	 �%               o%   o           � �    f
"   
   �          0    1�       � �  	   P �L 
�H T   %              �     }        �GG %              
"   
   �           �0    1� -  
   � �   �%               o%   o           � �    f
"   
   �           1    1� 8     �    �%               o%   o           %               
"   
   �           �1    1� E  	   � �   �%               o%   o           � �    f
"   
   �           2    1� O     � �   �%               o%   o           � �    f
"   
   �           �2    1� ]     �    �%               o%   o           %               
"   
   �           �2    1� m     � �   �%               o%   o           � �    f
"   
   �           p3    1� �     � �   �%               o%   o           o%   o           
"   
   �           �3    1� �     � �   �%               o%   o           o%   o           
"   
   �           h4    1� �     �    �%               o%   o           o%   o           
"   
   �           �4    1� �     �    �%               o%   o           o%   o           
"   
   �           `5    1� �     �    �%               o%   o           o%   o           
"   
   �           �5    1� �     � �   �%               o%   o           o%   o           
"   
   �           X6    1� �  	   � �  	 �%               o%   o           � �    f
"   
   �           �6    1� �  
   � �  	 �%               o%   o           � �    f
"   
   �           @7    1� �     � �   �%               o%   o           � �    f
"   
   �           �7    1� �     � �   �%               o%   o           o%   o           
"   
   �           08    1�      � �   �%               o%   o           o%   o           
"   
   �           �8    1�      � �   �%               o%   o           � �    f
"   
   �            9    1� '     � �   �%               o%   o           � �    f
"   
   �           �9    1� 6     � �  	 �%               o%   o           o%   o           
"   
   �          :    1� H     � .     
"   
   �           L:    1� T     � �   �%               o%   o           � �    f
"   
   �           �:    1� b     � �   �%               o%   o           o%   o           
"   
   �           <;    1� u     �    �%               o%   o           o%   o           
"   
   �           �;    1� �  
   � �   �%               o%   o           � �    f
"   
   �           ,<    1� �     � �   �%               o%   o           � �    f
"   
   �           �<    1� �     �    �%               o%   o           %               
"   
   �           =    1� �     � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �=    1� �  	   � .   �%               o%   o           o%   o           
"   
   �           h>    1� �     � .   �%               o%   o           o%   o           
"   
   �           �>    1� �     � .   �%               o%   o           o%   o           
"   
   �           `?    1� �     �    �%               o%   o           %              
"   
   �           �?    1� 	     � �   �%               o%   o           � "  M f
"   
   �           P@    1� p     �    �%               o%   o           %              
"   
   �           �@    1� �     �    �%               o%   o           %               
"   
   �           HA    1� �     �    �%               o%   o           %               
"   
   �           �A    1� �     � �  	 �%               o%   o           � �   f
"   
   �           8B    1� �     �    �%               o%   o           %               
"   
   �           �B    1� �     � �  	 �%               o%   o           o%   o           
"   
   �           0C    1� �     �    �o%   o           o%   o           %              
"   
   �           �C    1� �     � �  	 �o%   o           o%   o           � �    f
"   
   �            D    1�      � .   �o%   o           o%   o           o%   o           
"   
   �           �D    1�      � .   �o%   o           o%   o           o%   o           
"   
   �           E    1� .     � �  	 �o%   o           o%   o           o%   o           
"   
   �           �E    1� >     � .   �o%   o           o%   o           o%   o           
"   
   �           F    1� M     � �  	 �o%   o           o%   o           � [   f
"   
   �           �F    1� ]     � �  	 �o%   o           o%   o           � l   f
"   
   �           �F    1� x     �    �%               o%   o           %               
"   
   �           tG    1� �     �    �%               o%   o           %               
"   
   �          �G    1� �     � �  	   
"   
   �           ,H    1� �     �    �%               o%   o           %               
"   
   �           �H    1� �     � �   �%               o%   o           o%   o           
"   
   �           $I    1� �     � �   �%               o%   o           o%   o           
"   
   �           �I    1� �     �    �%               o%   o           o%   o           
"   
   �           J    1� �     � �   �%               o%   o           � �    f
"   
   �           �J    1� 	     �    �%               o%   o           %               
"   
   �           K    1�   	   �    �%               o%   o           %                "      %     start-super-proc ��%     adm2/smart.p � P �L 
�H T   %              �     }        �GG %              
"   
   �       (L    6�      
"   
   
�        TL    8
"   
   �        tL    ��     }        �G 4              
"   
   G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        XN    ��    � P   �        dN    �@    
� @  , 
�       pN    �� "     p�               �L
�    %              � 8      |N    � $         � )          
�    � C    
"   
   p� @  , 
�       �O    �� 5     p�               �L"  	    �   � U   f� W   ��     }        �A      |    "  	    � U   f%              (<   \ (    |    �     }        �A� Y   �A"  
        "  	    "  
      < "  	    "  
    (    |    �     }        �A� Y   �A"  
    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        `Q    ��    � P   �        lQ    �@    
� @  , 
�       xQ    �� "     p�               �L
�    %              � 8      �Q    � $         � )          
�    � C    
"   
   p� @  , 
�       �R    �� �  
   p�               �L"  	    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        8S    ��    � P   �        DS    �@    
� @  , 
�       PS    �� "     p�               �L
�    %              � 8      \S    � $         � )          
�    � C     
"   
   p� @  , 
�       lT    �� �  
   p�               �L%     SmartDataObject 
"   
   p� @  , 
�       �T    �� �     p�               �L%               
"   
   p� @  , 
�       4U    ��      p�               �L%               
"   
   p� @  , 
�       �U    �� �     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
    (   � 
"   
       �        tV    ��    �
"   
   � 8      �V    � $         � )          
�    � C    
"   
   �        W    �
"   
   �       8W    /
"   
   
"   
   �       dW    6�      
"   
   
�        �W    8
"   
   �        �W    �
"   
   �       �W    �
"   
   p�    � �   f
�    �     }        �G 4              
"   
   G %              G %              
�     }        �
"   
    (   � 
"   
       �        �X    �A"      
"   
   
�        �X    �@ � 
"   
   "      �       }        �
"   
   %              %                "      %     start-super-proc ��%     adm2/appserver.p +f�    �      
�    �     }        �%               %      Server  - �     }        �    "      � �    �%                   "      � �    �%      NONE    p�,  8         $     "              �     
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �         [    ��    � P   �        ,[    �@    
� @  , 
�       8[    �� "     p�               �L
�    %              � 8      D[    � $         � )          
�    � C    
"   
   p� @  , 
�       T\    �� �     p�               �L"      p�,  8         $     "              � +    
�     "      %     start-super-proc ��%     adm2/dataquery.p Cf
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �]    ��    � P   �        �]    �@    
� @  , 
�       �]    �� "     p�               �L
�    %              � 8      �]    � $         � )         
�    � C    
"   
   p� @  , 
�       �^    �� �     p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �_    ��    � P   �        �_    �@    
� @  , 
�       �_    �� "     p�               �L
�    %              � 8      �_    � $         � )         
�    � C    
"   
   p� @  , 
�       �`    �� 
     p�               �L%               "      %     start-super-proc ��%     adm2/query.p � %     start-super-proc ��%     adm2/queryext.p % 	    initProps  
�    %< 1 ,   FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION f�   � �     � �     �       
�     	         �G
"   
   �        \b    �G
"   
   
"   
    x    (0 4      �        |b    �G%                   �        �b    �GG %              � �     � �         %              %                   "      %              
"   
       "      �        xc    �
"   
   �        �c    �
"   
   
�       �c    �"       \      H   "      ((       "      %              � �      � �         
"   
   
"   
    \      H   "      ((       "      %              � �     � �   f�        pd    �%                   %              %                   "  (    %                  "  (        
"   
   
"   
   0 T       m � "  (    �        |e    �A @   "       $         � "  (    � Y   ��        �e    �� "  (    
"   
    \ H     H   "      ((       "      %              � �    �� �     (        "  !    � �    f�        0f    �"  !    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        4g    ��    � P   �        @g    �@    
� @  , 
�       Lg    �� "     p�               �L
�    %              � 8      Xg    � $         � )          
�    � C     
"   
   p� @  , 
�       hh    �� �     p�               �L%               
"   
   p� @  , 
�       �h    ��      p�               �L"      �,  8         $     "              � �  
  
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �i    ��    � P   �        �i    �@    
� @  , 
�       �i    �� "     p�               �L
�    %              � 8      �i    � $         � )         
�    � C     
"   
   p� @  , 
�       �j    �� H     p�               �L
"   
   
"   
   p� @  , 
�       8k    �� '     p�               �L"      
"   
   p� @  , 
�       �k    �� �     p�               �L"          "      � �    �%T H D   OPEN QUERY Query-Main FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION. �     "      � �   O-((        "      %                   "      � �     "       (   "           "      %              @ �,  8         $     "              � �    
�    p�,  8         $     � �   f        �     
�    %               �    "      � �         %              %                   "      %                  "      "      "      T(        "      %              "      � �   �"      �       "      �    "      � Y   �� �      � Y    �    "      � Y    S    "      "          "      %                � @    �     t T     P   4         "      (0       4         "      � �      � �     � �   fT ,  %              T   "      "      � �     � Y    � �   fT    �    "      � Y   �"      � Y    "      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "      %              � �    ��      4         "      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        r    ��    � P   �        r    �@    
� @  , 
�       $r    �� "     p�               �L
�    %              � 8      0r    � $         � )          
�    � C    
"   
   p� @  , 
�       @s    �� -  
   p�               �L"            "  
    �    �   Mf� �   �      "  	    �    �   M�� �   f�   � �     � �     �   M �   � �     � �    �   Mf�   � �     � �     �   M  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �t    ��    � P   �        �t    �@    
� @  , 
�       �t    �� "     p�               �L
�    %              � 8      �t    � $         � )          
�    � C     
"   
   p� @  , 
�       v    �� �     p�               �L"      
"   
   p� @  , 
�       \v    �� �     p�               �L"      
"   
   p� @  , 
�       �v    �� m     p�               �L"          %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    �   M  � �         "  	    �     "      T    "      "      @ A,    �   � �   ��      "      "       T      @   "      (        "      � �     � �      � �    "           "  	    %              D H   @ A,    �   � �    �      "      "      ,    S   "      �   Ma� �   �%                T      @   "      (        "      � �     � �      � �    "           "  
    %                         "      �      "                 "      �     "      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �z    ��    � P   �        �z    �@    
� @  , 
�       �z    �� "     p�               �L
�    %              � 8      �z    � $         � )         
�    � C   �
"   
   p� @  , 
�       |    �� �     p�               �L"      
"   
   p� @  , 
�       \|    �� m     p�               �L"      "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "      %     start-super-proc ��%     adm2/data.p %     start-super-proc ��%     adm2/dataext.p %     start-super-proc ��%     adm2/dataextcols.p %     start-super-proc ��%     adm2/dataextapi.p f
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    �� "     p�               �L
�    %              � 8      �    � $         � )         
�    � C    
"   
   p� @  , 
�       Ѐ    �� �     p�               �L%               %      ".\dItemPalletSap.i" ��
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    �� "     p�               �L
�    %              � 8      ā    � $         � )          
�    � C    
"   
   p� @  , 
�       Ԃ    �� �     p�               �L"      
�     	        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    �� "     p�               �L
�    %              � 8      ��    � $         � )          
�    � C    
"   
   p� @  , 
�       ��    �� �  
   p�               �L
"   
   
�     
        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        x�    ��    � P   �        ��    �@    
� @  , 
�       ��    �� "     p�               �L
�    %              � 8      ��    � $         � )          
�    � C    
"   
   p� @  , 
�       ��    �� �  	   p�               �L
"   
   
"   
        � �"  	   �        �    �
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    �� "     p�               �L
�    %              � 8      ��    � $         � )          
�    � C    
"   
   p� @  , 
�       ��    �� 	     p�               �L"      
"   
   �       �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    �� "     p�               �L
�    %              � 8      ��    � $         � )          
�    � C    
"   
   p� @  , 
�       Њ    �� �  	   p�               �L
"   
   
�             �Gp�,  8         $     
"   
           � �"    
�    
�             �Gp�,  8         $     
"   
           � �"    
�    �    � �"     
�        "      � �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � >#     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   p       ��                 }  �  �               �Tf                        O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       (V     
                    � ߱              �  ,  �      �V      4   �����V                �                      ��                  �  �                  �Uf                           �  <  �  �  �  �V            �  �  l      $W      4   ����$W                |                      ��                  �  �                  T`b                           �  �  �  o   �      ,                                 �  �   �  DW      �  �   �  pW      0  $  �    ���                       �W     
                    � ߱        D  �   �  �W      X  �   �  �W      l  �   �  �W          $   �  �  ���                       ,X  @         X              � ߱                     `          8  L   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   p       ��                 �    �               ab                        O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �X     
                    � ߱                  �  �                      ��                   �  �                   �b                          �  8      4   �����X      $  �  �  ���                       �X     
                    � ߱        �    �  <  L       Y      4   ���� Y      /  �  x                               3   ����Y  �  �   �   Y          O     ��  ��  XY                               , �                          
                               �      ��                            ����                                            �           �   p       ��            	     l  �  �               �b                        O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �a                         � ߱        �  $  �  <  ���                       b                         � ߱        Hb     
                �b  @         hb              � ߱        L  $   �  h  ���                         \      �  �                      ��        0         �  �                  )a      �c         `     �  �      $  �  �  ���                        c                         � ߱          $  �  �  ���                       Pc                         � ߱            4   �����c  �c     
                �c                     |d                         � ߱          $  �    ���                                     ,                      ��                  �  �                  <=a                    �     �  �  �  $  �  X  ���                       �d       !       !           � ߱                \  �                      ��        0         �  �                  >a     ( xe                �  �      $  �  0  ���                       e       (       (           � ߱        �  $  �  �  ���                       8e       (       (           � ߱            4   ����`e        �  �  `      �e      4   �����e                p                      ��                  �  �                  x>a                           �  �  �  $  �  �  ���                        f       !       !           � ߱            O   �  �� ��          $  �    ���                       <f                         � ߱        �f     
                dg                     �h  @        
 th          i  @        
 �h           i                     `i     
                �i                     ,k  @        
 �j          �k  @        
 Dk          �k  @        
 �k              � ߱        �  V   �  8  ���                        d	    �  �  8	      �k      4   �����k  l                     `l                     �l                     �l                         � ߱            $  �  �  ���                       �	    �  �	  �	      m      4   ����m      �   �  \m      �	  $  �  �	  ���                       �m                         � ߱        �
  $  �  (
  ���                       �m                         � ߱          �
      ,  0                      ��        0         �  �                  4Aa      Hn         �     �  T
      $  �     ���                       �m                         � ߱        �  $  �  X  ���                        n                         � ߱            4   ����(n  Tn                     �n                     �n                     �n                     o                         � ߱        \  $  �  �  ���                             �  x  �      8o      4   ����8o      $  �  �  ���                       `o          �p             � ߱        �  $  �    ���                       �p                         � ߱          �        x                      ��        0         �  �                  �^a      ,q         4     �  8      $  �  �  ���                       �p                         � ߱        h  $  �  <  ���                       �p                         � ߱            4   ����q      $  �  �  ���                       @q                         � ߱        �q     
                <r                     �s  @        
 Ls              � ߱        �  V   �  �  ���                        �s       
       
       �s       	       	        t                     ,t                         � ߱          $  6  `  ���                          $  �  8  ���                       Xt                         � ߱        �t     
                 u                     Pv  @        
 v          �v  @        
 hv           w  @        
 �v              � ߱        �  V   �  d  ���                          �        |                      ��        0    	     J  _                  ,ma      �w         \     J  ,      $  J  �  ���                       w                         � ߱        \  $  J  0  ���                       <w                         � ߱        l  4   ����dw      4   �����w  �  $  O  �  ���                       x                         � ߱        �    Q  �  p      $x      4   ����$x                �                      ��                  R  V                  �ma                           R     hx                     �x       	       	           � ߱            $  S  �  ���                             X    �      �x      4   �����x  	              �                      ��             	     Z  ^                  �na                           Z     �y                     �y       
       
           � ߱            $  [  �  ���                       z                     Pz                         � ߱          $  e    ���                       �z     
                 {                     P|  @        
 |          �|  @        
 h|              � ߱            V   s  �  ���                                    J �          �  �  � Xh                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            ]                          A�                                �   p       ��                  O  Z  �               ��&                        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   p       ��                  d  s  �               �&                        O   ����    e�          O   ����    R�          O   ����    ��       #       �              �                  $                  h  /  p  (     8  ��                      3   ������            X                      3   ������      O   q  ��  ��  ��               �          �  �    �                                             ��                            ����                                            L          �   p       ��                  }  �  �               ��&                        O   ����    e�          O   ����    R�          O   ����    ��      #       �              �                $                  )#       0             �          4#                      $         �  /  �  x     �  ،                      3   ������            �                      3   ������    /  �  �     �  �                      3   �����  |          $                  3   �����      $   �  P  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        \  $   �  0  ���                       (�                         � ߱            O   �  ��  ��  D�               �          �  �   @ �                                                              0              0           ��                            ����                                            $          �   p       ��                  �  �  �               ��&                        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  )#                    �          4#                      �              /  �  P     `  t�                      3   ����X�  �        �  �                  3   ����|�      $   �  �  ���                                                   � ߱                                      3   ������      $   �  D  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   p       ��                  �  �  �               ��a                        O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       ��      4   ������      �   �  ��    ��                            ����                            TXS appSrvUtils items_pallets Items Pallets D:\desarrollos\webservices\dItemPalletSap.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource WordIndexedFields RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs ".\dItemPalletSap.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; bultos calibre camara certificado cert_china codigo_trazabilidad cod_prod cod_trazabilidad contramarca con_testigo c_fecha c_hora c_usuario fecha_ingreso fecha_operativa id_articulo id_calidad id_caract id_categoria id_color id_empresa id_envase id_finca_senasa id_lote id_lote_senasa id_marca id_orden id_origen id_packing id_pallet id_proveedor id_punto_emisor id_sucursal_remito id_suc_trabajo id_tipo_movsto id_turno_packing id_variedad item item_factura item_pallet item_remito nro nro_certificado nro_partida_general renspa tipo_fruta tipo_proceso ubicacion unidad_productora zona_up Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p bultos calibre camara certificado cert_china codigo_trazabilidad cod_prod cod_trazabilidad contramarca con_testigo c_fecha c_hora c_usuario fecha_ingreso fecha_operativa id_articulo id_calidad id_caract id_categoria id_color id_empresa id_envase id_finca_senasa id_lote id_lote_senasa id_marca id_orden id_origen id_packing id_pallet id_proveedor id_punto_emisor id_sucursal_remito id_suc_trabajo id_tipo_movsto id_turno_packing id_variedad item item_factura item_pallet item_remito nro nro_certificado nro_partida_general renspa tipo_fruta tipo_proceso ubicacion unidad_productora zona_up RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   �9  @  <H      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   m	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �               �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  6  �  �  J  O  Q  R  S  V  X  Z  [  ^  _  e  s  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �      $  %  A  B  ^  _  {  |  �  �  �  �  �  �  �  �      )  *  F  G  c  d  �  �  �  �  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable  Z  �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    p  q  s  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    �  �  �  �  �  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  �  �  �  �*       <%      D*                      �  P  \  7   RowObject   �         �                                               4         @         T         `         l         t         |         �         �         �         �         �         �         �         �         �                                     (         4         @         L         X         d         t         �         �         �         �         �         �         �         �         �                                    0         8         D         T         `         t         |         �         �         �         �         bultos  calibre camara  certificado cert_china  codigo_trazabilidad cod_prod    cod_trazabilidad    contramarca con_testigo c_fecha c_hora  c_usuario   fecha_ingreso   fecha_operativa id_articulo id_calidad  id_caract   id_categoria    id_color    id_empresa  id_envase   id_finca_senasa id_lote id_lote_senasa  id_marca    id_orden    id_origen   id_packing  id_pallet   id_proveedor    id_punto_emisor id_sucursal_remito  id_suc_trabajo  id_tipo_movsto  id_turno_packing    id_variedad item    item_factura    item_pallet item_remito nro nro_certificado nro_partida_general renspa  tipo_fruta  tipo_proceso    ubicacion   unidad_productora   zona_up RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �  8   RowObjUpd   l"         t"         |"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         #         #         $#         0#         <#         H#         X#         d#         p#         |#         �#         �#         �#         �#         �#         �#         �#         �#         �#          $         $         $$         4$         H$         T$         \$         l$         x$         �$         �$         �$         �$         �$         �$         �$         �$         �$         �$          %         %         %          %         ,%         bultos  calibre camara  certificado cert_china  codigo_trazabilidad cod_prod    cod_trazabilidad    contramarca con_testigo c_fecha c_hora  c_usuario   fecha_ingreso   fecha_operativa id_articulo id_calidad  id_caract   id_categoria    id_color    id_empresa  id_envase   id_finca_senasa id_lote id_lote_senasa  id_marca    id_orden    id_origen   id_packing  id_pallet   id_proveedor    id_punto_emisor id_sucursal_remito  id_suc_trabajo  id_tipo_movsto  id_turno_packing    id_variedad item    item_factura    item_pallet item_remito nro nro_certificado nro_partida_general renspa  tipo_fruta  tipo_proceso    ubicacion   unidad_productora   zona_up RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   \%          P%  
   appSrvUtils �%       p%     xiRocketIndexLimit  �%        �%  
   gshAstraAppserver   �%        �%  
   gshSessionManager   �%        �%  
   gshRIManager     &        &  
   gshSecurityManager  H&        4&  
   gshProfileManager   t&  	 	     \&  
   gshRepositoryManager    �&  
 
     �&  
   gshTranslationManager   �&        �&  
   gshWebManager   �&        �&     gscSessionId    '        �&     gsdSessionObj   0'         '  
   gshFinManager   T'        D'  
   gshGenManager   x'        h'  
   gshAgnManager   �'        �'     gsdTempUniqueID �'        �'     gsdUserObj  �'        �'     gsdRenderTypeObj    (        �'     gsdSessionScopeObj  ((        (  
   ghProp  H(       <(  
   ghADMProps  l(       \(  
   ghADMPropsBuf   �(       �(     glADMLoadFromRepos  �(       �(     glADMOk �(       �(  
   ghContainer �(    	   �(     cObjectName )    
   )     iStart  ,)        )     cAppService L)       @)     cASDivision x)       `)     cServerOperatingMode    �)       �)     cContainerType  �)       �)     cQueryString    �)       �)  
   hRowObject   *       �)  
   hDataQuery   *       *     cColumns             4*     cDataFieldDefs  d*       T*  items_pallets   �*    X  t*  RowObject         X  �*  RowObjUpd            9   �   �   �   �   ;  <  =  >  U  a  b  c  e  g  h  i  m  n  q  r  s  t  v  x  z  |  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  2	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  .
  ^
  _
  a
  b
  c
  d
  e
  f
  h
  i
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  |
  }
  ~
  
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                                         !  "  #  $  %  &  '  (  )  *  +  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  L                    !  =  O  n  p  �    %  &  @  P  Q  R  U  V  W  ^  _  |  �  �  B  C  O  s  �  �  �  �  �  `  �  �  �  �  �  �    �  �  �  �  �  �  �      ,  -  7  Q  k  u  �  �  �  �      ��  "C:\Progress\OpenEdge102b\src\adm2\data.i �.  �) . )C:\Progress\OpenEdge102b\src\adm2\custom\datacustom.i    �.  �� - "C:\Progress\OpenEdge102b\src\adm2\robjflds.i @/  �K  , .\dItemPalletSap.i   x/  �:  "C:\Progress\OpenEdge102b\src\adm2\query.i    �/  z + "C:\Progress\OpenEdge102b\src\adm2\delrecst.i �/  `W * "C:\Progress\OpenEdge102b\src\adm2\tblprep.i  0  F� ) C:\Progress\OpenEdge102b\gui\fnarg   @0   ( )C:\Progress\OpenEdge102b\src\adm2\custom\querycustom.i   p0  �   "C:\Progress\OpenEdge102b\src\adm2\dataquery.i    �0  �Z ' )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerycustom.i   �0  �< ! "C:\Progress\OpenEdge102b\src\adm2\appserver.i    81  �� & )C:\Progress\OpenEdge102b\src\adm2\custom\appservercustom.i   t1  I� " "C:\Progress\OpenEdge102b\src\adm2\smart.i    �1  Ds % C:\Progress\OpenEdge102b\gui\fn  �1  tw $ )C:\Progress\OpenEdge102b\src\adm2\custom\smartcustom.i    2  Q. # C:\Progress\OpenEdge102b\gui\set d2  �>  "C:\Progress\OpenEdge102b\src\adm2\dataprop.i �2  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\datapropcustom.i    �2  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\dataprtocustom.i    3  YO  "C:\Progress\OpenEdge102b\src\adm2\qryprop.i  X3  -�  )C:\Progress\OpenEdge102b\src\adm2\custom\qrypropcustom.i �3  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\qryprtocustom.i �3   	 "C:\Progress\OpenEdge102b\src\adm2\dataqueryprop.i    4  �d  )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerypropcustom.i   X4  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\dataqueryprtocustom.i   �4  �l  "C:\Progress\OpenEdge102b\src\adm2\appsprop.i �4  ɏ  )C:\Progress\OpenEdge102b\src\adm2\custom\appspropcustom.i    (5  V  )C:\Progress\OpenEdge102b\src\adm2\custom\appsprtocustom.i    p5  i$  "C:\Progress\OpenEdge102b\src\adm2\smrtprop.i �5  �j  C:\Progress\OpenEdge102b\gui\get �5  �  )C:\Progress\OpenEdge102b\src\adm2\custom\smrtpropcustom.i    6  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\smrtprtocustom.i    d6  ��  "C:\Progress\OpenEdge102b\src\adm2\smrtprto.i �6  Su  "C:\Progress\OpenEdge102b\src\adm2\globals.i  �6  M�  )C:\Progress\OpenEdge102b\src\adm2\custom\globalscustom.i 7  )a  )C:\Progress\OpenEdge102b\src\adm2\custom\smartdefscustom.i   `7  �  "C:\Progress\OpenEdge102b\src\adm2\appsprto.i �7  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\appserverdefscustom.i   �7  ��  "C:\Progress\OpenEdge102b\src\adm2\dataqueryprto.i    ,8  ª 
 )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerydefscustom.i   l8  ��  "C:\Progress\OpenEdge102b\src\adm2\qryprto.i  �8  �  )C:\Progress\OpenEdge102b\src\adm2\custom\querydefscustom.i   �8  �`  "C:\Progress\OpenEdge102b\src\adm2\dataprto.i 89  �  )C:\Progress\OpenEdge102b\src\adm2\custom\datadefscustom.i    p9  e�  %C:\Progress\OpenEdge102b\gui\adecomm\appserv.i   �9      D:\desarrollos\webservices\dItemPalletSap.w      �   �      ,:  [  �     <:     �  %   L:  �        \:     �  .   l:  �   �     |:     �     �:  �   �     �:     w  #   �:  �   u     �:     S  #   �:  �   Q     �:     /  #   �:  �   ,     �:     
  #   ;  �        ;     �  #   ,;  �   �     <;     �  #   L;  �   �     \;     �  #   l;  �   �     |;     y  #   �;  �   l     �;     T  -   �;  �   P     �;       ,   �;  k   �     �;  �  �     �;     �  +   �;  �  �     <     �  +   <  �  �     ,<     �  +   <<  �  �     L<     f  +   \<  �  c     l<     I  +   |<  �  F     �<     ,  +   �<  �  )     �<       +   �<  �       �<     �  +   �<  �  �     �<     �  +   �<  �  �     =     �  +   =  �  �     ,=     �  +   <=  �  �     L=     ~  +   \=  �  {     l=     a  +   |=  �  ^     �=     D  +   �=  �  A     �=     '  +   �=  �  $     �=     
  +   �=  �       �=     �  +   �=  �  �     >     �  +   >  �  �     ,>     �  #   <>  �  �     L>     k  #   \>  k  F     l>     $  #   |>  j  #     �>       #   �>  i        �>     �  #   �>  _  �     �>     �  *   �>  ^  �     �>     �  *   �>  ]  �     ?     `  *   ?  \  _     ,?     9  *   <?  [  8     L?       *   \?  Z       l?     �  *   |?  Y  �     �?     �  *   �?  X  �     �?     �  *   �?  W  �     �?     v  *   �?  V  u     �?     O  *   �?  U  N     @     (  *   @  T  '     ,@       *   <@  S        L@     �  *   \@  R  �     l@     �  *   |@  Q  �     �@     �  *   �@  P  �     �@     e  *   �@  O  d     �@     >  *   �@  N  =     �@       *   �@  @  	     A     �  #   A  	  �     ,A     �  )   <A  �   �     LA     {  #   \A  �   z     lA     X  #   |A  �   W     �A     5  #   �A  �   4     �A       #   �A  �        �A     �  #   �A  �   �     �A     �  #   �A  �   \     B       (   B  g   �     ,B  a   �      <B     �  '   LB  _   �      \B     k  #   lB  ]   i      |B     G  #   �B  I   3      �B  �   *  !   �B     �  &   �B  �   �  !   �B     �  #   �B  �   �  !   �B     �  #   �B  �   �  !   C     d  #   C  g   J  !   ,C     +     <C  O     !   LC  �   �  "   \C     �  %   lC  �   k  "   |C       $   �C  �     "   �C     �  #   �C  �   �  "   �C     �  #   �C  �   �  "   �C     �  #   �C  �   �  "   �C     }  #   D  �   i  "   D     G  #   ,D  }   ;  "   <D       #   LD     �  "   \D     O  !   lD           |D     �     �D     U     �D  �   L     �D  O   >     �D     -     �D     �     �D  �   �     �D  �   �     �D  O   �     E     ~     E     0     ,E  y        <E  �   �
  	   LE  G   �
     \E     �
     lE     �
     |E  c   6
  	   �E  x   .
     �E  M   
     �E     
     �E     �	     �E  a   �	     �E  �  �	     �E     e	     �E  �  2	     F  O   $	     F     	     ,F     �     <F  �   �     LF     �     \F          lF  x        |F     �     �F     �     �F     |     �F     h     �F     O     �F  Q   ?     �F     �     �F     �     �F     �     G          G  ]   y  	   ,G     o     <G     '  	   LG       
   \G       	   lG  Z   �     |G          �G     �     �G     �     �G     �     �G  c   �     �G     a     �G          �G          �G     �      H     �      H     !       ,H           