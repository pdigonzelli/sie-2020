	��V@[�XL?  D �                                              9� 3F4C00EFutf-8 MAIN D:\desarrollos\qas\dPalletsSap.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,bultos integer 0 0,calibre character 1 0,camara integer 2 0,certificado character 3 0,cert_china character 4 0,china logical 5 0,codigo_trazabilidad character 6 0,cod_prod character 7 0,cod_trazabilidad character 8 0,contramarca character 9 0,c_fecha date 10 0,c_hora character 11 0,c_usuario character 12 0,estado logical 13 0,fecha_anul date 14 0,fecha_anulacion date 15 0,fecha_comp date 16 0,fecha_operativa date 17 0,fecha_peso date 18 0,fecha_prod date 19 0,fecha_proforma date 20 0,gln decimal 21 0,hora_anul character 22 0,hora_peso character 23 0,hora_prod character 24 0,id_articulo integer 25 0,id_calidad integer 26 0,id_caract integer 27 0,id_categoria integer 28 0,id_color integer 29 0,id_empresa integer 30 0,id_envase integer 31 0,id_estado_pallet integer 32 0,id_euroamerica integer 33 0,id_finca_senasa integer 34 0,id_lector_pallet integer 35 0,id_lote integer 36 0,id_lote_senasa integer 37 0,id_marca integer 38 0,id_orden integer 39 0,id_orden_anterior integer 40 0,id_origen integer 41 0,id_packing integer 42 0,id_pallet integer 43 0,id_pallet_anterior integer 44 0,id_pallet_sap character 45 0,id_pedido_sap character 46 0,id_proveedor integer 47 0,id_proveedor_caja integer 48 0,id_punto_emisor integer 49 0,id_punto_venta integer 50 0,id_punto_venta_prof integer 51 0,id_sucursal_remito integer 52 0,id_suc_destino integer 53 0,id_suc_trabajo integer 54 0,id_suc_trabajo_anterior integer 55 0,id_tipo_esquinero integer 56 0,id_tipo_movsto integer 57 0,id_tipo_pallet integer 58 0,id_turno_packing integer 59 0,id_variedad integer 60 0,id_viaje integer 61 0,item integer 62 0,item_pedido_sap character 63 0,item_remito integer 64 0,merma logical 65 0,nro integer 66 0,nromov integer 67 0,nro_comp integer 68 0,nro_comp_terceros character 69 0,nro_orden integer 70 0,nro_pack_list character 71 0,nro_proforma integer 72 0,pallet_chep integer 73 0,pallet_senasa character 74 0,peso decimal 75 0,renspa character 76 0,status_sap integer 77 0,temperatura decimal 78 0,temporada integer 79 0,testigo logical 80 0,tipo_fruta logical 81 0,tipo_proceso character 82 0,ubicacion character 83 0,unidad_productora character 84 0,union_europea logical 85 0,usuario_anul character 86 0,zona_up character 87 0,RowNum integer 88 0,RowIdent character 89 0,RowMod character 90 0,RowIdentIdx character 91 0,RowUserProp character 92 0,ChangedFields character 93 0        �t              @c             5H �t  D�              ��              P     +   �� �  W   �� h  X    � <  Y   <   [   P   \   l @  ]   � $  ^   � 4  `   ? ! 1'  ISO8859-1                                                                        $  <t    �                                      �                   �               |t  |    �   �@   ��              ��  �   �t      �t          �                                             PROGRESS                         �
           
        
                    �              �                                                                                                     
                   produccion                       PROGRESS                         �
        �                                d�W               ��                              �  t                      t  �  �[     ID_PALLETID_EMPRESAID_ORDENID_TURNO_PACKINGITEMID_ARTICULOID_VARIEDADID_MARCAID_ENVASEID_CALIDADID_CARACTCALIBREID_PROVEEDORID_ORIGENID_LOTEFECHA_PRODC_USUARIOC_FECHAC_HORAID_COLORNROMOVID_SUC_TRABAJOID_CATEGORIACOD_PRODBULTOSPESOID_TIPO_MOVSTONROID_VIAJEID_SUCURSAL_REMITOHORA_PRODCONTRAMARCAID_FINCA_SENASAUNIDAD_PRODUCTORACOD_TRAZABILIDADNRO_ORDENCODIGO_TRAZABILIDADCAMARAUBICACIONTEMPERATURAID_SUC_DESTINOTIPO_FRUTAFECHA_OPERATIVAPALLET_CHEPTESTIGOID_TIPO_PALLETID_TIPO_ESQUINEROTEMPORADAID_EUROAMERICAID_LOTE_SENASAITEM_REMITORENSPAID_PACKINGTIPO_PROCESOID_PUNTO_VENTANRO_COMPFECHA_COMPFECHA_PESOHORA_PESOUNION_EUROPEAID_PUNTO_EMISORPALLET_SENASAID_PUNTO_VENTA_PROFNRO_PROFORMAFECHA_PROFORMAESTADOID_ESTADO_PALLETNRO_PACK_LISTMERMAFECHA_ANULACIONFECHA_ANULHORA_ANULUSUARIO_ANULCHINAID_PROVEEDOR_CAJANRO_COMP_TERCEROSID_PEDIDO_SAPITEM_PEDIDO_SAPID_LECTOR_PALLETID_PALLET_SAPSTATUS_SAPID_SUC_TRABAJO_ANTERIORID_PALLET_ANTERIORID_ORDEN_ANTERIORCERTIFICADOCERT_CHINAGLNNRO_DTCZONA_UPPEDIDO_TRASLADOENTREGA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          l        �
  
        
                  �
  �             T                                                                                                    
               �  
        
                  �  T                                                                                                                 
      �  &      L  
        
                  8               �                                                                                          &          
      �  3         
        
                  �  �             p                                                                                          3          
      <  F      �  
        
                  �  p             $                                                                                          F          
      �  X      h  
        
                  T  $  	           �                                                                                          X          
      �  m        
        
                    �  
           �                                                                                          m          
      X  �      �  
        
                  �  �             @                                                                                          �          
        �      �                             p  @             �                                                                                          �                �  �      8                            $  �             �                                                                                          �                t  �      �  
        
                  �  �             \                                                                                          �          
      (  �      �  
        
                  �  \                                                                                                       �          
      �  �      T  
        
                  @               �                                                                                          �          
      �  �                                  �  �             x                                                                                          �                D  �      �                            �  x             ,                                                                                          �                �  �      p                            \  ,             �                                                                                          �                          $                              �             �                                                                                                                   �       �  X  dD  ~   �D  �  z�      @E  ]       �             �          �       �              �       �  X  �q     hr  �  �>      �r  ^       �         �    �F           N      �                 `�                                               d�            P  L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K                  L                  M                  N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                                 1  $1  ,1  <1  41                         @1  H1  P1  `1  X1                         d1  l1  p1  �1  x1                         �1  �1  �1  �1  �1                         �1  �1  �1  �1  �1                         �1  �1  �1  2  �1                         2  2  $2  82  02                         <2  H2  P2  h2  \2                         l2  �2  �2  �2  �2                         �2  �2  �2  �2  �2                         �2  �2  �2  �2  �2           3             ,3  43  <3  L3  D3          P3             |3  �3  �3  �3  �3          �3             �3  �3  �3  �3  �3                         �3  4  4  ,4   4                         04  @4  L4  h4  \4          l4             |4  �4  �4  �4  �4          �4             �4  �4  �4  5  �4          5             5  (5  45  L5  @5                         P5  \5  h5  �5  |5                         �5  �5  �5  �5                             �5  �5  �5  �5  �5                         �5  �5  �5  6  �5                         6  6   6  86  ,6                         <6  H6  P6  h6  \6                         l6  x6  �6  �6  �6          �6             �6  �6  �6  �6  �6          �6             �6   7  7   7  7          $7             87  H7  L7  d7  X7                         h7  t7  |7  �7  �7          �7             �7  �7  �7  �7  �7                         �7  �7  �7  �7  �7          �7             8  8   8  88  08          <8             X8  h8  t8  �8  �8                         �8  �8  �8  �8  �8          �8             �8  �8  �8  9  9                         9  9  9  ,9  $9                         09  @9  H9  \9  T9                         `9  l9  p9  �9  x9          �9             �9  �9  �9  �9  �9                         �9  �9  �9  �9  �9                          :  :  :  $:  :                         (:  4:  8:  H:  @:                         L:  X:  d:  |:  p:                         �:  �:  �:  �:  �:                         �:  �:  �:  �:  �:                         �:   ;  ;  (;  ;                         ,;  <;  D;  X;  P;                         \;  p;  x;  �;  �;                         �;  �;  �;  �;  �;                         �;  �;  �;  �;  �;                         �;  <  <   <                             $<  8<  <<  X<  L<                         \<  l<  p<  �<  |<                         �<  �<  �<  �<  �<                         �<  �<  �<  �<  �<                         �<  �<   =   =  =                         $=  4=  8=  P=  D=          T=             t=  �=  �=  �=  �=                         �=  �=  �=  �=  �=                         �=  �=  �=  �=  �=           >              >  ,>  8>  P>  D>                         T>  \>  `>  p>  h>  �>  t>  �>             �>  �>  �>  �>  �>                         �>  ?  ?  $?  ?                         (?  0?  8?  H?  @?                         L?  P?  X?  `?  \?                         d?  l?  x?  �?  �?                         �?  �?  �?  �?  �?                         �?  �?  �?  @   @                         @   @  (@  @@  4@          D@             `@  p@  x@  �@  �@                         �@  �@  �@  �@                             �@  �@  �@  �@  �@                         �@  A  A  0A   A                         4A  <A  HA  XA  PA                         \A  dA  lA  |A  tA          �A             �A  �A  �A  �A  �A                         �A  �A  �A  �A  �A                         �A  �A  �A  B  �A                         B  B  B  (B   B                         ,B  8B  HB  `B  TB                         hB  xB  �B  �B  �B                         �B  �B  �B  �B  �B                         �B  �B  �B  �B  �B                         �B  C  C  ,C  $C                         0C  @C  HC  hC  XC          lC             �C  �C  �C  �C  �C                         �C  �C  �C  �C                             �C  �C  �C  �C                               D  D  D  D                             D  (D  0D  <D                             @D  LD  TD  `D                                                                          bultos  >>,>>9  Bultos  Bultos  0   calibre XXX/XX  Calibre Calib.      camara  9   Camara  Cam.    0   certificado X(8)    Cert.Senasa Cert.Sen.       cert_china  X(8)    Cert.China  Cert.China      china   SI/NO   China   China   NO  codigo_trazabilidad X(7)    Cod.Traz.   C.Traz.     cod_prod    X(2)    Cod.Prod.   Cod.Prod.       cod_trazabilidad    X(12)   C�d.Trazabilidad    C�d.Traz.       contramarca X(4)    Letra   Letra       c_fecha 99/99/99    Fecha   Fecha   today    Fecha de �ltima modificaci�n del registro  c_hora  x(8)    Hora    Hora    ?    Hora de �ltima modificaci�n del registro   c_usuario   x(12)   Usuario Usuario ?    Nombre del usuario que modific� el registro    estado  SI/NO   Estado  Estado  SI  fecha_anul  99/99/99    Fecha Anulacion Fecha Anul. ?   fecha_anulacion 99/99/99    Fecha Anulacion Fecha Anul. ?   Dia de Proceso  fecha_comp  99/99/99    Fecha Comprobante   Fec.Comp.   ?   Fecha Comprobante   fecha_operativa 99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  fecha_peso  99/99/99    Fecha Peso  Fecha Peso  ?   fecha_prod  99/99/99    Fecha Produccion    Fecha Prod. ?   fecha_proforma  99/99/9999  fecha_proforma  ?   gln 99999999999999  Gln Gln 0   hora_anul   X(8)    Hora Anul.  Hora Anul.      hora_peso   X(8)    Hora Peso   Hora Peso       hora_prod   X(8)    Hora Prod.  Hora Prod.      id_articulo >>>>>>>>9   C�digo  C�digo  0    C�digo del producto    id_calidad  >>9 Calidad Calidad 0   C�digo de calidad del producto  id_caract   >>9 C�d.caracter�stica  C�digo  0   C�d.caracter�stica  id_categoria    >9  Cod.Categ.  Cod.Cat.    0   id_color    >>,>>9  C�d.de color    Color   0   C�d.de color    id_empresa  >>9 Cod.Empresa C.Emp.  0   id_envase   >>9 Envase  Envase  0   Codigo de envase    id_estado_pallet    >>9 Estado Pallet   E.P.    0   Estado del pallet en puerto id_euroamerica  >,>>>,>>9   Nro. Pallet Nro. Pallet 0   id_finca_senasa >>>9    Finca Senasa    F.S.    0   C�digo de finca senasa  id_lector_pallet    ->,>>>,>>9  Lector  Lector  0   id_lote >>9 Lote    Lote    0   id_lote_senasa  >>>9    Lote Senasa L.S.    0   id_marca    >>9 Marca   Marca   0   Marca del producto  id_orden    >,>>>,>>9   Nro. Orden  Orden   0   id_orden_anterior   >,>>>,>>9   Nro. Orden  Orden   0   id_origen   >>,>>9  Origen  Origen  0   id_packing  >>9 Packing Pack    0   id_pallet   >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_pallet_anterior  >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_pallet_sap   x(10)   Pallet Sap  Pall.Sap        id_pedido_sap   X(10)   Pedido Venta SAP    Ped.Vta.SAP     id_proveedor    >>>,>>9 Productor   Produc. 0   id_proveedor_caja   >>>,>>9 Productor   Produc. 0   id_punto_emisor >9  Punto Emisor    P.E.    0   id_punto_venta  9999    Punto Venta P.Vta.  0   id_punto_venta_prof >,>>9   id_punto_venta_prof 0   id_sucursal_remito  >>9 Sucursal Remito Suc.Remito  0   id_suc_destino  >>9 Sucursal    Suc.    0   id_suc_trabajo  >>9 Sucursal    Suc.    0   id_suc_trabajo_anterior >>9 Sucursal    Suc.    0   id_tipo_esquinero   >>9 Tipo Esquinero  Tipo Esquinero  0   id_tipo_movsto  >>9 T/Movsto    T/Movsto    0    Tipo de movimineto de stock    id_tipo_pallet  >>9 Tipo Pallet Tipo Pallet 0   id_turno_packing    >>9 Cod.Packing C.Pack. 0   id_variedad >>9 variedad    variedad    0   C�digo de variedad del producto id_viaje    >>,>>>,>>9  Nro.Viaje   Nro.Viaje   0   item    >>9 Item    Item    0    Debe ingresar un n�mero de �tem    item > 0 .   Item de pedidos    item_pedido_sap X(6)    Posicion Ped.Vta.SAP    Pos.Ped.Vta.SAP     item_remito >>9 Item Remito Item Rem.   0   merma   SI/NO   Merma   Merma   NO  nro >>>,>>9 Nro Nro 0   nromov  >>,>>>,>>9  Nro.Movim.  Nro.Movim.  0   nro_comp    99999999    Nro.Comprobante Nro.Comp.   0   nro_comp_terceros   XXXX-XXXXXXXX   Nro.Comprobante Nro.Comprobante ?   nro_orden   >>>>9   Nro.Orden   Nro.Orden   0   Ingrese el Numero de Orden  nro_pack_list   X(15)   Nro.Packing List    Nro.Packing List        nro_proforma    ->,>>>,>>9  nro_proforma    0   pallet_chep 9   P.Chep  P.Chep  0   pallet_senasa   X(18)   Pallet Senasa   Pallet Senasa       peso    >>,>>9.99   Peso    Peso    0   renspa  X(21)   Renspa  Renspa      Renspa  status_sap  9   Status Sap  Status Sap  0   temperatura ->9.9   Temperatura Temp.   0   temporada   >9  Temporada   Temp.   0   testigo Si/No   Testigo Testigo No  tipo_fruta  PROPIA/TERCEROS Tipo Fruta  Tipo Fruta  PROPIA  tipo_proceso    X(7)    T.Proc. T.Proc.     ubicacion   X(3)    Ubicacion   Ubic.       unidad_productora   X(3)    Unidad Prod.    Un.Prod.        union_europea   SI/NO   Union Europea   U.E.    SI  usuario_anul    x(12)   Usuario Anul.   Usuario Anul.   ?    Nombre del usuario que anulo el registro   zona_up x(4)    Zona UP ZonaUP      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     � 
   0 @ P `�  ���^������          ����������                                                �              �            �%        �%        �%                �     i     i     i    Z 	\ 	] 	    �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  "  
"  "  "  ""  -"  ="  H"  X"  c"  n"  }"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  #  #  ,#  4#  C#  L#  U#  g#  q#  |#  �#  �#  �#  �#  �#  �#  �#  �#  $  $  )$  8$  P$  b$  q$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  %  %  $%  2%  7%  >%  I%  U%  _%  g%  r%  %  �%  �%  �%  �%  �%  �%  �%  �%  �%                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K                  L                  M                  N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                                 t^  |^  �^  �^  �^                         �^  �^  �^  �^  �^                         �^  �^  �^  �^  �^                         �^  �^  �^  _  �^                         _  _   _  8_  ,_                         <_  D_  L_  \_  T_                         `_  t_  |_  �_  �_                         �_  �_  �_  �_  �_                         �_  �_  �_   `  �_                         `  `  `  (`   `                         ,`  4`  @`  P`  H`          X`             �`  �`  �`  �`  �`          �`             �`  �`  �`  �`  �`          �`             ,a  4a  <a  La  Da                         Pa  \a  ha  �a  xa                         �a  �a  �a  �a  �a          �a             �a  �a  �a  b   b          b             $b  4b  @b  `b  Pb          db             tb  �b  �b  �b  �b                         �b  �b  �b  �b  �b                         �b  �b   c  c                             c  c  (c  0c  ,c                         4c  @c  Hc  `c  Tc                         dc  pc  xc  �c  �c                         �c  �c  �c  �c  �c                         �c  �c  �c  �c  �c          �c             d  d  d  (d   d          ,d             Ld  Xd  \d  xd  pd          |d             �d  �d  �d  �d  �d                         �d  �d  �d  �d  �d          �d              e  e  e  $e  e                         (e  4e  8e  He  @e          Le             `e  te  xe  �e  �e          �e             �e  �e  �e  �e  �e                         �e  �e   f  f  f          f             4f  Hf  Tf  df  \f                         hf  pf  tf  �f  |f                         �f  �f  �f  �f  �f                         �f  �f  �f  �f  �f          �f             �f  �f  g  g  g                          g  4g  @g  Tg  Lg                         Xg  dg  lg  |g  tg                         �g  �g  �g  �g  �g                         �g  �g  �g  �g  �g                         �g  �g  �g  h  h                         h  $h  ,h  Dh  8h                         Hh  Xh  `h  �h  th                         �h  �h  �h  �h  �h                         �h  �h  �h  �h  �h                         �h  �h  �h  i  i                         i  (i  0i  Di  <i                         Hi  \i  di  xi                             |i  �i  �i  �i  �i                         �i  �i  �i  �i  �i                         �i  �i  �i  j   j                         j  $j  (j  <j  4j                         @j  Tj  Xj  xj  hj                         |j  �j  �j  �j  �j          �j             �j  �j  �j  �j  �j                         �j  k  k  (k   k                         ,k  8k  <k  Tk  Hk          Xk             xk  �k  �k  �k  �k                         �k  �k  �k  �k  �k  �k  �k  �k             l   l  (l  Pl  @l                         Tl  `l  dl  |l  pl                         �l  �l  �l  �l  �l                         �l  �l  �l  �l  �l                         �l  �l  �l  �l  �l                         �l  �l  m   m  m                         $m  8m  Hm  hm  Xm                         lm  xm  �m  �m  �m          �m             �m  �m  �m  �m  �m                         �m  n  n  (n                             ,n  8n  <n  Ln  Dn                         Pn  `n  hn  �n  xn                         �n  �n  �n  �n  �n                         �n  �n  �n  �n  �n          �n             �n  �n  �n  o  �n                         o  o   o  4o  ,o                         8o  Do  Ho  \o  To                         `o  ho  po  �o  xo                         �o  �o  �o  �o  �o                         �o  �o  �o  �o  �o                         �o  �o   p  p  p                         p  ,p  4p  Pp  Dp                         Tp  dp  lp  �p  |p                         �p  �p  �p  �p  �p          �p             �p  �p   q  q  q                         q  q  (q  0q                             4q  @q  Hq  Tq                              Xq  `q  hq  pq                             tq  �q  �q  �q                             �q  �q  �q  �q                              �q  �q  �q  �q                                                                          bultos  >>,>>9  Bultos  Bultos  0   calibre XXX/XX  Calibre Calib.      camara  9   Camara  Cam.    0   certificado X(8)    Cert.Senasa Cert.Sen.       cert_china  X(8)    Cert.China  Cert.China      china   SI/NO   China   China   NO  codigo_trazabilidad X(7)    Cod.Traz.   C.Traz.     cod_prod    X(2)    Cod.Prod.   Cod.Prod.       cod_trazabilidad    X(12)   C�d.Trazabilidad    C�d.Traz.       contramarca X(4)    Letra   Letra       c_fecha 99/99/99    Fecha   Fecha   today    Fecha de �ltima modificaci�n del registro  c_hora  x(8)    Hora    Hora    ?    Hora de �ltima modificaci�n del registro   c_usuario   x(12)   Usuario Usuario ?    Nombre del usuario que modific� el registro    estado  SI/NO   Estado  Estado  SI  fecha_anul  99/99/99    Fecha Anulacion Fecha Anul. ?   fecha_anulacion 99/99/99    Fecha Anulacion Fecha Anul. ?   Dia de Proceso  fecha_comp  99/99/99    Fecha Comprobante   Fec.Comp.   ?   Fecha Comprobante   fecha_operativa 99/99/99    Fecha Operativa Fec.Operativa   ?   Dia de Proceso  fecha_peso  99/99/99    Fecha Peso  Fecha Peso  ?   fecha_prod  99/99/99    Fecha Produccion    Fecha Prod. ?   fecha_proforma  99/99/9999  fecha_proforma  ?   gln 99999999999999  Gln Gln 0   hora_anul   X(8)    Hora Anul.  Hora Anul.      hora_peso   X(8)    Hora Peso   Hora Peso       hora_prod   X(8)    Hora Prod.  Hora Prod.      id_articulo >>>>>>>>9   C�digo  C�digo  0    C�digo del producto    id_calidad  >>9 Calidad Calidad 0   C�digo de calidad del producto  id_caract   >>9 C�d.caracter�stica  C�digo  0   C�d.caracter�stica  id_categoria    >9  Cod.Categ.  Cod.Cat.    0   id_color    >>,>>9  C�d.de color    Color   0   C�d.de color    id_empresa  >>9 Cod.Empresa C.Emp.  0   id_envase   >>9 Envase  Envase  0   Codigo de envase    id_estado_pallet    >>9 Estado Pallet   E.P.    0   Estado del pallet en puerto id_euroamerica  >,>>>,>>9   Nro. Pallet Nro. Pallet 0   id_finca_senasa >>>9    Finca Senasa    F.S.    0   C�digo de finca senasa  id_lector_pallet    ->,>>>,>>9  Lector  Lector  0   id_lote >>9 Lote    Lote    0   id_lote_senasa  >>>9    Lote Senasa L.S.    0   id_marca    >>9 Marca   Marca   0   Marca del producto  id_orden    >,>>>,>>9   Nro. Orden  Orden   0   id_orden_anterior   >,>>>,>>9   Nro. Orden  Orden   0   id_origen   >>,>>9  Origen  Origen  0   id_packing  >>9 Packing Pack    0   id_pallet   >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_pallet_anterior  >>,>>>,>>9  Nro. Pallet Nro. Pallet 0   id_pallet_sap   x(10)   Pallet Sap  Pall.Sap        id_pedido_sap   X(10)   Pedido Venta SAP    Ped.Vta.SAP     id_proveedor    >>>,>>9 Productor   Produc. 0   id_proveedor_caja   >>>,>>9 Productor   Produc. 0   id_punto_emisor >9  Punto Emisor    P.E.    0   id_punto_venta  9999    Punto Venta P.Vta.  0   id_punto_venta_prof >,>>9   id_punto_venta_prof 0   id_sucursal_remito  >>9 Sucursal Remito Suc.Remito  0   id_suc_destino  >>9 Sucursal    Suc.    0   id_suc_trabajo  >>9 Sucursal    Suc.    0   id_suc_trabajo_anterior >>9 Sucursal    Suc.    0   id_tipo_esquinero   >>9 Tipo Esquinero  Tipo Esquinero  0   id_tipo_movsto  >>9 T/Movsto    T/Movsto    0    Tipo de movimineto de stock    id_tipo_pallet  >>9 Tipo Pallet Tipo Pallet 0   id_turno_packing    >>9 Cod.Packing C.Pack. 0   id_variedad >>9 variedad    variedad    0   C�digo de variedad del producto id_viaje    >>,>>>,>>9  Nro.Viaje   Nro.Viaje   0   item    >>9 Item    Item    0    Debe ingresar un n�mero de �tem    item > 0 .   Item de pedidos    item_pedido_sap X(6)    Posicion Ped.Vta.SAP    Pos.Ped.Vta.SAP     item_remito >>9 Item Remito Item Rem.   0   merma   SI/NO   Merma   Merma   NO  nro >>>,>>9 Nro Nro 0   nromov  >>,>>>,>>9  Nro.Movim.  Nro.Movim.  0   nro_comp    99999999    Nro.Comprobante Nro.Comp.   0   nro_comp_terceros   XXXX-XXXXXXXX   Nro.Comprobante Nro.Comprobante ?   nro_orden   >>>>9   Nro.Orden   Nro.Orden   0   Ingrese el Numero de Orden  nro_pack_list   X(15)   Nro.Packing List    Nro.Packing List        nro_proforma    ->,>>>,>>9  nro_proforma    0   pallet_chep 9   P.Chep  P.Chep  0   pallet_senasa   X(18)   Pallet Senasa   Pallet Senasa       peso    >>,>>9.99   Peso    Peso    0   renspa  X(21)   Renspa  Renspa      Renspa  status_sap  9   Status Sap  Status Sap  0   temperatura ->9.9   Temperatura Temp.   0   temporada   >9  Temporada   Temp.   0   testigo Si/No   Testigo Testigo No  tipo_fruta  PROPIA/TERCEROS Tipo Fruta  Tipo Fruta  PROPIA  tipo_proceso    X(7)    T.Proc. T.Proc.     ubicacion   X(3)    Ubicacion   Ubic.       unidad_productora   X(3)    Unidad Prod.    Un.Prod.        union_europea   SI/NO   Union Europea   U.E.    SI  usuario_anul    x(12)   Usuario Anul.   Usuario Anul.   ?    Nombre del usuario que anulo el registro   zona_up x(4)    Zona UP ZonaUP      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       � 
   0 @ P `�  ���_������          ����������                                                �              �            �%        �%        �%                �     i     i     i    Z 	\ 	] 	    �!  �!  �!  �!  �!  �!  �!  �!  �!  �!  "  
"  "  "  ""  -"  ="  H"  X"  c"  n"  }"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  �"  #  #  ,#  4#  C#  L#  U#  g#  q#  |#  �#  �#  �#  �#  �#  �#  �#  �#  $  $  )$  8$  P$  b$  q$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  %  %  $%  2%  7%  >%  I%  U%  _%  g%  r%  %  �%  �%  �%  �%  �%  �%  �%  �%  �%  �%    ��                            ����                            !    ��                    �    &'   ��                    �s    undefined                                                               �       ��  �   p   ��  ��                    �����               $�                        O   ����    e�          O   ����    R�          O   ����    ��      x       �   �              4   ����      /                                    3   ����       $     L  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  <      d       4   ����d                 L                      ��                  �   �                   ,(�                           �   �  �  	  �   �                                        3   ����|       O   �   ��  ��  �   batchServices                               @  (      ��                  k  n  X              �P�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             p               ��                  �           ��                            ����                            clientSendRows                              �  �      ��                  p  v  �              �W�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   $             �               ��   L                            ��   t             @               ��                  h           ��                            ����                            commitTransaction                               l  T      ��                  x  y  �              �^�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             p  X      ��                  {  ~  �              !�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              �_�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  
              0h�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   \
             (
               �� 
          �       P
  
         ��                            ����                            destroyServerObject                             T  <      ��                  �  �  l              \r�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                d  L      ��                  �  �  |              �r�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              h  P      ��                  �  �  �              �u�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  |      ��                  �  �  �              Pz�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  �  �  �              �z�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              �                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              hҲ                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              X�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �                �                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �                ���                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $           ��                            ����                            printToCrystal                              $        ��                  �  �  <              |��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             T               ��   �             |               ��                  �           ��                            ����                            refreshRow                              �  �      ��                  �  �  �              84�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              �  �      ��                  �  �  �              8;�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��   4                             ��   \             (               ��   �             P               ��   �             x               ��   �             �               �� 
  �      �       �  
             ��                  �           ��                            ����                            restartServerObject                             �  �      ��                  �  �                ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �                P@�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                                �      ��                  �  �                �
�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            saveContextAndDestroy                               <   $       ��                  �  �  T               XK�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  l            ��                            ����                            serverSendRows                              l!  T!      ��                  �  �  �!              l�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �!             �!               ��   �!             �!               ��    "             �!               ��   H"             "               ��   p"             <"               �� 
          �       d"  
         ��                            ����                            serverFetchRowObjUpdTable                               p#  X#      ��                  �  �  �#              lQ�                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �#  
         ��                            ����                            setPropertyList                             �$  �$      ��                  �  �  �$              tU�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �$           ��                            ����                            serverSendRows                              �%  �%      ��                  �  �  �%              �Y�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   4&              &               ��   \&             (&               ��   �&             P&               ��   �&             x&               ��   �&             �&               �� 
          �       �&  
         ��                            ����                            startServerObject                               �'  �'      ��                  �  �  �'              �u�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �(  �(      ��                       �(              (x�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   8)             )               ��                  ,)           ��                            ����                            submitForeignKey                                0*  *      ��                    	  H*              y�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �*             `*               ��   �*             �*               ��                  �*           ��                            ����                            submitValidation                                �+  �+      ��                      �+              ,r�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ,             �+               ��                  ,           ��                            ����                            synchronizeProperties                               -  �,      ��                      ,-              H|�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   x-             D-               ��                  l-           ��                            ����                            transferToExcel                             l.  T.      ��                    "  �.              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �.             �.               ��   �.             �.               ��    /             �.               ��                  /           ��                            ����                            undoTransaction                             0  �/      ��                  $  %  ,0              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                              1  1      ��                  '  *  81              p��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �1             P1               ��                  x1           ��                            ����                            updateQueryPosition                             |2  d2      ��                  ,  -  �2              P��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �3  h3      ��                  /  1  �3              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �3           ��                            ����                            addRow          4      @4     k       CHARACTER,INPUT pcViewColList CHARACTER cancelRow    4      h4      �4   	 r       CHARACTER,  canNavigate t4      �4      �4    |       LOGICAL,    closeQuery  �4      �4      5   
 �       LOGICAL,    columnProps �4      5      <5    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   5      |5      �5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �5      �5      �5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   �5       6      L6   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   ,6      p6      �6   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    |6      �6      �6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   �6      (7      X7  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow 87      �7      �7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    �7      �7      8    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds �7      p8      �8    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  |8      �8      �8    �       CHARACTER,  hasForeignKeyChanged    �8      9      @9          LOGICAL,    openDataQuery    9      L9      |9    )      LOGICAL,INPUT pcPosition CHARACTER  openQuery   \9      �9      �9   	 7      LOGICAL,    prepareQuery    �9      �9      :    A      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    �9      (:      X:    N      LOGICAL,INPUT pcDirection CHARACTER rowValues   8:      |:      �:   	 [      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �:      ;      0;   	 e      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   ;      p;      �;   	 o      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   |;      �;      <    y      CHARACTER,  assignDBRow                             �<  �<      ��                      �<              ���                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �<  
         ��                            ����                            bufferCopyDBToRO                                �=  �=      ��                       �=              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D>             >  
             �� 
  l>             8>  
             ��   �>             `>               ��                  �>           ��                            ����                            compareDBRow                                �?  p?      ��                  "  #  �?              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �@  x@      ��                  %  '  �@              pЯ                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �@           ��                            ����                            dataAvailable                               �A  �A      ��                  )  +  �A              ���                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            fetchDBRowForUpdate                             �B  �B      ��                  -  .  C              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �C  �C      ��                  0  1  D              @��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �D  �D      ��                  3  4  E              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                                F  �E      ��                  6  7  F              X�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               G  �F      ��                  9  :  G              Xį                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              H  �G      ��                  <  >  ,H              Pů                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 DH  
         ��                            ����                            initializeObject                                HI  0I      ��                  @  A  `I              h̯                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                PJ  8J      ��                  C  E  hJ              8ͯ                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �J  
         ��                            ����                            releaseDBRow                                �K  hK      ��                  G  H  �K              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �L  lL      ��                  J  K  �L              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �M  tM      ��                  M  P  �M              T�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �M             �M               ��                  �M           ��                            ����                            addQueryWhere   �;      LN      |N    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    \N      �N      O    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO �N      `O      �O    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   tO       P      4P    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  P      pP      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �P      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �P      Q      PQ          CHARACTER,INPUT pcColumn CHARACTER  columnTable 0Q      tQ      �Q          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �Q      �Q      �Q     "      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �Q      R      LR  !  /      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  ,R      tR      �R  "  @      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �R      �R      �R  #  O      CHARACTER,INPUT iTable INTEGER  getDataColumns  �R      S      DS  $  ^      CHARACTER,  getForeignValues    $S      PS      �S  %  m      CHARACTER,  getQueryPosition    dS      �S      �S  &  ~      CHARACTER,  getQuerySort    �S      �S       T  '  �      CHARACTER,  getQueryString  �S      T      <T  (  �      CHARACTER,  getQueryWhere   T      HT      xT  )  �      CHARACTER,  getTargetProcedure  XT      �T      �T  *  �      HANDLE, indexInformation    �T      �T      �T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �T      PU      �U  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  dU      �U      V  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    �U      �V      �V  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �V      HW      xW  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  XW      �W      �W  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �W      <X      lX  1  +      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    LX      �X      �X  2  ;      LOGICAL,    removeQuerySelection    �X      �X      Y  3  L      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   �X      LY      |Y  4  a      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  \Y      �Y      �Y  5 
 o      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �Y      �Y       Z  6  z      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition     Z      |Z      �Z  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Z      �Z      [  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString  �Z      $[      T[  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   4[      |[      �[  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �[      �[       \  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �\  �\      ��                  �  �  �\              @+�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �]  �]      ��                  �  �  �]              �-�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �^  �^      ��                  �  �  �^              �.�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �_  �_      ��                  �  �  �_              �1�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �`  �`      ��                  �  �   a              �2�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �a  �a      ��                  �  �  b              �5�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �b  �b      ��                  �  �  c              `I�                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,c  
         ��                            ����                            startServerObject                               0d  d      ��                       Hd              �M�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                8e   e      ��                      Pe              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  he           ��                            ����                            getAppService   �[      �e       f  <  �      CHARACTER,  getASBound  �e      f      8f  = 
 �      LOGICAL,    getAsDivision   f      Df      tf  >  �      CHARACTER,  getASHandle Tf      �f      �f  ?  �      HANDLE, getASHasStarted �f      �f      �f  @  	      LOGICAL,    getASInfo   �f      �f      g  A 	       CHARACTER,  getASInitializeOnRun    �f      (g      `g  B  #      LOGICAL,    getASUsePrompt  @g      lg      �g  C  8      LOGICAL,    getServerFileName   |g      �g      �g  D  G      CHARACTER,  getServerOperatingMode  �g      �g       h  E  Y      CHARACTER,  runServerProcedure   h      ,h      `h  F  p      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   @h      �h      �h  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �h      �h      ,i  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle i      Pi      |i  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   \i      �i      �i  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �i      �i       j  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt   j      Dj      tj  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Tj      �j      �j  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �j      �j      $k  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �k  �k      ��                  �  �  �k              h��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Hl             l  
             ��   pl             <l               �� 
                 dl  
         ��                            ����                            addMessage                              `m  Hm      ��                  �  �  xm              ���                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �m             �m               ��   �m             �m               ��                  �m           ��                            ����                            adjustTabOrder                              �n  �n      ��                  �  �  �n              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Do             o  
             �� 
  lo             8o  
             ��                  `o           ��                            ����                            applyEntry                              \p  Dp      ��                  �  �  tp              ੰ                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            changeCursor                                �q  tq      ��                  �  �  �q              ,��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �q           ��                            ����                            createControls                              �r  �r      ��                  �  �  �r              X��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �s  �s      ��                  �  �  �s              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �t  �t      ��                  �  �  �t              �ʰ                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �u  �u      ��                  �  �  �u              �˰                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �v  �v      ��                  �  �  �v              lư                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �w  �w      ��                  �  �  �w              ǰ                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �x  �x      ��                  �  �  y              �ǰ                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �y  �y      ��                  �  �  z              ϰ                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `z             ,z  
             ��   �z             Tz               ��   �z             |z               ��                  �z           ��                            ����                            modifyUserLinks                             �{  �{      ��                  �    �{              �߰                        O   ����    e�          O   ����    R�          O   ����    ��            ��   |             �{               ��   0|             �{               �� 
                 $|  
         ��                            ����                            removeAllLinks                              $}  }      ��                      <}              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              (~  ~      ��                      @~              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �~             X~  
             ��   �~             �~               �� 
                 �~  
         ��                            ����                            repositionObject                                �  �      ��                      �              0��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            returnFocus                              �  �      ��                      �              ���                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 0�  
         ��                            ����                            showMessageProcedure                                8�   �      ��                      P�              �װ                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             h�               ��                  ��           ��                            ����                            toggleData                              ��  t�      ��                      ��              �?�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ��           ��                            ����                            viewObject                              ��  ��      ��                       Є              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  k      (�      T�  O 
 P      LOGICAL,    assignLinkProperty  4�      `�      ��  P  [      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   t�      �      �  Q  n      CHARACTER,  getChildDataKey ��      (�      X�  R  |      CHARACTER,  getContainerHandle  8�      d�      ��  S  �      HANDLE, getContainerHidden  x�      ��      Ԇ  T  �      LOGICAL,    getContainerSource  ��      ��      �  U  �      HANDLE, getContainerSourceEvents    �      �      X�  V  �      CHARACTER,  getContainerType    8�      d�      ��  W  �      CHARACTER,  getDataLinksEnabled x�      ��      ؇  X  �      LOGICAL,    getDataSource   ��      �      �  Y        HANDLE, getDataSourceEvents �      �      P�  Z        CHARACTER,  getDataSourceNames  0�      \�      ��  [  %      CHARACTER,  getDataTarget   p�      ��      ̈  \  8      CHARACTER,  getDataTargetEvents ��      ؈      �  ]  F      CHARACTER,  getDBAware  �      �      D�  ^ 
 Z      LOGICAL,    getDesignDataObject $�      P�      ��  _  e      CHARACTER,  getDynamicObject    d�      ��      ĉ  `  y      LOGICAL,    getInstanceProperties   ��      Љ      �  a  �      CHARACTER,  getLogicalObjectName    �      �      L�  b  �      CHARACTER,  getLogicalVersion   ,�      X�      ��  c  �      CHARACTER,  getObjectHidden l�      ��      Ȋ  d  �      LOGICAL,    getObjectInitialized    ��      Ԋ      �  e  �      LOGICAL,    getObjectName   �      �      H�  f  �      CHARACTER,  getObjectPage   (�      T�      ��  g  �      INTEGER,    getObjectParent d�      ��      ��  h        HANDLE, getObjectVersion    ��      ȋ      ��  i        CHARACTER,  getObjectVersionNumber  ܋      �      @�  j  )      CHARACTER,  getParentDataKey     �      L�      ��  k  @      CHARACTER,  getPassThroughLinks `�      ��      ��  l  Q      CHARACTER,  getPhysicalObjectName   ��      ̌      �  m  e      CHARACTER,  getPhysicalVersion  �      �      D�  n  {      CHARACTER,  getPropertyDialog   $�      P�      ��  o  �      CHARACTER,  getQueryObject  d�      ��      ��  p  �      LOGICAL,    getRunAttribute ��      ̍      ��  q  �      CHARACTER,  getSupportedLinks   ܍      �      <�  r  �      CHARACTER,  getTranslatableProperties   �      H�      ��  s  �      CHARACTER,  getUIBMode  d�      ��      ��  t 
 �      CHARACTER,  getUserProperty ��      Ȏ      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ؎       �      X�  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles 8�      ��      ��  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      Џ       �  x  '	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      <�      h�  y  4	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   H�      Ԑ      �  z  @	      CHARACTER,INPUT piMessage INTEGER   propertyType    �      (�      X�  {  N	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  8�      ��      ��  |  [	      CHARACTER,  setChildDataKey ��      ��      �  }  j	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ̑      �      H�  ~  z	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  (�      h�      ��    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    |�      ��      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ؒ      �      P�  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   0�      x�      ��  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ȓ      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ܓ      $�      X�  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   8�      ��      ��  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      Ԕ      �  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      ,�      X�  � 
 $
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject 8�      x�      ��  �  /
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      ԕ      �  �  C
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      $�      \�  �  T
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    <�      ��      ��  �  j
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      Ԗ      �  �  
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      ,�      \�  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent <�      |�      ��  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      ̗       �  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      (�      \�  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks <�      ��      ��  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ؘ      �  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      0�      d�  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute D�      ��      ��  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      ��      �  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      8�      t�  �  0      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  T�      ��      Ě  � 
 J      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  U      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      T�      ��  �  e      LOGICAL,INPUT pcMessage CHARACTER   Signature   `�      ��      Л  � 	 q      CHARACTER,INPUT pcName CHARACTER    Ԟ    6  �  ��      �       4   �����                 ��                      ��                  7  d                  �m�                           7   �        8  ��  <�      �       4   �����                 L�                      ��                  9  c                  @n�                           9  ̜  P�    P  h�  �      �       4   �����                 ��                      ��                  \  ^                  �]�                           \  x�         ]                                  ,     
                    � ߱        |�  $  `  $�  ���                           $  b  ��  ���                       x                         � ߱        �    h  �  p�      �      4   �����                ��                      ��                  i  -	                  @^�                           i   �  ��  o   l      ,                                 �  $   m  ��  ���                       �  @         �              � ߱         �  �   n        4�  �   o  �      H�  �   q        \�  �   s  x      p�  �   u  �      ��  �   w  `      ��  �   x  �      ��  �   y        ��  �   |  �      Ԡ  �   ~         �  �     |      ��  �   �  �      �  �   �  t      $�  �   �  �      8�  �   �  ,      L�  �   �  �      `�  �   �  �      t�  �   �  P	      ��  �   �  �	      ��  �   �   
      ��  �   �  t
      ġ  �   �  �
      ء  �   �  l      �  �   �  �       �  �   �  \      �  �   �  �      (�  �   �  D      <�  �   �  �      P�  �   �  �      d�  �   �  0      x�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  X      Ȣ  �   �  �      ܢ  �   �        �  �   �  L      �  �   �  �      �  �   �  �      ,�  �   �         @�  �   �  <      T�  �   �  x      h�  �   �  �      |�  �   �  �          �   �  ,                      ��          �   �      ��                  T	  �	  0�              �u�                        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        ؤ  $ h	  H�  ���                           O   �	  ��  ��  h               D�          4�  <�    $�                                             ��                            ����                                �;      ��      �     V     L�                       H�  y                     ��    �	  �  ��      t      4   ����t                ��                      ��                  �	  )
                  ��                           �	  �  ��  �   �	  �      ��  �   �	  H      Ц  �   �	  �      �  �   �	  @      ��  �   �	  �      �  �   �	  8       �  �   �	  �      4�  �   �	  (      H�  �   �	  �      \�  �   �	         p�  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �    Y
  ȧ  H�      x      4   ����x                X�                      ��                  Z
  �
                  4��                           Z
  ا  l�  �   \
  �      ��  �   ]
  T      ��  �   ^
  �      ��  �   _
  D      ��  �   `
  �      Ш  �   a
  �      �  �   c
  p      ��  �   d
  �      �  �   e
  X       �  �   f
  �      4�  �   g
  �      H�  �   h
  D       \�  �   i
  �       p�  �   j
  �       ��  �   k
  x!      ��  �   l
  �!      ��  �   m
  h"      ��  �   n
  �"      ԩ  �   o
  `#      �  �   p
  �#      ��  �   q
  X$      �  �   r
  �$      $�  �   s
  �$      8�  �   t
  L%      L�  �   u
  �%      `�  �   v
  <&      t�  �   w
  �&      ��  �   x
  4'      ��  �   y
  �'      ��  �   z
  ,(      Ī  �   {
  h(      ت  �   }
  �(      �  �   ~
  X)       �  �   
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
  $0      ��      4�  ��      T0      4   ����T0                Ĭ                      ��                    �                  ,��                             D�  ج  �     �0      �  �     (1       �  �     �1      �  �     2      (�  �     �2      <�  �     3      P�  �     |3      d�  �     �3      x�  �     t4      ��  �     �4      ��  �     l5      ��  �     �5      ȭ  �     d6      ܭ  �     �6      �  �     L7      �  �     �7      �  �     <8      ,�  �     �8      @�  �     ,9      T�  �     �9      h�  �     :      |�  �      X:      ��  �   !  �:      ��  �   "  H;      ��  �   #  �;      ̮  �   $  8<      �  �   %  �<          �   &  (=      �    �  �  ��      �=      4   �����=  	              ��                      ��             	     �  G                  ���                           �   �  ��  �   �  �=      ȯ  �   �  t>      ܯ  �   �  �>      �  �   �  l?      �  �   �  �?      �  �   �  \@      ,�  �   �  �@      @�  �   �  TA      T�  �   �  �A      h�  �   �  DB      |�  �   �  �B      ��  �   �  <C      ��  �   �  �C      ��  �   �  ,D      ̰  �   �  �D      �  �   �  $E      ��  �   �  �E      �  �   �  F      �  �   �  �F      0�  �   �  G      D�  �   �  �G      X�  �   �  �G      l�  �   �  8H      ��  �   �  �H      ��  �   �  0I      ��  �   �  �I      ��  �   �  (J      б  �   �  �J          �   �  K      getRowObjUpdStatic  deleteRecordStatic  ��    �  (�  8�      �K      4   �����K      /   �  d�     t�                          3   �����K            ��                      3   �����K  p�      ��  @�  ��  �K      4   �����K  
              P�                      ��             
       i                  D��                             в  d�  �     4L      ��  $    ��  ���                       `L     
                    � ߱        г  �     �L      (�  $     ��  ���                       �L  @         �L              � ߱        �  $    T�  ���                       �L       	       	           � ߱        N     
                �N                     �O  @        
 �O              � ߱        t�  V     ��  ���                        �O       	       	       P       
       
       TP       	       	           � ߱        �  $  8  �  ���                       Q     
                �Q                     �R  @        
 �R              � ߱            V   J  ��  ���                                      h�                      ��                  k                    P��                           k  0�  �R     
                hS                     �T  @        
 xT           U  @        
 �T          �U  @        
 @U          �U  @        
 �U              � ߱            V   �  ��  ���                        adm-clone-props ��  ��              �     W     l                          h  �                     start-super-proc    ��   �  �           �     X     (                          $  �                     �       ��  ��      lY      4   ����lY      /   !  ȸ     ظ                          3   ����|Y            ��                      3   �����Y  `�  $   ;  4�  ���                       �Y                         � ߱         �    K  |�  ��  ��  �Y      4   �����Y                p�                      ��                  L  P                  ر                           L  ��  �Y                      Z                     Z                         � ߱            $  M  �  ���                             Q  ��  ��      ,Z      4   ����,Z  LZ                         � ߱            $  R  Ⱥ  ���                       �    Y  <�  L�  ��  `Z      4   ����`Z      $  Z  x�  ���                       �Z                         � ߱            �   w  �Z      �Z     
                P[                     �\  @        
 `\              � ߱        H�  V   �  ��  ���                        \�  �   �  �\      X�    =  x�  ��      �\      4   �����\      /   >  ��     ļ                          3   �����\            �                      3   ����]  <]     
                �]                     _  @        
 �^              � ߱        �  V   J  ��  ���                        T_     
                �_                      a  @        
 �`              � ߱        �  V   n  ��  ���                        ��    �  0�  ��      4a      4   ����4a                ��                      ��                  �  �                  �۱                           �  @�  ,�  /   �  �     ��                          3   ����Da            �                      3   ����da      /   �  X�     h�                          3   �����a            ��                      3   �����a  ��  /  [  Ŀ         �a                      3   �����a  initProps   �  Կ              4     Y     �                       �  &!  	                                   �          ��  ��      ��                �  �  ��              ��                        O   ����    e�          O   ����    R�          O   ����    ��      0!                      ��          ��  p   �  �|  8�      �  8�  ��     �|                ��                      ��                  �  �                  |��                           �  H�  ��  :  �                 $  �  �  ���                       �|                         � ߱        ��  ��     �|                                        ��                  �                    ���                           �  H�  X�  H�     �|                                        ��                                      ���                             ��  ��  ��     }                                        ��                     <                  ���                              h�  x�  h�     }                                        ��                  =  Y                  ��                           =  ��  �  ��     ,}                                        ��                  Z  v                  p�                           Z  ��  ��  ��     @}                                        ��                  w  �                  D�                           w  �  (�  �     T}                                        ��                  �  �                  �                           �  ��  ��  ��     h}  	                                      ��             	     �  �                  ��                           �  8�  H�  8�     |}  
                                      ��             
     �  �                  |J�                           �  ��  ��  ��     �}                                        ��                  �                    PK�                           �  X�  h�  X�     �}                                        ��                    $                  $L�                             ��  ��  ��     �}                                        ��                  %  A                  �L�                           %  x�  ��  x�     �}                                        ��                  B  ^                  í                           B  �  �  �     �}                                        ��                  _  {                  �í                           _  ��  ��  ��     �}                                        ��                  |  �                  �ĭ                           |  (�  8�  (�     ~                                        ��                  �  �                  hŭ                           �  ��      ��     ~                                        ��                  �  �                  <ƭ                           �  H�      O   �  ��  ��  0~               L�          4�  @�   , �                                                       �     ��                            ����                            �  ��   �  <�      ��     Z     T�                      � P�  B!                     ��    �  �  ��      <~      4   ����<~                ��                      ��                  �                     $��                           �  �  �  /   �  ��     ��                          3   ����L~            ��                      3   ����l~  t�  /   �  4�     D�                          3   �����~            d�                      3   �����~  ��  /   �  ��     ��                          3   �����~            ��                      3   �����~      /   �  �     �                          3   ����             <�                      3   ����   @     
                �                     �  @        
 ̀              � ߱        ��  V   �  L�  ���                        ��  $  �  �  ���                        �                         � ߱        <�     
                ��                     �  @        
 Ȃ              � ߱        ��  V   �  4�  ���                        ��  $  �  ��  ���                       �     
                    � ߱        (�     
                ��                     �  @        
 ��              � ߱        ��  V   �  �  ���                        h�  $    ��  ���                        �     
                    � ߱        �     
                ��                     ��  @        
 ��              � ߱        ��  V     �  ���                        P�  $  )  ��  ���                       ��                         � ߱         �     
                ��                     �  @        
 ��              � ߱        |�  V   3  ��  ���                        ��  �   M  �      L�  $  N  ��  ���                       $�     
                    � ߱        8�     
                ��                     �  @        
 Ċ              � ߱        x�  V   X  ��  ���                        ��  $  r  ��  ���                       �     
                    � ߱        ��  �   �  $�      <�  $  �  �  ���                       d�     
                    � ߱        P�  �   �  x�      ��  $   �  |�  ���                       ��                         � ߱              �  ��  ��      ԋ      4   ����ԋ      /   �   �     �                          3   �����  @�     
   0�                      3   �����  p�        `�                      3   �����  ��        ��                      3   ����0�            ��                      3   ����L�  pushRowObjUpdTable  ��  ��  �                   [      �                               �&                     pushTableAndValidate    ��  @�  �           �     \     �                          �  �&                     remoteCommit    X�  ��  �           t     ]                                �  �&                     serverCommit    ��   �  �           p     ^     �                          �  �&                                     D�          �  ��      ��                      ,�              ��                        O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  |�    ��                            ����                            0�  �      ��              _      \�                      
�     '                     disable_UI  ��  ��                      `      �                               '  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����       ��  �      viewObject  ,   ��  �  $�      toggleData  ,INPUT plEnabled LOGICAL    �  P�  h�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  @�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  0�  <�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE  �  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �  0�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��   �      editInstanceProperties  ,   ��  �  $�      displayLinks    ,   �  8�  H�      createControls  ,   (�  \�  l�      changeCursor    ,INPUT pcCursor CHARACTER   L�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  8�  D�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER (�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  �      unbindServer    ,INPUT pcMode CHARACTER ��  0�  @�      runServerObject ,INPUT phAppService HANDLE   �  l�  ��      disconnectObject    ,   \�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ,�  8�      startFilter ,   �  L�  \�      releaseDBRow    ,   <�  p�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   `�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  �  ,�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  \�  l�      compareDBRow    ,   L�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   p�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  @�  L�      updateState ,INPUT pcState CHARACTER    0�  x�  ��      updateQueryPosition ,   h�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��  ��  �      undoTransaction ,   ��  �  ,�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  $�  8�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  t�  �  �      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  `�  t�      startServerObject   ,   P�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   x�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  �  (�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  $�  4�      rowObjectState  ,INPUT pcState CHARACTER    �  `�  p�      retrieveFilter  ,   P�  ��  ��      restartServerObject ,   t�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  H�  X�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  8�  ��  ��      initializeServerObject  ,   x�  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��  ��  �      genContextList  ,OUTPUT pcContext CHARACTER ��  4�  @�      fetchPrev   ,   $�  T�  `�      fetchNext   ,   D�  t�  ��      fetchLast   ,   d�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��  �  ,�      destroyServerObject ,   �  @�  P�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    0�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ,�  @�      commitTransaction   ,   �  T�  d�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    D�  ��  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
   %     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� +   ?   %               � 
" 	   
   %              h �P  \         (          
�                          
�            � {   �
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
   � �   �%               o%   o           � �    �
"   
   �           �    1� �     � �   �%               o%   o           � �   �
"   
   �           �    1� �  
   � �   �%               o%   o           � �   �
"   
   �           l    1� �     � �   �%               o%   o           � �    �
"   
   �           �    1� �     � �   �%               o%   o           � �   �
"   
   �           T    1� �     � 
   �%               o%   o           %               
"   
   �          �    1�      � "     
"   
   �               1� )     � �   �%               o%   o           � <  �
"   
   �           �    1� >     � �   �%               o%   o           � M  S �
"   
   �           �    1� �     � 
   �%               o%   o           %               
"   
   �           p    1� �     � 
   �%               o%   o           %               
"   
   �           �    1� �     � 
   �%               o%   o           %              
"   
   �          h    1� �     � 
     
"   
   �           �    1� �  
   � 
   �%               o%   o           %               
"   
   �                1� �     � �   �%               o%   o           � �    �
"   
   �          �    1� �     � "     
"   
   �           �    1�      � �   �%               o%   o           �   t �
"   
   �          D	    1� �  
   � "     
"   
   �           �	    1� �     � �   �%               o%   o           � �  � �
"   
   �           �	    1� 6     � �   �%               o%   o           � �    �
"   
   �           h
    1� M  
   � X   �%               o%   o           %               
"   
   �           �
    1� \     � 
   �%               o%   o           %              
"   
   �           `    1� d     � �   �%               o%   o           � �    �
"   
   �           �    1� u     � �   �%               o%   o           o%   o           
"   
   �           P    1� �  
   � �   �%               o%   o           � �    �
"   
   �           �    1� �     � �  	 �%               o%   o           � �  / �
"   
   �          8    1� �     � �  	   
"   
   �           t    1� �     � �  	 �o%   o           o%   o           � �    �
"   
   �          �    1�       � �  	   
"   
   �           $    1�      � �  	 �o%   o           o%   o           � �    �
"   
   �          �    1�      � 
     
"   
   �          �    1� -     � �  	   
"   
   �              1� :     � �  	   
"   
   �          L    1� G     � �  	   
"   
   �           �    1� U     � 
   �o%   o           o%   o           %              
"   
   �              1� f     � �  	   
"   
   �          @    1� t  
   �      
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
   �                1�      � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �           
�    � 7     
"   
   � @  , 
�           �� �  
   p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� :  
   � �   �%               o%   o           � �    �
"   
   �           <    1� E  
   � �   �%               o%   o           o%   o           
"   
   �           �    1� P     � "   �%               o%   o           o%   o           
"   
   �           4    1� Y     � 
   �%               o%   o           %               
"   
   �           �    1� h     � 
   �%               o%   o           %               
"   
   �           ,    1� u     � �   �%               o%   o           � �    �
"   
   �           �    1� |     � 
   �%               o%   o           %              
"   
   �               1� �     � 
   �%               o%   o           o%   o           
"   
   �           �    1� �     � �   �%               o%   o           o%   o           
"   
   �               1� �  	   � �   �%               o%   o           � �    �
"   
   �           �    1� �     � �   �%               o%   o           o%   o           
"   
   �               1� �     � �   �%               o%   o           o%   o           
"   
   �           �    1� �     � 
   �%               o%   o           %               
"   
   �           �    1� �     � 
   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� �  
   � 
   �%               o%   o           %              
"   
   �           H    1� �     � �   �%               o%   o           o%   o           
"   
   �           �    1�      � �   �%               o%   o           � �    �
"   
   �           8    1�      � �   �%               o%   o           o%   o           
"   
   �          �    1� "     � "     
"   
   �           �    1� /     � �   �%               o%   o           � B  ! �
"   
   �           d    1� d     � �   �%               o%   o           � �    �
"   
   �           �    1� q     � �   �%               o%   o           � �   �
"   
   �          L    1� �     � �     
"   
   �          �    1� �     � "     
"   
   �           �    1� �     � �   �%               o%   o           � �    �
"   
   �          8     1� �  
   � "     
"   
   �           t     1� �     � 
   �%               o%   o           o%   o           
"   
   �           �     1� �     � 
   �%               o%   o           %               
"   
   �           l!    1� �     � 
   �%               o%   o           %               
"   
   �           �!    1� �     � �   �%               o%   o           � �    �
"   
   �           \"    1�      � �   �%               o%   o           o%   o           
"   
   �           �"    1�      � 
   �%               o%   o           %              
"   
   �           T#    1� *     � 
   �%               o%   o           %               
"   
   �           �#    1� 7     � 
   �%               o%   o           %               
"   
   �          L$    1� G     � "     
"   
   �          �$    1� T     � �     
"   
   �           �$    1� a     � X   �%               o%   o           o%   o           
"   
   �           @%    1� m     � �   �%               o%   o           � �    �
"   
   �           �%    1� {     � �   �%               o%   o           o%   o           
"   
   �           0&    1� �     � 
   �o%   o           o%   o           o%   o           
"   
   �           �&    1� �     � �  	 �%               o%   o           o%   o           
"   
   �           ('    1� �     � �   �%               o%   o           o%   o           
"   
   �           �'    1� �  
   � X   �%               o%   o           o%   o           
"   
   �           (    1� �     � �     
"   
   �           \(    1� �     � �   �%               o%   o           � �  4 �
"   
   �           �(    1� $  
   � 
   �%               o%   o           %              
"   
   �          L)    1� /     � "     
"   
   �           �)    1� @     � �   �%               o%   o           � �    �
"   
   �           �)    1� N     � 
   �%               o%   o           %              
"   
   �           x*    1� ]     � �   �%               o%   o           � �    �
"   
   �           �*    1� j     � �   �%               o%   o           � �    �
"   
   �           `+    1� x     � �   �%               o%   o           � �    �
"   
   �           �+    1� �     � 
   �%               o%   o           %               
"   
   �           P,    1� �  	   � "   �%               o%   o           o%   o           
"   
   �           �,    1� �     � �   �%               o%   o           � �  	 �
"   
   �           @-    1� �     � X   �%               o%   o           %       �       
"   
   �           �-    1� �     � �   �%               o%   o           � �    �
"   
   �           0.    1� �     � 
   �o%   o           o%   o           %              
"   
   �           �.    1� �     � 
   �%               o%   o           %               
"   
   �           (/    1� �     � �   �%               o%   o           o%   o           
"   
   �           �/    1�      � �  	 �%               o%   o           � �    �
"   
   �          0    1�      � �  	   P �L 
�H T   %              �     }        �GG %              
"   
   �           �0    1� !  
   � �   �%               o%   o           � �    �
"   
   �           1    1� ,     � 
   �%               o%   o           %               
"   
   �           �1    1� 9  	   � �   �%               o%   o           � �    �
"   
   �           2    1� C     � �   �%               o%   o           � �    �
"   
   �           �2    1� Q     � 
   �%               o%   o           %               
"   
   �           �2    1� a     � �   �%               o%   o           � �    �
"   
   �           p3    1� t     � �   �%               o%   o           o%   o           
"   
   �           �3    1� |     � �   �%               o%   o           o%   o           
"   
   �           h4    1� �     � 
   �%               o%   o           o%   o           
"   
   �           �4    1� �     � 
   �%               o%   o           o%   o           
"   
   �           `5    1� �     � 
   �%               o%   o           o%   o           
"   
   �           �5    1� �     � �   �%               o%   o           o%   o           
"   
   �           X6    1� �  	   � �  	 �%               o%   o           � �    �
"   
   �           �6    1� �  
   � �  	 �%               o%   o           � �    �
"   
   �           @7    1� �     � �   �%               o%   o           � �    �
"   
   �           �7    1� �     � �   �%               o%   o           o%   o           
"   
   �           08    1� �     � �   �%               o%   o           o%   o           
"   
   �           �8    1�      � �   �%               o%   o           � �    �
"   
   �            9    1�      � �   �%               o%   o           � �    �
"   
   �           �9    1� *     � �  	 �%               o%   o           o%   o           
"   
   �          :    1� <     � "     
"   
   �           L:    1� H     � �   �%               o%   o           � �    �
"   
   �           �:    1� V     � �   �%               o%   o           o%   o           
"   
   �           <;    1� i     � 
   �%               o%   o           o%   o           
"   
   �           �;    1� {  
   � �   �%               o%   o           � �    �
"   
   �           ,<    1� �     � �   �%               o%   o           � �    �
"   
   �           �<    1� �     � 
   �%               o%   o           %               
"   
   �           =    1� �     � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �=    1� �  	   � "   �%               o%   o           o%   o           
"   
   �           h>    1� �     � "   �%               o%   o           o%   o           
"   
   �           �>    1� �     � "   �%               o%   o           o%   o           
"   
   �           `?    1� �     � 
   �%               o%   o           %              
"   
   �           �?    1� �     � �   �%               o%   o           �   M �
"   
   �           P@    1� d     � 
   �%               o%   o           %              
"   
   �           �@    1� u     � 
   �%               o%   o           %               
"   
   �           HA    1� �     � 
   �%               o%   o           %               
"   
   �           �A    1� �     � �  	 �%               o%   o           � �   �
"   
   �           8B    1� �     � 
   �%               o%   o           %               
"   
   �           �B    1� �     � �  	 �%               o%   o           o%   o           
"   
   �           0C    1� �     � 
   �o%   o           o%   o           %              
"   
   �           �C    1� �     � �  	 �o%   o           o%   o           � �    �
"   
   �            D    1� �     � "   �o%   o           o%   o           o%   o           
"   
   �           �D    1�      � "   �o%   o           o%   o           o%   o           
"   
   �           E    1�      � �  	 �o%   o           o%   o           o%   o           
"   
   �           �E    1� -     � "   �o%   o           o%   o           o%   o           
"   
   �           F    1� <     � �  	 �o%   o           o%   o           � J   �
"   
   �           �F    1� L     � �  	 �o%   o           o%   o           � [   �
"   
   �           �F    1� g     � 
   �%               o%   o           %               
"   
   �           tG    1� {     � 
   �%               o%   o           %               
"   
   �          �G    1� �     � �  	   
"   
   �           ,H    1� �     � 
   �%               o%   o           %               
"   
   �           �H    1� �     � �   �%               o%   o           o%   o           
"   
   �           $I    1� �     � �   �%               o%   o           o%   o           
"   
   �           �I    1� �     � 
   �%               o%   o           o%   o           
"   
   �           J    1� �     � �   �%               o%   o           � �    �
"   
   �           �J    1� �     �    �%               o%   o           %               
"   
   �           K    1�   	   � 
   �%               o%   o           %                "      %     start-super-proc ��%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       (L    6�      
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
   (�  L ( l       �        XN    ��    � P   �        dN    �@    
� @  , 
�       pN    ��      p�               �L
�    %              � 8      |N    � $         �           
�    � 7   �
"   
   p� @  , 
�       �O    �� )     p�               �L"  	    �   � D   �� F   ��     }        �A      |    "  	    � D   �%              (<   \ (    |    �     }        �A� H   �A"  
        "  	    "  
      < "  	    "  
    (    |    �     }        �A� H   �A"  
    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        `Q    ��    � P   �        lQ    �@    
� @  , 
�       xQ    ��      p�               �L
�    %              � 8      �Q    � $         �           
�    � 7   �
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
   (�  L ( l       �        8S    ��    � P   �        DS    �@    
� @  , 
�       PS    ��      p�               �L
�    %              � 8      \S    � $         �           
�    � 7     
"   
   p� @  , 
�       lT    �� �  
   p�               �L%     SmartDataObject 
"   
   p� @  , 
�       �T    �� �     p�               �L%               
"   
   p� @  , 
�       4U    ��      p�               �L%               
"   
   p� @  , 
�       �U    �� �     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
    (   � 
"   
       �        tV    ��    �
"   
   � 8      �V    � $         �           
�    � 7   �
"   
   �        W    �
"   
   �       8W    /
"   
   
"   
   �       dW    6�      
"   
   
�        �W    8
"   
   �        �W    �
"   
   �       �W    �
"   
   p�    � q   �
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
   %              %                "      %     start-super-proc ��%     adm2/appserver.p f��    � �     
�    �     }        �%               %      Server  - �     }        �    "      � �    �%                   "      � �    �%      NONE    p�,  8         $     "              �    �
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �         [    ��    � P   �        ,[    �@    
� @  , 
�       8[    ��      p�               �L
�    %              � 8      D[    � $         �           
�    � 7   �
"   
   p� @  , 
�       T\    �� �     p�               �L"      p�,  8         $     "              �    �
�     "      %     start-super-proc ��%     adm2/dataquery.p ��
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �]    ��    � P   �        �]    �@    
� @  , 
�       �]    ��      p�               �L
�    %              � 8      �]    � $         �    �     
�    � 7   �
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
   (�  L ( l       �        �_    ��    � P   �        �_    �@    
� @  , 
�       �_    ��      p�               �L
�    %              � 8      �_    � $         �    �     
�    � 7   �
"   
   p� @  , 
�       �`    �� �     p�               �L%               "      %     start-super-proc ��%     adm2/query.p ��%     start-super-proc ��%     adm2/queryext.p % 	    initProps �
�    %4 + $   FOR EACH pallets NO-LOCK INDEXED-REPOSITION �   � �     � �     �       
�     	         �G
"   
   �        Tb    �G
"   
   
"   
    x    (0 4      �        tb    �G%                   �        �b    �GG %              � m    �� n         %              %                   "      %              
"   
       "      �        pc    �
"   
   �        �c    �
"   
   
�       �c    �"       \      H   "      ((       "      %              � �      � �   �     
"   
   
"   
    \      H   "      ((       "      %              � �     � �   ��        hd    �%                   %              %                   "  (    %                  "  (        
"   
   
"   
   0 T       m � "  (    �        te    �A @   "       $         � "  (    � H   ��        �e    �� "  (    
"   
    \ H     H   "      ((       "      %              � �    �� �     (        "  !    � �    ��        (f    �"  !    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ,g    ��    � P   �        8g    �@    
� @  , 
�       Dg    ��      p�               �L
�    %              � 8      Pg    � $         �           
�    � 7     
"   
   p� @  , 
�       `h    �� �     p�               �L%               
"   
   p� @  , 
�       �h    ��      p�               �L"      �,  8         $     "              � u  
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
   (�  L ( l       �        �i    ��    � P   �        �i    �@    
� @  , 
�       �i    ��      p�               �L
�    %              � 8      �i    � $         �    �     
�    � 7     
"   
   p� @  , 
�       �j    �� <     p�               �L
"   
   
"   
   p� @  , 
�       0k    ��      p�               �L"      
"   
   p� @  , 
�       �k    �� �     p�               �L"          "      � �    �%L B <   OPEN QUERY Query-Main FOR EACH pallets NO-LOCK INDEXED-REPOSITION.     "      � �    I((        "      %                   "      � �     "       (   "           "      %              @ �,  8         $     "              � �    
�    p�,  8         $     � �   �        � �   �
�    %               �    "      � �         %              %                   "      %                  "      "      "      T(        "      %              "      � �   �"      �       "      �    "      � H   �� �      � H   ��    "      � H    S    "      "          "      %                � @    �     t T     P   4         "      (0       4         "      � �      � �    �� �   �T ,  %              T   "      "      � �     � H   �� �   �T    �    "      � H   �"      � H   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "      %              � �    �� �     4         "      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �q    ��    � P   �        r    �@    
� @  , 
�       r    ��      p�               �L
�    %              � 8       r    � $         �           
�    � 7   �
"   
   p� @  , 
�       0s    �� !  
   p�               �L"            "  
    �    � �    �� �   �      "  	    �    � �  (�� �   ��   � �     � �     � �    ��   � �     � �   �� �  (��   � �     � �     � �  (  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �t    ��    � P   �        �t    �@    
� @  , 
�       �t    ��      p�               �L
�    %              � 8      �t    � $         �           
�    � 7     
"   
   p� @  , 
�       �u    �� �     p�               �L"      
"   
   p� @  , 
�       Lv    �� �     p�               �L"      
"   
   p� @  , 
�       �v    �� a     p�               �L"          %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �      � �         "  	    �     "      T    "      "      @ A,    �   � �   �� �     "      "       T      @   "      (        "      � �    �� �      � �   �"           "  	    %              D H   @ A,    �   � �   �� �     "      "      ,    S   "      � �    �� �   �%                T      @   "      (        "      � �    �� �      � �   �"           "  
    %                         "      � �     "                 "      � �   �"      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �z    ��    � P   �        �z    �@    
� @  , 
�       �z    ��      p�               �L
�    %              � 8      �z    � $         �    �     
�    � 7   �
"   
   p� @  , 
�       �{    �� �     p�               �L"      
"   
   p� @  , 
�       L|    �� a     p�               �L"      "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "      %     start-super-proc ��%     adm2/data.p %     start-super-proc ��%     adm2/dataext.p %     start-super-proc ��%     adm2/dataextcols.p %     start-super-proc ��%     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    ��      p�               �L
�    %              � 8      �    � $         �    �     
�    � 7   �
"   
   p� @  , 
�       ��    �� �     p�               �L%               %     "dPalletsSap.i" 
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    ��      p�               �L
�    %              � 8      ��    � $         �           
�    � 7   �
"   
   p� @  , 
�       ��    �� �     p�               �L"      
�     	        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        t�    ��    � P   �        ��    �@    
� @  , 
�       ��    ��      p�               �L
�    %              � 8      ��    � $         �           
�    � 7   �
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
   (�  L ( l       �        `�    ��    � P   �        l�    �@    
� @  , 
�       x�    ��      p�               �L
�    %              � 8      ��    � $         �           
�    � 7   �
"   
   p� @  , 
�       ��    �� �  	   p�               �L
"   
   
"   
        � /&  	   �        �    �
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        l�    ��    � P   �        x�    �@    
� @  , 
�       ��    ��      p�               �L
�    %              � 8      ��    � $         �           
�    � 7   �
"   
   p� @  , 
�       ��    �� �     p�               �L"      
"   
   �       ��    �"      
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
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       ��    ��      p�               �L
�    %              � 8      ��    � $         �           
�    � 7   �
"   
   p� @  , 
�       ��    �� �  	   p�               �L
"   
   
�             �Gp�,  8         $     
"   
           � 9&   �
�    
�             �Gp�,  8         $     
"   
           � K&   �
�    �    � ]&     
�        "      � �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �&     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   p       ��                 x  �  �               �ʱ                        O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       (V     
                    � ߱              �  ,  �      �V      4   �����V                �                      ��                  �  �                  �ѱ                           �  <  �  �  �  �V            �  �  l      $W      4   ����$W                |                      ��                  �  �                  �ӱ                           �  �  �  o   �      ,                                 �  �   �  DW      �  �   �  pW      0  $  �    ���                       �W     
                    � ߱        D  �   �  �W      X  �   �  �W      l  �   �  �W          $   �  �  ���                       ,X  @         X              � ߱                     `          8  L   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   p       ��                 �    �               �Ա                        O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �X     
                    � ߱                  �  �                      ��                   �  �                  Tձ                          �  8      4   �����X      $  �  �  ���                       �X     
                    � ߱        �    �  <  L       Y      4   ���� Y      /  �  x                               3   ����Y  �  �   �   Y          O   �  ��  ��  XY                               , �                          
                               �      ��                            ����                                            �           �   p       ��            	     g  �  �               ݱ                        O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �a                         � ߱        �  $  �  <  ���                       b                         � ߱        @b     
                �b  @         `b              � ߱        L  $   �  h  ���                         \      �  �                      ��        0         �  �                  ���      �c         `     �  �      $  �  �  ���                       c                         � ߱          $  �  �  ���                       Hc                         � ߱            4   ����|c  �c     
                �c                     td                         � ߱          $  �    ���                                     ,                      ��                  �  �                  ���                    �     �  �  �  $  �  X  ���                       �d       !       !           � ߱                \  �                      ��        0         �  �                  �ݱ     ( pe                �  �      $  �  0  ���                        e       (       (           � ߱        �  $  �  �  ���                       0e       (       (           � ߱            4   ����Xe        �  �  `      �e      4   �����e                p                      ��                  �  �                  ���                           �  �  �  $  �  �  ���                       f       !       !           � ߱            O   �  �� ��          $  �    ���                       4f                         � ߱        �f     
                \g                     �h  @        
 lh          i  @        
 �h          i                     Xi     
                �i                     $k  @        
 �j          |k  @        
 <k          �k  @        
 �k              � ߱        �  V   �  8  ���                        d	    �  �  8	      �k      4   �����k   l                     Pl                     pl                     �l                         � ߱            $  �  �  ���                       �	    �  �	  �	      m      4   ����m      �   �  Lm      �	  $  �  �	  ���                       �m                         � ߱        �
  $  �  (
  ���                       �m                         � ߱          �
      ,  0                      ��        0         �  �                  1�      8n         �     �  T
      $  �     ���                       �m                         � ߱        �  $  �  X  ���                       �m                         � ߱            4   ����n  Dn                     �n                     �n                     �n                     o                         � ߱        \  $  �  �  ���                             �  x  �      (o      4   ����(o      $  �  �  ���                       Po          |p             � ߱        �  $  �    ���                       �p                         � ߱          �        x                      ��        0         �  �                  2�      q         4     �  8      $  �  �  ���                       �p                         � ߱        h  $  �  <  ���                       �p                         � ߱            4   �����p      $  �  �  ���                       0q                         � ߱        �q     
                ,r                     |s  @        
 <s              � ߱        �  V   �  �  ���                        �s       
       
       �s       	       	       �s                     t                         � ߱          $  1  `  ���                          $  �  8  ���                       Ht                         � ߱        tt     
                �t                     @v  @        
  v          �v  @        
 Xv          �v  @        
 �v              � ߱        �  V   �  d  ���                          �        |                      ��        0    	     E  Z                  �	�      |w         \     E  ,      $  E  �  ���                       �v                         � ߱        \  $  E  0  ���                       ,w                         � ߱        l  4   ����Tw      4   �����w  �  $  J  �  ���                       �w                         � ߱        �    L  �  p      x      4   ����x                �                      ��                  M  Q                   
�                           M     Xx                     �x       	       	           � ߱            $  N  �  ���                             S    �      �x      4   �����x  	              �                      ��             	     U  Y                  |
�                           U     |y                     �y       
       
           � ߱            $  V  �  ���                       z                     @z                         � ߱          $  `    ���                       tz     
                �z                     @|  @        
  |          �|  @        
 X|              � ߱            V   n  �  ���                                    J �          �  �  � Xh                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            !                          �                                �   p       ��                  p  {  �               ��                        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   p       ��                  �  �  �               ��                        O   ����    e�          O   ����    R�          O   ����    ��      �&       �              �                  $                  h  /  �  (     8  ��                      3   ����h�            X                      3   ������      O   �  ��  ��  ��               �          �  �    �                                             ��                            ����                                            L          �   p       ��                  �  �  �               ��                        O   ����    e�          O   ����    R�          O   ����    ��      �&       �              �                $                  �&       0             �          �&                      $         �  /  �  x     �  ��                      3   ������            �                      3   ����Ȍ    /  �  �     �  ��                      3   ����Ԍ  |          $                  3   ������      $   �  P  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        \  $   �  0  ���                       �                         � ߱            O   �  ��  ��  ,�               �          �  �   @ �                                                              0              0           ��                            ����                                            $          �   p       ��                  �  �  �               ��                        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �&                    �          �&                      �              /  �  P     `  \�                      3   ����@�  �        �  �                  3   ����d�      $   �  �  ���                                                   � ߱                                      3   ����p�      $   �  D  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   p       ��                  �  �  �               ��                        O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       ��      4   ������      �   �  ��    ��                            ����                            TXS appSrvUtils pallets Archivo de pallets D:\desarrollos\qas\dPalletsSap.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource WordIndexedFields RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dPalletsSap.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH pallets NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH pallets NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; bultos calibre camara certificado cert_china china codigo_trazabilidad cod_prod cod_trazabilidad contramarca c_fecha c_hora c_usuario estado fecha_anul fecha_anulacion fecha_comp fecha_operativa fecha_peso fecha_prod fecha_proforma gln hora_anul hora_peso hora_prod id_articulo id_calidad id_caract id_categoria id_color id_empresa id_envase id_estado_pallet id_euroamerica id_finca_senasa id_lector_pallet id_lote id_lote_senasa id_marca id_orden id_orden_anterior id_origen id_packing id_pallet id_pallet_anterior id_pallet_sap id_pedido_sap id_proveedor id_proveedor_caja id_punto_emisor id_punto_venta id_punto_venta_prof id_sucursal_remito id_suc_destino id_suc_trabajo id_suc_trabajo_anterior id_tipo_esquinero id_tipo_movsto id_tipo_pallet id_turno_packing id_variedad id_viaje item item_pedido_sap item_remito merma nro nromov nro_comp nro_comp_terceros nro_orden nro_pack_list nro_proforma pallet_chep pallet_senasa peso renspa status_sap temperatura temporada testigo tipo_fruta tipo_proceso ubicacion unidad_productora union_europea usuario_anul zona_up Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p bultos calibre camara certificado cert_china china codigo_trazabilidad cod_prod cod_trazabilidad contramarca c_fecha c_hora c_usuario estado fecha_anul fecha_anulacion fecha_comp fecha_operativa fecha_peso fecha_prod fecha_proforma gln hora_anul hora_peso hora_prod id_articulo id_calidad id_caract id_categoria id_color id_empresa id_envase id_estado_pallet id_euroamerica id_finca_senasa id_lector_pallet id_lote id_lote_senasa id_marca id_orden id_orden_anterior id_origen id_packing id_pallet id_pallet_anterior id_pallet_sap id_pedido_sap id_proveedor id_proveedor_caja id_punto_emisor id_punto_venta id_punto_venta_prof id_sucursal_remito id_suc_destino id_suc_trabajo id_suc_trabajo_anterior id_tipo_esquinero id_tipo_movsto id_tipo_pallet id_turno_packing id_variedad id_viaje item item_pedido_sap item_remito merma nro nromov nro_comp nro_comp_terceros nro_orden nro_pack_list nro_proforma pallet_chep pallet_senasa peso renspa status_sap temperatura temporada testigo tipo_fruta tipo_proceso ubicacion unidad_productora union_europea usuario_anul zona_up RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   �A  @  �O      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   h	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �             �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  1  �  �  E  J  L  M  N  Q  S  U  V  Y  Z  `  n  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �           <  =  Y  Z  v  w  �  �  �  �  �  �  �  �      $  %  A  B  ^  _  {  |  �  �  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable  {  �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    �  �  �  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    �  �  �  �  �  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic      �       `               �                  disable_UI  �  �  �  T2       �,      2                      �#  P  \  ]   RowObject   �         �         �         �         �         �         �                           $         0         8         @         L         T         `         p         |         �         �         �         �         �         �         �         �         �         �                                        (          4          H          X          h          |          �          �          �          �          �          �          �          �          �          !         !         (!         <!         L!         \!         p!         �!         �!         �!         �!         �!         �!         �!         "         "         "         $"         4"         @"         H"         L"         T"         `"         t"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         #         #          #         4#         D#         T#         \#         d#         p#         x#         �#         bultos  calibre camara  certificado cert_china  china   codigo_trazabilidad cod_prod    cod_trazabilidad    contramarca c_fecha c_hora  c_usuario   estado  fecha_anul  fecha_anulacion fecha_comp  fecha_operativa fecha_peso  fecha_prod  fecha_proforma  gln hora_anul   hora_peso   hora_prod   id_articulo id_calidad  id_caract   id_categoria    id_color    id_empresa  id_envase   id_estado_pallet    id_euroamerica  id_finca_senasa id_lector_pallet    id_lote id_lote_senasa  id_marca    id_orden    id_orden_anterior   id_origen   id_packing  id_pallet   id_pallet_anterior  id_pallet_sap   id_pedido_sap   id_proveedor    id_proveedor_caja   id_punto_emisor id_punto_venta  id_punto_venta_prof id_sucursal_remito  id_suc_destino  id_suc_trabajo  id_suc_trabajo_anterior id_tipo_esquinero   id_tipo_movsto  id_tipo_pallet  id_turno_packing    id_variedad id_viaje    item    item_pedido_sap item_remito merma   nro nromov  nro_comp    nro_comp_terceros   nro_orden   nro_pack_list   nro_proforma    pallet_chep pallet_senasa   peso    renspa  status_sap  temperatura temporada   testigo tipo_fruta  tipo_proceso    ubicacion   unidad_productora   union_europea   usuario_anul    zona_up RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �#  �#  ^   RowObjUpd   (         (         $(         ,(         8(         D(         L(         `(         l(         �(         �(         �(         �(         �(         �(         �(         �(         �(         �(         �(          )         )         )          )         ,)         8)         D)         P)         \)         l)         x)         �)         �)         �)         �)         �)         �)         �)         �)         �)         *         *         (*         4*         @*         T*         d*         t*         �*         �*         �*         �*         �*         �*         �*          +         +         ,+         <+         L+         `+         l+         x+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         �+         ,         ,          ,         (,         4,         @,         L,         T,         `,         p,         |,         �,         �,         �,         �,         �,         �,         �,         �,         �,         bultos  calibre camara  certificado cert_china  china   codigo_trazabilidad cod_prod    cod_trazabilidad    contramarca c_fecha c_hora  c_usuario   estado  fecha_anul  fecha_anulacion fecha_comp  fecha_operativa fecha_peso  fecha_prod  fecha_proforma  gln hora_anul   hora_peso   hora_prod   id_articulo id_calidad  id_caract   id_categoria    id_color    id_empresa  id_envase   id_estado_pallet    id_euroamerica  id_finca_senasa id_lector_pallet    id_lote id_lote_senasa  id_marca    id_orden    id_orden_anterior   id_origen   id_packing  id_pallet   id_pallet_anterior  id_pallet_sap   id_pedido_sap   id_proveedor    id_proveedor_caja   id_punto_emisor id_punto_venta  id_punto_venta_prof id_sucursal_remito  id_suc_destino  id_suc_trabajo  id_suc_trabajo_anterior id_tipo_esquinero   id_tipo_movsto  id_tipo_pallet  id_turno_packing    id_variedad id_viaje    item    item_pedido_sap item_remito merma   nro nromov  nro_comp    nro_comp_terceros   nro_orden   nro_pack_list   nro_proforma    pallet_chep pallet_senasa   peso    renspa  status_sap  temperatura temporada   testigo tipo_fruta  tipo_proceso    ubicacion   unidad_productora   union_europea   usuario_anul    zona_up RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   -          -  
   appSrvUtils D-       0-     xiRocketIndexLimit  l-        X-  
   gshAstraAppserver   �-        �-  
   gshSessionManager   �-        �-  
   gshRIManager    �-        �-  
   gshSecurityManager  .        �-  
   gshProfileManager   4.  	 	     .  
   gshRepositoryManager    `.  
 
     H.  
   gshTranslationManager   �.        t.  
   gshWebManager   �.        �.     gscSessionId    �.        �.     gsdSessionObj   �.        �.  
   gshFinManager   /        /  
   gshGenManager   8/        (/  
   gshAgnManager   \/        L/     gsdTempUniqueID |/        p/     gsdUserObj  �/        �/     gsdRenderTypeObj    �/        �/     gsdSessionScopeObj  �/       �/  
   ghProp  0       �/  
   ghADMProps  ,0       0  
   ghADMPropsBuf   T0       @0     glADMLoadFromRepos  p0       h0     glADMOk �0       �0  
   ghContainer �0    	   �0     cObjectName �0    
   �0     iStart  �0       �0     cAppService 1        1     cASDivision 81        1     cServerOperatingMode    \1       L1     cContainerType  �1       p1     cQueryString    �1       �1  
   hRowObject  �1       �1  
   hDataQuery  �1       �1     cColumns             �1     cDataFieldDefs  2       2  pallets 82    X  ,2  RowObject         X  H2  RowObjUpd            9   �   �   �   �   6  7  8  9  P  \  ]  ^  `  b  c  d  h  i  l  m  n  o  q  s  u  w  x  y  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  )
  Y
  Z
  \
  ]
  ^
  _
  `
  a
  c
  d
  e
  f
  g
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
                                                   !  "  #  $  %  &  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  G  �  �                  8  J  i  k  �       !  ;  K  L  M  P  Q  R  Y  Z  w  �  �  =  >  J  n  �  �  �  �  �  [  �  �  �  �  �  �     �  �  �  �  �      )  3  M  N  X  r  �  �  �  �  �  �      ��  "C:\Progress\OpenEdge102b\src\adm2\data.i �6  �) . )C:\Progress\OpenEdge102b\src\adm2\custom\datacustom.i    �6  �� - "C:\Progress\OpenEdge102b\src\adm2\robjflds.i �6  &� , D:\desarrollos\qas\dPalletsSap.i 07  �:  "C:\Progress\OpenEdge102b\src\adm2\query.i    \7  z + "C:\Progress\OpenEdge102b\src\adm2\delrecst.i �7  `W * "C:\Progress\OpenEdge102b\src\adm2\tblprep.i  �7  F� ) C:\Progress\OpenEdge102b\gui\fnarg   8   ( )C:\Progress\OpenEdge102b\src\adm2\custom\querycustom.i   48  �   "C:\Progress\OpenEdge102b\src\adm2\dataquery.i    x8  �Z ' )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerycustom.i   �8  �< ! "C:\Progress\OpenEdge102b\src\adm2\appserver.i    �8  �� & )C:\Progress\OpenEdge102b\src\adm2\custom\appservercustom.i   89  I� " "C:\Progress\OpenEdge102b\src\adm2\smart.i    �9  Ds % C:\Progress\OpenEdge102b\gui\fn  �9  tw $ )C:\Progress\OpenEdge102b\src\adm2\custom\smartcustom.i   �9  Q. # C:\Progress\OpenEdge102b\gui\set (:  �>  "C:\Progress\OpenEdge102b\src\adm2\dataprop.i T:  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\datapropcustom.i    �:  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\dataprtocustom.i    �:  YO  "C:\Progress\OpenEdge102b\src\adm2\qryprop.i  ;  -�  )C:\Progress\OpenEdge102b\src\adm2\custom\qrypropcustom.i T;  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\qryprtocustom.i �;   	 "C:\Progress\OpenEdge102b\src\adm2\dataqueryprop.i    �;  �d  )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerypropcustom.i   <  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\dataqueryprtocustom.i   h<  �l  "C:\Progress\OpenEdge102b\src\adm2\appsprop.i �<  ɏ  )C:\Progress\OpenEdge102b\src\adm2\custom\appspropcustom.i    �<  V  )C:\Progress\OpenEdge102b\src\adm2\custom\appsprtocustom.i    4=  i$  "C:\Progress\OpenEdge102b\src\adm2\smrtprop.i |=  �j  C:\Progress\OpenEdge102b\gui\get �=  �  )C:\Progress\OpenEdge102b\src\adm2\custom\smrtpropcustom.i    �=  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\smrtprtocustom.i    (>  ��  "C:\Progress\OpenEdge102b\src\adm2\smrtprto.i p>  Su  "C:\Progress\OpenEdge102b\src\adm2\globals.i  �>  M�  )C:\Progress\OpenEdge102b\src\adm2\custom\globalscustom.i �>  )a  )C:\Progress\OpenEdge102b\src\adm2\custom\smartdefscustom.i   $?  �  "C:\Progress\OpenEdge102b\src\adm2\appsprto.i l?  ��  )C:\Progress\OpenEdge102b\src\adm2\custom\appserverdefscustom.i   �?  ��  "C:\Progress\OpenEdge102b\src\adm2\dataqueryprto.i    �?  ª 
 )C:\Progress\OpenEdge102b\src\adm2\custom\dataquerydefscustom.i   0@  ��  "C:\Progress\OpenEdge102b\src\adm2\qryprto.i  |@  �  )C:\Progress\OpenEdge102b\src\adm2\custom\querydefscustom.i   �@  �`  "C:\Progress\OpenEdge102b\src\adm2\dataprto.i �@  �  )C:\Progress\OpenEdge102b\src\adm2\custom\datadefscustom.i    4A  e�  %C:\Progress\OpenEdge102b\gui\adecomm\appserv.i   |A  
t    D:\desarrollos\qas\dPalletsSap.w     �         �A  [  �     �A     �  %   B  �   <     B     �  .   $B  �   �     4B     �     DB  �   �     TB     �  #   dB  �   �     tB     t  #   �B  �   r     �B     P  #   �B  �   M     �B     +  #   �B  �   )     �B       #   �B  �        �B     �  #   C  �   �     C     �  #   $C  �   �     4C     �  #   DC  �   �     TC     u  -   dC  �   q     tC       ,   �C  k   �     �C  �  �     �C     �  +   �C  �  �     �C     �  +   �C  �  �     �C     ~  +   �C  �  {     D     a  +   D  �  ^     $D     D  +   4D  �  A     DD     '  +   TD  �  $     dD     
  +   tD  �       �D     �  +   �D  �  �     �D     �  +   �D  �  �     �D     �  +   �D  �  �     �D     �  +   �D  �  �     E     y  +   E  �  v     $E     \  +   4E  �  Y     DE     ?  +   TE  �  <     dE     "  +   tE  �       �E       +   �E  �       �E     �  +   �E  �  �     �E     �  +   �E  �  �     �E     �  #   �E  �  �     F     f  #   F  k  A     $F       #   4F  j       DF     �  #   TF  i  �     dF     �  #   tF  _  �     �F     �  *   �F  ^  �     �F     �  *   �F  ]  �     �F     [  *   �F  \  Z     �F     4  *   �F  [  3     G       *   G  Z       $G     �  *   4G  Y  �     DG     �  *   TG  X  �     dG     �  *   tG  W  �     �G     q  *   �G  V  p     �G     J  *   �G  U  I     �G     #  *   �G  T  "     �G     �  *   �G  S  �     H     �  *   H  R  �     $H     �  *   4H  Q  �     DH     �  *   TH  P  �     dH     `  *   tH  O  _     �H     9  *   �H  N  8     �H       *   �H  @       �H     �  #   �H  	  �     �H     �  )   �H  �   �     I     v  #   I  �   u     $I     S  #   4I  �   R     DI     0  #   TI  �   /     dI       #   tI  �        �I     �  #   �I  �   �     �I     �  #   �I  �   W     �I     �  (   �I  g   �     �I  a   �      �I     �  '   J  _   �      J     f  #   $J  ]   d      4J     B  #   DJ  I   .      TJ  �   %  !   dJ     �  &   tJ  �   �  !   �J     �  #   �J  �   �  !   �J     �  #   �J  �   �  !   �J     _  #   �J  g   E  !   �J     &     �J  O     !   K  �   �  "   K     �  %   $K  �   f  "   4K       $   DK  �     "   TK     �  #   dK  �   �  "   tK     �  #   �K  �   �  "   �K     �  #   �K  �   �  "   �K     x  #   �K  �   d  "   �K     B  #   �K  }   6  "   �K       #   L     �  "   L     J  !   $L           4L     �     DL     P     TL  �   G     dL  O   9     tL     (     �L     �     �L  �   �     �L  �   �     �L  O   �     �L     y     �L     +     �L  y        �L  �   �
  	   M  G   �
     M     �
     $M     �
     4M  c   1
  	   DM  x   )
     TM  M   
     dM     
     tM     �	     �M  a   �	     �M  �  	     �M     `	     �M  �  -	     �M  O   	     �M     	     �M     �     �M  �   �     N     �     N          $N  x        4N     �     DN     {     TN     w     dN     c     tN     J     �N  Q   :     �N     �     �N     �     �N     �     �N     z     �N  ]   t  	   �N     j     �N     "  	   O       
   O        	   $O  Z   �     4O          DO     �     TO     �     dO     �     tO  c   ~     �O     \     �O          �O           �O     �      �O     �      �O     !       �O           