import xml.etree.ElementTree as ET

import sys
import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import ctypes
import urllib2
import string

def devuelveDatosNodo(nodo, tags):
    res = []
    for tag in tags:
        res.append(nodo.findtext(tag))
    return res

#ctypes.windll.user32.MessageBoxA(0, 'entro', "mio", 1)
try:
    cparametro = sys.argv[1]
    #cparametro = "C:\\OpenEdge\\WRK102b\\P160Q09817000220.XML,C:\\OpenEdge\\WRK102b\\P160Q09817000220.RES"
    archivo =  string.split(cparametro, ',')[0]
    archivo1 =  string.split(cparametro, ',')[1]



    xml = ET.parse(archivo)

    cabecera_pallet = devuelveDatosNodo(xml.find('PALLET'),['Operacion' ])

    #print cabecera_pallet


    client = Client(url='http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/511c8634ef4231619160b4f40327f2ea',    username='PIAPPLPID', password='SanMigu3l2015')

    #print client

    request = client.factory.create('DT_Request')
    request.Operacion = cabecera_pallet[0]

    posiciones = xml.find('PALLET').find('POSICIONES').findall('POSICION')

    posiciones_pallet = []

    for posicion1 in posiciones:
        posiciones_pallet.append( devuelveDatosNodo(posicion1, (
        'posicion', 'orden_mto', 'material_sap', 'cantidad', 'UM', 'lote_sap', 'Almacen', 'orden_mts', 'AlmacenMTS',
        'tipo_mercado', 'Ccalidad', 'Ccalibre', 'tipo_packaging',
        'Ccolor', 'porcentaje_mrl', 'Cmarca', 'planta_empaque', 'fecha_produccion', 'turno_produccion',
        'nro_up_puc_spa', 'nro_renspa', 'ggn', 'finca_quinta', 'liote_cuadro',
        'trazabilidad', 'esp_fitosanitaria', 'var_fitosanitaria', 'centro_costo'))  )


    request.T_Posiciones = []
    #pos = client.factory.create('DT_Request.momento1.cabecera.posicion')
    #print pos

    for p in posiciones_pallet:
        posicion = client.factory.create('DT_Request.T_Posiciones')





        posicion.Orden_Mto = p[1]
        posicion.Material_Sap = p[2]
        posicion.Cantidad = p[3]
        posicion.UM = p[4]
        posicion.Lote_Sap = p[5]
        posicion.Almacen = p[6]
        posicion.Orden_Mts = p[7]
        posicion.Almacen_Mts = p[8]
        posicion.Tipo_Mercado = p[9]
        posicion.Calidad = p[10]
        posicion.Calibre = p[11]
        posicion.Tipo_Packaging = p[12]
        posicion.Color = p[13]
        posicion.Porcentaje_Mrl = p[14]
        posicion.Marca = p[15]
        posicion.Planta_Empaque = p[16]
        posicion.Fecha_Produccion = p[17]
        posicion.Turno_Produccion = p[18]
        posicion.Nro_Up_Puc_Spa = p[19]
        posicion.Nro_Renspa = p[20]
        posicion.Ggn = p[21]
        posicion.Finca_Quinta = p[22]
        posicion.Lote_Cuadro = p[23]
        posicion.Trazabilidad = p[24]
        posicion.Esp_Fitosanitaria = p[25]
        posicion.Var_Fitosanitaria = p[26]
        posicion.Centro_Costo    = p[27]

        request.T_Posiciones.append(posicion)

#    cabecera.posicion = []
    result = client.service.SI_OS_PP160ING(request)
    if result.Resultado == 'NOT OK':
        if result.Mensaje is None:
            retval='ERROR: sin mensaje'
        else:
            retval='ERROR:' + result.Mensaje.encode('utf8')


except urllib2.URLError as e1:
#    ctypes.windll.user32.MessageBoxA(0, str(e1.reason) , "ERROR1", 1)
    retval = 'ERROR:' + str(e1.reason)
except IOError as e3:
#    ctypes.windll.user32.MessageBoxA(0, str(e.message), "ERROR2", 1)
    retval = 'ERROR:' + str(e3.strerror)
except Exception as e:
#    ctypes.windll.user32.MessageBoxA(0, str(e.message), "ERROR2", 1)
    retval = 'ERROR:' + str(e.message)


finally:
    #retval = 'um:0123456789012345678|1:012345'
    exit(retval)



