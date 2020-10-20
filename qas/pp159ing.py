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
    #cparametro = "C:\\OpenEdge\\WRK102b\\PQ09816000070.XML,C:\\OpenEdge\\WRK102b\\PQ09816000070.RES"
    archivo =  string.split(cparametro, ',')[0]
    archivo1 =  string.split(cparametro, ',')[1]



    xml = ET.parse(archivo)

    cabecera_pallet = devuelveDatosNodo(xml.find('PALLET'),['sucursal', 'pallet_id', 'material_embalaje','tipo_pallet', 'contramarca', 'ppecb', 'organizacion' ])

    #print cabecera_pallet


    client = Client(url='http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/924ace1f53bb36398105642b440ec670',    username='PIAPPLPID', password='SanMigu3l2015')

    #print client

    request = client.factory.create('DT_Request')
    momento1 = client.factory.create('DT_Request.momento1')
    cabecera = client.factory.create('DT_Request.momento1.cabecera')
    pos = client.factory.create('DT_Request.momento1.cabecera.posicion')
    #print pos

    cabecera.sucursal = cabecera_pallet[0]
    cabecera.pallet_id = cabecera_pallet[1]
    cabecera.material_embalaje = cabecera_pallet[2]
    cabecera.tipo_pallet = cabecera_pallet[3]
    cabecera.contramarca = cabecera_pallet[4]
    cabecera.ppecb = cabecera_pallet[5]
    cabecera.organizacion = 'SAMI' #cabecera_pallet[6]
    cabecera.posicion = []

    posiciones = xml.find('PALLET').find('POSICIONES').findall('POSICION')

    posiciones_pallet = []

    for posicion1 in posiciones:
        posiciones_pallet.append( devuelveDatosNodo(posicion1, (
        'posicion', 'orden_mto', 'material_sap', 'cantidad', 'UM', 'lote_sap', 'Almacen', 'orden_mts', 'AlmacenMTS',
        'tipo_mercado', 'Ccalidad', 'Ccalibre', 'tipo_packaging',
        'Ccolor', 'porcentaje_mrl', 'Cmarca', 'planta_empaque', 'fecha_produccion', 'turno_produccion',
        'nro_up_puc_spa', 'nro_renspa', 'ggn', 'finca_quinta', 'liote_cuadro',
        'trazabilidad', 'esp_fitosanitaria', 'var_fitosanitaria'))  )


    for p in posiciones_pallet:
        posicion = client.factory.create('DT_Request.momento1.cabecera.posicion')
        posicion.orden_mto = p[1]
        posicion.material_sap = p[2]
        posicion.cantidad = p[3]
        posicion.UM = p[4]
        posicion.lote_sap = p[5]
        posicion.Almacen = p[6]
        posicion.orden_mts = p[7]
        posicion.AlmacenMTS = p[8]
        posicion.tipo_mercado = p[9]
        posicion.calidad = p[10]
        posicion.calibre = p[11]
        posicion.tipo_packaging = p[12]
        posicion.color = p[13]
        posicion.porcentaje_mrl = p[14]
        posicion.marca = p[15]
        posicion.planta_empaque = p[16]
        posicion.fecha_produccion = p[17]
        posicion.turno_produccion = p[18]
        posicion.nro_up_puc_spa = p[19]
        posicion.nro_renspa = p[20]
        posicion.ggn = p[21]
        posicion.finca_quinta = p[22]
        posicion.liote_cuadro = p[23]
        posicion.trazabilidad = p[24]
        posicion.esp_fitosanitaria = p[25]
        posicion.var_fitosanitaria = p[26]
        cabecera.posicion.append(posicion)

    momento1.cabecera = cabecera

    result = client.service.SI_OS_PP159ING('QAS300', momento1 )
    if result.UnidadManipuleo is None:
        retval = 'Error: Sin Unidad de Manipuleo'
        for pos in result.posiciones:
            if pos.Resultado == 'NOT OK':
                if pos.Mensaje is None:
                    retval= retval + '. sin mensaje'
                else:
                    retval= retval + '.' + pos.Mensaje.encode('utf8')
    else:
        crespuesta = 'um:' + result.UnidadManipuleo + '|'
        retval = ''
        i = 0
        for pos in result.posiciones:
            if pos.Resultado == 'NOT OK':
                if pos.Mensaje is None:
                    retval='ERROR: sin mensaje'
                else:
                    retval='ERROR:' + pos.Mensaje.encode('utf8')
            else:
                i = i + 1
                crespuesta = crespuesta + str(pos.Cantidad) + ':' +  str(pos.Lote) + '|'
        if len(crespuesta) > 0 and retval == '':
            crespuesta = crespuesta[:-1]
            #print crespuesta
            #f=open(archivo1,"w")
            #f.write(crespuesta)
            #f.close()
            retval = crespuesta

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



