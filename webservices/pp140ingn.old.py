#coding=utf-8
import suds
from suds.client import Client

from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string

import ctypes
retval = ''

#url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
#t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
#client = Client(url, transport=t)


#cparametro = sys.rgv[1]

try:

#    ctypes.windll.user32.MessageBoxA(0, cparametro , "ERROR1", 1)
    cparametro = 'SPL,425008,30300010101,,2000,KG,1109,PG,LIMON,EUREKA,05O03,MONTE GRANDE,LOMA 3,UP-TU-0175-020,VERDE,20160311,5,20160311,TT,UE,NOUSA,NOCHINA,,'
    cparametros = string.split(cparametro, ',')
#    ctypes.windll.user32.MessageBoxA(0, cparametro, "mio", 1)
    client = Client(url='http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/1b62d5a1867b3c67a1740cfc789a2725',
                username='PIAPPLPID', password='SanMigu3l2015')

    print client

    momento1 = client.factory.create('DT_Request.momento1')
    momento2 = client.factory.create('DT_Request.momento2')

    momento1.sucursal = cparametros[0]
    momento1.partida = cparametros[1]
    momento1.material_sap = cparametros[2]
    momento1.lote_sap = cparametros[3]
    momento1.peso = cparametros[4]
    momento1.UM = cparametros[5]
    momento1.almacen = cparametros[6]
    momento1.plantaEmpaque = cparametros[7] #no se de donde sacarla
    momento1.especieFitosanitaria = cparametros [8]
    momento1.variedadFitosanitaria = cparametros[9]
    momento1.trazabilidad = cparametros[10]
    momento1.fincaQuinta = cparametros[11]
    momento1.loteCuadro = cparametros[12]
    momento1.NroupPucSpa = cparametros[13]
    momento1.color = cparametros[14]
    momento1.fecha_cosecha = cparametros[15]
    momento1.cantidad_bins = cparametros[16]
    momento1.fechaProduccion = cparametros[17]
    momento1.turnoProduccion = cparametros[18]
    momento1.mercadoUE = cparametros[19]
    momento1.mercadoUSA = cparametros[20]
    momento1.mercadoCHINA = cparametros[21]
    momento1.mercadoExpIntInd = cparametros[22]
    momento1.fechaVencimiento = cparametros[23]
#<plantaEmpaque>?</plantaEmpaque>
#<especieFitosanitaria>?</especieFitosanitaria>
#<variedadFitosanitaria>?</variedadFitosanitaria>
#<trazabilidad>?</trazabilidad>
#<fincaQuinta>?</fincaQuinta>
#<loteCuadro>?</loteCuadro>
#<NroupPucSpa>?</NroupPucSpa>
#<color>?</color>
#<fecha_cosecha>?</fecha_cosecha>
#<cantidad_bins>?</cantidad_bins>
#<fechaProduccion>?</fechaProduccion>
#<turnoProduccion>?</turnoProduccion>
#<mercadoUE>?</mercadoUE>
#<mercadoUSA>?</mercadoUSA>
#<mercadoCHINA>?</mercadoCHINA>
#<mercadoExpIntInd>?</mercadoExpIntInd>
#<fechaVencimiento>?</fechaVencimiento>







    momento2.documentoMaterial = None
    momento2.ejercicio = None
    momento2.fechaContabilizacion = None

#object.SYSID = 'DEV130'
#object.momento1 = momento1
#object.momento2 = momento2

    result = client.service.SI_OS_PP140ING('QAS300',momento1,momento2,)
    documento = result.momento1.documentoMaterial

    print result

    retval = documento

    if retval == None:
        retval = 'ERROR:' + result.momento1.mensaje.encode('utf8')

except urllib2.URLError as e1:
#    ctypes.windll.user32.MessageBoxA(0, str(e1.reason) , "ERROR1", 1)
    retval = 'ERROR:' + str(e1.reason)

except Exception as e:
#    ctypes.windll.user32.MessageBoxA(0, str(e.message), "ERROR2", 1)
    retval = 'ERROR:' + str(e.message)

finally:
#    ctypes.windll.user32.MessageBoxA(0, str(retval), "salio python", 1)
    exit(retval)
