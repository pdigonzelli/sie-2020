import suds
from suds.client import Client
from suds.wsdl import WObject

from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string

import ctypes
retval = ''

#url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
#t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
#client = Client(url, transport=t)

#cparametro = sys.argv[1]
cparametro = 'SPL,424980,50111010101,0000000777,800,KG,1109,,LIMON,EUREKA,25F0101,ESPINILLO I,FAMAILLA,UP-TU-0172-013,3,20160220,2,20160223,,NOUE,NOUSA,NOCHINA,,"'

try:

    cparametros = string.split(cparametro, ',')



#    ctypes.windll.user32.MessageBoxA(0, cparametro, "entro python", 1)


    client = Client(url='http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f',
                username='PIAPPLPID', password='SanMigu3l2015')


    momento1 = client.factory.create('DT_Request.momento1')
    momento2 = client.factory.create('DT_Request.momento2')


    momento1.sucursal = cparametros[0]
    momento1.partida = cparametros[1]
    momento1.material_sap = cparametros[2]
    momento1.lote_sap = cparametros[3]
    momento1.peso = cparametros[4]
    momento1.UM = cparametros[5]
    momento1.almacen = cparametros[6]

    momento2.documentoMaterial = None
    momento2.ejercicio = None
    momento2.fechaContabilizacion = None

    result = client.service.SI_OS_PP141ING('DEV130',momento1,momento2,)
#    ctypes.windll.user32.MessageBoxA(0, str(result) , "python1", 1)
    documento = result.momento1.documentoMaterial

    retval = documento
    if retval == None:
        retval = 'ERROR:' + result.momento1.mensaje


except urllib2.URLError as e1:
#    ctypes.windll.user32.MessageBoxA(0, str(e1.reason) , "ERROR", 1)
    retval = 'ERROR:' + str(e1.reason)

except Exception as e:
#    ctypes.windll.user32.MessageBoxA(0, "2", "ERROR", 1)
    retval = 'ERROR:' + str(e.message)

finally:
#    ctypes.windll.user32.MessageBoxA(0, retval, "salio python", 1)
    exit(retval)
