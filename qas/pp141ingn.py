#coding=utf-8
import suds
from suds.client import Client
from suds.wsdl import WObject

from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string

import ctypes
from datetime import datetime

retval = ''

#url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
#t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
#client = Client(url, transport=t)

try:

    cparametro = sys.argv[1]
    #cparametro = "SPL,425042,50111010101,0000001799,200,KG,1101"

    cparametros = string.split(cparametro, ',')


#    ctypes.windll.user32.MessageBoxA(0, cparametro, "entro python", 1)
    client = Client(url='http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/866fa931fbed3c668d1c8e1279a8d2f0',

#    client = Client(url='http://smazapppiprd.sanmiguel.local:50000/dir/wsdl?p=sa/6ac60074b3f13c27aaa7f82a85174b9c',
                username='PIAPPLPID', password='SanMigu3l2015')


    #print client
    #respuesta = client.factory.create('DT_Response')

    #print respuesta
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

    result = client.service.SI_OS_PP141ING('QAS300',momento1,momento2,)
#    ctypes.windll.user32.MessageBoxA(0, str(result) , "python1", 1)
    #print result


    documento = result.momento1.documentoMaterial

    if documento is None:
        retval = 'ERROR:' + result.momento1.mensaje.encode('utf8') + '(QAS)'
    elif not documento.isdigit():
        retval = 'ERROR: documento no numerico' + '(QAS)'
    else:
        if result.momento1.ejercicio is None:
            ano = str(datetime.now().year)
        else:
            ano = result.momento1.ejercicio

        retval = (documento + '&' + ano + '&' + result.momento1.ordenMTS + '&' + result.momento1.mensaje).encode('utf8')
#        f=open('d:\\temp\\pablo1.res','w')
#        f.write(retval)
#        f.close()
except suds.WebFault as e2:
    retval = 'ERROR:' + str(e2.message).encode('utf8') + '(QAS)'

except urllib2.URLError as e1:
    retval = 'ERROR:' + str(e1.reason).encode('utf8') + '(QAS)'

except Exception as e:
    retval = 'ERROR:' + str(e.message).encode('utf8') +  '(QAS)'
finally:
    exit(retval)
