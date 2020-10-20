from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string
import ctypes


cparametro = sys.argv[1]
#cparametro = '01000000000000011109,0035000496,000010,0035000500,000020'
try:

    cparametros = string.split(cparametro, ',')
    retval = ''

    client = Client(url='http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/924ace1f53bb36398105642b440ec670',username='PIAPPLPID', password='SanMigu3l2015')
    #print client


    object = client.factory.create('DT_Request')
    #print object

#    object.EXIDV = sys.argv(1)
#    object.VBELN1 = sys.argv(2)
#    object.POSNR1 = sys.argv(3)
#    object.VBELN2 = sys.argv(4)
#    object.POSNR2 = sys.argv(5)

    resultado = client.service.SI_OS_PP378ING(cparametros[0],cparametros[1],cparametros[2],cparametros[3],cparametros[4])
#    resultado = client.service.SI_OS_PP378ING('1','2','3','4','5')
    # print resultado
    if resultado.RESULT == 'NOT_OK':
        retval= 'ERROR:' +  resultado.MESSAGE.encode('utf-8')
    else:
        retval = resultado.RESULT

except urllib2.URLError as e1:
#    ctypes.windll.user32.MessageBoxA(0, str(e1.reason) , "ERROR1", 1)
    retval = 'ERROR:' + str(e1.reason)

except Exception as e:
#    ctypes.windll.user32.MessageBoxA(0, str(e.message), "ERROR2", 1)
    retval = 'ERROR:' + str(e.message)

finally:
#    ctypes.windll.user32.MessageBoxA(0, str(retval), "salio python", 1)
    exit(retval)
