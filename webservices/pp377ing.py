import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string
import ctypes

try:
    retval = ''

    client = Client(url='http://smazapppiprd.sanmiguel.local:50000/dir/wsdl?p=sa/a774cfda79e13cf38f807b9ca660dd07',username='PIAPPLPID', password='SanMigu3l2015')
    object = client.factory.create('DT_Request')
    unidad = client.factory.create('DT_Request.Unidad')
    unidad.EXIDV = sys.argv[1] #'01000000000000010718'
    object.Unidad.append(unidad)
    result = client.service.SI_OS_PP377ING(object.Unidad)
    if result[0]. result != 'OK':
        retval = ('ERROR: No se desarmo correctamente el pallet. ' + result[0].MESSAGE).encode('utf8')
    else:
        retval = result[0].result


except suds.WebFault as e2:
    retval = 'ERROR:' + str(e2.message).encode('utf8') + '(QAS)'

except urllib2.URLError as e1:
    retval = 'ERROR:' + str(e1.reason).encode('utf8') + '(QAS)'

except Exception as e:
    retval = 'ERROR:' + str(e.message).encode('utf8') +  '(QAS)'
finally:
    exit(retval)
