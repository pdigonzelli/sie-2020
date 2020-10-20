import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string
import ctypes

try:
    retval = ''

    client = Client(url='http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/08b3370daf2a396592e703975276cdeb',username='PIAPPLPID', password='SanMigu3l2015')
    object = client.factory.create('MT_Request')
    unidad = client.factory.create('MT_Request.Unidad')
    unidad.EXIDV = '01000000000000017970' #sys.argv[1]  # '01000000000000011103' # sys.argv[1]

    object.Unidad.append(unidad)
    result = client.service.SI_OS_PP377ING(object.Unidad)
    if result[0].result != 'OK':
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
