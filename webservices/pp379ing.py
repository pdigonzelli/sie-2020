import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2
import sys
import string
import ctypes

try:
    retval = ''

    client = Client(url='http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/f5cb25e71c39392283563425fb2fc850',username='PIAPPLPID', password='SanMigu3l2015')
    print client


    object = client.factory.create('DT_Request')
    #print object

    unidad = client.factory.create('DT_Request.unidad')
    print unidad


    momento2 = client.factory.create('DT_Request.momento2')

    respuesta = client.factory.create('DT_Response')
    print respuesta

    result = client.service.SI_OS_PP379ING(26022016,'1','0001','0001','1',20000,'1','1','000001')
    print result

except urllib2.URLError as e1:
    ctypes.windll.user32.MessageBoxA(0, str(e1.reason) , "ERROR1", 1)
    retval = 'ERROR:' + str(e1.reason)

except Exception as e:
    ctypes.windll.user32.MessageBoxA(0, str(e.message), "ERROR2", 1)
    retval = 'ERROR:' + str(e.message)

finally:
#    ctypes.windll.user32.MessageBoxA(0, str(retval), "salio python", 1)
    exit(retval)
