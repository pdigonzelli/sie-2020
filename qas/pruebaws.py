import xml.etree.ElementTree as ET

import sys
import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import ctypes
import urllib2
import string


#ctypes.windll.user32.MessageBoxA(0, 'entro', "mio", 1)
try:

    client = Client(url='http://smazapppiprd.sanmiguel.local:50000/dir/wsdl?p=sa/8623e4b3eb713799b20596ca87656c6e',    username='PIAPPLPID', password='SanMigu3l2015')

    #print client

    request = client.factory.create('DT_Request')
    momento1 = client.factory.create('DT_Request.momento1')
    cabecera = client.factory.create('DT_Request.momento1.cabecera')
    pos = client.factory.create('DT_Request.momento1.cabecera.posicion')
    #print pos


    result = client.service.SI_OS_PP159ING('PRD300', momento1 )
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



