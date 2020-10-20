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



client = Client(url='http://192.168.4.10:8080/wsa/wsa1/wsdl?targetURI=urn:services-sie-com:sap:mm050ing')
print client

client.service.mm050ing()