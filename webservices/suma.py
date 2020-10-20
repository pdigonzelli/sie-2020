import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2

# url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
# t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
# client = Client(url, transport=t)

client = Client(url='http://192.168.1.107:8080/wsa/wsa1/wsdl?targetURI=urn:services-sie-com:sap:suma')
print client

result = client.service.suma('2.00','3.00')

print result