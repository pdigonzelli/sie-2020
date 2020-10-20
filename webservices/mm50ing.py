import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated

client = Client(url="http://192.168.1.107:8080/wsa/wsa1/wsdl?targetURI=urn:services-sie-com:sap:ingFru")
print client

posicion = client.factory.create('ns0:ingFru_POSICIONESRow')

print posicion
print posicion.__class__
#posicion.__setattr__('PSNROREMITO' , '0001R00000001')

client.service.ingFru('SPF', '', '' , 2000 , '20160110' , '08:00' , 100 , '20160110' , '09.:00' , 0 , 1900 , '1' , ['001R00000001' , '20160110' , '20160110' , None , None, None, None , None, None,None,None,None,1900,'Kg',None,'1','1'] )
