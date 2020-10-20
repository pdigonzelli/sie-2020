from suds.client import Client

conneccion = "http://192.168.1.115:8080/wsa/wsa1/wsdl?targetURI=urn:sie:packing:consulta"
client = Client(url=conneccion, faults=False)
print client.service.embalador("0123456789")

