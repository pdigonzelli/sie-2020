import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2

# url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
# t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
# client = Client(url, transport=t)

client = Client(url='http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/866fa931fbed3c668d1c8e1279a8d2f0',
                username='PIAPPLPID', password='SanMigu3l2015')

object = client.factory.create('DT_Request')
momento1 = client.factory.create('DT_Request.momento1')
momento2 = client.factory.create('DT_Request.momento2')

momento1.sucursal = '1010'
momento1.partida = '1'
momento1.material_sap = 'LIMON001         '
momento1.lote_sap = '15FF001111'
momento1.peso = '2000'
momento1.UM = 'KG'
momento1.almacen = '1010'

momento2.documentoMaterial = None
momento2.ejercicio = None
momento2.fechaContabilizacion = None

object.SYSID = 'DEV130'
object.momento1 = momento1
object.momento2 = momento2


result = client.service.SI_OS_PP141ING('DEV130',momento1,momento2,)

print result