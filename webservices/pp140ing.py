import suds
from suds.client import Client
from suds.transport.http import HttpAuthenticated
import urllib2

# url = 'http://samisap03.sanmiguel.local:50000/dir/wsdl?p=sa/0ac8b193d2c133daa8f54c5b7556005f'
# t = HttpAuthenticated(username='PIAPPLPID', password='SanMigu3l2015')
# client = Client(url, transport=t)

client = Client(url="http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/1b62d5a1867b3c67a1740cfc789a2725",
                username='PIAPPLPID', password='SanMigu3l2015')
print client
object = client.factory.create('DT_Request')
print object
momento1 = client.factory.create('DT_Request.momento1')
momento2 = client.factory.create('DT_Request.momento2')

#momento1.sucursal = '1010'
#momento1.partida = '10000'
#momento1.material_sap = 'LIMON001         '
#momento1.lote_sap = '15FF001111'
#momento1.peso = '2000'
#momento1.UM = 'KG'
#momento1.almacen = '1010'
#momento1.orden = ''
#momento1.especie = ''
#momento1.variedad = ''
#momento1.mercado = ''
#momento1.trazabilidad = ''
#momento1.finca = ''
#momento1.lote = ''
#momento1.unid_prod = ''
#momento1.color_fruta = ''
#momento1.fecha_cosecha = ''
#momento1.cantidad_bins = ''

momento2.documentoMaterial = None
momento2.ejercicio = None
momento2.fechaContabilizacion = None

#object.SYSID = 'DEV130'
#object.momento1 = momento1
#object.momento2 = momento2


result = client.service.SI_OS_PP140ING('DEV130',momento1,momento2,)

print result