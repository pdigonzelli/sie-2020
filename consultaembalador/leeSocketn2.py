import socket
import sys
import time
import serial
import suds
from suds.client import Client

def envioembalador(cadena,cliente):
    try:
        response = cliente.service.embalador(cadena)
        if response[0] <> 200 or response[1].RESPONSE <> 'OK':
            print "Error ws embalador: ", response
        else:
            print "Enviado ws"
    except Exception as e1:
        print "Error enviando TCP", str(e1)
        pass


def lecturaembalador(ser1):
    dato = "TIMEOUT"
    time.sleep(5)
    print "Embalador"
    return "0123456789"
    try:
        if not ser1.isOpen():
            ser1.open()
        dato = "TIMEOUT"
        while 1:
            dato = (ser1.readline()).rstrip("\n").rstrip("\r")
            print "dato:", dato
            if dato != "":
                break
    except serial.SerialTimeoutException:
        pass
    except Exception as e:
        dato = str(e)
    finally:
        if ser1.isOpen():
            ser1.close()
        print "resultado:", dato
        return dato


ser = None
# cport = sys.arg[1]

cport = 'COM3'
try:

    args = sys.argv
    ser = serial.Serial(
        port=cport,
        parity=serial.PARITY_NONE,
        baudrate=9600,
        stopbits=serial.STOPBITS_ONE,
        bytesize=serial.EIGHTBITS,
        timeout=5
    )

    try:
        conneccion = "http://192.168.1.115:8080/wsa/wsa1/wsdl?targetURI=urn:sie:packing:consulta"
        client = Client(url=conneccion, faults=False)
        while 1:
            embalador = lecturaembalador(ser)  # '14692684'
            if embalador == "TIMEOUT" or embalador == "Listo" or embalador == "":
                if ser.isOpen():
                    ser.close()
                continue
            envioembalador(embalador, client)
    except suds.WebFault as detail:
        print detail.fault

except Exception as e:
    print str(e)
finally:
    if ser is not None and ser.isOpen():
        ser.close()
