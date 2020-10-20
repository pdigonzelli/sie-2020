import os
import sys
import time

archivo = sys.argv[1]
texto = sys.argv[2]
file = open(archivo, 'w')
file.write(texto)
file.close()
time.sleep(6)
exit(texto)

