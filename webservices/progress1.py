import os
import sys

archivo = sys.argv[1]

def addToClipBoard(text):
    command = 'echo ' + text.strip() + '| clip'
    os.system(command)

#example
addToClipBoard('pablo')