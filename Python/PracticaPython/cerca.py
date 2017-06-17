#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
    parametres:
        
        --lan seguit de 'cat', 'es', 'en' o 'fr' -> indica l'arxiu (idioma),
        si no es possa sera 'cat'.
        
        --key seguit de la consulta.
        Consulta:
            - conjuncions: representades amb tuples.
            - disjuncions: representades amb llistes.
            * si estan asociades a camps d'informacio usarem diccionaris.
            - strings
        Camps: name, content, location (inclou tags address, district i
        barri).
        
        * Consulta sense distincions entre mayus minus ni accents.
        * Si nomes hi ha un element no calen llistes ni tuples.
        * Si no s'indica cap camp es mirara nomes name i location (amb els
          tres tags indicats anteriorment).
        
"""

import argparse

parser = argparse.ArgumentParser()
parser.parse_args()

p = 0

"""
import urllib.request

url_getstations = "http://wservice.viabicing.cat/v1/getstations.php?v=1"
sock = urllib.request.urlopen(url_getstations)
xml_getstations = sock.read()
sock.close()

url_pits_cat = "http://www.bcn.cat/tercerlloc/pits_opendata.xml"
url_pits_es = "http://www.bcn.cat/tercerlloc/pits_opendata_es.xml"
utl_pits_en = "http://www.bcn.cat/tercerlloc/pits_opendata_en.xml"
url_pits_fr = "http://www.bcn.cat/tercerlloc/pits_opendata_fr.xml"

print(xml_getstations)
"""