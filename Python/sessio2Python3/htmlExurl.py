#!/usr/bin/python3
# -*- coding: utf-8 -*-

from html.parser import HTMLParser
import urllib.request

# creem una subclasse i sobreescribim el metodes del han
class MHTMLParser(HTMLParser):
    def handle_starttag(self, tag, attrs):
        print("Hem trobat un tag d'inici:", tag)
    def handle_endtag(self, tag):
        print("Hem trobat un tag de final:", tag)
    def handle_data(self, data):
        print("Hem trobat dades:", data)

# instanciem el parser i li passem HTML de la url
parser = MHTMLParser()
sock = urllib.request.urlopen("http://www.cs.upc.edu/") 
htmlSource = sock.read().decode('utf-8')
sock.close()

parser.feed(htmlSource)


