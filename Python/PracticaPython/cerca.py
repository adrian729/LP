#!/usr/bin/python3
# -*- coding: utf-8 -*-

import argparse
import unicodedata
import urllib.request
import xml.etree.ElementTree as ElementTree
from math import sin, cos, sqrt, atan2, radians
from ast import literal_eval

# Define valid call arguments
parser = argparse.ArgumentParser()
parser.add_argument("--lan", help="select consults language ('cat' for "
                                  "Catalan, 'es' for Spanish, 'en' for "
                                  "English, 'fr' for French). If "
                                  "nothing is selected it will be "
                                  "Catalan by default.")
parser.add_argument("--key", help="consult to do over the interest"
                                  " points. Use tuples representing "
                                  "conjunctions, lists for disjunctions"
                                  " and strings for single consults. "
                                  "Use dictionaries to associate values"
                                  " with fields, where the valid fields"
                                  " are name, content and location.")
args = parser.parse_args()

# Catch arguments and halt if invalid
# Catalan by default
url_pits = 'http://www.bcn.cat/tercerlloc/pits_opendata.xml'
if args.lan:
    if args.lan == 'cat':
        url_pits = 'http://www.bcn.cat/tercerlloc/pits_opendata.xml'
    elif args.lan == 'es':
        url_pits = 'http://www.bcn.cat/tercerlloc/pits_opendata_es.xml'
    elif args.lan == 'en':
        url_pits = 'http://www.bcn.cat/tercerlloc/pits_opendata_en.xml'
    elif args.lan == 'fr':
        url_pits = 'http://www.bcn.cat/tercerlloc/pits_opendata_fr.xml'
    else:
        # invalid lan param
        print(
            'ERROR: The value {} is not a valid lan value.'
            ''.format(args.lan)
        )
        exit(1)
# if no key specified, nothing to do.
if not args.key:
    print('No key value.')
    exit(0)
# key
key = literal_eval(args.key)
if not isinstance(key, (str, list, tuple, dict)):
    print('Invalid key type {}.'.format(type(key)))
    exit(1)

# Extract pits and stations info
url_stations = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
sock_stations = urllib.request.urlopen(url_stations)
xml_stations = sock_stations.read()
sock_stations.close()

sock_pits = urllib.request.urlopen(url_pits)
xml_pits = sock_pits.read()
sock_pits.close()

# globals
# xml trees
stations_root = ElementTree.fromstring(xml_stations)
pits_root = ElementTree.fromstring(xml_pits)


def get_elem_text(elem):
    if elem is not None:
        return elem.text
    return ""


# data
# stations
stations_list = []
for item in stations_root.findall('station'):
    status = item.find('status').text
    if status == 'OPN':
        item_data = {
            'id': get_elem_text(item.find('id')),
            'lat': get_elem_text(item.find('lat')),
            'long': get_elem_text(item.find('long')),
            'street': get_elem_text(item.find('street')),
            'street_number': get_elem_text(item.find('streetNumber')),
            'slots': get_elem_text(item.find('slots')),
            'bikes': get_elem_text(item.find('bikes'))
        }
        stations_list.append(item_data)
stations_list = tuple(stations_list)

# pits
pits_list = []
for item in pits_root.findall('*/row'):
    item_data = {
        'name': item.find('name').text,
        'address': get_elem_text(item.find('addresses/item/address')),
        'district': get_elem_text(item.find('addresses/item/district')),
        'barri': get_elem_text(item.find('addresses/item/barri')),
        'content': get_elem_text(item.find('content')),
        'lat': item.find('gmapx').text,
        'long': item.find('gmapy').text,
        'short_desc': get_elem_text(
            item.find('custom_fields/descripcio-curta-pics')
        )
    }
    pits_list.append(item_data)
pits_list = tuple(pits_list)

all_pits_index = []
for pits_index in range(len(pits_list)):
    all_pits_index.append(pits_index)
all_pits_index = tuple(all_pits_index)


# methods
def calculate_lat_lon_m_distance(lat1, lon1, lat2, lon2):
    earth_radius = 6373000.0

    lat1 = radians(lat1)
    lon1 = radians(lon1)
    lat2 = radians(lat2)
    lon2 = radians(lon2)

    distance_lon = lon2 - lon1
    distance_lat = lat2 - lat1

    a = sin(distance_lat / 2) ** 2 \
        + cos(lat1) * cos(lat2) * sin(distance_lon / 2) ** 2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))

    return earth_radius * c


def u_format(string):
    u_string = unicodedata.normalize("NFKD", string.lower())
    u_string = u_string.encode("ascii", "ignore")
    return u_string


def has_name(consult_str, pit):
    u_consult_str = u_format(consult_str)
    return u_consult_str in u_format(pit['name'])


def has_content(consult_str, pit):
    u_consult_str = u_format(consult_str)
    return u_consult_str in u_format(pit['content'])


def has_location(consult_str, pit):
    u_consult_str = u_format(consult_str)
    return u_consult_str in u_format(pit['address']) \
        or u_consult_str in u_format(pit['district']) \
        or u_consult_str in u_format(pit['barri'])


def exec_consult_search(consult, field=''):
    """
    Executes a consult over all the interest points listed on pits_list.
    
    :rtype: set
    """
    data_set = set()
    if isinstance(consult, str):
        for p_index in range(len(pits_list)):
            if (field == 'name' or field == '') \
                    and has_name(consult, pits_list[p_index]) \
                    or field == 'content' \
                    and has_content(consult, pits_list[p_index]) \
                    or (field == 'location' or field == '') \
                    and has_location(consult, pits_list[p_index]):
                data_set.add(p_index)
        return data_set
    elif isinstance(consult, tuple):
        data_set = set(all_pits_index)  # initial tuple full
        # conjunction
        for cons_conj in consult:
            conj_res = exec_consult_search(cons_conj, field)
            data_set = data_set & conj_res
        return data_set
    elif isinstance(consult, list):
        # disjunction
        for cons_disj in consult:
            disj_res = exec_consult_search(cons_disj, field)
            data_set = data_set | disj_res
        return data_set
    elif isinstance(consult, dict):
        data_set = set(all_pits_index)  # initial tuple full
        # fields conjunction
        for f_key in consult:
            if f_key != 'name' and f_key != 'content' \
                    and f_key != 'location':
                print('Invalid key "{}"'.format(f_key))
                print('The consult', key, 'is an invalid consult.')
                exit(1)
            f_consult = consult[f_key]
            f_conj_res = exec_consult_search(f_consult, f_key)
            data_set = data_set & f_conj_res
        return data_set
    else:
        print('The consult "{}" is an invalid consult.'.format(key))
        exit(1)


def insert_station(elem, st_list, field):
    if int(stations_list[elem][field]) > 0:
        for index in range(len(st_list)):
            st = st_list[index]
            if int(stations_list[st][field]) \
                    < int(stations_list[elem][field]):
                st_list.insert(index, elem)
                break
        else:
            if len(st_list) < 5:
                st_list.append(elem)
        if len(st_list) > 5:
            st_list.pop()


def nearest_stations(p_index):
    slots = []
    bikes = []
    pits_item = pits_list[p_index]
    for st_index in range(len(stations_list)):
        pits_lat = float(pits_item['lat'])
        pits_long = float(pits_item['long'])
        st_lat = float(stations_list[st_index]['lat'])
        st_long = float(stations_list[st_index]['long'])
        dist = calculate_lat_lon_m_distance(pits_lat, pits_long,
                                            st_lat, st_long)
        if dist < 500.0:
            insert_station(st_index, slots, 'slots')
            insert_station(st_index, bikes, 'bikes')

    return {'slots': slots, 'bikes': bikes}


# script body
consult_result = exec_consult_search(key, '')

content = 'content'
if len(consult_result) > 2:
    content = 'short_desc'

html = '<!DOCTYPE html>\n' \
       '<html>' \
       '\n\t<head>' \
       '\n\t\t<meta charset="UTF-8">' \
       '\n\t\t<title>Practica LP - Python</title>' \
       '\n\t</head>' \
       '\n\t<body>'

html += '\n\t\t<table style="width:100%">' \
        '\n\t\t\t<tr style="border-bottom: 2em solid red;">' \
        '\n\t\t\t\t<th>name</th>' \
        '\n\t\t\t\t<th>address</th>' \
        '\n\t\t\t\t<th>content</th>' \
        '\n\t\t\t\t<th>stations with slots</th>' \
        '\n\t\t\t\t<th>stations with bikes</th>' \
        '\n\t\t\t</tr>'

for i in consult_result:
    html += '\n\t\t\t<tr>' \
            '\n\t\t\t\t<td style="border: 1px solid black"><p>' \
            + pits_list[i]['name'] \
            + '</p></td>' \
              '\n\t\t\t\t<td style="border: 1px solid black"><p>' \
            + pits_list[i]['address'] \
            + '</p></td>' \
              '\n\t\t\t\t<td style="border: 1px solid black">' \
            + pits_list[i][content] \
            + '</td>'

    pit_bikes_slots = nearest_stations(i)
    # slots
    html += '\n\t\t\t\t<td style="border: 1px solid black"><p>\n'
    num = 1
    for slot_st in pit_bikes_slots['slots']:
        html += '\n\t\t\t\t\t<b>Station ' + str(num) + '</b><br>'
        html += '\n\t\t\t\t\t address: ' \
                + stations_list[slot_st]['street'] + ' ' \
                + str(stations_list[slot_st]['street_number']) + '<br>'
        html += '\n\t\t\t\t\t slots: ' \
                + stations_list[slot_st]['slots'] + '<br>'
        num += 1
    html += '\n\t\t\t\t</p></td>'
    # bikes
    html += '\n\t\t\t\t<td style="border: 1px solid black"><p>\n'
    num = 1
    for bikes_st in pit_bikes_slots['bikes']:
        html += '\n\t\t\t\t\t<b>Station ' + str(num) + '</b><br>'
        html += '\n\t\t\t\t\t address: ' \
                + stations_list[bikes_st]['street'] + ' ' \
                + str(stations_list[bikes_st]['street_number']) + '<br>'
        html += '\n\t\t\t\t\t bikes: ' \
                + stations_list[bikes_st]['bikes'] + '<br>'
        num += 1
    html += '\n\t\t\t\t</p></td>'

    html += '\n\t\t\t</tr>'

html += '\n\t\t</table>'

html += '\n\t</body>' \
        '\n</html>'

print(html)
