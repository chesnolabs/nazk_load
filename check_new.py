import requests
import json
from time import sleep
import socks
import socket
import urllib.request
from datetime import datetime
import os

import settings, common

OVERALL_API = "https://public-api.nazk.gov.ua/v1/declaration/?page={page_number:d}"


def load_new_guids():
    api = OVERALL_API.format(page_number = 1)
    print(api)
    r = requests.get(api).json()
    print(r)
    print("first page loaded")
    total_items = r['page']['totalItems']
    size = r['page']['batchSize']
    max_num_pages = (total_items // size) + 2
    guids = [i['id'] for i in r['items'] if i['id'] not in content.keys()]
    flag = True
    p_number = [i for i in range(2, max_num_pages)]
    for i in range(len(p_number)):
        p = p_number[i]
        print("Fetching page #%s" % p)
        try:
            r = requests.get(OVERALL_API.format(page_number = p)).json()
            guids += [i['id'] for i in r['items'] if i['id'] not in content.keys()]
        except Exception:
            flag = False
        if not flag:
            print("Site error")
            i -= 1
        if len(guids) >= settings.MAX_PER_RUN:
            print("the limit of new declarations has been reached")
            return guids[:settings.MAX_PER_RUN-1:]
        print(i)
        sleep(settings.SLEEP_TIME + common.rnd())
    return data    

"""def get_new_guids():
	all_data = load_all()
	return """

content = common.load_content()


