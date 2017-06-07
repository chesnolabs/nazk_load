import requests
import json
from time import sleep
import urllib.request
import json
import os
import socks
import socket

import settings, common, check_new

DECL_API = "https://public-api.nazk.gov.ua/v1/declaration/{person_id:s}"

def add_guids_to_content(guids):
	for g in guids:
		sleep(0.5)
		decl_api = DECL_API.format(person_id = g)
		decl = requests.get(decl_api).json()
		filename = os.path.join(settings.JSON_FOLDER, g) + ".json"
		with open(filename, "w") as f:
			json.dump(decl, f)
		content[g] = filename
		common.dump_content(content)
		return decl

def check_and_download():
	while True:
		guids = check_new.load_new_guids()
		add_guids_to_content(guids)

socks.setdefaultproxy(
    proxy_type=socks.PROXY_TYPE_SOCKS5,
    addr="127.0.0.1", port=9050)
socket.socket = socks.socksocket

content = common.load_content()
