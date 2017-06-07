import requests
import json
import csv
from time import sleep
import socks
import socket
import urllib.request
from datetime import datetime
import os

import settings, common, refresh_content


def get_guids_by_name(p):
	api = OVERALL_API.format(**{"person":p.replace("'", "ʼ")})
	r = requests.get(api).json()
	if not "error" in r.keys():
		guids = [i['id'] for i in r['items']]
	else:
		guids = []
	return guids


OVERALL_API = "https://public-api.nazk.gov.ua/v1/declaration/?q={person:s}"

content = common.load_content()

with open(settings.PREDEFINED_FILE, "r") as pdf:
	pdf_csv = csv.reader(pdf)
	lines = [l for l in pdf_csv]

for l in lines[1:]:
	sleep(1)
	print(l[-1])
	decl = refresh_content.add_guids_to_content([l[-1]])
	fullname = ' '.join([decl["data"]["step_1"]["lastname"], decl["data"]["step_1"]["firstname"], decl["data"]["step_1"]["middlename"]]).title()
	sleep(0.2)
	same_name_guids = get_guids_by_name(fullname)
	print(same_name_guids)
	for g in same_name_guids:
		if not g in content:
			refresh_content.add_guids_to_content([g])
			sleep(1)



