import requests
import json
from time import sleep
import urllib.request
from datetime import datetime
import os

import settings, common, refresh_content

SHORTLIST_FILE = "shortlist.csv"
OVERALL_API = "https://public-api.nazk.gov.ua/v1/declaration/?q={person:s}"


def get_guids_by_name(p):
	api = OVERALL_API.format(**{"person":p.replace("'", "ʼ")})
	r = requests.get(api).json()
	if not "error" in r.keys():
		guids = [i['id'] for i in r['items']]
	else:
		guids = []
	return guids

with open(SHORTLIST_FILE, "r") as sf:
	names = sf.readlines()

for n in names:
	print(n)
	guids = get_guids_by_name(n)
	sleep(1)
	refresh_content.add_guids_to_content(guids)
