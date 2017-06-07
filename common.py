import random
import settings
import os
import json

def rnd():
    return random.random() * settings.RANDOM_MULTIPLIER

def change_date_format(s):
    parts = s.split(".")
    return parts[2] + '-' + parts[1] + '-' + parts[0]

def load_content():
	if not os.path.isfile(settings.CONTENT_FILENAME):
		a = open(settings.CONTENT_FILENAME, "w")
		json.dump({}, a)
		a.close()
	with open(settings.CONTENT_FILENAME, "r") as cf:
		content = json.load(cf)
	return content
	
def load_types():
	if not os.path.isfile(settings.DOCS_TYPE_FILE):
		a = open(settings.DOCS_TYPE_FILE, "w")
		json.dump({}, a)
		a.close()
	with open(settings.DOCS_TYPE_FILE, "r") as dtf:
		types = json.load(dtf)
	return types

def dump_content(c):
	with open(settings.CONTENT_FILENAME, "w") as cf:
		json.dump(c, cf)

def load_regions():
	if not os.path.isfile(settings.REGIONS_FILE):
		a = open(settings.REGIONS_FILE, "w")
		json.dump({}, a)
		a.close()
	with open(settings.REGIONS_FILE, "r") as cf:
		regions = json.load(cf)
	return regions

def load_headed_content():
	if not os.path.isfile(settings.HEADERS_FILE):
		a = open(settings.HEADERS_FILE, "w")
		json.dump({}, a)
		a.close()
	with open(settings.HEADERS_FILE, "r") as cf:
		headed_content = json.load(cf)
	return headed_content

def load_deprecated():
	if not os.path.isfile(settings.DEPRECATED_FILE):
		a = open(settings.DEPRECATED_FILE, "w")
		json.dump({}, a)
		a.close()
	with open(settings.DEPRECATED_FILE) as df:
		deprecated = json.load(df)
	return deprecated


