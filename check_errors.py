import requests
import json
from time import sleep
import urllib.request
from datetime import datetime
import os

import settings, common, check_new, refresh_content

filenames =  os.listdir(settings.JSON_FOLDER)
filenames = [f for f in filenames if ".json" in f]
step0_unique = []
for f in filenames:
	print(f)
	with open(os.path.join(settings.JSON_FOLDER, f), "r") as jf:
		js = json.load(jf)
	if type(js) == type(dict()) and len(js) > 0:
		if "error" in js.keys():
			refresh_content.add_guids_to_content([f.split(".")[0]])
	else:
		os.remove(os.path.join(settings.JSON_FOLDER, f))
content = common.load_content()
shorts = [c for c in content if len(c) < 5]
for s in shorts:
	print(s)
	del content[s]
with open(settings.CONTENT_FILENAME, "w") as cf:
			json.dump(content, cf)
