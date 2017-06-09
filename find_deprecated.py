from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import json
import settings, common
from datetime import datetime
from pyquery import PyQuery as pq
import re
from time import sleep


START_PAGE = "https://public.nazk.gov.ua"
SEARCH_BUTTON_SELECTOR = '//button[@class="btn btn-default"]'
INPUT_SELECTOR = '//input[@class="form-control ng-pristine ng-untouched ng-valid"]'
LONG_WAIT = 1
DECL_SELECTOR = "header h1 a.break-words"
UNRESOLVED_PAIRS_FILE = "unresolved.json"


LINKS_RE = re.compile("/declaration/(?P<GUID>\w{8}-\w{4}-\w{4}-\w{4}-\w{12})")
SLEEP_TIME = 4



def check_identity(d1, d2):
	if headed_content[d1['id']]['declarationYear'] == headed_content[d2['id']]['declarationYear']:
		for step in d1["data"]:
			if not ("empty" in d1["data"][step]) and step != "step_0" and len(d1["data"][step]) > 0 and d1["data"][step] == d2["data"][step]:
				return True
	return False

def ask_types(c1, c2):
	global browser
	with open(settings.DOCS_TYPE_FILE, 'r') as dtf:
		docs_types = json.load(dtf)
	if c1 in docs_types:
		if "Виправлена" in docs_types[c1]:
			return c2
		elif c2 in docs_types and "Виправлена" in docs_types[c2]:
			return c1
	else:
		sleep(SLEEP_TIME)
		#inp = browser.find_element_by_xpath(INPUT_SELECTOR)
		button = browser.find_element_by_xpath(SEARCH_BUTTON_SELECTOR)
		inp = WebDriverWait(browser, LONG_WAIT).until(
			EC.presence_of_element_located((By.XPATH, INPUT_SELECTOR)))
		inp.clear()
		button = WebDriverWait(browser, LONG_WAIT).until(
			EC.presence_of_element_located((By.XPATH, SEARCH_BUTTON_SELECTOR)))
		inp.send_keys(headed_content[c1]["fullname"])
		button.click()
		sleep(SLEEP_TIME)
		pairs = get_pairs(browser.page_source)
		docs_types.update(pairs)
		with open(settings.DOCS_TYPE_FILE, 'w') as dtf:
			json.dump(docs_types, dtf)
		if "Виправл" in pairs[c1]:
			return c2
		elif "Виправл" in pairs[c2]:
			return c1
	

def get_pairs(p):
	with open("temp_file.html", "w") as tf:
		tf.write(p)
	page = pq(filename = "temp_file.html")
	decl = page(DECL_SELECTOR)
	pairs = {}
	for d in decl:
		d = pq(d)
		guid = LINKS_RE.fullmatch(d.attr("href")).group('GUID')
		t = d.parent().next().next().text()
		pairs[guid] = t
	return pairs


browser = webdriver.Firefox()
browser.get(START_PAGE)

headed_content = common.load_headed_content()
content = common.load_content()
names = {}
deprecated_list = common.load_deprecated()

for c in headed_content:
	if headed_content[c]["fullname"] in names:
		same_name = names[headed_content[c]["fullname"]]
		with open(content[c], "r") as decl_file:
			d1 = json.load(decl_file)
		for c2 in same_name:
			with open(content[c2], "r") as decl_file:
				d2 = json.load(decl_file)
				if check_identity(d1, d2):
					if datetime.strptime(headed_content[c]["lastmodified_date"], "%Y-%m-%d") < datetime.strptime(headed_content[c2]["lastmodified_date"], "%Y-%m-%d"):
						if not c in deprecated_list:
							deprecated_list.append(c)
					elif datetime.strptime(headed_content[c]["lastmodified_date"], "%Y-%m-%d") > datetime.strptime(headed_content[c2]["lastmodified_date"], "%Y-%m-%d"):
						if not c2 in deprecated_list:
							deprecated_list.append(c2)
					else:
						try:
							asked_type = ask_types(c, c2)
							if not asked_type in deprecated_list:
								deprecated_list.append(asked_type)
						except Exception:
							print("website seems to be unavailable or " + headed_content[c]["fullname"] + "' declaration has disappeared")
						
		names[headed_content[c]["fullname"]].append(c)
	else:
		names[headed_content[c]["fullname"]] = [c]

with open(UNRESOLVED_PAIRS_FILE, "r") as upf:
	deps_dict = json.load(upf)
	additional_pairs = [deps_dict[k] for k in deps_dict if len(deps_dict[k]) > 1]

for p in additional_pairs:
	c = p[0]
	c2 = p[1]
	with open(content[c], "r") as decl_file:
			d1 = json.load(decl_file)
	with open(content[c2], "r") as decl_file:
				d2 = json.load(decl_file)
	if datetime.strptime(headed_content[c]["lastmodified_date"], "%Y-%m-%d") < datetime.strptime(headed_content[c2]["lastmodified_date"], "%Y-%m-%d"):
		deprecated_list.append(c)
	elif datetime.strptime(headed_content[c]["lastmodified_date"], "%Y-%m-%d") > datetime.strptime(headed_content[c2]["lastmodified_date"], "%Y-%m-%d"):
		deprecated_list.append(c2)
	else:
		try:
			asked_type = ask_types(c, c2)
			deprecated_list.append(asked_type)
		except Exception:
			print("website seems to be unavailable, skipping this part")

with open(settings.DEPRECATED_FILE, "w") as df:
	json.dump(deprecated_list, df)


browser.quit()
