import json
from pyquery import PyQuery as pq
from time import sleep
import settings, common, refresh_content

HTML_TEMPLATE = "https://public.nazk.gov.ua/declaration/{g:s}"

REGION_SELECTOR = "div.person-info label"
REGIONS_FILE = "regions.json"

SLEEP_TIME = 1

def extract_oblast(s):
    sp = s.split("/")
    oblast = [p for p in sp if "область" in p.lower() or "Київ " in p.lower()]
    if len(oblast) == 0:
        return s
    else:
        return oblast[0]

def get_region(guid):
    link = HTML_TEMPLATE.format(g = guid)
    print(link)
    page = pq(link)
    divs = page(REGION_SELECTOR)
    region_element = [d for d in divs if " Місто, селище чи село: " in d.text]
    if len(region_element) > 1:
        region_element = region_element[1]
    elif len(region_element) == 1: 
        region_element = region_element[0]
    else:
        return {"oblast":"", "full_region_info":""}
    region = pq(region_element).parent().text()
    print(region)
    oblast = pq(region_element).parent().text().split(":")[1].strip()
    oblast = extract_oblast(region)
    return {"oblast":region, "full_region_info":region}
    
content = common.load_content()
regions = common.load_regions()
for c in content:
	if not c in regions.keys():
		sleep(SLEEP_TIME)
		try:
			regions[c] = get_region(c)
		except Exception: 
			sleep(SLEEP_TIME * 100)
			regions[c] = get_region(c)
		with open(settings.REGIONS_FILE, "w") as rf:
			json.dump(regions, rf)
