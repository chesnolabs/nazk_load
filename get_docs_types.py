from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
import re
import random
import os
from pyquery import PyQuery as pq
import json

import settings, common

LINKS_RE = re.compile("/declaration/(?P<GUID>\w{8}-\w{4}-\w{4}-\w{4}-\w{12})")

NEXT_PAGE_SELECTOR = "//ul[@id='table-pagination']/li/a/span[text()='Наступна']"
PREVIOUS_PAGE_SELECTOR = "//ul[@id='table-pagination']/li/a[@data-page={page:d}]/span[text()='Попередня']"
LAST_PAGE_SELECTOR = "//ul[@id='table-pagination']/li[last()]/a[@class='pager']"
START_PAGE = "https://public.nazk.gov.ua/search"

DECL_SELECTOR = "a.break-words"

SLEEP_TIME = 2.5
LONG_WAIT = 60

def get_new_guids(backwards = False):
    global types 
    browser = webdriver.Firefox()
    browser.get(START_PAGE)
    data = []
    if backwards:
        elem = WebDriverWait(browser, LONG_WAIT).until(
            EC.presence_of_element_located((By.XPATH, LAST_PAGE_SELECTOR))
            )
        page_number = int(elem.get_attribute('data-page')) - 1
        page_selector_pattern = PREVIOUS_PAGE_SELECTOR
        elem.click()
        inc = -1
        sleep(SLEEP_TIME*3)
    else:
        inc = 1
        page_selector_pattern = NEXT_PAGE_SELECTOR
        page_number = 2
    while True:
        print(page_number-1)
        page = browser.page_source
        page_pairs = get_pairs(page)
        types.update(page_pairs)
        with open(settings.DOCS_TYPE_FILE, "w") as tf:
            json.dump(types, tf)
        selector = page_selector_pattern.format(page = page_number)
        elem = WebDriverWait(browser, LONG_WAIT).until(
            EC.presence_of_element_located((By.XPATH, selector))
            )
        elem.click()
        page_number += inc
        sleep(SLEEP_TIME)
    browser.quit()
    return data
        
def check_repetion(new_guids):
    checked = []
    for ng in new_guids:
        if not(ng in guids):
            checked.append(ng)
    return checked

def get_guids_from_file(f):
    l = []
    with open(f, 'r') as gf:
        for g in gf:
            l.append(g.strip())
    return l

def get_pairs(page):
	with open("temp_file.html", "w") as tf:
		tf.write(page)
	page = pq(filename = "temp_file.html")
	decl = page(DECL_SELECTOR)
	pairs = {}
	for d in decl:
		d = pq(d)
		#print(d.attr("href"))
		guid = LINKS_RE.fullmatch(d.attr("href")).group('GUID')
		t = d.parent().next().next().text()
		pairs[guid] = t
	return pairs
    
types = common.load_types()
get_new_guids()
