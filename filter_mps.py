import csv
import json
import settings, common

MPS_FILE = "mp_list.csv"
DEFINED_FILE = "defined.csv"
UNRESOLVED_PAIRS_FILE = "unresolved.json"

HEADERS = ["council", "region", "person_name", "birth_year", "full_info", "guid"]


headed_content = common.load_headed_content()
content = common.load_content()
deprecated = common.load_deprecated()

with open(MPS_FILE) as mf:
	rows = csv.reader(mf)
	mps = [r[0] for r in rows]

of = open(DEFINED_FILE, "w")
defwriter = csv.writer(of)
defwriter.writerow(HEADERS)

mps_list = {}
counter = 0
lower_case_mps = [m.lower() for m in mps]
for c in headed_content:
	if c not in deprecated:	
		if headed_content[c]["fullname"].lower() in lower_case_mps and headed_content[c]["responsiblePosition"] == "Народний депутат України" and headed_content[c]["declarationYear"] == "2015":
			if not headed_content[c]["fullname"].lower() in mps_list:
				mp_fullname_index = lower_case_mps.index(headed_content[c]["fullname"].lower())
				defwriter.writerow(['','',mps[mp_fullname_index],'','', c])
				counter += 1
				mps_list[headed_content[c]["fullname"].lower()] = [c]
			else:
				mps_list[headed_content[c]["fullname"].lower()].append(c)
print(counter)
with open(UNRESOLVED_PAIRS_FILE, "w") as urp:
	json.dump(mps_list, urp)

of.close()
