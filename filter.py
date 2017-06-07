import json
from csv import reader, writer

import settings, common

SHORTLIST_LONG_NAME = "shortlist_long.csv"
DEFINED_FILE = "defined.csv"
UNDEFINED_FILE = "undefined.csv"

CONFIDENCE_MARKERS = {"postType":"самоврядування", "workPlace":"рада", "workPost":"депутат"}

def check_confidence(d):
	#global conf
	for m in CONFIDENCE_MARKERS:
		if CONFIDENCE_MARKERS[m].lower() in d[m].lower():
			#conf += 1
			return True
	#print(d)
	return False

def extract_town(s):
    sp = s.split("/")
    town = sp[0]
    #print(town)
    return town

content = common.load_content()
headed_content = common.load_headed_content()
deprecated = common.load_deprecated()
deprecated.remove("933718bc-b5a5-43a9-ba92-db402213a130")


with open(SHORTLIST_LONG_NAME, "r") as slf:
	rows = reader(slf)
	lines = [r for r in rows]
	names = [r[2] for r in lines]
	regions = [r[1] for r in lines]

conf = 0
df = open(DEFINED_FILE, 'w')
defwriter = writer(df)
defwriter.writerow(lines[0] + ['guid'])

udf = open(UNDEFINED_FILE, 'w')
undefwriter = writer(udf)
undefwriter.writerow(lines[0] + ['guid'])

with open(settings.PREDEFINED_FILE, "r") as pdf:
	predefined = reader(pdf)
	predef = [l[0:5] for l in predefined]

for p in predef[1:]:
	print(p)
	lines.remove(p)

with open("false_positive.csv", "r") as fp:
	fpr = reader(fp)
	print(fpr)
	false_positive = [f[0].strip() for f in fpr]

print(false_positive)


for l in lines[1:]:
	name = l[2]
	decls = [k for k in headed_content if headed_content[k]['fullname'] == name and headed_content[k]["declarationYear"] == '2016' and not (k in deprecated) and not (k in false_positive)]
	undefined = decls[:]
	indices = [i for i in range(len(names)) if names[i] == name]
	if len(indices) == 1:
		if len(decls) == 1:
			l.append(decls[0])
			conf += 1
		elif len(decls) > 0:
			selfgovernance_decl = [d for d in decls if check_confidence(headed_content[d])]
			if len(selfgovernance_decl) == 1:
				l.append(selfgovernance_decl[0])
				conf += 1
			elif len(selfgovernance_decl) > 0:
				same_region = [d for d in selfgovernance_decl if headed_content[d]['oblast'] == l[1]]
				if len(same_region) == 1:
					l.append(same_region[0])
					conf += 1
				else:
					same_town = [d for d in decls if extract_town(headed_content[d]['adress']) in l[4]]
					if len(same_town) == 1:
						l.append(same_town[0])
						conf += 1
					elif len(same_town) > 1:
						undefined = same_town
					else:
						print(same_town)
			else:
				same_region = [d for d in decls if headed_content[d]['oblast'] == l[1]]
				if len(same_region) == 1:
					l.append(same_region[0])
					conf += 1
				else:
					same_town = [d for d in decls if extract_town(headed_content[d]['adress']) in l[4]]
					if len(same_town) == 1:
						l.append(same_town[0])
						conf += 1
					elif len(same_town) > 1:
						undefined = same_town
	else:
		same_region = [d for d in decls if headed_content[d]['oblast'] == l[1]]
		if len(same_region) == 1:
			l.append(same_region[0])
			conf += 1
		elif len(same_region) > 1:
			undefined = same_region
	if len(l) == 6:
		defwriter.writerow(l)
	else:
		if len(undefined) > 0:
			for u in undefined:
				undefwriter.writerow(l + [u])
		else:
			undefwriter.writerow(l)
with open(settings.PREDEFINED_FILE, "r") as pdf:
	predefined = reader(pdf)
	predef = [l for l in predefined]
print("Ці декларації могли застаріти")
for l in predef[1:]:
	if not l[-1] in deprecated:
		defwriter.writerow(l)
	else:
		print(l)
df.close()
udf.close()

