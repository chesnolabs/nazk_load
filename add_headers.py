import json
import re
import settings, common

def extract_oblast(s):
    sp = re.split("\s+\/\s+",s)
    oblast = [p for p in sp if "область" in p.lower() or "київ" in p.lower()]
    if len(oblast) == 0:
        return s
    else:
        return oblast[0]

content = common.load_content()
headed_content = common.load_headed_content()
regions = common.load_regions()

for c in content:
	with open(content[c], "r") as jf:
		decl = json.load(jf)
	if not "changesYear" in decl["data"]["step_0"]:
		declarationType = decl["data"]["step_0"]['declarationType']
		if declarationType != '2':
			print(c)
			declarationYear = decl["data"]["step_0"]['declarationYear' + declarationType]
			created_date = common.change_date_format(decl['created_date'])
			lastmodified_date = common.change_date_format(decl['lastmodified_date'])
			responsiblePosition = decl["data"]["step_1"]["responsiblePosition"]
			corruptionAffected = decl["data"]["step_1"]["corruptionAffected"]
			workPlace = decl["data"]["step_1"]["workPlace"]
			workPost = decl["data"]["step_1"]["workPost"]
			postType = decl["data"]["step_1"]["postType"]
			decl_id = decl["id"]
			adress = regions[c]['full_region_info'].replace("Місто, селище чи село:","").strip()
			oblast = extract_oblast(adress)
			print(oblast)
			fullname = ' '.join([decl["data"]["step_1"]["lastname"], decl["data"]["step_1"]["firstname"], decl["data"]["step_1"]["middlename"]]).title()
			headed_content[c] = {"id":decl_id, "declarationType":declarationType, "declarationYear":declarationYear,
								"created_date":created_date, "lastmodified_date":lastmodified_date,
								"fullname":fullname, "workPlace":workPlace, "workPost":workPost, "postType":postType,
								"oblast":oblast, "adress":adress, "corruptionAffected":corruptionAffected,
								"responsiblePosition":responsiblePosition}
with open(settings.HEADERS_FILE, "w") as hf:
	json.dump(headed_content, hf)


