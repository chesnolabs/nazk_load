import json
import os
from csv import writer

import settings, common

BLOCKS = ["Інформація про суб'єкта декларування", "Про членів сім'ї", "Нерухомість", "Незавершене будівництво", "Цінне нерухоме майно", "Транспортні засоби", "Цінні папери", "Корпоративні права", "Юридичні особи", "Нематеріальні активи", "Доходи", "Грошові активи", "Фінансові зобов'язання", "Видатки та правочини", "Робота за сумісництвом", "Членство в організаціях"]

RIGHTS_FIELDS = ["ownershipType","otherOwnership","percent-ownership"]

STEP = "step_{n:d}"

FIELDS_FILE = "fields_all.json"

def unpack_dict(l):
	if type(l) == type(dict()):
		return l
	elif l == []:
		return []
	else:
		return unpack_dict(l[0])

def check_blank_list(l):
	if l == []:
		return True
	elif type(l) != type(list()):
		return False
	else:
		return check_blank_list(l[0])

def unpack_organisations(b, common_columns):
	fields = INTERESTING_FIELDS["step_16"]
	types = ['org', 'part_org']
	r = []
	for t in types:
		if not check_blank_list(b[t]):
			for k in b[t]:
				r.append(common_columns + [t])
				record = b[t][k]
				for f in fields:
					if f in record:
						r[-1].append(record[f])
					else:
						r[-1].append('')
	if r != []:
		filename = os.path.join(settings.CSV_FOLDER, "step_16" + ".csv")
		headers = COMMON_COLUMNS_HEADERS + ["type"] + fields
		if not (os.path.exists(filename)):
			out_f = open(filename, 'w')
			csvwriter = writer(out_f)
			csvwriter.writerow(headers)
			out_f.close()
		with open(filename, 'a+') as out_f:
			csvwriter = writer(out_f)
			for row in r:
				csvwriter.writerow(row)
		

#sn for step number, block for the step data, 
def get_step(sn, block, common_columns):
	global  rights_keys, rights_keys_written
	rows = []
	step = STEP.format(n = sn + 1)
	fields = INTERESTING_FIELDS[step]
	if fields != [] :
		there_were_rights = False
		block = unpack_dict(block)
		if not(check_blank_list(block)) and not(len(block) == 1 and "empty" in block.keys()):
			for k in block.keys():
				record = block[k]
				object_id = k
				r = common_columns[:] + [object_id]
				for f in fields:
					if type(record) == type(dict()):
						if f in record.keys():
							r.append(record[f])
						else:
							r.append('')
					else:
						r.append('')
				if type(record) == type(dict()) and "rights" in record.keys():
					there_were_rights = True
					rights = record['rights']
					if type(rights) == type(dict()) and len(rights) > 0:
						for rights_record in rights.keys():
							rr = rights[rights_record]
							if rights_record == record['person']:
								for rk in RIGHTS_FIELDS:
									r.append(rr[rk])
							else:
								if not rights_keys_written:
									rights_keys = list(rr.keys())
									rights_writer.writerow(['object_id'] + rights_keys)
									rights_keys_written = True
								rights_row = [object_id]
								for rk in rights_keys:
									if rk in rr.keys():
										rights_row.append(rr[rk])
									else:
										rights_row.append('')
								rights_writer.writerow(rights_row)
				rows.append(r)
			filename = os.path.join(settings.CSV_FOLDER, STEP.format(n = sn + 1) + ".csv")
			headers = COMMON_COLUMNS_HEADERS + fields
			if there_were_rights:
				headers += RIGHTS_FIELDS
			if not (os.path.exists(filename)):
				out_f = open(filename, 'w')
				csvwriter = writer(out_f)
				csvwriter.writerow(headers)
				out_f.close()
			with open(filename, 'a+') as out_f:
				csvwriter = writer(out_f)
				for r in rows:
					csvwriter.writerow(r)
	
if not os.path.exists(settings.CSV_FOLDER):
	os.mkdir(settings.CSV_FOLDER)

for the_file in os.listdir(settings.CSV_FOLDER):
	file_path = os.path.join(settings.CSV_FOLDER, the_file)
	if os.path.isfile(file_path):
		os.unlink(file_path)

headed_content = common.load_headed_content()
content = common.load_content()

with open(settings.FIELDS_FILE, 'r') as ff:
	INTERESTING_FIELDS = json.load(ff)

arf = open(settings.ADDITIONAL_RIGHTS_FILE, 'w')
rights_writer = writer(arf)
rights_keys_written = False
	
for c in headed_content:
		for b in range(1,len(BLOCKS)):
			with open(content[c], "r") as df:
				decl = json.load(df)
			d = headed_content[c]
			COMMON_COLUMNS_HEADERS = list(d.keys()) + ["object_id"]
			step = STEP.format(n = b + 1)
			block = decl["data"][step]
			if step != "step_16":
				block_name = BLOCKS[b]
				if block != []:
					get_step(b, block, common_columns = [d[k] for k in d])
			else:
				if block != []:
					unpack_organisations(block, common_columns = [d[k] for k in d])

arf.close()
