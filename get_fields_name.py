import json
import settings, common

STEP_TEMPLATE = "step_{n:d}"

def get_step(step, d):
	s = d['data'][step]
	if step != 'step_1':
		if type(s) == type(dict()):
			return_keys = []
			for k in s:
				if type(s[k]) == type(dict()):
					if step != 'step_16':
						return_keys = list(set(return_keys + list(s[k].keys())))
					else:
						s2 = s[k]
						for k2 in s2:	
							if type(s2[k2]) == type(dict()):
								return_keys = list(set(return_keys + list(s2[k2].keys())))
			return return_keys
		else:
			return []
	else:
		if type(s) == type(dict()):
			return list(s.keys())
		else:
			return []
			
steps = {}
content = common.load_content()
headed_content = common.load_headed_content()

for i in range(1,17):
	step = STEP_TEMPLATE.format(n = i)
	print(step)
	steps[step] = []
	counter = 0
	for c in headed_content:
		with open(content[c], 'r') as decl_file:
			decl = json.load(decl_file)
			steps[step] = list(set(steps[step] + get_step(step, decl)))
		if counter == 100:
			break
		counter += 1
	steps[step] = list(set(steps[step]) - set(['rights']))

with open(settings.FIELDS_FILE, 'w') as hf:
	json.dump(steps, hf)

print(steps)
