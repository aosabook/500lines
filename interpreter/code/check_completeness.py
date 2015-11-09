

if __name__ == '__main__':
	with open('chapter.txt') as f:
		chapter = f.readlines()
		chapter_set = set(chapter)
	with open('byterun/pyvm2.py') as g:
		source = g.readlines()
		source_set = set(source)

	zero_missing = True
	for line in source:
		if line and line not in chapter_set:
			if zero_missing:
				print "Missing lines of source!"
				zero_missing = False
			print line,

	for line in chapter:
		if line not in source_set and line not in ["```", "~~~~"]:
			print line,


