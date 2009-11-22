

import sys, re

TAB_LEN = 4
NBSP = "&nbsp;"
BR = "<br>"
CODE = '<blockquote><code><font size="2" face="Courier New">%s</font></code></blockquote>'
FNT = '<font color="%s">%s</font>'
B = "<b>%s</b>"
FNTB = FNT % ("%s", B)
FNTI = FNT % ("%s", "<i>%s</i>")
FNTBI = FNTB % ("%s", "<i>%s</i>")

if (len(sys.argv) != 3):
	print("""Usage:
	hlpl.py inp.pl out.html
	""")
	sys.exit(1)

_, inp, outp = sys.argv

code = open(inp).read()

STR_M = "###s#"
ATOM_M = "###a#"
GARBAGE = "|||||"

keywords = """
is dynamic style_check use_module
true fail false append member read write nl
assert retractall retract forall findall
atom_concat concat_atom concat
number atom
throw catch repeat
""".split()

keywords1 = """
= ! \+
""".split()

def hl(code):
	strs = []
	def replStrF(mo):
		strs.append(mo.group(1))
		return STR_M + str((len(strs) - 1))

	atoms = []
	def replAtomF(mo):
		atoms.append(mo.group(1))
		return ATOM_M + str((len(atoms) - 1))

	def stringBack(mo):
		return FNTB % ("#008000", strs[int(mo.group(1))])

	def atomBack(mo):
		return FNT % ("#008000", atoms[int(mo.group(1))])
		
	# strings
	code = re.sub(r"(?s)(\".*?\")", replStrF, code)

	# atoms
	code = re.sub(r"('.*?')", replAtomF, code)

	# keywords
	# first of all =
	for k in keywords1:
		code = code.replace(k, FNTB % ("blue", k))

	code = re.sub(r"\b(%s)\b" % "|".join(keywords), FNTB % ("blue", r"\1" + GARBAGE), code)

	# predicates
	code = re.sub(r"\b([a-z_]\w*)(?=\(|\s*:-|\s*\.|\s*,|\s*(\]|\)|\}|\|))", B % r"\1", code)

	# vars
	code = re.sub(r"\b([A-Z]\w*)\b", FNTI % ("#808000", r"\1"), code)

	# comments
	code = re.sub(r'(?m)(%.*?)$', FNTI % ("#909090", r"\1"), code)

	# strings back
	code = re.sub(STR_M + "(\d+)", stringBack, code)

	# atoms back
	code = re.sub(ATOM_M + "(\d+)", atomBack, code)

	# kill garbage
	code = code.replace(GARBAGE, "")

	# whitespaces
	#code = code.replace(" ", NBSP).replace("\t", NBSP * TAB_LEN)
	code = code.replace("\t", " " * TAB_LEN)

	def replSpace(mo):
		return NBSP * len(mo.group(1))

	code = re.sub(r"(?m)^( +)", replSpace, code)

	# newlines
	#code = code.replace("\n", BR)

	code = CODE % code
	
	return code

code = re.sub(r'(?s)\{pl(.*?)pl\}', lambda mo: hl(mo.group(1)), code)
open(outp, "w").write(code)	
#open(outp, "w").write(hl(code))