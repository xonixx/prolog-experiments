
s = open('1.html').read().replace('<br>', '<br>\n')

import re

s = re.sub(r'<div.*?>', '', s)
s = re.sub(r'</div.*?>', '', s)

open('1_1.html', 'w').write(s)