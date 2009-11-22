
import os

os.system('python qq.py > qq.pro')
os.system('"C:/Program Files/SWI-Prolog/bin/plcon.exe" -q -g halt -f qq.pro > qq1.py')
print ':)' if open('qq.py').read()==open('qq1.py').read() else ':('