
import os

os.system('python qqq.py > qqq1.py')
os.system('python qqq1.py > qqq2.py')
print ':)' if open('qqq.py').read()==open('qqq2.py').read() else ':('