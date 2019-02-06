import os
import sys

sys.stdout = open(os.devnull, 'w')
print("HI")