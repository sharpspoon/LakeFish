#!C:\Users\I868538\source\repos\sharpspoon\LakeFish\LakeFish\env\Scripts\python.exe
# EASY-INSTALL-ENTRY-SCRIPT: 'pyjade==2.2.0','console_scripts','pyjade'
__requires__ = 'pyjade==2.2.0'
import re
import sys
from pkg_resources import load_entry_point

if __name__ == '__main__':
    sys.argv[0] = re.sub(r'(-script\.pyw?|\.exe)?$', '', sys.argv[0])
    sys.exit(
        load_entry_point('pyjade==2.2.0', 'console_scripts', 'pyjade')()
    )
