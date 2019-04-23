import os
import shutil

for filename in os.listdir('formatted data'):
    if filename[:6] == 'arLitt':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\arLitr{filename[6:]}")
        continue
    if filename[:5] == 'mnEly':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\mnElyx{filename[5:]}")
        continue
    if filename[:6] == 'moSt.L':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\moStLo{filename[6:]}")
        continue
    if filename[:6] == 'mtCut ':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\mtcban{filename[6:]}")
        continue
    if filename[:6] == 'mtMile':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\mtmcit{filename[6:]}")
        continue
    if filename[:6] == 'njNeww':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\njNewa{filename[6:]}")
        continue
    if filename[:6] == 'paBrad':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\pawbar{filename[6:]}")
        continue
    if filename[:6] == 'sdSiou':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\sdsfal{filename[6:]}")
        continue
    if filename[:6] == 'txAbli':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\txAbil{filename[6:]}")
        continue
    if filename[:6] == 'vaNorf':
        shutil.move(f'formatted data\\{filename}', f"formatted data\\vanfol{filename[6:]}")
        continue