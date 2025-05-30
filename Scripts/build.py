import os
import sys

print("PYTHON PREBUILD SCRIPT")

path = ''.join(sys.argv.pop(0))
path = path.removesuffix("Scripts/build.py")
path += "Standard" 

os.environ["CLEAR_STANDARD_LIBRARY_PATH"] = path 