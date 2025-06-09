import tomllib
import os
import sys

path = ''.join(sys.argv.pop(0))
path = path.removesuffix("Scripts/Build.py")

error_path = os.path.join(path, "Scripts", "Errors.toml")
error_header = os.path.join(path, "Source", "Errors", "ErrorCode.h")

def load_errors():
    global path

    with open(error_path, 'rb') as f:
        config = tomllib.load(f)

    enums    = "\tenum ErrorCode\n\t{\n"
    messages = "\tstatic const char* g_ErrorMessages[] = {\n"
    advices  = "\tstatic const char* g_ErrorAdvices[] = {\n"

    for key, value in config.items():
        enums += f"\t\tErrorCode_{key},\n"
        messages += f"\t\t\"{value['Message']}\",\n"
        advices += f"\t\t\"{value['Advice']}\",\n"

    enums += "\t\tErrorCode_Count\n"

    buffer = "#pragma once\n"
    buffer += "namespace clear\n{\n"

    buffer += enums
    buffer += "\t};\n"

    buffer += messages
    buffer += "\t};\n"

    buffer += advices 
    buffer += "\t};\n"

    buffer += "}\n"

    with open(error_header, "w") as f:
        f.write(buffer)

load_errors()
    