import tomllib
import os
import sys

path = ''.join(sys.argv.pop(0))
path = path.removesuffix("Scripts/Build.py")

error_path = os.path.join(path, "Scripts", "Errors.toml")
error_header = os.path.join(path, "Source", "Diagnostics", "DiagnosticCode.h")

def load_errors():
    global path

    with open(error_path, 'rb') as f:
        config = tomllib.load(f)

    enums    = "\tenum DiagnosticCode\n\t{\n"
    messages = "\tinline const char* g_DiagnosticMessages[] = {\n"
    advices  = "\tinline const char* g_DiagnosticAdvices[] = {\n"

    for key, value in config.items():
        enums += f"\t\tDiagnosticCode_{key},\n"
        messages += f"\t\t\"{value['Message']}\",\n"
        advices += f"\t\t\"{value['Advice']}\",\n"

    enums += "\t\tDiagnostic_Count\n"

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
    