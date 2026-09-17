#!/usr/bin/env python3
"""Create two offline LSP projects: binary-only JAR and attached-source JAR.

Requires a JDK (javac on PATH). Writes only inside the supplied new directory.
No Maven/Gradle downloads or changes to the user's dependency caches.
"""

import argparse
import json
from pathlib import Path
import shutil
import subprocess
import zipfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    root = args.directory.resolve()
    root.mkdir(parents=True, exist_ok=False)
    fixture = Path(__file__).parent / "fixtures" / "navigation"
    classes = root / "classes"
    classes.mkdir()
    javac = Path(shutil.which("javac") or "javac").resolve()
    subprocess.run([str(javac), "--release", "17", "-g", "-d", str(classes),
                    str(fixture / "library" / "demo" / "Greeter.java")], check=True)
    binary = root / "greeter.jar"
    sources = root / "greeter-sources.jar"
    with zipfile.ZipFile(binary, "w") as jar:
        for file in classes.rglob("*.class"):
            jar.write(file, file.relative_to(classes))
    with zipfile.ZipFile(sources, "w") as jar:
        jar.write(fixture / "library" / "demo" / "Greeter.java", "demo/Greeter.java")
    for name, with_sources in [("binary", False), ("sources", True)]:
        project = root / name
        src = project / "src"
        src.mkdir(parents=True)
        shutil.copyfile(fixture / "Main.kt", src / "Main.kt")
        roots = [{"path": str(binary), "type": "CLASSES"}]
        if with_sources:
            roots.append({"path": str(sources), "type": "SOURCES"})
        model = {
            "modules": [{"name": name, "dependencies": [
                {"type": "inheritedSdk"}, {"type": "moduleSource"},
                {"type": "library", "name": "greeter", "scope": "compile"}],
                "contentRoots": [{"path": str(project), "sourceRoots": [
                    {"path": str(src), "type": "java-source"}]}]}],
            "libraries": [{"name": "greeter", "type": "JpsJavaLibraryType", "roots": roots}],
        }
        (project / "workspace.json").write_text(json.dumps(model, indent=2) + "\n")
    (root / "jdk-home").write_text(str(javac.parent.parent))
    print(root)


if __name__ == "__main__":
    main()
