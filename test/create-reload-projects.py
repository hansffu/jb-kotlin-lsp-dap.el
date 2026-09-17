#!/usr/bin/env python3
"""Create offline Gradle reload fixtures using an installed Gradle distribution.

Each project initially lacks the locally built Greeter dependency. The smoke
test adds it via a build-file save, then removes it. No plugin downloads needed.
"""

import argparse
from pathlib import Path
import shutil
import subprocess
import zipfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--gradle-home", required=True, type=Path)
    args = parser.parse_args()
    gradle = args.gradle_home.resolve()
    if not (gradle / "lib").is_dir() or not (gradle / "bin/gradle").is_file():
        parser.error("--gradle-home must contain bin/gradle and lib/")
    root = args.directory.resolve()
    root.mkdir(parents=True, exist_ok=False)
    # A local wrapper URL lets the Tooling API use Gradle without downloading it
    # or touching the user's Gradle cache. Keep executable file attributes.
    archive = root / "gradle-local.zip"
    with zipfile.ZipFile(archive, "w") as bundle:
        bundle.write(gradle, gradle.name)
        for file in gradle.rglob("*"):
            bundle.write(file, Path(gradle.name) / file.relative_to(gradle))
    classes = root / "classes"
    classes.mkdir()
    javac = Path(shutil.which("javac") or "javac").resolve()
    fixture = Path(__file__).parent / "fixtures/navigation/library/demo/Greeter.java"
    subprocess.run([str(javac), "--release", "17", "-d", str(classes), str(fixture)], check=True)
    jar = root / "greeter.jar"
    with zipfile.ZipFile(jar, "w") as bundle:
        for file in classes.rglob("*.class"):
            bundle.write(file, file.relative_to(classes))
    for kind in ("groovy", "kotlin"):
        project = root / kind
        source = project / "src/main/java"
        source.mkdir(parents=True)
        (source / "Main.kt").write_text(
            "import demo.Greeter\nimport java.util.ArrayList\n"
            "fun main() { val values = ArrayList<String>(); println(Greeter()) }\n")
        suffix = ".kts" if kind == "kotlin" else ""
        (project / ("build.gradle" + suffix)).write_text('plugins { id("java") }\n')
        (project / ("settings.gradle" + suffix)).write_text(f'rootProject.name = "reload-{kind}"\n')
        (project / "gradle.properties").write_text("org.gradle.daemon.idletimeout=10000\n")
        wrapper = project / "gradle/wrapper"
        wrapper.mkdir(parents=True)
        (wrapper / "gradle-wrapper.properties").write_text(
            f"distributionUrl={archive.as_uri()}\n")
    (root / "jdk-home").write_text(str(javac.parent.parent))
    print(root)


if __name__ == "__main__":
    main()
