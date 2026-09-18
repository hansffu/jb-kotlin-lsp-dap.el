#!/usr/bin/env python3
"""Create isolated Gradle/Maven Kotlin debug fixtures from local dependencies."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import zipfile
import urllib.request

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('directory', type=Path)
p.add_argument('--gradle-home', type=Path, required=True)
p.add_argument('--maven-repository', type=Path, required=True)
p.add_argument('--download-missing-docs', action='store_true', help='Fetch missing stdlib documentation into the private repository')
a = p.parse_args()
root = a.directory.resolve()
root.mkdir(parents=True, exist_ok=False)
repo = root / 'repository'
subprocess.run(['cp', '-a', '--reflink=auto', str(a.maven_repository.resolve()), str(repo)], check=True)
gradle = a.gradle_home.resolve()
archive = root / 'gradle-local.zip'
with zipfile.ZipFile(archive, 'w') as z:
    z.write(gradle, gradle.name)
    for f in gradle.rglob('*'):
        z.write(f, Path(gradle.name) / f.relative_to(gradle))
compiler_coords = [
    ('org/jetbrains/kotlin', 'kotlin-compiler', '2.2.21'),
    ('org/jetbrains/kotlin', 'kotlin-stdlib', '2.2.21'),
    ('org/jetbrains/kotlin', 'kotlin-script-runtime', '2.2.21'),
    ('org/jetbrains/kotlin', 'kotlin-reflect', '1.6.10'),
    ('org/jetbrains/kotlinx', 'kotlinx-coroutines-core-jvm', '1.8.0'),
    ('org/jetbrains', 'annotations', '13.0'),
]
jars = [repo / group / name / ver / f'{name}-{ver}.jar' for group, name, ver in compiler_coords]
for jar in jars:
    if not jar.is_file(): raise SystemExit(f'Missing local compiler dependency: {jar}')
stdlib = jars[1]
# The server resolves library documentation along with runtime artifacts.
# Missing attachments can make an offline Maven import omit the library.
for classifier in ('sources', 'javadoc'):
    attachment = stdlib.with_name(f'kotlin-stdlib-2.2.21-{classifier}.jar')
    if not attachment.exists():
        if not a.download_missing_docs:
            raise SystemExit(f'Missing {attachment}; use --download-missing-docs to fetch it')
        url = 'https://repo.maven.apache.org/maven2/org/jetbrains/kotlin/kotlin-stdlib/2.2.21/' + attachment.name
        urllib.request.urlretrieve(url, attachment)
fixture = Path(__file__).parent / 'fixtures/debug/Main.kt'
for kind in ('gradle', 'maven'):
    project = root / kind
    source = project / 'src/main/kotlin/demo'
    source.mkdir(parents=True)
    shutil.copyfile(fixture, source / 'Main.kt')
    if kind == 'gradle':
        (project / 'settings.gradle').write_text("rootProject.name = 'debug-fixture'\n")
        (project / 'build.gradle').write_text('''plugins { id 'java' }
sourceSets.main.java.srcDirs = ['src/main/kotlin']
dependencies { implementation files(%s) }
tasks.register('compileKotlin', JavaExec) {
    classpath = files(%s)
    mainClass = 'org.jetbrains.kotlin.cli.jvm.K2JVMCompiler'
    args '-no-stdlib', '-no-reflect', '-jvm-target', '17', '-classpath', %s,
         '-d', layout.buildDirectory.dir('classes/java/main').get().asFile.absolutePath,
         file('src/main/kotlin/demo/Main.kt').absolutePath
}
tasks.named('classes') { dependsOn 'compileKotlin' }
''' % (json.dumps(str(stdlib)), ','.join(json.dumps(str(j)) for j in jars), json.dumps(str(stdlib))))
        (project / 'gradle.properties').write_text('org.gradle.daemon.idletimeout=10000\n')
        wrapper = project / 'gradle/wrapper'
        wrapper.mkdir(parents=True)
        (wrapper / 'gradle-wrapper.properties').write_text(f'distributionUrl={archive.as_uri()}\n')
    else:
        (project / 'pom.xml').write_text('''<project xmlns="http://maven.apache.org/POM/4.0.0">
<modelVersion>4.0.0</modelVersion><groupId>demo</groupId><artifactId>debug-fixture</artifactId><version>1</version>
<properties><kotlin.compiler.daemon>false</kotlin.compiler.daemon></properties>
<dependencies><dependency><groupId>org.jetbrains.kotlin</groupId><artifactId>kotlin-stdlib</artifactId><version>2.2.21</version></dependency></dependencies>
<build><sourceDirectory>src/main/kotlin</sourceDirectory><plugins>
<plugin><groupId>org.apache.maven.plugins</groupId><artifactId>maven-install-plugin</artifactId><version>3.1.3</version></plugin>
<plugin><groupId>org.jetbrains.kotlin</groupId><artifactId>kotlin-maven-plugin</artifactId><version>2.2.21</version><executions><execution><id>compile</id><phase>compile</phase><goals><goal>compile</goal></goals></execution></executions><configuration><jvmTarget>17</jvmTarget></configuration></plugin>
<plugin><groupId>org.apache.maven.plugins</groupId><artifactId>maven-resources-plugin</artifactId><version>3.3.1</version></plugin>
<plugin><groupId>org.apache.maven.plugins</groupId><artifactId>maven-compiler-plugin</artifactId><version>3.14.0</version></plugin>
</plugins></build></project>\n''')
        config = project / '.mvn'
        config.mkdir()
        (config / 'maven.config').write_text(f'--offline\n-Dmaven.repo.local={repo}\n')
(root / 'jdk-home').write_text(str(Path(shutil.which('javac')).resolve().parent.parent))
(root / 'stdlib').write_text(str(stdlib))
print(root)
