# Installation von DrHaskell

## Installation per `cabal` (Linux)

Zur Installation von *DrHaskell* benötigen Sie den
[Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/)
in Version 7 oder höher sowie
[cabal](https://www.haskell.org/cabal/download.html) zur Installation von
Haskell-Paketen.
Am einfachsten ist es, wenn Sie sich die Haskell Platform installieren,
da darin bereits der Compiler, cabal und andere häufig genutzte Pakete enthalten
sind. Diese können Sie entweder herunterladen oder direkt aus den Paketquellen
der von Ihnen genutzten Distribution installieren. Unter Ubuntu geht dies
beispielsweise mit dem Kommando `sudo apt install haskell-platform`.

Als nächstes müssen Sie sich den Quellcode des *DrHaskell*-Tools herunterladen.
Dieser Quellcode liegt in einem [Git](https://git-scm.com/)-Repository.
Installieren Sie sich zunächst Git aus den Paketquellen Ihrer
Linux-Distribution. Unter Ubuntu geht dies beispielsweise mit dem Kommando
`sudo apt install git`.

Danach können Sie den Quellcode von *DrHaskell* mit folgendem Befehl aus dem
Repository in ein lokales Verzeichnis namens `drhaskell` klonen:

`git clone https://git.ps.informatik.uni-kiel.de/student-projects/mapro-2017-ss.git drhaskell`

Wechseln Sie nun in das `drhaskell`-Verzeichnis und geben Sie nacheinander
die folgenden Kommandos ein:

```
cabal update
cabal install quickcheck happy
```

Damit wird zunächst die `cabal`-Paketdatenbank aktualisiert sowie einige globale
Abhängigkeiten installiert.

Mit den folgenden Kommandos legen Sie dann eine lokale Sandbox an,
in der *DrHaskell* und alle seine Abhängigkeiten installiert werden.
Durch die Verwendung der Sandbox wird sichergestellt,
dass es zu keinen Versionskonflikten mit etwaigen bereits installierten
Haskell-Paketen kommen kann. Das Installieren der Abhängigkeiten kann etwas
Zeit in Anspruch nehmen.

```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal install
```

Die *DrHaskell*-Binary finden Sie nun in dem Verzeichnis
`drhaskell/.cabal-sandbox/bin`. Um auch global darauf zugreifen zu können,
sollten Sie dieses Verzeichnis in Ihren Pfad aufnehmen, indem Sie die
`$PATH`-Umgebungsvariable in der `.bashrc` geeignet erweitern.

## Installation über Docker

*DrHaskell* kann auch mit Hilfe einer Docker-Umgebung installiert und genutzt
werden.
Nachfolgend finden Sie eine Anleitung zur Installation von Docker unter
verschiedenen Betriebssystemen sowie die Beschreibung, wie man *DrHaskell* über
Docker installieren kann.

### Installation Docker (Windows)

Unter Windows (aktuell nur Windows 10 oder Windows Server 2016 als 64-Bit
Version mit aktiviertem Hyper-V) kann
[Docker for Windows](https://docs.docker.com/docker-for-windows/install/)
installiert werden. Die Installationsanleitung auf der Docker-Homepage umfasst
alle relevanten Teile der Installation.

Nachdem Docker for Windows installiert wurde, ist es erforderlich,
die Festplatte der Docker-VM bekannt zu machen.
Hierzu muss über das Kontextmenü von Docker for Windows aus der Symbolleiste
die Einstellungsseite aufgerufen werden.

![image001](/uploads/acf6c51e99a052369b2dfa1a33c59f9a/image001.png)

![image001-1](/uploads/b12fffcf6302cf72fc9a55a2138836d2/image001-1.png)

Der Menüpunkt "Shared Drives" bietet nun die gewünschten Optionen.
Im Normalfall genügt es, die entsprechende Festplatte mittels der Checkbox
auszuwählen und den Vorgang über "Apply" zu bestätigen.
Das gezeigte Powershell-Kommando ermöglicht es, die Verbindung zu testen.
Sollte das Kommando keine Ausgabe oder einen Fehler zeigen, so hilft es häufig,
die Berechtigungen zurückzusetzen.
Diese Funktionalität verbirgt sich hinter dem Button/Link "Reset credentials"
innerhalb des "Shared Drives"-Dialogs. Das Zurücksetzen der Berechtigungen ist
ebenfalls notwendig, wenn die Windows-Installation mit einem Domain Controller
verbunden und dieser nicht erreichbar ist und somit die Berechtigungen des
Windows-Benutzers nicht geprüft werden kann.

### Installation Docker (macOS)

Zuerst muss Docker mittels der [Docker-Toolbox]
(https://www.docker.com/products/docker-toolbox) installiert werden.
Im Normalfall wird durch die Installation bereits eine default-VM erstellt und
gestartet. Sollte dies nicht der Fall sein, muss per Kommandozeile der Befehl
`docker-machine create default` ausgeführt werden.

### Installation Docker (Linux)

Unter Linux ist die Docker-Toolbox nicht vorhanden. Die Installation erfolgt
vorzugsweise über die systemintegrierte Paketverwaltung wie z.B. `apt`,
`pacman`, `dnf`, `emerge`, ... und nach der Anleitung der jeweiligen Distribution
([Ubuntu](https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/#install-using-the-repository),
[Debian](https://docs.docker.com/engine/installation/linux/docker-ce/debian/),
[Generische Binärdateien](https://docs.docker.com/engine/installation/linux/docker-ce/binaries/)).
Falls dies nicht möglich ist, kann Docker mit einem Installationsskript
installiert werden:

`curl -fsSL get.docker.com -o get-docker.sh`

**Achtung:** Da das Skript per `sudo`-Kommando ausgeführt werden muss,
sollten Sie es **vor der Ausführung** auf schädliche Inhalte überprüfen.
Ausgeführt wird es dann mit dem Befehl `sudo sh get-docker.sh`.

## Docker Image herunterladen

Das Einspielen des eigentlichen Docker-Images ist mit einem Befehl erledigt.
Mit `docker pull jonasbusse/drhaskell` wird das aktuelle Image aus dem
Docker-Hub geladen und in der aktuell laufenden Docker-VM installiert.

## Startskripte installieren

Zur Nutzung von *DrHaskell* innerhalb des Docker-Containers sind im Ordner
`dockercalls` für macOS/Linux und Windows Startskripte hinterlegt.
Die Startskripte können auch unter folgendem
[Link](https://git.ps.informatik.uni-kiel.de/student-projects/mapro-2017-ss/tree/master/dockercalls)
heruntergeladen werden.
Unter macOS können die Skripte `drhaskell.sh` für die *DrHaskell*-REPL und
`drhaskell-lint.sh` für den *DrHaskell*-Linter (siehe unten) verwendet werden.
Für Linux sind die Skripte `linux-drhaskell.sh` und `linux-drhhaskell-lint.sh`
vorgesehen.
Die beiden verbleibenden Batch-Dateien sind unter Windows-Systemen zu verwenden.

Die Skripte für *DrHaskell* mounten das aktuelle Verzeichnis in die VM und
starten die *DrHaskell*-REPL innerhalb des gelinkten Ordners.
`drhaskell-lint` setzt zusätzlich noch eine Umgebungsvariable, um dem Linter
das richtige Pfadmapping mitzuteilen.

# Installation des DrHaskell-Linters

Neben der *DrHaskell*-REPL stellt das *DrHaskell*-Tool auch einen Linter für
einige IDEs bzw. Texteditoren zur Verfügung. Unterstützt werden derzeit
[Visual Studio Code](https://code.visualstudio.com/), [Atom](https://atom.io/)
und [Emacs](https://www.gnu.org/software/emacs/).

Im Folgenden finden Sie eine kurze Installationsanleitung für die verschiedenen,
unterstützten IDEs.

## Visual Studio Code

Installieren Sie sich zunächst die Extension
[haskell-linter](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter)
Danach müssen Sie die Benutzerkonfiguration so konfigurieren, dass anstelle der
hlint-Implementierung der Linter `drhaskell-lint` angesprochen wird.
Klicken Sie dazu auf "Code" - "Preferences" - "Settings":

![vscode3](/uploads/a29753ea0a4777adc80867b122173b69/vscode3.png)

Nun sollte sich eine geteilte Ansicht öffen. Auf der linken Seite werden
standardmäßig die default-Werte der Konfiguration angegeben. Auf der rechten
Seite stehen benutzerspezifische Wertzuweisungen.
Es reicht, die folgenden Zeilen in die Benutzerkonfiguration
aufzunehmen. Dabei ist darauf zu achten, dass die Gültigkeit des JSON-Strings
erhalten bleibt.

```
"haskell.hlint.executablePath": "drhaskell-lint",
"haskell.hlint.logLevel": "log",
"haskell.hlint.run": "onSave",
"haskell.hlint.hints": [
    "l1"
]
```

Innerhalb des `hints`-Konstrukts wird das aktuelle Level angegeben.
Aktuell stehen zur Auswahl: `l1`, `l2`, `l3` und `l0` (LevelFull).

## Atom

Um den *DrHaskell*-Linter unter Atom verwenden zu können, müssen Sie zunächst
die folgenden Atom-Pakete installieren:

 * `linter`: Basislinter für Atom
 * `language-haskell`: Syntaxhighlighting für Haskell in Atom
 * `linter-hlint`: Linting von Haskell-Code durch das Haskell-Tool `hlint`

Starten Sie dazu Atom und klicken Sie auf "Edit" - "Preferences" und dann auf
den Tab "Packages". Dort finden Sie eine Übersicht über die installierten
Atom-Pakete. Über das Suchfeld können Sie die obigen Pakete suchen und
installieren.

Anschließend müssen Sie noch die Konfiguration des Atom-Pakets `linter-hlint`
anpassen, das anstelle von der `hlint`-Binary die Binary `drhaskell-lint`
verwendet wird.
Klicken Sie dazu in der Übersicht der installierten Atom-Pakete unterhalb des
`linter-hlint`-Pakets auf "Settings" und schreiben Sie `drhaskell-lint` bzw.
den vollständigen Pfad zur Binary (falls die `drhaskell-lint`-Binary nicht in
Ihrer `$PATH`-Variable liegt) in das Feld "The hlint executable path".
In das Feld "List of hints to use" tragen Sie `l1` ein.
Damit legen Sie fest, dass beim Starten von Atom mit dem *DrHaskell*-Linter
initial die Sprachstufe 1 verwendet wird.
Der folgende Screenshot zeigt nochmals die angepasste Konfiguration.

![atom1](/uploads/0098c5f7ffbae8eeb55e6e3f56289264/atom1.jpeg)

## Emacs/Spacemacs

Zur Benutzung des Emacs-Plugins und des Linters werden die Pakete
`haskell-mode` und `flycheck` benötigt, welche durch das MELPA
(Milkypostman’s Emacs Lisp Package Archive) installiert werden können.
[Hier](https://melpa.org/#/getting-started) finden Sie eine Anleitung zur
Einrichtung von MELPA. Zur Installation von `haskell-mode` und `flycheck`
befolgen Sie die folgenden Anweisungen:

 * [Installation von haskell-mode](http://haskell.github.io/haskell-mode/manual/latest/Installation.html#Installation)
 * [Installation von flycheck](http://www.flycheck.org/en/latest/user/installation.html)

Das Emacs-Plugin können Sie sich
[hier](https://git.ps.informatik.uni-kiel.de/student-projects/mapro-2017-ss/blob/master/plugins/emacs/haskell-drhaskell.el)
herunterladen. Kopieren Sie es in den Load-Path von Emacs oder laden Sie es in
die `.emacs`-Datei durch `(load "/path/to/haskell-drhaskell.el")` und
aktivieren Sie es bis zum Schließen des Programms mittels

```
M-x turn-on-haskell-drhaskell
```

oder fügen Sie

```
(add-hook 'haskell-mode-hook 'turn-on-haskell-haskell)
```

in der `.emacs`-Datei hinzu, um *DrHaskell* dauerhaft in Emacs zu aktivieren.

### Verwendung des Plugins

Das Plugin stellt die folgenden Funktionen zur Verfügung:

 * `C-c C-s` startet einen *DrHaskell*-Buffer.
 * `C-c C-l` lädt das aktuell geöffnete Programm in *DrHaskell*.
 * `C-c C-r` lädt das aktuell geöffnete Programm neu.
 * `C-c C-b` setzt den Cursor in den *DrHaskell*-Buffer.
 * `C-c M-l` aktiviert den *DrHaskell*-Linter.
 * `C-c M-{1,2,3,4}` setzt das Linter Level auf die angegebene Stufe.

# Aktualisierung von DrHaskell

Es kann sein, dass im Laufe des Semesters Updates für *DrHaskell*
zur Verfügung gestellt werden.
Wir werden Sie dann auf diese Aktualisierungen aufmerksam machen.

Falls Sie *DrHaskell* über das Git-Repository per `cabal` installiert haben,
können Sie einfach in Ihrem `drhaskell`-Verzeichnis den Befehl `git pull`
ausführen. Damit wird die aktuellste Version von *DrHaskell* aus dem Repository
ausgecheckt.
Anschließend müssen Sie nur erneut `cabal build` und `cabal install` ausführen,
um die neueste Version zu bauen und lokal in der Sandbox zu installieren.

# Entfernen von DrHaskell

Falls Sie *DrHaskell* über das Git-Repository per `cabal` installiert haben,
können Sie es einfach wieder von Ihrem Rechner entfernen,
indem Sie den `drhaskell`-Ordner löschen, z.B. durch `rm -rf drhaskell` im
darüberliegenden Ordner.

# Verwendung der DrHaskell-REPL

Die *DrHaskell*-REPL entspricht einem Interpreter für Haskell
(ähnlich dem `ghci`) mit dem Unterschied das verschiedene Sprachstufen
unterstützt werden.
Durch diese Sprachstufen wird der Sprachumfang von Haskell auf den unteren
Stufen beschränkt und komplexere Sprachfeatures erst auf höheren Stufen
zugänglich gemacht.

Nach dem Starten der *DrHaskell*-REPL sollten Sie die folgende
Eingabeaufforderung sehen:

```
DrHaskell (L2)>
```

In Klammern wird die derzeit ausgewählte Sprachstufe angezeigt. Hier ist
beispielsweise die Sprachstufe 2 ausgewählt.

# Übersicht über die Sprachstufen

## Stufe 1

Nur die einfachsten Sprachelemente stehen zur Verfügung. So können (rekursive)
Funktionen über primitiven Typen wie `Int`, `Float`, `Bool` oder `String`
definiert werden.
Die Angabe von Typsignaturen ist auf Stufe 1 für alle Funktionen erforderlich.

## Stufe 2

Auf Stufe 2 können zusätzlich eigene (polymorphe) Datentypen sowie polymorphe
Funktionen definiert werden. Außerdem werden ab dieser Stufe Funktionen höherer
Ordnung sowie die Programmierung mit Lambda-Ausdrücken unterstützt.
Auch hier sind Typsignaturen zwingend erforderlich.

## Stufe 3

Stufe 3 bietet zusätzlich zu den vorhergehenden Sprachstufen Unterstützung für
Modularisierung. Das heißt, es können Haskell-Module mit expliziten
Export-Listen definiert werden. Außerdem ermöglicht diese Stufe die Verwendung
von Import-Anweisungen.
Nach wie vor müssen Typsignaturen angegeben werden.

## Stufe 4

Ab Stufe 4 wird der volle Sprachumfang von Haskell bereitgestellt. Das heißt,
nun können Typklassen zur Überladung von Funktionen verwendet werden.
Außerdem ist die Angabe von Typsignaturen nun optional.
