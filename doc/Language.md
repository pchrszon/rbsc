Role-based Modeling Language
============================

Syntax
------

- Typebene unterstützen?
    - (+) Komponenten und Rollen können mehrfach instanziert werden (wie bei
      ProFeat)
    - (+) Constraints können potentiell Typen von Rollen adressieren und nicht
      nur einzelne Instanzen
    - (-) Komplexere Eingabesprache, mehr Implementierungsaufwand
- Generische Definitionen unterstützen (d.h. mit Template-Parametern)?
- "Minimalimplementierung"
    - Keine Typebene, nur Instanzen
    - Keine generischen Definitionen
    - Keine statischen Constraints (d.h. kein Äquivalent zum Featuremodell in
      ProFeat)

### Components

- Verhaltensbeschreibung einer atomaren Komponente
- Standard PRISM Module: Variablen und Commands
- Beispiel:
    ```
    component Node {
        has_file : bool init false;

        [store] !has_file -> (has_file' = true);
        [load] has_file -> true;
        [del] has_file -> (has_file' = false);
    }
    ```

### Roles

- Verhaltensbeschreibung einer Rolle
- Rollenannotationen sind implizit und werden automatisch an jede Transition
  angefügt
- Eine Rollendefinition erzeugt immer eine non-blocking Rolle?
    - (+) Binding erhält garantiert das Basisverhalten, wenn die Rolle nicht
      gespielt wird
    - (+) "Unbinding" hat genau den erwarteten Effekt, dass das Basisverhalten
      wiederhergestellt wird
    - (-) Weniger flexibel bzw. ausdrucksstark als das formale Framework (?)
- Ein Command kann mit `+` (plus) gekennzeichnet werden
- Beispiel:
    ```
    role Server(i) {
        got_req: bool init false;
        req_from : [0..NUM_NODES] init 0;

        [req[k][i]] !got_req -> (got_req' = true) & (req_from' = k);
        // ...

        // Verhalten überschreiben: del-Aktion ignorieren
        [+del] true -> true;
    }
    ```
- Welche Beschränkungen gibt es an `+` Commands?
    - Können lokale Variablen anderer Komponenten/Rollen gelesen werden?
    - Können lokale Variablen des Spielers gelesen werden? Vorteil wäre, dass
      Überschreiben abhängig vom Zustand des Spielers sein kann.
- Sollte eine Rollendefinition die "required actions" explizit auflisten?
- Wenn Zugriff auf Variablen des Spielers möglich sind: Gibt es zusätzlich zu
  den "required actions" auch "required variables"?

### Binding Coordinator

- Entspricht im Wesentlichen dem Feature-Controller in ProFeat
- Prinzipiell kann es mehrere Binding Coordinators geben
- Falls es eine Typebene gibt: Referenziert der Binding Coordinator Instanzen,
  Typen, oder beides? (Im Formalismus bezieht er sich auf Instanzen)
- Im Binding Coordinator gibt es die speziellen Aktionen `bind` und `unbind`
- Beispiel:
    ```
    binding coordinator {
        [store[i]] true -> bind(Server[i] to Node[i]);
        [del[i]] true -> unbind(Server[i] from Node[i]);
    }
    ```

### Playing Coordinator

- Beeinflusst das Rollenspiel durch Synchronisation mit den Rollenannotationen
- Wenn Komposition mit Playing Coordinator die MDP-Semantik ergibt, kann es nur
  einen Playing Coordinator geben (oder Komposition mehrerer Coordinators)
- Interpretationen (d.h. Menge der gespielten Rollen) als Guard (?)
- Beispiel:
    ```
    playing coordinator {
        scenario : [1..2];

        // Spielen der Instanzen Server 0, Relay 1 und Client 2 erlauben
        [Server[0] & Relay[1] & Client[2]] scenario = 0 -> true;
    }
    ```
- Zwei Arten von Transitionen einführen (oder kombinierte Transitionen)?
    1. Synchronisation über gespielte Rollen
    2. Synchronisation über gemeinsame Aktionen mit dem System
- Sollte der Playing Coordinator auch Typen referenzieren können?
    - Erfordert das, dass Typ des Spielers mit angegeben wird (d.h. wer spielt
      eine bestimmte Rolle)?

### System Structure

- Irgendwo muss die Komposition des Gesamtsystems aus den einzelnen Components
  und Roles angegeben werden
- Möglichkeiten:
    1. Wenn es eine Typebene gibt, die auch explizit die Compartments
       beschreibt, kann man daraus evtl. die richtige Anwendung der
       Kompositionsoperatoren ableiten (?)
    2. Die Struktur des Systems explizit beschreiben im Stil des "system"-Blocks
       von PRISM:
        ```
        system {
            (Server[0] || Server[1] || Client[0] || Client[1])[Server[0] -> Node[0]][Client[1] -> Node[1]]
        }
        ```
- Der Unterschied zwischen Binding im `system`-Block und `bind` im
  `binding controller` könnte etwas verwirrend sein
- Der `binding controller` und der `system` Block müssen konsistent sein bzw.
  konsistent gemacht werden (so wie Featuremodell und Feature-Controller in
  ProFeat)

### Constraints

- Statische Constraints an das Rollenspiel, die dann automatisch vom
  `playing coordinator` umgesetzt werden
- Beispiel:
    ```
    constraints {
        // Node 0 kann nicht gleichzeitig Server und Client spielen
        !(Server[0] & Client[0]);
    }
    ```
- Constraints auf Typebene?


Übersetzung
-----------

- Mögliche Ansätze:
    1. Sämtliche Komposition bei der Übersetzung durchführen. Ergebnis wäre
       ein großes PRISM `module`.
    2. Components, Roles und Coordinators so übersetzen, dass PRISMs
       Parallelkomposition zum gleichen Ergebnis führt wie Ansatz (1).
- Mischen beider Ansätze vermutlich nicht möglich, da Binding und
  Parallelkomposition potentiell beliebig gemischt sein können
- Da die Reihenfolge der Anwendung von Binding und Parallelkomposition
  entscheidend ist, könnte es sein, dass Ansatz (2) gar nicht möglich ist (?)

### Ansatz 2: Parallelkomposition von PRISM nutzen

- Unterschiede zwischen Parallelkomposition und Rollenbindung:
    1. Behandlung von `+` Transitionen
    2. Beachten der benötigten Aktionen
- (2) sollte sich
