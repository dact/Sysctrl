# Sysctrl

Is a concurrent implementation of a set of DFA's, where each state is
a independent process and the state transitions are represented as
pipes between process.  everything is controlled from a central
process whose work is receive user commands and print the result
accordingly.

## Usage

```
sysctrl [-hv] [-f script] infile
```

## Input file format
```yaml
- "automata" : "Example"
  "description" : "This is just an example"
  "alpha"  : "a"
  "states" : ["a","b"]
  "start"  : "a"
  "final"  : [ "b" ]
  "delta"  :
    - "node"  : "a"
      "trans" :
      - "symbol" : "a"
        "next"   : "b"
    - "node"  : "b"
      "trans" : []
```
where:
<dl>
    <dt>automata</dt>
    <dd>Is a descriptive name for the automata</dd>

    <dt>description</dt>
    <dd>Is a description</dd>

    <dt>alpha</dt>
    <dd>Is a set of characters representing the symbols recognized by the DFA</dd>

    <dt>states</dt>
    <dd>Is a set of states (Strings are allowed)</dd>

    <dt>start</dt>
    <dd>Is the initial state, and should be contained in states</dd>

    <dt>final</dt>
    <dd>Is a set of final states that  should be present in states</dd>

    <dt>delta</dt>
    <dd> Is an array of objects defining state transitions
        <dl>
            <dt>node</dt>
            <dd>the name of the node</dd>
            <dt>trans</dt>
            <dd> An array whit objects representing transitions
                <dl>
                    <dt>symbol</dt>
                    <dd>character of the transition</dd>
                    <dt>next</dt>
                    <dd>next state in case of read symbol</dd>
                </dl>
            </dd>
        </dl>
    </dd>

</dl>
