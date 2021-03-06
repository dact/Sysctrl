# Sysctrl

Is a concurrent implementation of a set of DFA's, where each state is
a independent process and the state transitions are represented as
pipes between process.  everything is controlled from a central
process whose work is receive user commands and print the result
accordingly.

## Usage

```
sysctrl [-h|-v|-n|-f script] infile
```

## Input file format
Is an array of objects as follows

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
	    <dd> An array with objects representing transitions
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

### additional considerations

* Write a DFA, be careful with delta
* Delta **MUST** contain all the states, even those with no transitions (use empty Array)
* Do not use Tabs!

## Commands

### Info
For information on all automata
```yaml
{ "cmd" : "info" , "msg" : "" }
#or
{ "cmd" : "info" }
```
For information on specific automaton
```yaml
{ "cmd" : "info" , "msg" : "automata1" }

```

#### Output

```yaml
msgtype: info
info:
- automata: One
  ppid: 1201
  nodes:
  - node: A
    pid: 1204
  - node: B
    pid: 1205
  - node: C
    pid: 1206
- automata: Two
  ppid: 1201
  nodes:
  - node: A
    pid: 1209
  - node: B
    pid: 1211
```

### Stop
Ends execution
```yaml
 { "cmd" : "stop" , "msg" : "" }
 #or
 { "cmd" : "stop" }
```

### Send
Send a String to be processed by all automata
or
```yaml
{ "cmd" : "send" , "msg" : "message" }
```

#### Output

```yaml
- msgtype: accept
  accept:
  - automata: One
    msg: abbc
  - automata: Three
    msg: abbc
- msgtype: reject
  reject:
  - automata: Two
    msg: abbc
    pos: 1
- msgtype: error
  error:
  - where: "Four"
    cause: "Error cause"

```

# Bug Report

* Submit an issue on [github](https://github.com/ST0257/Sysctrl/issues)
* Email [me](mailto://agomezl@eafit.edu.co)
* Submit a pull request with fixes
