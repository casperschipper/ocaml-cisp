# ideas for midi parser:

let's think of a stream of midi events.

note-on 60 100
.
.
.
.
.
note-off 60 0
.
note-on 62 100
note-on 64 100
.
.
.
.
note-off 62 0
.
.
.
note-off 64 100
ctrl 1 100

We may want this converted into state and triggers.
At any point t, there is:
Is there a trigger ? Which Pitch, Channel and Velocity has it?
Is there a control message ? 
What is the current state of the controls ?
What is the model ?
Which notes are currently depressed ?

The trigger is for synchronisation between external source.
The information is used to control the algorithm.

type midist =
   { currentNotes = Pitch list
   ; trigger = bool 
   ; controllers = Ctrl list
   }
   
mkMidiSt nts trig =
   { currentNotes = nts
   ; trigger = trig 
   }
   
let midiParse = Succeed mkMidiSt <*> readCurrentNotes <*> readTrigger

midiIn |> run midiParse |> map fromState 


   


