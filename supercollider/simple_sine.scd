(
SynthDef(\simple, {
	| freq, dur |
	var sig, out;
	sig = SinOsc.ar(freq,0.0,1.0);
	Out.ar( 0, sig * EnvGen.ar( Env.perc(0.0001,dur), doneAction:2));
}).add;
)

a = Synth(\simple, [\freq,440,\dur,2]);