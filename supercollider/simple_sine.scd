(
SynthDef(\simple, {
	| freq, dur |
	var sig, out, panned, pos;
	sig = SinOsc.ar(freq,0.0,0.004);
	out = sig * EnvGen.ar( Env.perc(0.0001,dur), doneAction:2);
	pos = Rand.new(-1.0,1.0);
	panned = Pan2.ar(out,pos);
	OffsetOut.ar( 0, panned);
}).add;
)

a = Synth(\simple, [\freq,440,\dur,2]);

s.boot;