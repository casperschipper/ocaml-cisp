(
SynthDef(\simple, {
	| freq, dur, pos = 0.0|
	var env, out, panned;
	env = EnvGen.ar( Env.perc(0.0001,dur), doneAction:2);
	out = SinOsc.ar(freq,mul:0.1) * env;
	panned = Pan2.ar( out, pos);
	OffsetOut.ar(0,panned);
}).add;
)

a = Synth(\simple, [\freq,220.0,\dur,2]);

s.boot;
s.devices
s.scope;

s = Server.local;

s.quit;
s.boot;

WFS.startup;

s.quit;