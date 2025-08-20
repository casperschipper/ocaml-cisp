(
SynthDef(\simple, {
	| freq, dur |
	var env, out;
	env = EnvGen.ar( Env.perc(0.0001,dur), doneAction:2);
	out = SinOsc.ar(freq,0.0,0.001 * env);
	OffsetOut.ar(0,Pan2.ar(out,pos:0.0));
}).add;
)

a = Synth(\simple, [\freq,220.0,\dur,2]);

s.boot;

s = Server.local;

s.quit;

WFS.startup;

s.quit;