WFSLib.startup;

Server.supernova;

s.reboot;
s.quit;

s.boot;
Server.killAll;

(
SynthDef(\simple, {
	| freq, dur, pos = 0.0|
	var env, out, panned, pitch_env;
	env = EnvGen.ar( Env.perc(0.00001,(0.1-((freq*0.01)/20000))), doneAction:2);
	pitch_env = EnvGen.ar( Env.perc(0.000000001,dur*0.1));
	out = SinOsc.ar(freq+(pitch_env*freq*100).clip(-20000,20000),mul:0.1) * env;
	panned = Pan2.ar( (out).tanh , pos);
	OffsetOut.ar(0,panned);
}).add;
)

SynthDef(\antCrackle, {
    arg out=0, freq=3000,range=100,imp=10, q=1.0,releaseDur=1.0, pan=0.0;
    var trig, env, burst, filtered, panSig, crackle,reso,bigEnv;

    // Trigger: random impulses using Dust
    trig = Saw.ar(imp); // Average 20 impulses per second

    // Envelope: short, snappy decay envelope per impulse
    env = Decay2.ar(trig, 0.001, Rand(0.001, 0.5));
	bigEnv = EnvGen.ar(Env.perc(0.0001,releaseDur),1.0,doneAction:Done.freeSelf);

    // Source: short burst of noise with random bandpass filtering
    burst = WhiteNoise.ar * env;
	reso = 1.0 / (q.clip(1.0,1000000));

	filtered = BPF.ar(burst,Rand(freq, freq+range), reso);

    // Optional slight distortion to enhance crispness
    filtered = (filtered * Rand(10.0, 20.0)).tanh * bigEnv;

    // Random stereo placement
    panSig = Pan2.ar(filtered, pos:pan,level:0.1);

    Out.ar(out, panSig);
}).add;
)

