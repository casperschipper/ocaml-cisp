(
SynthDef(\antCrackle, {
    arg out=0, freq=3000,range=100,imp=10, q=1.0,releaseDur=2.0, pan=0.0;
    var trig, env, burst, filtered, panSig, crackle,reso,bigEnv;

    // Trigger: random impulses using Dust
    trig = Impulse.ar(imp); // Average 20 impulses per second

    // Envelope: short, snappy decay envelope per impulse
    env = Decay2.ar(trig, 0.001, Rand(0.001, 0.005));
	bigEnv = EnvGen.ar(Env.perc(0.0001,releaseDur),1.0,doneAction:Done.freeSelf);

    // Source: short burst of noise with random bandpass filtering
    burst = WhiteNoise.ar * env;
	reso = 1.0 / (q.clip(1.0,1000000));

	filtered = BPF.ar(burst,Rand(freq, freq+range), reso);

    // Optional slight distortion to enhance crispness
    filtered = (filtered * Rand(10.0, 20.0)).tanh * bigEnv;

    // Random stereo placement
    panSig = Pan2.ar(filtered, pos:pan,level:0.3);

    Out.ar(out, panSig);
}).add;
)

(
SynthDef(\antCrackle, {
    arg out=0, freq=3000,range=100,imp=10, q=1.0,releaseDur=2.0, pan=0.0;
    var trig, env, burst, filtered, panSig, crackle,reso,bigEnv;

    // Trigger: random impulses using Dust
    trig = Saw.ar(imp); // Average 20 impulses per second

    // Envelope: short, snappy decay envelope per impulse
    env = Decay2.ar(trig, 0.001, Rand(0.001, 0.005));
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

(
// Play the synth
x = Synth(\antCrackle, [\freq,100,\range,10000,\imp,(-30.0).rrand(10.0).midicps,\q , 10.0,\releaseDur,0.1,\pan:1.0]);
)

ActionFunc

// Stop
x.free;
s.freeAll;
