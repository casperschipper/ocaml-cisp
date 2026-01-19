// SynthDef for playing audio fragments from a buffer
// Each fragment is 1 second apart (44100 samples at 44.1kHz)

(
SynthDef(\fragmentPlayer, {
	arg out = 0,
	    buf = 0,
	    duration = 1.0,
	    amp = 0.5,
	    offset = 0,
	    transpose = 0;

	var sig, env;
	var attackTime, decayTime, releaseTime, startPos;

	duration = duration * 0.1;
	attackTime = duration * 0.0;
	releaseTime = duration * 0.0;

	startPos = offset * 44100;
	//0 hh
	//3 tsss
	//4 mesh
	//7 tom, 8 heavy
	//11 kick
	//17 glass
	//37 breath
	//18
	//25 conga
	//46 cowbell




	env = EnvGen.kr(
		Env.linen(attackTime, max(0.0,duration - attackTime - releaseTime), releaseTime, amp*1,curve:-1.0),
		doneAction: 2
	);
	sig = PlayBuf.ar(
		numChannels: 1,
		bufnum: buf,
		rate: BufRateScale.kr(buf) * (transpose-20.midiratio),
		startPos: startPos,
		loop: 0
	);

	sig = sig * env;

	OffsetOut.ar(out.wrap(0,24), sig); // Duplicate mono to stereo
}).add;


)

// Example usage:

// First, load your buffer
(
b = Buffer.read(s, "/Users/casperschipper/devel/ocaml/cisp/examples/csound/concatJV1010.wav");
)

(
b = Buffer.read(s, "/Users/casperschipper/Library/CloudStorage/GoogleDrive-casper.schipper@gmail.com/My Drive/GuineaPigs/BowieMono.wav");
)

b.inspect;

// Play fragment at offset 0 (first fragment)
Synth(\fragmentPlayer, [\buf, b, \duration, 1.0, \amp, 0.5, \offset, 0]);

