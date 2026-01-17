// SynthDef for playing audio fragments from a buffer
// Each fragment is 1 second apart (44100 samples at 44.1kHz)
s.boot;

(
SynthDef(\fragmentPlayer, {
	arg out = 0,
	    buf = 0,
	    duration = 1.0,
	    amp = 0.5,
	    offset = 0;

	var sig, env;
	var attackTime, decayTime, releaseTime, startPos;

	// Calculate envelope times from duration
	// Attack: 5% of duration
	// Decay: 20% of duration
	// Release: 25% of duration
	// Sustain time: remaining 50%
	attackTime = duration * 0.05;
	decayTime = duration * 0.2;
	releaseTime = duration * 0.25;

	// Calculate the starting position in the buffer
	// offset is multiplied by 44100 to get the sample position
	startPos = offset * 44100;

	// Create fixed-duration envelope
	// Attack + Decay + Sustain + Release = duration
	env = EnvGen.kr(
		Env.linen(attackTime, duration - attackTime - releaseTime, releaseTime, amp),
		doneAction: 2
	);

	// Read from buffer starting at the calculated position
	sig = PlayBuf.ar(
		numChannels: 1,
		bufnum: buf,
		rate: BufRateScale.kr(buf),
		startPos: startPos,
		loop: 0
	);

	// Apply envelope
	sig = sig * env;

	// Output
	Out.ar(out, sig ! 2); // Duplicate mono to stereo
}).add;
)

// Example usage:

// First, load your buffer
b = Buffer.read(s, "/Users/casperschipper/devel/ocaml/cisp/examples/csound/concatJV1010.wav");

b.inspect;

// Play fragment at offset 0 (first fragment)
Synth(\fragmentPlayer, [\buf, b, \duration, 1.0, \amp, 0.5, \offset, 0]);

// Play fragment at offset 5 (6th fragment, at 5 seconds into the file)
Synth(\fragmentPlayer, [\buf, b, \duration, 1.0, \amp, 0.5, \offset, 5]);

// Play fragment at offset 63 (last fragment)
Synth(\fragmentPlayer, [\buf, b, \duration, 0.8, \amp, 0.3, \offset, 63]);

// With custom ADSR settings
Synth(\fragmentPlayer, [
	\buf, b,
	\duration, 2.0,
	\amp, 0.6,
	\offset, 10,
	\attackTime, 0.05,
	\decayTime, 0.2,
	\sustainLevel, 0.6,
	\releaseTime, 0.5
]);

