(
b = Buffer.read(s, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/short_klang_1.aiff");
b.bufnum;
)



(
SynthDef(\arBufControl, {
	arg out = 0, bufnum = 0, scale = 1.0, startpos = 0,
	attackStart, releaseStart, releaseEnd,
	attackTime, attackType = \linear, releaseType = \linear;
	var arEnv, dur;

	dur = (BufDur.kr(bufnum) * (1 - startpos)) * (1 / scale);
	arEnv = Env(
		levels:[attackStart, releaseStart, releaseEnd],
		times: [attackTime, 1 - attackTime],
		curve: [attackType, releaseType]);
	arEnv.duration = dur;
	Out.kr(out, EnvGen.kr(arEnv, doneAction:2));
}).add;

SynthDef(\playStereoSound, {
	arg out = 0, bufnum = 0, scale = 1.0, ampBus = 0, startPos = 0;
	var startFrame, amp;
	startFrame = BufFrames.kr(bufnum) * startPos;

	amp = In.kr(ampBus);

	Out.ar(out,
		PlayBuf.ar(
			numChannels: 2,
			bufnum: bufnum,
			rate: BufRateScale.kr(bufnum) * scale,
			startPos: startFrame,
			doneAction:2) * amp);

}).add;
)

(
a = BufDur.kr(0) * 0.1;
a.postln;
)

(
Synth("arBufControl", [out: 16, bufnum: b.bufnum, scale: 0.10, startpos: 0.13, attackStart: 0, releaseStart: 3, releaseEnd: 0, attackTime: 0.5], addAction:\addToTail);
Synth("playStereoSound", [out: 0, bufnum: b.bufnum, scale: 0.10, startPos: 0.13, ampBus: 16]);
)


(
Synth("arBufControl", [out: 16, bufnum: b.bufnum, scale: 0.04, startpos: 0.5, attackStart: 0, releaseStart: 20, releaseEnd: 0, attackTime: 0.7], addAction:\addToTail);
Synth("playStereoSound", [out: 0, bufnum: b.bufnum, scale: 0.04, startPos: 0.5, ampBus: 16], addAction:\addToTail);
)


(
b.free;
)