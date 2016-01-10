(
b = Buffer.read(s, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/cut_1.aiff");
)

(
SynthDef(\playStereoSound, {
	arg out = 0, bufnum = 0, scale = 1.0;

	Out.ar(out,
		PlayBuf.ar(2, bufnum, BufRateScale.kr(bufnum) * scale, doneAction:2));

}).add;
)

(
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.10]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.103]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.106]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.109]);
)

(
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.40]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.41]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.42]);
Synth("playStereoSound", [out: 0, bufnum: b, scale: 0.43]);
)


Synth("playStereoSound", [out: 0, bufnum: b, scale: 1]);

(
b.free;
)