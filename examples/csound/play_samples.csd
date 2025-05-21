<CsoundSynthesizer>
<CsOptions>
; Output 64-channel WAV file
; Adjust filename and sample rate/format as needed
-test1.wav -W -n -d --format=wav --sample-rate=48000 --nchnls=64
</CsOptions>

<CsInstruments>
sr = 48000
ksmps = 16
nchnls = 64
0dbfs = 1

; Load the audio file into a table at init time
giSample ftgen 1, 0, 0, 1, "concatJV1010.wav", 0, 0, 1

instr 1
    ; p4 = offset (in samples)
    ; p5 = transpose ratio (1 = original pitch)
    ; p6 = output channel (0–63)
    ; p7 = duration in seconds
    ; p8–p11 = ADSR (a, d, s, r)

    ioffset  = p4
    irate    = p5
    ichn     = p6
    idur     = p7
    iatt     = p8
    idec     = p9
    isus     = p10
    irel     = p11

    aenv     linen ADSR(iatt, idec, isus, irel), 1
    asig     loscil3 aenv, irate, 1, 1, ioffset ; High-quality interpolated playback
    aout[]   init nchnls
    aout[ichn] = asig
    outq aout
endin
</CsInstruments>

<CsScore>
; Example: p1 p2  p3    p4    p5   p6  p7   p8   p9   p10  p11
;          |  |   |     |     |    |   |    |    |    |    |
;       instr st  dur offset trans chn dur  A    D    S    R

;i1     0   2.0   0      1.0   0   2.0  0.01 0.2  0.7  0.5
;i1     1   1.5   10000  0.5   5   1.5  0.01 0.1  0.5  0.3
;i1     2   1.0   20000  1.2   12  1.0  0.02 0.1  0.6  0.4

;e

#include "antscore.sco"
</CsScore>
</CsoundSynthesizer>
