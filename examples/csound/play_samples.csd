<CsoundSynthesizer>
<CsOptions>
; Output 64-channel WAV file
; Adjust filename and sample rate/format as needed
-o output113.wav -W -d --format=wav --sample-rate=48000 --nchnls=64
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 16
nchnls = 64
0dbfs = 1

; Load the audio file into a table at init time
giSample ftgen 1, 0, 0, 1, "concatJV1010.wav", 0, 0, 1

; Get file duration at init time
giFileDur filelen "concatJV1010.wav"

instr 1
  ; p4 = offset (in samples)
  ; p5 = transpose ratio (1 = original pitch)
  ; p6 = output channel (0–63)
  ; p7 = duration in seconds
  ; p8–p11 = ADSR (a, d, s, r)
  
  ; Get parameters
  iOffset = p4
  iTranspose = p5
  iChannel = p6
  iDur = p3
  iAttack = p7
  iDecay = p8
  iSustain = p9
  iRelease = p10
  
  ; Create ADSR envelope
  kEnv madsr iAttack, iDecay, iSustain, iRelease
  
  ; Play the sample with transposition and ADSR using our loaded table
  ;aSig flooper2 kEnv, iTranspose, iOffset / 44100, giFileDur, 0.1, giSample
  aindex line iOffset, iDur * iTranspose, iOffset + (iDur * 44100)
  asig table3 aindex, giSample
  amupped = asig * kEnv
  
  ; Route to correct channel using the interleave opcode
  aOut[] init nchnls
  aOut[iChannel] = amupped
  
  ; Output all channels
  outch 1, aOut[0], 2, aOut[1], 3, aOut[2], 4, aOut[3], 5, aOut[4], 6, aOut[5], 7, aOut[6], 8, aOut[7]
  outch 9, aOut[8], 10, aOut[9], 11, aOut[10], 12, aOut[11], 13, aOut[12], 14, aOut[13], 15, aOut[14], 16, aOut[15]
  outch 17, aOut[16], 18, aOut[17], 19, aOut[18], 20, aOut[19], 21, aOut[20], 22, aOut[21], 23, aOut[22], 24, aOut[23]
  outch 25, aOut[24], 26, aOut[25], 27, aOut[26], 28, aOut[27], 29, aOut[28], 30, aOut[29], 31, aOut[30], 32, aOut[31]
  outch 33, aOut[32], 34, aOut[33], 35, aOut[34], 36, aOut[35], 37, aOut[36], 38, aOut[37], 39, aOut[38], 40, aOut[39]
  outch 41, aOut[40], 42, aOut[41], 43, aOut[42], 44, aOut[43], 45, aOut[44], 46, aOut[45], 47, aOut[46], 48, aOut[47]
  outch 49, aOut[48], 50, aOut[49], 51, aOut[50], 52, aOut[51], 53, aOut[52], 54, aOut[53], 55, aOut[54], 56, aOut[55]
  outch 57, aOut[56], 58, aOut[57], 59, aOut[58], 60, aOut[59], 61, aOut[60], 62, aOut[61], 63, aOut[62], 64, aOut[63]
endin

; Alternative version using panning across channels
instr 2
  ; p4 = offset (in samples)
  ; p5 = transpose ratio (1 = original pitch)
  ; p6 = pan position (0-1, where 0 = left, 1 = right)
  ; p7 = duration in seconds
  ; p8–p11 = ADSR (a, d, s, r)
  
  ; Get parameters
  iOffset = p4
  iTranspose = p5
  iPan = p6 * (nchnls - 1)  ; Normalize to channel count
  iDur = p3
  iAttack = p7
  iDecay = p8
  iSustain = p9
  iRelease = p10
  
  ; Create ADSR envelope
  kEnv madsr iAttack, iDecay, iSustain, iRelease
  
  ; Play the sample with transposition and ADSR using our loaded table
  aSig flooper2 kEnv, iTranspose, iOffset / sr, giFileDur, 0.1, giSample
  
  ; Calculate channel distribution using simple equal power panning
  iChanLow = int(iPan)
  iChanHigh = iChanLow + 1
  iFrac = iPan - iChanLow
  
  ; Limit channel range
  iChanHigh = (iChanHigh >= nchnls ? 0 : iChanHigh)
  
  ; Calculate pan values
  iPanLow = sqrt(1 - iFrac)
  iPanHigh = sqrt(iFrac)
  
  ; Route to channels
  aOut[] init nchnls
  aOut[iChanLow] = aSig * iPanLow
  aOut[iChanHigh] = aSig * iPanHigh
  
  ; Output all channels
  outch 1, aOut[0], 2, aOut[1], 3, aOut[2], 4, aOut[3], 5, aOut[4], 6, aOut[5], 7, aOut[6], 8, aOut[7]
  outch 9, aOut[8], 10, aOut[9], 11, aOut[10], 12, aOut[11], 13, aOut[12], 14, aOut[13], 15, aOut[14], 16, aOut[15]
  outch 17, aOut[16], 18, aOut[17], 19, aOut[18], 20, aOut[19], 21, aOut[20], 22, aOut[21], 23, aOut[22], 24, aOut[23]
  outch 25, aOut[24], 26, aOut[25], 27, aOut[26], 28, aOut[27], 29, aOut[28], 30, aOut[29], 31, aOut[30], 32, aOut[31]
  outch 33, aOut[32], 34, aOut[33], 35, aOut[34], 36, aOut[35], 37, aOut[36], 38, aOut[37], 39, aOut[38], 40, aOut[39]
  outch 41, aOut[40], 42, aOut[41], 43, aOut[42], 44, aOut[43], 45, aOut[44], 46, aOut[45], 47, aOut[46], 48, aOut[47]
  outch 49, aOut[48], 50, aOut[49], 51, aOut[50], 52, aOut[51], 53, aOut[52], 54, aOut[53], 55, aOut[54], 56, aOut[55]
  outch 57, aOut[56], 58, aOut[57], 59, aOut[58], 60, aOut[59], 61, aOut[60], 62, aOut[61], 63, aOut[62], 64, aOut[63]
endin
</CsInstruments>

<CsScore>
; p1 inst ; p2 start ; p3 dur ; p4 = offset (in samples) ; p5 = transpose ratio (1 = original pitch) ; p6 = output channel (0–63) ; p7 = duration in seconds ; p8–p11 = ADSR (a, d, s, r)
#include "../antscore2.sco"

</CsScore>
</CsoundSynthesizer>