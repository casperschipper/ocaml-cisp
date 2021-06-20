# Ideas

* simple read and write to arrays (state)
* simple parser
* merge multiple streams of events into bundles


# midi over ip example:
* <elm/apiTest/src/>

# interesting idea under elm
* impossible states impossible
* dependencies for something to work
* many interacting elements needed
* pallets, things that work well with the other things in the set
* a typeface are gliffs that work well together.
* perceptual, often non-linear

# concurrent OSC handling

https://ocaml.github.io/ocamlunix/threads.html

## constants for doing conversion of clocks

const int32 kSECONDS_FROM_1900_to_1970 = (int32)2208988800UL; /* 17 leap years */
const double kOSCtoSecs = 2.328306436538696e-10;

const double kSecondsToOSCunits = 4294967296.; // pow(2,32)
const double kMicrosToOSCunits = 4294.967296; // pow(2,32)/1e6
const double kNanosToOSCunits = 4.294967296; // pow(2,32)/1e9
