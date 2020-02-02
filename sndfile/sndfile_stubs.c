#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <float.h>
#include  <sndfile.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>


value caml_snd_write (value input_ar, value filename, value format_tuple)
{	
  CAMLparam3(input_ar,filename,format_tuple);
  SNDFILE	*file ;
  SF_INFO	sfinfo ;
  int		k;

  int sr = Int_val(Field(format_tuple,0));
  int chs = Int_val(Field(format_tuple,1));
  int bits = Int_val(Field(format_tuple,2));
  int format = Int_val(Field(format_tuple,3));

  char *fname = String_val(filename);

  int size = Bigarray_val(input_ar)->dim[0];
  float *input_data = Data_bigarray_val(input_ar);

  memset (&sfinfo, 0, sizeof (sfinfo)) ;

  sfinfo.samplerate	= sr;
  sfinfo.frames		= size/chs;
  sfinfo.channels	= chs ;
  sfinfo.format		= (format | bits) ;
  
  if (! (file = sf_open (fname, SFM_WRITE, &sfinfo))) {	
    failwith("couldn't open output file.");
    } ;

  
  if (sf_write_float (file, input_data, sfinfo.channels * (size/chs)) !=
      sfinfo.channels * (size/chs))
    puts (sf_strerror (file)) ;
  
  sf_close (file) ;
  CAMLreturn (Val_unit) ;
}
 

value caml_n_channels (value path) {
  CAMLparam1(path);
  int nchannels;
  char *fname = String_val(path);
  SNDFILE	 	*file ;
  SF_INFO	 	sfinfo ;
  float *data;
  long size[1];

  memset (&sfinfo, 0, sizeof (sfinfo)) ;
  
  if ((file = sf_open (fname, SFM_READ, &sfinfo)) == NULL)
    {	failwith("couldn't open input file.");
    } ;

  nchannels = sfinfo.channels;
  sf_close (file) ;
  CAMLreturn (Val_int(nchannels));
}

value caml_n_samplerate (value path) {
  CAMLparam1(path);
  int rate;
  char *fname = String_val(path);
  SNDFILE	 	*file ;
  SF_INFO	 	sfinfo ;
  float *data;
  long size[1];

  memset (&sfinfo, 0, sizeof (sfinfo)) ;
  
  if ((file = sf_open (fname, SFM_READ, &sfinfo)) == NULL)
    {	failwith("couldn't open input file.");
    } ;

  rate = sfinfo.samplerate;
  sf_close (file) ;
  CAMLreturn (Val_int(rate));
}

value caml_snd_read (value path) {
  CAMLparam1(path);
  CAMLlocal1(outar);
  char *fname = String_val(path);
  SNDFILE	 	*file ;
  SF_INFO	 	sfinfo ;
  float *data;
  long size[1];

  memset (&sfinfo, 0, sizeof (sfinfo)) ;

  if ((file = sf_open (fname, SFM_READ, &sfinfo)) == NULL)
    {	failwith("couldn't open input file.");
    } ;
  
  size[0] = sfinfo.channels * sfinfo.frames;
  outar = alloc_bigarray(BIGARRAY_MANAGED|BIGARRAY_FLOAT32, 1,NULL, size);
  data = Data_bigarray_val(outar);

  sf_read_float(file,data,sfinfo.channels * sfinfo.frames);

  sf_close (file) ;
  CAMLreturn(outar);

}
