/*
    Copyright (C) 2004 Ian Esten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <jack/jack.h>
#include <jack/midiport.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h> 

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

jack_client_t *client;
jack_port_t *output_port;
/* midi buffer, a unsigned array of chars */
/* [status1,data,data,status2,data,data etc...*/
unsigned char *midi_output_buffer;


static void signal_handler(int sig)
{
	jack_client_close(client);
	fprintf(stderr, "signal received, exiting ...\n");
	exit(0);
}

// a message with status 0x00 is used to signal absense
bool isMessage(unsigned char *bytes) {
  return (bytes[0] != 0x00) ? true : false;
}



/*
static void usage()
{
	fprintf(stderr, "usage: jack_midiseq name nsamp [startindex note nsamp] ...... [startindex note nsamp]\n");
	fprintf(stderr, "eg: jack_midiseq Sequencer 24000 0 60 8000 12000 63 8000\n");
	fprintf(stderr, "will play a 1/2 sec loop (if srate is 48khz) with a c4 note at the start of the loop\n");
	fprintf(stderr, "that lasts for 12000 samples, then a d4# that starts at 1/4 sec that lasts for 800 samples\n");
}
*/

/* midi message is represented by (bool * int * int * int) (message? status ch+data1 data2) */

static int process(jack_nframes_t nframes, void *arg)
{
    int i,midi_frame;
    void* port_buf = jack_port_get_buffer(output_port, nframes);
    unsigned char *currentMidiMsg;

    jack_midi_clear_buffer(port_buf);

    
    value closure;
    value* closurePt = (value*) arg;
    closure = *closurePt;
    

    unsigned char* buffer;

    

    //
    caml_callback(closure, Val_int((int) nframes)); /* fills midi_output_buffer with values */
    
    for (i = 0; i < nframes; i++) {
      midi_frame = i*3;
      currentMidiMsg = &midi_output_buffer[midi_frame];
      //if (true) {
      if (isMessage(currentMidiMsg)) {
	// TODO: handle midi realtime messages (248, 252, 250), which are only one byte long
	fprintf(stderr,"pitch: %i\n", currentMidiMsg[1]);
	if((buffer = jack_midi_event_reserve(port_buf, i, 3))) {
	  buffer[0] = currentMidiMsg[0];
	  buffer[1] = currentMidiMsg[1];
	  buffer[2] = currentMidiMsg[2];
	}
      }
    }
    
    return 0;
}

/*
CAMLPrim value open_midi_stream(value closure, value set_sr_closure) {
  client = jack_client_open ("midi-caml", JackNullOption, NULL);
  if (client == 0) {
    fprintf (stderr, "jack server not running?\n");
    return 1;
  }
  jack_set_process_c
*/

CAMLprim value open_midi_stream (value midi_msg_array, value closure, value set_sr_closure)
{
  CAMLparam3(midi_msg_array, closure, set_sr_closure);
	int i;
	jack_nframes_t nframes;
	jack_status_t status;
	jack_nframes_t sample_rate;
	const char **ports;
	int connect_result;

	if ((client = jack_client_open ("ocaml midi out", JackNullOption, &status, NULL)) == 0) {
	  fprintf (stderr, "JACK server not running?\n");
	  CAMLreturn(Val_unit);
	}
	
	if (true) {
	  if (status & JackServerFailed) {
	    fprintf (stderr,"%i Unable to connect to JACK server\n", status );;
	  }
	  if (status & JackFailure) {
	    fprintf (stderr,"%i Jack Failure\n",status);
	  }
	  if (status & JackInvalidOption) {
	    fprintf (stderr,"%i Jack invalid option\n", status );;
	  }
	  if (status & JackNameNotUnique) {
	    fprintf (stderr,"%i Jack name not unique\n", status );;
	  }
	  if (status & JackServerStarted) {
	    fprintf (stderr,"%i Jack server started\n", status );;
	  }
	  if (status & JackServerFailed) {
	    fprintf (stderr,"%i Jack server failed\n", status );;
	  }
	  if (status & JackServerError) {
	    fprintf (stderr,"%i Jack server error\n", status );;
	  }
	  if (status & JackNoSuchClient) {
	    fprintf (stderr,"%i No such client\n", status );;
	  }
	  if (status & JackLoadFailure) {
	    fprintf (stderr,"%i Jack Load Failure\n", status );;
	  }
	  if (status & JackInitFailure) {
	    fprintf (stderr,"%i failure to initialize cleint\n", status );;
	  }
	  if (status & JackShmFailure) {
	    fprintf (stderr,"%i failure to access shared memory\n", status );;
	  }
	  if (status & JackVersionError) {
	    fprintf (stderr,"%i Jack Version Error !\n", status );;
	  }
	  if (status & JackBackendError) {
	    fprintf (stderr,"%i Jack backend error\n", status );;
	  }
	  if (status & JackClientZombie) {
	    fprintf (stderr,"%i There is a Jack client zombified :-O \n", status );;
	  }
	}

	// we assume jack is running from this point

	jack_set_process_callback (client, process, &closure);

	sample_rate = jack_get_sample_rate(client);
	caml_callback(set_sr_closure, Val_int((int) sample_rate));

	output_port = jack_port_register (client, "ocamlout", JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);

	if (output_port == NULL) {
	  fprintf(stderr, "cannot register midi port with JACK");
	} else {
	  fprintf(stderr, "midi port success!\n");
	}
	
	nframes = jack_get_buffer_size(client);

	fprintf(stderr, "nframes is set %i\n",nframes);

	midi_output_buffer = Caml_ba_data_val(midi_msg_array); 
	
	caml_register_global_root(&midi_msg_array); // protect this pointer from the Ocaml garbage collector

	caml_enter_blocking_section();
	
	if (jack_activate(client)) {
		fprintf (stderr, "cannot activate client");
		return 1;
	}

	//ports = jack_get_ports(client, NULL,NULL, JackPortIsPhysical|JackPortIsInput);

	if (ports == NULL) {
	  fprintf(stderr, "no physical ports\n");
	  exit(1);
	}

	/*
	connect_result = jack_connect(client, jack_port_name(output_port), ports[0]);
	if (connect_result) {
	  fprintf (stderr, "cannot connect output ports error: %i\n",connect_result);
	}
	*/

	/* run until interrupted */
	while (1) {
	  sleep(1);
	};

	caml_leave_blocking_section();
	jack_client_close(client);

	CAMLreturn(Val_unit);
}
