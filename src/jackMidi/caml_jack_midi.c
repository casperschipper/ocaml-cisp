/*
    -- This is adopted from the example:
    https://github.com/jackaudio/jack2/blob/develop/example-clients/midisine.c 

    and futher based on:
    https://github.com/lucdoebereiner/processes/blob/master/src/jack/caml_jack.c --

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
jack_port_t *input_port;

/* midi buffer, a unsigned array of chars */
/* [status1,data,data,status2,data,data etc...*/
unsigned char *midi_output_buffer;
unsigned char *midi_input_buffer;


static void signal_handler(int sig)
{
	jack_client_close(client);
	fprintf(stderr, "signal received, exiting ...\n");
        exit(0);
}

// some helpers
bool isMessage(unsigned char *bytes) { 
  return (bytes[0] != 0x00);
}

void print_failure(jack_status_t status) {
    if (status & JackServerFailed) {
      fprintf (stderr,"%i Una\ble to connect to JACK server\n", status );;
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
      fprintf (stderr,"%i There is a Jack client zombified  [¬º-°]¬ \n", status );;
    }

    return;
}


// this procedure is called from Jackd, whenever new midi is needed/has arrived.
static int process(jack_nframes_t nframes, void *arg)
{
  // to protect against over-ambitious GC:
  caml_c_thread_register(); 

  unsigned int i, midi_frame;
  void* port_buf = jack_port_get_buffer(output_port, nframes);
  void* in_port_buf = jack_port_get_buffer(input_port, nframes);

  jack_midi_event_t in_event; // midi event in.

  jack_nframes_t event_index = 0;
  jack_nframes_t event_count = jack_midi_get_event_count(in_port_buf);

  unsigned char *currentMidiMsg;
    
  jack_midi_clear_buffer(port_buf); // clear the output

  value* closurePt = (value*) arg;
  value closure = *closurePt;
  unsigned char* buffer; 

  // get the first event (if there is one)
  if (event_count > 0) {
    jack_midi_event_get(&in_event, in_port_buf, 0);
  };

  caml_acquire_runtime_system ();
  caml_callback(closure, Val_int((int) nframes)); /* a callback fills the output buffer with raw midi */
  caml_release_runtime_system ();

  for (i = 0; i<nframes; i++) {
    midi_frame = i * 3; // midi message consists of 3 bytes: channel/status value1 value2

    // Input. All events of the vector are already here, so we have to schedule them based on timing:
    if ((in_event.time == i) && event_index < event_count) {
      midi_input_buffer[midi_frame] = *in_event.buffer;
      midi_input_buffer[midi_frame+1] = (in_event.buffer[1]);
      midi_input_buffer[midi_frame+2] = (in_event.buffer[2]);
       
      event_index++;
      if (event_index < event_count) {
	jack_midi_event_get(&in_event,in_port_buf,event_index);
      }
    } else { // no event at this t, so silence = zeros
      midi_input_buffer[midi_frame] = 0;
      midi_input_buffer[midi_frame+1] = 0;
      midi_input_buffer[midi_frame+2] = 0;
	
    }

    //output to jackd:
    currentMidiMsg = &midi_output_buffer[midi_frame];
      
    if (isMessage(currentMidiMsg)) {
      if((buffer = jack_midi_event_reserve(port_buf, i, 3))) {
	buffer[0] = currentMidiMsg[0];
	buffer[1] = currentMidiMsg[1];
	buffer[2] = currentMidiMsg[2];
      }
    }
  }

  

  return 0;
}


// this starts the jack client and registers the process function 
CAMLprim value open_midi_stream (value midi_msg_array_out,value midi_msg_array_in, value closure, value set_sr_closure)
{
  CAMLparam4(midi_msg_array_out,midi_msg_array_in,closure, set_sr_closure);

  int i;
  jack_nframes_t nframes;
  jack_status_t status;
  jack_nframes_t sample_rate;
  const char **ports; // to store names of ports in
  const char **in_ports; // to store names of input ports in

  //caml_c_thread_register(); 
  //I do not think call above is necessary
  
  midi_output_buffer = Caml_ba_data_val(midi_msg_array_out);
  midi_input_buffer = Caml_ba_data_val(midi_msg_array_in);

  caml_register_global_root(&midi_msg_array_out); // protect this pointer from the Ocaml garbage collector
  caml_register_global_root(&midi_msg_array_in);

  if ((client = jack_client_open ("ocaml_midi", JackNullOption, &status, NULL)) == 0) {
    fprintf (stderr, "JACK server not running?\n");
    // Failure states, print some useful errors.
    print_failure(status);
    CAMLreturn(Val_unit);
  }

  // this is the main callback registration:
  jack_set_process_callback (client, process, &closure);
  sample_rate = jack_get_sample_rate(client);

  // this is a single callback to let caml know of current sample rate:
  caml_callback(set_sr_closure, Val_int((int) sample_rate));

  // create io ports for our jack client:
  output_port = jack_port_register (client, "ocaml_midi_out", JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);
  input_port = jack_port_register (client, "ocaml_midi_in", JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0);

  if (output_port == NULL) {
    fprintf(stderr, "caml_jack_midi: cannot register output midi port with JACK");
  } else {
    fprintf(stderr, "caml_jack_midi: midi port success!\n");
  }

  if (input_port == NULL) {
    fprintf(stderr, "caml_jack_midi: cannot register input midi port with JACK");
  } else {
    fprintf(stderr, "caml_jack_midi: midi port success!\n");
  }

  nframes = jack_get_buffer_size(client);
  fprintf(stderr, "nframes is set %i\n",nframes);

  caml_release_runtime_system ();  

  if (jack_activate(client)) {
    fprintf (stderr, "cannot activate client");
    return 1; // todo: maybe CAMLreturn unit () here, or is it too late ?
  }
  
  // below is optional, it just connecting some midi ports for ease of use:	
  ports = jack_get_ports(client, NULL,"midi", JackPortIsInput);
	
  if (ports == NULL) {
    fprintf(stderr, "no physical ports\n");
    exit(1);
  }      

  int connect_result;
	
  connect_result = jack_connect(client, jack_port_name(output_port), ports[0]);
  if (connect_result) {
    fprintf (stderr, "cannot connect output ports error: %i\n",connect_result);
  }

  in_ports = jack_get_ports(client, "system_midi:capture_2", "midi" , JackNullOption);
  if (in_ports[0] == NULL) {
    fprintf(stderr, "no output found to use as input");
    fprintf(stderr, "name %s\n", in_ports[0]);
  }
  connect_result = jack_connect(client, in_ports[0], jack_port_name(input_port));
  if (connect_result) {
    fprintf(stderr, "sorry, cannot connect an output to your ocaml input: %i \n",connect_result);
  } else {
    fprintf(stderr, "connected to system_midi:capture_2");
  }

  // run until interrupted 
  while (true) {
    sleep(1);
  };

  // this part is never reached:
  caml_acquire_runtime_system();
  caml_c_thread_unregister();
  jack_client_close(client);
  CAMLreturn(Val_unit);
}

