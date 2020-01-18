#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <float.h>
#include <jack/jack.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>


//jack_port_t *output_port1, *output_port2;
jack_port_t **output_ports;
jack_port_t **input_ports;
jack_client_t *client;
int n_out_channels;
int n_in_channels;
jack_default_audio_sample_t **outputs;
jack_default_audio_sample_t **inputs;
float *input_buffer;
float *output_buffer;

/* static void signal_handler(int sig) */
/* { */
/* 	jack_client_close(client); */
/* 	fprintf(stderr, "signal received, exiting ...\n"); */
/* 	exit(0); */
/* } */

/**
 * The process callback for this JACK application is called in a
 * special realtime thread once for each audio cycle.
 *
 * This client follows a simple rule: when the JACK transport is
 * running, copy the input port to the output.  When it stops, exit.
 */




int process (jack_nframes_t nframes, void *arg)
{
  //	jack_default_audio_sample_t *out1, *out2;
	double sample;
	unsigned int i;
	value closure;
	value* closurePt = (value*) arg;
	closure = *closurePt;
	caml_callback(closure, Val_int((int) nframes));

	for (int i = 0; i<n_out_channels; i++) {
	  outputs[i] = (jack_default_audio_sample_t*)jack_port_get_buffer(output_ports[i], nframes);
	}


	for (int i = 0; i<n_in_channels; i++) {
	  inputs[i] = (jack_default_audio_sample_t*)jack_port_get_buffer(input_ports[i], nframes);
	}

	
	for( i=0; i<nframes; i++ )
	{

	  for (int c = 0; c<n_in_channels; c++) {
	    int idx = (i*n_in_channels)+c;
	    float sample_in =  inputs[c][i];
	    input_buffer[idx] = sample_in;
	  };

	  for (int c = 0; c<n_out_channels; c++) {
	    int idx = (i*n_out_channels)+c;
	    sample = output_buffer[idx];
	    outputs[c][i] = sample;
	  }


	  
	}
    
	return 0;      
}

/**
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
/* void */
/* jack_shutdown (void *arg) */
/* { */
/* 	exit (1); */
/* } */

CAMLprim value open_stream (value output_array, value input_array, value closure, value n_channels, value set_sr_closure)
{
  CAMLparam4(output_array, input_array, closure, set_sr_closure);

	const char **ports;
	const char *client_name = "ocaml";
	const char *server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;
	jack_nframes_t sample_rate;

	output_buffer = Caml_ba_data_val(output_array);
	input_buffer = Caml_ba_data_val(input_array);
	caml_register_global_root(&output_array);
	caml_register_global_root(&input_array);
	//	caml_register_generational_global_root(&output_array);
	//	caml_register_generational_global_root(&input_array);
	n_out_channels = Int_val(Field(n_channels, 0));
	n_in_channels = Int_val(Field(n_channels, 1));

	//	printf("n out channels: %d\n", n_out_channels);
	
	/* open a client connection to the JACK server */

	client = jack_client_open (client_name, options, &status, server_name);
	if (client == NULL) {
		fprintf (stderr, "jack_client_open() failed, "
			 "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf (stderr, "Unable to connect to JACK server\n");
		}
		exit (1);
	}
	if (status & JackServerStarted) {
		fprintf (stderr, "JACK server started\n");
	}
	if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf (stderr, "unique name `%s' assigned\n", client_name);
	}

	// set sample rate
	sample_rate = jack_get_sample_rate(client);
	caml_callback(set_sr_closure, Val_int((int) sample_rate));
	
	/* tell the JACK server to call `process()' whenever
	   there is work to be done.
	*/
	
	jack_set_process_callback (client, process, &closure);

	/* tell the JACK server to call `jack_shutdown()' if
	   it ever shuts down, either entirely, or if it
	   just decides to stop calling us.
	*/

	//	jack_on_shutdown (client, jack_shutdown, 0);
	

	output_ports = malloc(sizeof(jack_port_t *) * n_out_channels);
	outputs = malloc(sizeof(jack_default_audio_sample_t *) * n_out_channels);
	
	for (int i = 0; i < n_out_channels; i++) {

	  char output_name[10];
	  sprintf(output_name, "output_%d", i);
	  
	  output_ports[i] = jack_port_register (client, output_name,
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);
	}


	input_ports = malloc(sizeof(jack_port_t *) * n_in_channels);
	inputs = malloc(sizeof(jack_default_audio_sample_t *) * n_in_channels);


	for (int i = 0; i < n_in_channels; i++) {

	  char input_name[10];
	  sprintf(input_name, "input_%d", i);
	  
	  input_ports[i] = jack_port_register (client, input_name,
						JACK_DEFAULT_AUDIO_TYPE,
						JackPortIsInput, 0);
	}

	
	/* if ((output_port1 == NULL) || (output_port2 == NULL)) { */
	/* 	fprintf(stderr, "no more JACK ports available\n"); */
	/* 	exit (1); */
	/* } */

	/* Tell the JACK server that we are ready to roll.  Our
	 * process() callback will start running now. */

	caml_enter_blocking_section();

	if (jack_activate (client)) {
		fprintf (stderr, "cannot activate client");
		exit (1);
	}

	/* Connect the ports.  You can't do this before the client is
	 * activated, because we can't make connections to clients
	 * that aren't running.  Note the confusing (but necessary)
	 * orientation of the driver backend ports: playback ports are
	 * "input" to the backend, and capture ports are "output" from
	 * it.
	 */
 	
	ports = jack_get_ports (client, NULL, NULL,
				JackPortIsPhysical|JackPortIsInput);
	if (ports == NULL) {
		fprintf(stderr, "no physical playback ports\n");
		exit (1);
	}

	for (int i = 0; i < n_out_channels; i++) {
	  if (jack_connect (client, jack_port_name (output_ports[i]), ports[i])) {
	    fprintf (stderr, "cannot connect output ports\n");
	  }
	}
	
	/* if (jack_connect (client, jack_port_name (output_port1), ports[0])) { */
	/* 	fprintf (stderr, "cannot connect output ports\n"); */
	/* } */


	jack_free (ports);
    
    /* install a signal handler to properly quits jack client */
/* #ifdef WIN32 */
/* 	signal(SIGINT, signal_handler); */
/* 	signal(SIGABRT, signal_handler); */
/* 	signal(SIGTERM, signal_handler); */
/* #else */
/* 	signal(SIGQUIT, signal_handler); */
/* 	signal(SIGTERM, signal_handler); */
/* 	signal(SIGHUP, signal_handler); */
/* 	signal(SIGINT, signal_handler); */
/* #endif */

	/* keep running until the Ctrl+C */

	while (1) {
	  sleep (1);
	}
	
	caml_leave_blocking_section();
  
	
	jack_client_close (client);
	
	CAMLreturn(Val_unit);
}
