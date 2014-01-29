#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdint>
#include <memory>
#include <vector>
extern "C" {
	#include <caml/mlvalues.h>
	#include <caml/memory.h>
	#include <caml/fail.h>
	#include <jack/jack.h>
	#include <jack/midiport.h>
}

using namespace std;


struct MidiFrame {
	uint64_t time;
	uint32_t size;
	uint8_t  msg[4];

	MidiFrame( uint64_t t, uint32_t s, uint8_t* m ):
		time( t ),
		size( s )
	{
		copy( m, m + s, msg );
	}
};

struct JackPlayer {
	jack_client_t*    jack;
	jack_port_t*      port;
	uint64_t          base0;
	uint64_t          base1;
	vector<MidiFrame> frames0;
	vector<MidiFrame> frames1;
	atomic<bool>      updating;
	atomic<bool>      reseting;

	JackPlayer():
		jack( nullptr ),
		port( nullptr ),
		base0( 480 ),
		base1( 480 ),
		updating( false ),
		reseting( false )
	{
	}
};

int jackProc( jack_nframes_t n, void* _self ) {
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	if( self->updating ) {
		swap( self->base0,   self->base1   );
		swap( self->frames0, self->frames1 );
		self->updating = false;
	}

	jack_position_t pos;
	if( jack_transport_query( self->jack, &pos ) != JackTransportRolling ) {
		return 0;
	}

	void* buf = jack_port_get_buffer( self->port, n );
	jack_midi_clear_buffer( buf );

	if( self->reseting ) {
		uint8_t msg[] = { 0xb0, 0x7b, 0x00 };
		for( int ch = 0; ch < 16; ++ch ) {
			msg[0] = 0xb0 + ch;
			jack_midi_event_write( buf, 0, msg, sizeof msg );
		}
		self->reseting = false;
	}

	auto compare = [&]( MidiFrame const& x, uint64_t y ) {
		return x.time * pos.frame_rate < y * self->base0;
	};
	auto fbgn = lower_bound( self->frames0.cbegin(), self->frames0.cend(), pos.frame,     compare );
	auto fend = lower_bound( self->frames0.cbegin(), self->frames0.cend(), pos.frame + n, compare );
	for( auto fit = fbgn; fit != fend; ++fit ) {
		uint64_t i = fit->time * pos.frame_rate / self->base0 - pos.frame;
		jack_midi_event_write( buf, i, fit->msg, fit->size );
	}

	return 0;
}

extern "C" value jackInit( value name ) {
	CAMLparam1( name );

	unique_ptr<JackPlayer> self( new JackPlayer() );

	self->jack = jack_client_open( String_val( name ), JackNullOption, 0 );
	if( !self->jack ) {
		caml_failwith( "" ); // XXX: dtor
	}
	if( jack_set_process_callback( self->jack, jackProc, self.get() ) ) {
		jack_client_close( self->jack );
		caml_failwith( "" ); // XXX: dtor
	}
	self->port = jack_port_register( self->jack, "out", JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0 );
	if( !self->port ) {
		jack_client_close( self->jack );
		caml_failwith( "" ); // XXX: dtor
	}
	if( jack_activate( self->jack ) ) {
		jack_client_close( self->jack );
		caml_failwith( "" ); // XXX: dtor
	}

	CAMLreturn( (value)self.release() );
}

extern "C" value jackTerm( value _self ) {
	CAMLparam1( _self );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	jack_client_close( self->jack );
	delete self;

	CAMLreturn( Val_unit );
}

extern "C" value jackStop( value _self ) {
	CAMLparam1( _self );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	jack_transport_stop( self->jack );

	CAMLreturn( Val_unit );
}

extern "C" value jackPlay( value _self ) {
	CAMLparam1( _self );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	jack_transport_start( self->jack );

	CAMLreturn( Val_unit );
}

extern "C" value jackSeek( value _self, value time, value base ) {
	CAMLparam3( _self, time, base );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	jack_position_t pos;
	jack_transport_query( self->jack, &pos );
	uint64_t i = Int_val( time ) * pos.frame_rate / Int_val( base );
	jack_transport_locate( self->jack, i );

	CAMLreturn( Val_unit );
}

extern "C" value jackConnect( value _self, value src, value dst ) {
	CAMLparam3( _self, src, dst );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	jack_connect( self->jack, String_val( src ), String_val( dst ) );

	CAMLreturn( Val_unit );
}

extern "C" value jackData( value _self, value data, value base ) {
	CAMLparam3( _self, data, base );
	JackPlayer* self = reinterpret_cast<JackPlayer*>( _self );

	while( self->updating );

	self->base1 = Int_val( base );
	self->frames1.clear();
	for( value it = data; it != Val_int( 0 ); it = Field( it, 1 ) ) {
		value v = Field( it, 0 );
		uint64_t time = Int_val( Field( v, 0 ) );
		uint32_t size = string_length( Field( v, 1 ) );
		uint8_t* msg = reinterpret_cast<uint8_t*>( String_val( Field( v, 1 ) ) );
		self->frames1.emplace_back( time, size, msg );
	}

	self->updating = true;

	CAMLreturn( Val_unit );
}
