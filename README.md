ujson - microjson protocol
==========================

Getting Started
---------------

I use beamer to build (https://github.com/cthulhuology/beamer)

	beamer make

About the protocol
------------------

The microjson protocol (ujson) is designed to provide a simple way to send both
structured and unstructured data to and from microcontrollers.  

ujson features
--------------

* packed binary encoding for efficient transfer of structure data
* length tagged encoding for loosly structure data
* application specific schemas for efficient transfer of meta-data
* datagram oriented message passing

While [JSON](http://json.org) is designed to seamlessly transfer JavaScript objects, 
and works well over a persistent TCP connection suitable for large scale applications 
like webbrowsers, it's verbosity can be difficult to parse and manage on small 
microcontrollers with only a few kilobytes for code and data.  Microjson remedies 
this problem by allowing for a packed binary encoding with length tagged values, 
which makes translation to machine native data structures easier, and reduces the
overhead necessary for encoding.  Unlike many other wire encoding schemes, microjson 
is designed to translate directly to and from JSON format, preserving the flexibility
and support for sparse data that makes JSON useful format for exchange.

The microjson application protocol is designed also to operate in constrained application
conditions similar to those addressed by protocols like CoAP, and serves much the
same function.  The microjson application protocol is typically run over UDP transport,
typically adding only a single byte of overhead over the IP, and UDP headers. 
This makes the microjson protocol very useful for applications utilizing serial
to celluar, WiFi, LoRa wireless connections.  It is particularly well suited for
distributed sensor networks where connectivity is unreliable.

ujson schema 
------------

In order to avoid repeatedly sending meta-data over a constrained connection, the
microjson protocol supports ujson schemas.  A ujson schema is a machine readable object 
that describes the content of a datagram.  Each ujson schema is application specific,
and discoverable via the ujson protocol.  When a ujson message is sent, the first byte
specifies the application specific schema that describes the datagram's contents. Each
schema is identified by a URI of the form:

	ujson:// [ <user> : <token> @ ] <host> : <port> / <schema index>

The method to query for a ujson schema is to send a UDP datagram to the given host and
port, with a ujson schema message: 

	/<schema index>

where the <schema index> is a single byte, with a value of 1 - 255.  The index 0 is
reserved for schema-less messages, wherein all of the necessary meta-data is included
inline using the ujson protocol tagged value scheme.  This flexibility allows common
messages, such as a temperature reading, or an accelerometer's 6 axis readings, to be
sent repeatedly over time, without including the field descriptors each time.

For example, consider the case of a device that reports it's movement and orientation:

	{ "x": 123, "y": 20, "z": 0, "r": 45, "p": 0, "y": 23 }

This JSON representation consists of x, y, z, roll, pitch, yaw components, and we'll
assume that these values are constrained to 8 bits of precision (-128 to 127) for each
dimension







