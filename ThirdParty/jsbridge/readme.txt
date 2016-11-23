Delphi code authored by Sterling Bates
Linux compatibility by Theo Lustenberger (theo@theo.ch)
Please send comments for enhancements, fixes, etc, to whoelse@sterlingbates.com

All code contained within these files is freely available for any use whatsoever.  There 
are no restrictions; I only ask that you share any fixes or enhancements you may make with
the rest of us.

As is common with free code, these files come with no warranties or guarantees.

------------------------------------------------------------------------------------------

The source code used to build the JS engine is covered by the Mozilla Public License, and
was not modified by myself in any way.  To read the terms of the MPL regarding distri-
bution of the DLL, please read section 3.6 of http://www.mozilla.org/MPL/MPL-1.1.html

(In general, you're safe -- just be sure your license meets MPL requirements.  Also, I am
not a lawyer.)

------------------------------------------------------------------------------------------

NOTES
	December 13, 2003

	This release has some token code updates, but is mostly a help release.  I've
	added detailed help on all of the bridge classes, including properties, methods,
	and uses.

	I've also started building a portable set of test routines.  These are found in
	jstesting.pas.  Feel free to run these on your system to see whether all of the
	features work.  The return strings from each test function can be placed inside
	a TMemo or TRichEdit to see how things worked.  Known issues are also listed in
	the test function return value, but there are no critical issues.

	----------------------------------------------------------------------------------
	December 8, 2003

	Several updates to this release, including a small help file.  The help file will
	grow with time as I receive requests and make updates.  Some of the other updates
	include:
		- Enumeration of javascript objects.  There is a new routine in
		  jssamples.pas which demonstrates how this works.
		- Added support for a custom script error reporter.  I haven't wrapped
		  this in a Delphi object yet, but that'll come in the next release I 
		  hope.
		- A major update in the TJS* objects.  TJSEngine has seen its methods
		  scaled down and shifted over to the TJSObject class.  See the samples 
		  file to see how things work.

	The next release should see TJSArray functioning propertly, which means the
	TJSStringArray should also work.

	I'm also seeking recommendations on how to further develop this code.  Beyond
	fixes and minor updates I'm pretty much at the end of the development cycle for
	this set of code.  One thing that I've thought of, but haven't planned to act on,
	is a script editor.  If enough people are interested, then I'll probably devote 
	some time to that.

	As always, please let me know if you find bugs!

	----------------------------------------------------------------------------------
	November 26, 2003

	I decided the release of the 24th would wait until I finished the TJSArray class.
	It was a good idea, since a kind fellow named Theo Lustenberger (theo@theo.ch)
	has made the necessary changes for the code to work in Kylix.  The zip now 
	contains the Linux shared object (.so) file.

	I've also done up a prototype of the TJSArray class.  It works under my simple
	tests, but there will be bugs in it.  Please let me know.  One of the known bugs
	is an AnsiString memory leak.

	All of the TJS* classes are psuedo-garbage collected.  The TJSEngine instance will
	track them and free all non-freed objects during shutdown.

	The next major feature will likely be enumeration of all objects.  I have a few
	things to work out before this is implemented, and so far everything looks good 
	for it.

	----------------------------------------------------------------------------------
	November 24, 2003
	
	Several major updates and revisions in this release.  Most memory leaks should be
	gone.  I've used the AQTime 3 profiler, and everything checks out so far.  Once
	this was done, I found the code to be much more stable as well.

	New classes have been added to create integers, doubles, and strings that are 
	simultaneously accessible to both Delphi and Javascript.  The classes are
	TJSInteger, TJSDouble, and TJSString.  All three require a TJSEngine instance to
	work with JS.  A name is also required when manipulating the values of those
	objects from Javascript.  The name is not required when the TJS* object is
	created to call a function inside JS.  (See jssamples.pas for examples.)

	In addition, all methods in TJSEngine now ask for TJSBase-descendent objects 
	(such as TJS*), rather than the mysterious jsval/PJSString/PJSObject.  As time 
	permits I'll finish up the TJSArray class.

	----------------------------------------------------------------------------------
	November 8, 2003

	I've updated the JS engine DLL to version 1.5 release, and changed the name of the
	header declarations file to js15decl.pas.  The bridge class (jsbridge.pas) is in-
	between revisions right now, but it works.  I had separated the engine interface
	functions from the bridge class, but that forced me to make GetProperty et al
	public, instead of protected.  I'm not a fan of that at all, so the next release
	will be slightly modified again.


CONTRIBUTORS
	Anthoni Gardner			- MSVCR70.DLL inclusion, documentation
	Theo Lustenberger		- Linux compatibility, documentation


FILES

	help\*.*
		Includes detailed help on all of the bridge classes, plus an FAQ.
	jsintf.pas
		This is the primary include file.  Everything you need to work with should
		be here.  If it's not, please let me know and I'll update it.
	js15decl.pas
		The headers for interfacing with js3215R.dll.
	js3215R.dll
		The actual javascript engine from Mozilla.  This file is compiled by me
		from the mozilla/js/src folder.
	jsbridge.pas
		Contains the bridge class.
	jsbridge_pvt.pas
		The code, used by the bridge class, which interacts with the JS engine.
	jssamples.pas
		A few small routines that demonstrate how to use the JS engine wrapper
		in jsintf.pas.
	jssamples_bridge.pas
		An example of a small bridge class.
	jssamples_natives.pas
		These are simple procedures that show how to work with the JS engine 
		without any wrapper assistance.  These are not well documented, and 
		show a very small subset of the JS engine's capabilities.
	msvcr70.dll
		This file is required to be in your application's main folder.  The JS
		engine dll depends on it.
