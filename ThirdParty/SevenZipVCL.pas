unit SevenZipVCL;
(*
   This Unit is under Mozilla Public Licence
    ( 
     - You can use this Unit for free in free, share and commercial application.
     - Mark clearly in your Readme or Help file that you use this unit/VCL with a link the 
       SevenZipVCL Homepage ( http://www.rg-software.de )
     - Any changes of the source must be publised ( Just send it to me :- ) SevenZipVCL@rg-software.de )
    )


   TsevenZip by Ivo Andonov
   TSevenZipVCL by Rainer Geigenberger ( -> http://www.rg-software.de - SevenZipVCL@rg-software.de )

   Thanks to:
    - Marko Kamin
    - Craig Peterson
    - Roberto
    - Erik Smith
    - Sergey Prokhorov
    - Flurin Honegger
    - Zach Saw
    - Guillaume Di Giusto 
	
   Dll Usage:
        For develpoment put the dll into the Windows directory
        Running the Application stand alone you can put the dll into the Application directory	
		
 History:

   Version 0.1
    - Start

   Version 0.2
    - Changed everything to Widestring
    - Added WideStringList_
    - Added TWideStringArray
    - Adding RelativePath works
    - Adding works with Recursive Directory
    - Adding with Wildcards ( only *.txt or something )
    - Progress for files ( With SetCompleted VCL interaction works )

   Version 0.3
    - Added time reading and writing

   Version 0.4
    - Added Extract
    - Added Test via Extract( True )
    - Set Filetime during extraction
    - Progress during extraction works ( small files will not be displayed alone )

   Version 0.5
    - Changes here and there
    - Clean up code

   Version 0.6
    - Changed "Extract all files"
    - Fixed bug in handling directories during extract
    - Attribute to extracted files works now
    - Fixed LastwriteTime during adding
    - Changed some functions

   Version 0.6.1
    - Some minor changes
    - Fixed some bugs
    - Added MaxProgrees to selected files
    - Added some missing GUID - not used now :- )

   Version 0.6.2
    - Compression strength could be set
    - Drive letter to Storepath option included
    - Changes made by Marko Kamin

   Version 0.6.3
    - Changed Archive options to new type Addopts
    - Solid settings works now

   Version 0.6.4
    - Implemented SFX creation

   Version 0.6.4b
    - LZMAStrength added

   Version 0.6.5
    - PPMD method added

   Version 0.6.5a
    - Fixed bug during listing of 0 byte files
   
   Version 0.6.5b
    - Fixed bug during creating files for extract/SFXarchive in Tstreamwriter 

   Version 0.6.5c
    - Fixed bug Creating SFX

   Version 0.6.6
    - Adding and extracting can be canceled

   Version 0.6.6a
    - Adding Extract without path

   Version 0.6.6c
    - Reading SFX
    - Number of files
    - New: IsSFX and SXFOffset
    - New: Function ConvertSFXto7z
    - New: Function Convert7ztoSFX

   Version 0.6.7
    - Added some Widestring function form TNTWare TNT Controls http://www.tntware.com/
    - New: OnExtractOverwrite - Do not work with Messageboxes right now
    - Include Extractoverwrite in Extractoption

   Version 0.6.7a
    - Changed constructor and destructor to avoid excaption - Thanks to Roberto jjw
    - Fixed Unicode bug during adding

   Version 0.6.7b
    - Clean up code. Thanks to Erik Smith
    - Rewrote Add function. Thanks to Erik Smith

   Version 0.6.7c
   - Multivolume support added - Thanks to Sergey Prokhorov
   - Begin password support - Thanks to Sergey Prokhorov

   Version 0.6.7e
   - Multivolume support improved
   - Clean up code

   Version 0.6.8a
   - Password implemented - Thanks to Sergey Prokhorov

   Version 0.6.8e
   - Implementation of 9x support started
   - Cleanup some comments

   Version 0.7.0
   - OpenarchiveCallback implemented
   - Encrypt filename option implemented

   Version 0.7.1
   - Fixed Bug in password support

   Version 0.7.1c
   - Changes by Flurin Honegger
   - Fixed 4GB limit (Filesize, Archive and Multivolume)
   - Added fileseek for Int64 (some Delphi versions do not take the right internal one)

   Version 0.7.2
   - added kpidLastAccessTime with value = 0
   - fixed bug in addrootdir during add

   Version 0.7.3
   - changed Getindexbyfilename -> InternalGetindexbyfilename by Zach Saw
   - added UseLog switch for smaller exe by Guillaume Di Giusto
   - Some other minor changes

   Version 0.7.4
   - Bugfixes
   - Changed function name

   Version 0.7.4a
   - Fixed bug while adding files with AddStoreonlyfilename

   Version 0.7.5
   - Fixed bug ainPPMP routine. Thanks to Dean Mustakinov

   Version 0.7.6
   - Added/fixed support for D07-D10. Thanks to Grant van Wyk


   Author Shortcuts:   FHO    Flurin Honegger             fhonegger@b2i.info
  Who    When          What
  --------------------------------------------------------------------------
  FHO    17.01.2007  - Need the filenames back when creating a multivolume
                       archive.

         20.01.2007  - Handles are of type cardinal. Comparison of the type
                       "if Returned_Handle <=0 then" are not correct.
                       Code changed to look like
                       "if Returned_Handle = INVALID_HANDLE_VALUE".

  FHO    21.01.2007  - Call to onmessage event handler references:
                        a.) Fxxx constant used
                        b.) Messagestrings collected in c7zipResMsg and
                            referenced by Fxxx constants

  FHO    22.01.2007  - Need better reason for error (GetLastError) return!
                       Corresponding code added.
                     - {$IFNDEF RegisterInThisUnit} added. I prefer to
                       register in a different "Collection unit".

  FHO    25.01.2007  - Resident non residient code realized with one
                       switch UseRes7zdll only.
                     - Comparison after LoadLibrary must be "<>0" and not
                       ">0". Similar problem as the one fixed 20.01.2007.

*)

(*
   Known Issues / Things ToDo:
   
    - No archive properties during listing
    - With Solid archives filenames and progress during extract comes very late
     ( at the end )
    - No deleting files from archive
    - No adding to existing archives
    - If a wrong password is given a crash occures at position marked with
                     "//FHO crash at wrong pw 25.01.2007"
    - Multi volume sfx does not work.                 

  Please mark all changes with your sign and date e.g. rg 01.01.06
  and send it to me SevenZipVCL@rg-software.de
*)


//----------------------------------------------------------------------
// Conditional switches
//----------------------------------------------------------------------

// {$WARN UNIT_PLATFORM OFF}    //works with higer Delphi versions
// {$WARN SYMBOL_PLATFORM OFF}  //works with higer Delphi versions


//----------------------------------------------------------------------
// Define to use Resfile with 7z.dll, no external dll, accessing through
//  BTMemoryModule                                             //FHO 25.01.2007
//----------------------------------------------------------------------
//{$DEFINE UseRes7zdll}

//----------------------------------------------------------------------
//Register within this unit or in external Collection
//----------------------------------------------------------------------
{$DEFINE RegisterInThisUnit}

//----------------------------------------------------------------------
// Dynamically load dll
//----------------------------------------------------------------------
//{$DEFINE DynaLoadDLL} // Not used now

// GDG 21.02.07 : added conditional switch to disable log functions and make final program smaller
//----------------------------------------------------------------------
// Define if you want to use log functions
//----------------------------------------------------------------------
//{$DEFINE UseLog}

interface
{$IFDEF UseRes7zdll}
  {$R 7za.res}
{$ENDIF}

uses
  Windows, SysUtils, Classes, ActiveX,comobj,filectrl
  {$IFDEF UseRes7zdll}
  ,BTMemoryModule
  {$ENDIF}
  ;

const
//7z internal consts

//Extract
  //NAskMode
  kExtract = 0;
  kTest    = 1;
  kSkip    = 2;

  //NOperationResult
  kOK                = 0;
  kUnSupportedMethod = 1;
  kDataError         = 2;
  kCRCError          = 3;

  FNAME_MAX32 = 512;

// SevenZIP onMessage Errorcode
  FNoError             = 0;
  FFileNotFound        = 1;
  FDataError           = 2;
  FCRCError            = 3;
  FUnsupportedMethod   = 4;
  FIndexOutOfRange     = 5;                                    //FHO 21.01.2007
  FUsercancel          = 6;
  FNoSFXarchive        = 7;
  FSFXModuleError      = 8;
  FSXFileCreationError = 9;                                    //FHO 21.01.2007
  FNoFilesToAdd        =10;                                    //FHO 21.01.2007
  FNoFileCreated       =11;

  c7zipResMsg:array[FNoError..FNoFileCreated] of string=       //FHO 21.01.2007
  { 0}('Success',                                              //FHO 21.01.2007
  { 1} 'File not found',                                       //FHO 21.01.2007
  { 2} 'Data Error',                                           //FHO 21.01.2007
  { 3} 'CRC Error',                                            //FHO 21.01.2007
  { 4} 'Unsupported Method',                                   //FHO 21.01.2007
  { 5} 'Index out of Range',                                   //FHO 21.01.2007
  { 6} 'User canceled operation',                              //FHO 21.01.2007
  { 7} 'File is not an 7z SFX archive',                        //FHO 21.01.2007
  { 8} 'SFXModule error ( Not found )',                        //FHO 21.01.2007
  { 9} 'Could not create SFX',                                 //FHO 21.01.2007
  {10} 'No files to add',                                      //FHO 21.01.2007
  {11} 'Could not create file'                                 //FHO 21.01.2007

       );                                                      //FHO 21.01.2007



const
  kpidNoProperty = 0;
  kpidHandlerItemIndex = 2;
  kpidPath = 3;
  kpidName = 4;
  kpidExtension = 5;
  kpidIsFolder = 6;
  kpidSize = 7;
  kpidPackedSize = 8;
  kpidAttributes = 9;
  kpidCreationTime = 10;
  kpidLastAccessTime = 11;
  kpidLastWriteTime = 12;
  kpidSolid = 13;
  kpidCommented = 14;
  kpidEncrypted = 15;
  kpidSplitBefore = 16;
  kpidSplitAfter = 17;
  kpidDictionarySize = 18;
  kpidCRC = 19;
  kpidType = 20;
  kpidIsAnti = 21;
  kpidMethod = 22;
  kpidHostOS = 23;
  kpidFileSystem = 24;
  kpidUser = 25;
  kpidGroup = 26;
  kpidBlock = 27;
  kpidComment = 28;
  kpidPosition = 29;

  kpidTotalSize = $1100;
  kpidFreeSpace = $1101;
  kpidClusterSize = $1102;
  kpidVolumeName = $1103;

  kpidLocalName = $1200;
  kpidProvider = $1201;
  kpidUserDefined = $10000;


//jjw 18.10.2006
type
  TCreateObjectFunc = function ( const clsid: PGUID; const iid: PGUID; out _out ): Integer; stdcall;


//----------------------------------------------------------------------------------------------------
//--------------Widestring Classes--------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

type
  TWideStringArray = array of WideString;

  TWideStringList_ = class( TObject )
   private
   public
    WStrings: array of WideString;
    Count: Longword;
    constructor Create;
    procedure Clear;
    procedure AddString( s: WideString );
    procedure RemoveString( s: WideString );
   end;


type
  TCompressType = ( LZMA,PPMD );
  TCompressStrength = ( SAVE,FAST,NORMAL,MAXIMUM,ULTRA );
  TLZMAStrength = 0..27;
  TPPMDMem = 1..31;
  TPPMDSize = 2..32;

  AddOptsEnum = ( AddRecurseDirs, AddSolid, AddStoreOnlyFilename, AddIncludeDriveLetter, AddEncryptFilename );
  AddOpts = Set Of AddOptsEnum;

  ExtractOptsEnum = ( ExtractNoPath,ExtractOverwrite );
  ExtractOpts = Set Of ExtractOptsEnum;

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//--------------Start SevenZip Interface-------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------


type
  TInterfacedObject = class( TObject, IUnknown )
  protected
    FRefCount: Integer;
    function QueryInterface( const IID: TGUID; out Obj ): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

const
//  Correct below for 7-Zip 4.23 or 4.29. Comment this line
//  {$DEFINE 7z423}
  {$DEFINE 7z429}

  {$IFDEF 7z423}
  szCLSID_CFormat7z = '{23170F69-40C1-278A-1000-000110050000}';
  szIID_IInArchive = '{23170F69-40C1-278A-0000-000100080000}';
  szIID_IOutArchive = '{23170F69-40C1-278A-0000-000100020000}';
  szIID_ISetProperties = '{23170F69-40C1-278A-0000-000100030000}';
  szIID_IOutStream = '{23170F69-40C1-278A-0000-000000040000}';
  szIID_ISequentialInStream = '{23170F69-40C1-278A-0000-000000010000}';
  szIID_IInStream = '{23170F69-40C1-278A-0000-000000030000}';
  szIID_IStreamGetSize = '{23170F69-40C1-278A-0000-000000060000}';
  szIID_IArchiveOpenCallback = '{23170F69-40C1-278A-0000-000100010000}';
  szIID_IArchiveExtractCallback = '{23170F69-40C1-278A-0000-000100090000}';
  szIID_IArchiveUpdateCallback = '{23170F69-40C1-278A-0000-000100040000}';
  szIID_IProgress = '{23170F69-40C1-278A-0000-000000050000}';
  szIID_ISequentialOutStream = '{23170F69-40C1-278A-0000-000000020000}';
  {$ENDIF}

  {$IFDEF 7z429}
//000
  szIID_IProgress =                         '{23170F69-40C1-278A-0000-000000050000}';
//30
  szIID_ISequentialInStream =               '{23170F69-40C1-278A-0000-000300010000}';
  szIID_ISequentialOutStream =              '{23170F69-40C1-278A-0000-000300020000}';
  szIID_IInStream =                         '{23170F69-40C1-278A-0000-000300030000}';
  szIID_IOutStream =                        '{23170F69-40C1-278A-0000-000300040000}';
  szIID_IStreamGetSize =                    '{23170F69-40C1-278A-0000-000300060000}';
  szIID_IOutStreamFlush =                   '{23170F69-40C1-278A-0000-000300070000}';
//400
  szIID_ICompressProgressInfo =             '{23170F69-40C1-278A-0000-000400040000}';
  szIID_ICompressCoder =                    '{23170F69-40C1-278A-0000-000400050000}';
  szIID_ICompressCoder2 =                   '{23170F69-40C1-278A-0000-000400180000}';
  szIID_ICompressSetCoderProperties =       '{23170F69-40C1-278A-0000-000400200000}';
  szIID_ICompressSetDecoderProperties =     '{23170F69-40C1-278A-0000-000400210000}';
  szIID_ICompressSetDecoderProperties2 =    '{23170F69-40C1-278A-0000-000400220000}';
  szIID_ICompressWriteCoderProperties =     '{23170F69-40C1-278A-0000-000400230000}';
  szIID_ICompressGetInStreamProcessedSize = '{23170F69-40C1-278A-0000-000400240000}';
  szIID_ICompressGetSubStreamSize =         '{23170F69-40C1-278A-0000-000400300000}';
  szIID_ICompressSetInStream =              '{23170F69-40C1-278A-0000-000400310000}';
  szIID_ICompressSetOutStream =             '{23170F69-40C1-278A-0000-000400320000}';
  szIID_ICompressSetInStreamSize =          '{23170F69-40C1-278A-0000-000400330000}';
  szIID_ICompressSetOutStreamSize =         '{23170F69-40C1-278A-0000-000400340000}';
  szIID_ICompressFilter =                   '{23170F69-40C1-278A-0000-000400400000}';
  szIID_ICryptoProperties =                 '{23170F69-40C1-278A-0000-000400800000}';
  szIID_ICryptoSetPassword =                '{23170F69-40C1-278A-0000-000400900000}';
  szIID_ICryptoSetCRC =                     '{23170F69-40C1-278A-0000-000400A00000}';
//500
  szIID_ICryptoGetTextPassword =            '{23170F69-40C1-278A-0000-000500100000}';
  szIID_ICryptoGetTextPassword2 =           '{23170F69-40C1-278A-0000-000500110000}';
//600
  szIID_ISetProperties =                    '{23170F69-40C1-278A-0000-000600030000}';
  szIID_IArchiveOpenCallback =              '{23170F69-40C1-278A-0000-000600100000}';
  szIID_IArchiveExtractCallback =           '{23170F69-40C1-278A-0000-000600200000}';
  szIID_IArchiveOpenVolumeCallback =        '{23170F69-40C1-278A-0000-000600300000}';
  szIID_IInArchiveGetStream =               '{23170F69-40C1-278A-0000-000600400000}';
  szIID_IArchiveOpenSetSubArchiveName =     '{23170F69-40C1-278A-0000-000600500000}';
  szIID_IInArchive =                        '{23170F69-40C1-278A-0000-000600600000}';
  szIID_IArchiveUpdateCallback =            '{23170F69-40C1-278A-0000-000600800000}';
  szIID_IArchiveUpdateCallback2 =           '{23170F69-40C1-278A-0000-000600820000}';
  szIID_IOutArchive =                       '{23170F69-40C1-278A-0000-000600A00000}';

  szCLSID_CFormat7z =                       '{23170F69-40C1-278A-1000-000110070000}';

  szIID_CCrypto_Hash_SHA256                = '{23170F69-40C1-278B-0703-000000000000}';

  szIID_CCrypto7zAESEncoder                = '{23170F69-40C1-278B-06F1-070100000100}';
  szIID_CCrypto7zAESDecoder                = '{23170F69-40C1-278B-06F1-070100000000}';
  {$ENDIF}

  CLSID_CFormat7z: TGUID = szCLSID_CFormat7z;
  IID_IInArchive: TGUID = szIID_IInArchive;
  IID_IOutArchive: TGUID = szIID_IOutArchive;
  IID_ISetProperties: TGUID = szIID_ISetProperties;
  IID_ICompressCoder: TGUID = szIID_ICompressCoder;
  IID_ICryptoGetTextPassword: TGUID = szIID_ICryptoGetTextPassword;
  IID_ICryptoGetTextPassword2: TGUID = szIID_ICryptoGetTextPassword2;
  IID_ICryptoSetPassword: TGUID = szIID_ICryptoSetPassword;
  IID_IOutStream: TGUID = szIID_IOutStream;
  IID_ISequentialInStream: TGUID = szIID_ISequentialInStream;
  IID_IInStream: TGUID = szIID_IInStream;
  IID_IStreamGetSize: TGUID = szIID_IStreamGetSize;
  IID_IArchiveOpenCallback: TGUID = szIID_IArchiveOpenCallback;
  IID_ICompressGetSubStreamSize: TGUID = szIID_ICompressGetSubStreamSize;
  IID_IArchiveOpenSetSubArchiveName: TGUID = szIID_IArchiveOpenSetSubArchiveName;
  IID_IArchiveExtractCallback: TGUID = szIID_IArchiveExtractCallback;
  IID_IArchiveOpenVolumeCallback: TGUID = szIID_IArchiveOpenVolumeCallback;
  IID_IArchiveUpdateCallback: TGUID = szIID_IArchiveUpdateCallback;
  IID_IArchiveUpdateCallback2: TGUID = szIID_IArchiveUpdateCallback2;
  IID_IProgress: TGUID = szIID_IProgress;
  IID_ISequentialOutStream: TGUID = szIID_ISequentialOutStream;
  IID_CCrypto7zAESEncoder: TGUID = szIID_CCrypto7zAESEncoder;


type
  HARC = THandle;
  PInt64        = ^Int64;

type
  ISetProperties = interface( IUnknown )
    [ szIID_ISetProperties ]
    function SetProperties( const names: PWideChar; const values: PPROPVARIANT; numProperties: Integer ): Integer; stdcall;
  end;

  ICompressProgressInfo = interface( IUnknown )
    [ szIID_ICompressProgressInfo ]
    function SetRatioInfo( const inSize, outSize: Int64 ): Integer; stdcall;
  end;

  ISequentialOutStream = interface( IUnknown )
    [ szIID_ISequentialOutStream ]
    function Write( const data; size: DWORD; processedSize: PDWORD ): Integer; stdcall;
    {$IFDEF 7z423}
    function WritePart( const data; size: DWORD; processedSize: PDWORD ): Integer; stdcall;
    {$ENDIF}
  end;

  ISequentialInStream = interface( IUnknown )
    [ szIID_ISequentialInStream ]
    function Read( var data; size: DWORD; processedSize: PDWORD ): Integer; stdcall;
    {$IFDEF 7z423}
    function ReadPart( var data; size: DWORD; processedSize: PDWORD ): Integer; stdcall;
    {$ENDIF}
  end;

  ICryptoGetTextPassword = interface( IUnknown )
    [ szIID_ICryptoGetTextPassword ]
    function CryptoGetTextPassword( var Password: PWideChar ): Integer; stdcall;
  end;

  ICryptoGetTextPassword2 = interface( IUnknown )
    [ szIID_ICryptoGetTextPassword2 ]
    function CryptoGetTextPassword2( passwordIsDefined: PInteger; var Password: PWideChar ): Integer; stdcall;
  end;

  ICryptoProperties = interface( IUnknown )
    [ szIID_ICryptoProperties ]
    function SetKey( const Data; Size: DWORD ): Integer; stdcall;
    function SetInitVector( const Data; Size: DWORD ): Integer; stdcall;
  end;

  ICompressCoder = interface( IUnknown )
    [ szIID_ICompressCoder ]
    function Code( inStream: ISequentialInStream; outStream: ISequentialOutStream;
      const inSize, outSize: Int64; Progress: ICompressProgressInfo ): Integer; stdcall;
  end;

  ICryptoSetPassword = interface( IUnknown )
    [ szIID_ICryptoSetPassword ]
    function CryptoSetPassword( const Data; Size: DWORD ): Integer; stdcall;
  end;

  ICryptoSetCRC = interface( IUnknown )
    [ szIID_ICryptoSetCRC ]
    function CryptoSetCRC( CRC: DWORD ): Integer; stdcall;
  end;

  IInStream = interface( ISequentialInStream )
    [ szIID_IInStream ]
    function Seek( offset: Int64; seekOrigin: DWORD;newPosition: PInt64 ): Integer; stdcall;
  end;

  IStreamGetSize = interface( IUnknown )
    [ szIID_IStreamGetSize ]
    function GetSize( var size: Int64 ): Integer; stdcall;
  end;

  IArchiveOpenCallback = interface( IUnknown )
    [ szIID_IArchiveOpenCallback ]
    function SetTotal( const files: Int64; const bytes: Int64 ): Integer; stdcall;
    function SetCompleted( const files: Int64; const bytes: Int64 ): Integer; stdcall;
  end;

  IArchiveOpenVolumeCallback = interface( IUnknown )
    [ szIID_IArchiveOpenVolumeCallback ]
    function GetProperty( propID: PROPID; var value: PROPVARIANT ): Integer; stdcall;
    function GetStream( const name:Widechar; var inStream: IInStream ): Integer; stdcall;
  end;

  IArchiveOpenSetSubArchiveName = interface( IUnknown )
    [ szIID_IArchiveOpenSetSubArchiveName ]
    function SetSubArchiveName( const Name: PWideString ): Integer; stdcall;
  end;

  IProgress = interface( IUnknown )
    [ szIID_IProgress ]
    function SetTotal( total: Int64 ): Integer; stdcall;
    function SetCompleted( const completeValue: PInt64 ): Integer; stdcall;
  end;

  IArchiveExtractCallback = interface( IProgress )
    [ szIID_IArchiveExtractCallback ]
    function GetStream( index: DWORD; out outStream: ISequentialOutStream;  askExtractMode: DWORD ): Integer; stdcall;
    // GetStream OUT: S_OK - OK, S_FALSE - skeep this file
    function PrepareOperation( askExtractMode: Integer ): Integer; stdcall;
    function SetOperationResult( resultEOperationResult: Integer ): Integer; stdcall;
  end;

  IInArchive = interface( IUnknown )
    [ szIID_IInArchive ]
    function Open( stream: IInStream; const maxCheckStartPosition: PInt64; openArchiveCallback: IArchiveOpenCallback ): Integer; stdcall;
    function Close( ): Integer; stdcall;
    function GetNumberOfItems( out numItems: DWORD ): Integer; stdcall;
    function GetProperty( index: DWORD; propID: PROPID; var value: PROPVARIANT ): Integer; stdcall;
    function Extract( const indices: PDWORD; numItems: DWORD;   testMode: Integer; extractCallback: IArchiveExtractCallback ): Integer; stdcall;
    function GetArchiveProperty( propID: PROPID; value: PPROPVARIANT ): Integer; stdcall;
    function GetNumberOfProperties( var numProperties: DWORD ): Integer; stdcall;
    function GetPropertyInfo( index: DWORD; var name: TBSTR; var propID: PROPID; var varType: {PVARTYPE}Integer ): Integer; stdcall;
    function GetNumberOfArchiveProperties( var numProperties ): Integer; stdcall;
    function GetArchivePropertyInfo( index: DWORD; name: PBSTR; propID: PPROPID; varType: {PVARTYPE}PInteger ): Integer; stdcall;
  end;

  IArchiveUpdateCallback = interface( IProgress )
    [ szIID_IArchiveUpdateCallback ]
    //function EnumProperties( var enumerator: IEnumSTATPROPSTG ): Integer; stdcall;
    function GetUpdateItemInfo( index: DWORD;
      newData: PInteger; // 1 - new data, 0 - old data
      newProperties: PInteger; // 1 - new properties, 0 - old properties
      indexInArchive: PDWORD // -1 if there is no in archive, or if doesn't matter
      ): Integer; stdcall;
    function GetProperty( index: DWORD; propID: PROPID; value: PPROPVARIANT ): Integer; stdcall;
    function GetStream( index: DWORD; var inStream: ISequentialInStream ): Integer; stdcall;
    function SetOperationResult( operationResult: Integer ): Integer; stdcall;
  end;


  IArchiveUpdateCallback2 = interface( IProgress )
    [ szIID_IArchiveUpdateCallback2 ]
    //function EnumProperties( var enumerator: IEnumSTATPROPSTG ): Integer; stdcall;
    function GetVolumeSize( index: DWORD; Size:DWord ): Integer; stdcall;
    function GetVolumeStream( index: DWORD; var volumeStream: ISequentialInStream ): Integer; stdcall;
  end;

  IOutArchive = interface( IUnknown )
    [ szIID_IOutArchive ]
    function UpdateItems( outStream: ISequentialOutStream; numItems: DWORD; updateCallback: IArchiveUpdateCallback ): Integer; stdcall;
    function GetFileTimeType( var _type: DWORD ): Integer; stdcall;
  end;

  IOutStream = interface( ISequentialOutStream )
    [ szIID_IOutStream ]
    function Seek( offset: Int64; seekOrigin: DWORD; newPosition: PInt64 ): Integer; stdcall;
    function SetSize( newSize: Int64 ): Integer; stdcall;
  end;

// -----------------------------------------------------------------------------

  TSevenZip = class;   // for reference only, implementated later below
  TOpenVolume = procedure( var arcFileName: WideString; Removable: Boolean; out Cancel: Boolean ) of object;

  TFiles = record
    Name: WideString;
    Handle: cardinal; //Integer;                              //FHO  20.01.2007
    Size: Int64;//DWORD;                                      // RG  26.01.2007
    OnRemovableDrive: Boolean;
  end;

  TArrayOfFiles = array of TFiles;                             //FHO 17.01.2007

  TMyStreamWriter = class( TInterfacedObject, ISequentialOutStream, IOutStream )
  private
    arcName: WideString;
    arcDate: Tdatetime;
    arcAttr: DWORD;
    arcCreateSFX: Boolean;
    arcVolumeSize: DWORD;
    arcPosition, arcSize: int64; // DWORD;                     // RG  26.01.2007
    FPLastError:PInteger;                                      //FHO 22.01.2007
    MyLastError: Integer;                                      //FHO 22.01.2007
    Files: TArrayOfFiles;

    function CreateNewFile: Boolean;
  protected
    property TheFiles: TArrayOfFiles read Files;
  public
    destructor Destroy; override;
    constructor Create( PLastError:PInteger;sz: Widestring;    //FHO 22.01.2007
                        szDate: Tdatetime; FAttr: Cardinal;
                        VolumeSize: Integer = 0; CreateSFX: Boolean = FALSE );
    function Write( const Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
    function WritePart( const Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
    function Seek( Offset: Int64; SeekOrigin: DWORD; NewPosition: PInt64 ): Integer; stdcall;
    function SetSize( newSize: Int64 ): Integer; stdcall;
  end;

  TMyStreamReader = class( TInterfacedObject, IInStream, IStreamGetSize, ISequentialInStream )
    FSevenZip: TSevenZip;
    arcName: WideString;
    arcPosition, arcSize: Int64; //DWORD;                     // RG  26.01.2007
    Files: TArrayOfFiles;
    FOnOpenVolume: TOpenVolume;
    FArchive: Boolean;
    MyLastError: Integer;                                      //FHO 22.01.2007
    
    FMultivolume: Boolean;
    function BrowseForFile( Title: PWideChar; var Name: WideString ): Boolean;
    function OpenVolume( Index: Integer ): Boolean;
    function OpenNextVolume: Boolean;
    function OpenLastVolume: Boolean;
  public
    constructor Create( Owner: TSevenZip; sz: Widestring; asArchive: Boolean );
    destructor Destroy; override;
    function Seek( Offset: Int64; SeekOrigin: DWORD; NewPosition: PInt64 ): Integer; stdcall;
    function Read( var Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
    function ReadPart( var Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
    function GetSize( var Size: Int64 ): Integer; stdcall;
  end;

// -----------------------------------------------------------------------------
  TMyArchiveUpdateCallback = class( TInterfacedObject, IArchiveUpdateCallback, ICryptoGetTextPassword2, IProgress )
    FSevenZip: TSevenZip;
    Files: TWideStringArray;//TStringList;
    Files_size: array of int64;
    Files_Date: array of TFiletime;
    Files_Attr: array of Cardinal;
    FProgressFile: Widestring;
    FProgressFilePos: int64;
    FprogressFileSize: int64;
    FLastPos: int64;
    RootDir: WideString;
    FPassword: WideString;
//    FIncludeDriveletter: Boolean;
    constructor Create( Owner: TSevenZip );
//    destructor destroy;
//    function EnumProperties( var enumerator: IEnumSTATPROPSTG ): Integer; stdcall;
    function GetUpdateItemInfo(
      index: DWORD;
      newData: PInteger; // 1 - new data, 0 - old data
      newProperties: PInteger; // 1 - new properties, 0 - old properties
      indexInArchive: PDWORD // -1 if there is no in archive, or if doesn't matter
    ): Integer; stdcall;
    function GetProperty( index: DWORD; propID: PROPID; value: PPROPVARIANT ): Integer; stdcall;
    function GetStream( index: DWORD; var inStream: ISequentialInStream ): Integer; stdcall;
    function SetOperationResult( operationResult: Integer ): Integer; stdcall;
// Shadow 29.11.2006
    function CryptoGetTextPassword2( passwordIsDefined: PInteger; var Password: PWideChar ): Integer; stdcall;
    function SetTotal( total: Int64 ): Integer; stdcall;
    function SetCompleted( const completeValue: PInt64 ): Integer; stdcall;
  end;

  TMyArchiveExtractCallback = class( TInterfacedObject, IArchiveExtractCallback, ICryptoGetTextPassword )
    FSevenzip: TSevenzip;
    FExtractDirectory: Widestring;
    FProgressFile: Widestring;
    FProgressFilePos: int64;
    FProgressFileSize: int64;
    FLastPos: int64;
    FFilestoextract: int64;
    FLastFileToExt: Boolean;
    FAllFilesExt: Boolean;
    FPassword: WideString;
    constructor Create( Owner: TSevenZip );
    function GetStream( index: DWORD; out outStream: ISequentialOutStream; askExtractMode: DWORD ): Integer; stdcall;
    // GetStream OUT: S_OK - OK, S_FALSE - skeep this file
    function PrepareOperation( askExtractMode: Integer ): Integer; stdcall;
    function SetOperationResult( resultEOperationResult: Integer ): Integer; stdcall;
    function SetTotal( total: Int64 ): Integer; stdcall;
    function SetCompleted( const completeValue: PInt64 ): Integer; stdcall;
// Shadow 29.11.2006
    function CryptoGetTextPassword( var Password: PWideChar ): Integer; stdcall;
  end;


  TMyArchiveOpenCallback = class( TInterfacedObject, IArchiveOpenCallback, ICryptoGetTextPassword )
    FSevenzip: TSevenzip;
    FPassword: WideString;
    constructor Create( Owner: TSevenZip );
    function SetTotal( const files: Int64; const bytes: Int64 ): Integer; stdcall;
    function SetCompleted( const files: Int64; const bytes: Int64 ): Integer; stdcall;
    function CryptoGetTextPassword( var Password: PWideChar ): Integer; stdcall;
  end;

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//--------------END SevenZip Interface--------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------




//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//--------------Start SevenZip VCL -------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

//type
  T7zListfileEvent = procedure( Sender: TObject; Filename: Widestring; Fileindex,FileSizeU,FileSizeP,Fileattr,Filecrc:int64;Filemethod:Widestring ;FileTime:double ) of object;
  T7zExtractfileEvent = procedure( Sender: TObject; Filename: Widestring; Filesize:int64 ) of object;
  T7zAddFileEvent = procedure( Sender: TObject; Filename: Widestring; Filesize:int64 ) of object;
  T7zPreProgressEvent = procedure( Sender: TObject; MaxProgress: int64 ) of object;
  T7zProgressEvent = procedure( Sender: TObject; Filename: Widestring; FilePosArc,FilePosFile: int64 ) of object;
  T7zMessageEvent = procedure( Sender: TObject; ErrCode: Integer; Message: string;Filename:Widestring )  of object;
//  T7zCRC32ErrorEvent = procedure( Sender: TObject; ForFile: string;  FoundCRC, ExpectedCRC: LongWord; var DoExtract: Boolean ) of object;
//  TC7zommentEvent = procedure( Sender: TObject;Comment: string; ) of object;

// GDG 21.02.07 : added FileIndex to this event in case we're managing a list of files.
  T7zSetNewNameEvent = procedure( Sender: TObject; FileIndex: DWORD; var OldFileName: WideString ) of object;

  T7zExtractOverwrite = procedure( Sender: TObject; FileName: WideString; var DoOverwrite: Boolean ) of object;

//type
  TSevenZip = class( TComponent )       // Twincontrol   TComponent
  private
    FErrCode: Integer;
    FLastError:Integer;                                        //FHO 22.01.2007
    FHandle: HWND;
//    FMessage: Widestring; // Not used now ErikGG 08.11.06
    FExtrBaseDir: Widestring;
    FSevenZipFileName: Widestring;

    FComment: Widestring;
    FRootDir: Widestring;

    Ffiles: TWideStringList_;

    { Event variables }
    FOnProgress: T7zProgressEvent;
    FOnPreProgress: T7zPreProgressEvent;
    FOnMessage: T7zMessageEvent;
    FOnlistfile: T7zlistfileEvent;
    FOnextractfile: T7zextractfileEvent;
    FOnaddfile: T7zaddfileEvent;
    FOnSetAddName: T7zSetNewNameEvent;
    FOnSetExtractName: T7zSetNewNameEvent;
    FOnExtractOverwite: T7zExtractOverwrite;

    FAddOptions: Addopts;
    FExtractOptions: Extractopts;
    FNumberOfFiles: Integer;
    FIsSFX: Boolean;
    FSFXOffset: Int64;
    FSFXCreate: Boolean;
    FSFXModule: Widestring;
    FCompresstype: TCompresstype;
    FCompstrength: TCompressStrength;
    FLZMAStrength: TLZMAStrength;
    FPPMDSize: TPPMDSize;
    FPPMDMem: TPPMDMem;
    FMainCancel: Boolean;

// Shadow 28.11.2006
{$IFDEF UseRes7zdll}
    mp_MemoryModule: PBTMemoryModule;
    mp_DllData: Pointer;
    m_DllDataSize: Integer;
{$ELSE}                                                        //FHO 25.01.2007
    F7zaLibh: THandle;
{$ENDIF}

//{$IFDEF DynaLoadDLL}
    FCreateObject: TCreateObjectFunc;
//{$ENDIF}

    FVolumeSize: Integer;
    FOnOpenVolume: TOpenVolume;
    FPassword: WideString;
    FNamesOfVolumesWritten: TWideStringArray;                  //FHO 17.01.2007

    { Private "helper" functions }

//    procedure LogMessage( var msg: TMessage ); message 9999;
    procedure ResetCancel;
    function AppendSlash( sDir: widestring ): widestring;
    procedure SetVolumeSize( const Value: Integer );
    procedure SetSFXCreate( const Value: Boolean );
    function InternalGetIndexByFilename( FileToExtract:Widestring ): Integer;		//ZSA 21.02.2007
    procedure ClearNamesOfVolumeWritten;
    procedure SetLastError(const Value: Integer);                       //FHO 17.01.2007
  protected
    inA: IInArchive;
    outA: IOutArchive;
    sp: ISetProperties;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    { Public Properties ( run-time only ) }
    property Handle: HWND read fHandle write fHandle;
    property ErrCode: Integer read fErrCode write fErrCode;
    property LastError:Integer read FLastError write SetLastError;// FLastError;//FHO 22.01.2007
    property IsSFX: Boolean read FIsSFX write FIsSFX;
    property SFXOffset: int64 read FSFXOffset write FSFXOffset;

    property SevenZipComment: Widestring read Fcomment write FComment;
    property Files: TWideStringList_ read Ffiles write ffiles;
    property NamesOfVolumesWritten: TWideStringArray read FNamesOfVolumesWritten;  //FHO 17.01.2007

    { Public Methods }
    function Add: Integer;
    function Extract( TestArchive:Boolean=False ): Integer;
    function List: Integer;
    procedure Cancel;
    function GetIndexByFilename( FileToExtract:Widestring ): Integer;
    function SFXCheck( Fn:Widestring ): Boolean;
    function ConvertSFXto7z( Fn:Widestring ): boolean;
    function Convert7ztoSFX( Fn:Widestring ): boolean;
  published
    { Public properties that also show on Object Inspector }
    property AddRootDir: Widestring read FRootDir write FRootDir;
    property SFXCreate: Boolean read FSFXCreate write SetSFXCreate;
    property SFXModule: Widestring read FSFXModule write FSFXModule;
    property AddOptions: AddOpts read FAddOptions write FAddOptions;
    property ExtractOptions: ExtractOpts read FExtractOptions write FExtractOptions;
    property ExtrBaseDir: Widestring read FExtrBaseDir write FExtrBaseDir;
    property LZMACompressType: TCompresstype read FCompresstype write FCompresstype;
    property LZMACompressStrength: TCompressStrength read FCompstrength write FCompstrength;
    property LZMAStrength: TLZMAStrength read FLZMAStrength write FLZMAstrength;
    property LPPMDmem: TPPMDmem read FPPMDmem write FPPMDmem;
    property LPPMDsize: TPPMDsize read FPPMDsize write FPPMDsize;
    property SZFileName: Widestring read FSevenZipFileName write FSevenZipFilename;
    property NumberOfFiles: Integer read FNumberOfFiles write FNumberOfFiles;
// Shadow 29.11.2006
    property VolumeSize: Integer read FVolumeSize write SetVolumeSize;
    property Password: WideString read FPassword write FPassword;
    { Events }

    property OnListfile: T7zlistfileEvent read FOnlistfile write FOnlistfile;
    property OnAddfile: T7zaddfileEvent read FOnaddfile write FOnaddfile;
    property OnExtractfile: T7zextractfileEvent read FOnextractfile write FOnextractfile;
    property OnProgress: T7zProgressEvent read FOnProgress  write FOnProgress;
    property OnPreProgress: T7zPreProgressEvent read FOnPreProgress  write FOnPreProgress;
    property OnMessage: T7zMessageEvent read fOnMessage write fOnMessage;
    property OnSetAddName: T7zSetNewNameEvent read FOnSetAddName write FOnSetAddName;
    property OnSetExtractName: T7zSetNewNameEvent read FOnSetExtractName write FOnSetExtractName;
    property OnExtractOverwrite: T7zExtractOverwrite read FOnExtractOverwite write FOnExtractOverwite;
    property OnOpenVolume: TOpenVolume read FOnOpenVolume write FOnOpenVolume;
  end;


// jjw 18.10.2006 FCreateobject - function CreateObject( const clsid: PGUID; const iid: PGUID; out _out ): Integer; stdcall; external '7za.dll';
//{$IFNDEF DynaLoadDLL}
//function CreateObject( const clsid: PGUID; const iid: PGUID; out _out ): Integer; stdcall; external '7za.dll'
//{$ENDIF}

{$IFDEF UseLog}
function PropTypeToString( propType: Integer ): string;
function PropIDToString( propID: Integer ): string;
procedure Log( sz: string );
{$ENDIF}
function FileTimeToDateTime( const rFileTime: TFileTime; const Localize: Integer = 0 ): TDateTime;
procedure SortDWord( var A: array of DWord; iLo, iHi: DWord );
function DriveIsRemovable( Drive: WideString ): Boolean;
function TryStrToInt_( const S: string; out Value: Integer ): Boolean;

//Unicode procedures
function UppercaseW_( s:WideString ):Widestring;
function GetFileSizeandDateTime_Int64( fn: Widestring; var fs:int64; var ft:Tfiletime; var fa:Integer ): int64;
function FileExists_( fn: Widestring ): Boolean;
function createfile_(lpFileName:Pwidechar; Access:Cardinal; Share:Cardinal;SecAttr:PSecurityattributes;
                     CreationDisposition:Cardinal;Flags:Cardinal;Temp:Cardinal) : integer;

{$IFDEF RegisterInThisUnit}
procedure Register;
{$ENDIF}

var FMainhandle: HWND; //for debug messages
var isUnicode : Boolean;

implementation

uses
  Forms, CommDlg;

//--------------------------------------------------------------------------------------------------
//-------------------Start UniCode procedures-------------------------------------------------------
//--------------------------------------------------------------------------------------------------

function isEqualW( s1, s2: WideString ): Boolean;
var
  i: Integer;
begin
  Result := FALSE;
  if Length( s1 ) <> Length( s2 ) then Exit;
  for i := 1 to Length( s1 ) do if WideChar( s1[ i ] ) <> WideChar( s2[ i ] ) then Exit;
  Result := TRUE;
end;

function FileExists_( fn: Widestring ): Boolean;
var
 fs:int64;
 ft:Tfiletime;
 fa:Integer;
begin
 if isUnicode then
   Result := ( GetFileSizeandDateTime_Int64( fn,fs,ft,fa ) > -1 )
  else
   Result := fileexists(string(fn));
end;

function PrevDir( Path: WideString ): WideString;
var
  l: Integer;
begin
  l := Length( Path );
  if ( l > 0 ) and ( Path[ l ] = '\' ) then Dec( l );
  while Path[ l ] <> '\' do Dec( l );
  Result := Copy( Path, 1, l );
end;

function ClearSlash( Path: WideString ): WideString;
var
  l: Integer;
begin
  l := Length( Path );
  if Path[ l ] = '\' then Dec( l );
  Result := Copy( Path, 1, l );
end;

function DirectoryExistsW( const Directory: WideString ): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributesW( PWideChar( Directory ) );
  Result := ( Code <> -1 ) and ( FILE_ATTRIBUTE_DIRECTORY and Code <> 0 );
end;

//START function from TNTControls http://www.tntware.com/
function StrScanWide( const Str: PWideChar; Chr: WideChar ): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc( Result );
  end;
end;

function LastDelimiterW( const Delimiters, S: WideString ): Integer;
var
  P: PWideChar;
begin
  Result := Length( S );
  P := PWideChar( Delimiters );
  while Result > 0 do
  begin
    if ( S[ Result ] <> #0 ) and ( StrScanWide( P, S[ Result ] ) <> nil ) then
      Exit;
    Dec( Result );
  end;
end;

function ChangeFileExtW( const FileName, Extension: WideString ): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW( '.\:',Filename );
  if ( I = 0 ) or ( FileName[ I ] <> '.' ) then I := MaxInt;
  Result := Copy( FileName, 1, I - 1 ) + Extension;
end;

function ExtractFilePathW( const FileName: WideString ): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW( '\:', FileName );
  Result := Copy( FileName, 1, I );
end;

function ExtractFileNameW(const FileName: WideString ): WideString;
var
  I: Integer;
begin
 I := LastDelimiterW( '\:', FileName );
 Result := Copy( FileName, I + 1, MaxInt );
end;

procedure GetfilenameW(var FileName: WideString );
var
  I: Integer;
begin
  if Filename <> '' then
   begin
     i := length(filename);
     while (filename[i] <> '\') and (i > 0) do dec(i);
     if i > 0 then Filename := copy( FileName, I + 1, MaxInt )
   end;
end;

function ExtractFileExtW( const FileName: WideString ): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW( '.\:', FileName );
  if ( I > 0 ) and ( FileName[ I ] = '.' ) then
    Result := Copy( FileName, I, MaxInt ) else
    Result := '';
end;
//END function from TNTControls http://www.tntware.com/

function GetFileSizeandDateTime_Int64( fn: Widestring; var fs:int64; var ft:Tfiletime; var fa:Integer ): int64;
var
  FindDataW: _Win32_Find_Dataw;
  FindDataA: _Win32_Find_DataA;
  SearchHandle: THandle;
begin
  //Result := 0;

  if isUnicode then
   SearchHandle := FindFirstFilew( PWideChar( fn ), FindDataW )
  else
   SearchHandle := FindFirstFilea( PAnsiChar( Ansistring( fn ) ), FindDataA );

  if SearchHandle = INVALID_HANDLE_VALUE then
   begin
    Result := -1;
    fs := -1;
    fa := -1;
    ft.dwLowDateTime := 0;
    ft.dwHighDateTime := 0;
    exit;
   end;

  if isUnicode then
   begin
     LARGE_Integer( Result ).LowPart := FindDataW.nFileSizeLow;
     LARGE_Integer( Result ).HighPart := FindDataW.nFileSizeHigh;

     LARGE_Integer( fs ).LowPart := FindDataW.nFileSizeLow;
     LARGE_Integer( fs ).HighPart := FindDataW.nFileSizeHigh;

     ft.dwLowDateTime  := FinddataW.ftLastWriteTime.dwLowDateTime;
     ft.dwHighDateTime := FinddataW.ftLastWriteTime.dwHighDateTime;
     fa := FinddataW.dwFileAttributes;
   end
  else
   begin
     LARGE_Integer( Result ).LowPart := FindDataA.nFileSizeLow;
     LARGE_Integer( Result ).HighPart := FindDataA.nFileSizeHigh;

     LARGE_Integer( fs ).LowPart := FindDataA.nFileSizeLow;
     LARGE_Integer( fs ).HighPart := FindDataA.nFileSizeHigh;

     ft.dwLowDateTime  := FindDataA.ftLastWriteTime.dwLowDateTime;
     ft.dwHighDateTime := FindDataA.ftLastWriteTime.dwHighDateTime;
     fa := FindDataA.dwFileAttributes;
   end;

  Windows.FindClose( SearchHandle );
end;

function ForceDirectoriesW( Path: WideString; Attr: Word ): Boolean;
var
  E: EInOutError;
begin
  Result := TRUE;

  if Path = '' then begin
//    E := EInOutError.Create( 'Unable to create directory' );
//    E.ErrorCode := 3;
//    raise E;
   // DM 2008-03-20
   // do not raise an exception if path is empty - there is no directory to create
   exit;
  end;

  Path := ClearSlash( Path );
  if DirectoryExistsW( Path ) then Exit;

  if ( Length( Path ) < 3 ) or DirectoryExistsw( Path )
    or ( ExtractFilePath( Path ) = Path ) then Exit; // avoid 'xyz:\' problem.

  Result := ForceDirectoriesW( PrevDir( Path ), 0 ) and CreateDirectoryW( PWideChar( Path ), nil );
  if Result and ( Attr > 0 ) then SetFileAttributesW( PWideChar( Path ), Attr );
end;

function UppercaseW_( s:WideString ):Widestring;
begin
  Result := S;
  if Length( Result ) > 0 then
    CharUpperBuffW( PWideChar( Result ), Length( Result ) );
end;

//--------------------------------------------------------------------------------------------------
//-------------------End UniCode procedures---------------------------------------------------------
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//-------------------Start Twidestringlist_-----------------------------------------------------------
//--------------------------------------------------------------------------------------------------

procedure TWideStringList_.AddString( s: WideString );
var i:Longword;
begin
 i := length( WStrings );
 Setlength( WStrings,i+1 );
 WStrings[ i ] := s;
 Count := i+1;
end;

procedure TWideStringList_.RemoveString( s: WideString );
var
  i: LongWord;
  f: Boolean;
begin
  f := FALSE;
  s := UpperCase( s );
  for i := Low( WStrings ) to High( WStrings ) do begin
    if isEqualW( UppercaseW_( WStrings[ i ] ), s ) then begin
      f := TRUE;
      Break;
    end;
  end;
  if f then begin
    WStrings[ i ] := WStrings[ High( WStrings ) ];
    WStrings[ High( WStrings ) ] := '';
    SetLength( WStrings, Length( WStrings ) - 1 );
    Dec( Count );
  end;
end;


Procedure TWideStringList_.Clear;
begin
 Setlength( WStrings,0 );
 Count := 0;
end;

Constructor TWideStringList_.Create;
begin
 clear;
end;

//--------------------------------------------------------------------------------------------------
//-------------------END Twidestringlist_-------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//  Start common functions
//------------------------------------------------------------------------------------------------

function createfile_(lpFileName:Pwidechar; Access:Cardinal; Share:Cardinal;SecAttr:PSecurityattributes;
                     CreationDisposition:Cardinal;Flags:Cardinal;Temp:Cardinal) : integer;
begin
if isUnicode then
 Result := createfilew(lpFilename,access,share,SecAttr,Creationdisposition,flags,temp)
else
 Result := createfilea(PAnsichar( AnsiString(lpFilename)),access,share,SecAttr,Creationdisposition,flags,temp)
end;

//some Delphi veriosn do not take the Int64 overload
function FileSeek(Handle: Integer; const Offset: Int64; Origin: Integer): Int64;
begin
  Result := Offset;
  Int64Rec(Result).Lo := SetFilePointer(THandle(Handle), Int64Rec(Result).Lo,@Int64Rec(Result).Hi, Origin);
end;

function TSevenZip.AppendSlash( sDir: widestring ): widestring;
begin
  if ( sDir <> '' ) and ( sDir[ Length( sDir ) ] <> '\' ) then
    Result := sDir + '\'
  else
    Result := sDir;
end;

procedure TSevenZip.SetVolumeSize( const Value: Integer );
begin
// Shadow 27.11.2006
  if not FSFXCreate then
    FVolumeSize := Value
  else begin
    if ( Value > 0 ) and ( Value < FSFXOffset ) then
      FVolumeSize := FSFXOffset + 7
    else FVolumeSize := Value;
  end;
end;

procedure TSevenZip.SetSFXCreate( const Value: Boolean );

  function FileSizeW( fn: WideString ): DWORD;
  var
    f: Integer;
  begin
    Result := 0;
    f := CreateFile_( PwideChar( fn ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    if dword(f)=INVALID_HANDLE_VALUE then Exit;               //FHO  20.01.2007
    try
      Result := FileSeek( f, int64(0), soFromEnd );
    finally
      FileClose( f );
    end;
  end;
var
  s: Int64;
begin
// Shadow 27.11.2006
  FSFXCreate := FALSE;
  if Value then begin
    s := FileSizeW( FSFXModule );
    if ( s > 0 ) then begin // FileExists
      if ( ( FVolumeSize > 0 ) and ( FVolumeSize < s + 7 ) ) then FVolumeSize := s + 7;
      FSFXOffset := s;
      FSFXCreate := TRUE;
    end;
  end;
end;

function FileTimeToDateTime( const rFileTime: TFileTime; const Localize: Integer = 0 ): TDateTime;
var
  dOffset: Double;
  rWork: TFileTime;
begin
  // offset to or from local time
  if Localize > 0 then
    FileTimeToLocalFileTime( rFileTime, rWork )
  else if Localize < 0 then
    LocalFileTimeToFileTime( rFileTime, rWork )
  else begin
    rWork := rFileTime;
  end;

  dOffset := 0.0000001 * ( ( Int64( rWork.dwHighDateTime ) shl 32 ) or rWork.dwLowDateTime );
  dOffset := dOffset / ( 60 * 60 * 24 );
  Result := EncodeDate( 1601, 1, 1 ) + dOffset;
end;

procedure SortDWord( var A: array of DWord; iLo, iHi: DWord );
var
  Lo, Hi, Mid, T: DWord;
begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[ ( Lo + Hi ) div 2 ];
    repeat
      while A[ Lo ] < Mid do Inc( Lo );
      while A[ Hi ] > Mid do Dec( Hi );
      if Lo <= Hi then
      begin
        T := A[ Lo ];
        A[ Lo ] := A[ Hi ];
        A[ Hi ] := T;
        Inc( Lo );
        if Hi > 0 then Dec( Hi ); //Using DWord and not Integers
      end;
    until Lo > Hi;
    if Hi > iLo then SortDWord( A, iLo, Hi );
    if Lo < iHi then SortDWord( A, Lo, iHi );
end;

function DriveIsRemovable( Drive: WideString ): Boolean;
var
  DT: Cardinal;
begin
  DT := GetDriveTypeW( PWideChar( Drive ) );
  Result := ( DT <> DRIVE_FIXED );
end;

function TryStrToInt_( const S: string; out Value: Integer ): Boolean;
var
   E: Integer;
begin
   Val( S, Value, E );
   Result := ( E = 0 );
end;


//------------------------------------------------------------------------------------------------
//  End common functions
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------
//-------------------Start SevenZip Interface -----------------------------------------------
//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

function TInterfacedObject.QueryInterface( const IID: TGUID; out Obj ): HResult;
const
  E_NOINTERFACE = HResult( $80004002 );
begin
  if GetInterface( IID, Obj ) then
  begin
    Result := 0;
{$IFDEF UseLog}
    Log( 'INTERFACEOK:' + ClassName + ' ' + GUIDToString( IID ) );
{$ENDIF}
  end else
  begin
    Result := E_NOINTERFACE;
{$IFDEF UseLog}
    Log( '  NOINTERFACE: ' + ClassName + ' ' + GUIDToString( IID ) );
{$ENDIF}
  end;
end;

function TInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement( FRefCount );
end;

function TInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement( FRefCount );
  if Result = 0 then
    Destroy;
end;

procedure TInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement( FRefCount );
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  //if RefCount <> 0 then Error( reInvalidPtr );
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedObject( Result ).FRefCount := 1;
end;

constructor TMyArchiveUpdateCallback.Create( Owner: TSevenZip );
begin
  inherited Create;
  FSevenzip := Owner;
// Shadow 29.11.2006
  if Assigned( FSevenzip ) then
    FPassword := FSevenzip.Password
  else FPassword := '';
end;

function TMyArchiveUpdateCallback.GetUpdateItemInfo( index: DWORD;
  newData: PInteger; // 1 - new data, 0 - old data
  newProperties: PInteger; // 1 - new properties, 0 - old properties
  indexInArchive: PDWORD // -1 if there is no in archive, or if doesn't matter
  ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveUpdateCallback.GetUpdateItemInfo( %d )', [ index ] ) );
{$ENDIF}
  if newData <> nil then newData^ := 1;
  if newProperties <> nil then newProperties^ := 1;
  if indexInArchive <> nil then indexInArchive^ := DWORD( -1 );
  Result := S_OK;
end;

function TMyArchiveUpdateCallback.CryptoGetTextPassword2( passwordIsDefined: PInteger; var Password: PWideChar ): Integer;
begin
  if Length( FPassword ) > 0 then begin
    passwordIsDefined^ := Integer( Bool( TRUE ) );
    Password := SysAllocString( @FPassword[ 1 ] );
    Result := S_OK;
  end else begin
    passwordIsDefined^ := Integer( Bool( FALSE ) );
    Result := S_OK;
  end;
end;

function TMyArchiveUpdateCallback.GetProperty( index: DWORD; propID: PROPID; value: PPROPVARIANT ): Integer; stdcall;
var
  sz: WideString;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveUpdateCallback.GetProperty( %d, %s ( %d ), %.8x )', [ index, PropIDToString( propID ), propID, Integer( value ) ] ) );
{$ENDIF}
  Result := S_OK;
  case propID of
    //kpidPath ( 3 ) VT_BSTR ( 8 )
    kpidPath:
    begin
      value^.vt := VT_BSTR;

//get relative path if wanted
      sz := Files[ index ];
      if rootdir <> '' then
      begin
        if Uppercasew_( copy( sz,1,length( rootdir ) ) ) = rootdir then
          delete( sz,1,length( rootdir ) );
      end;

//User set filename in archive if wanted
      if assigned( Fsevenzip.OnSetAddName ) then
        Fsevenzip.OnSetAddName( Fsevenzip, Index, sz );

//remove drive / Include drive if wanted
      if sz[ 2 ] = ':' then
        begin
         if char( sz[ 1 ] ) in [ 'A'..'Z','a'..'z' ] then
           if ( AddIncludeDriveLetter in Fsevenzip.FAddOptions ) then //include
            delete( sz,2,1 )
           else
             delete( sz,1,3 );
        end;

//just store filename
      if ( AddStoreOnlyFilename in Fsevenzip.FAddOptions ) then
       GetfilenameW( sz );

//rg 07.11.2006 StringToOleStr( )
      value^.bstrVal := Pwidechar( sz );
    end;
    //kpidAttributes ( 9 ) VT_UI4 ( 19 )
    kpidAttributes:
    begin
      value^.vt := VT_UI4;
      value^.ulVal := Files_Attr[ index ];//filegetattr( files[ index ] );
    end;
    kpidCreationTime:
    begin
      value^.vt := VT_FILETIME;

      value^.filetime.dwLowDateTime := 0;
      value^.filetime.dwHighDateTime := 0;
    end;
    kpidLastAccessTime:
    begin
      value^.vt := VT_FILETIME;
      value^.filetime.dwLowDateTime := 0;
      value^.filetime.dwHighDateTime := 0;
    end;
    //kpidLastWriteTime ( 12 ) VT_FILETIME ( 64 )
    kpidLastWriteTime:
    begin
      value^.vt := VT_FILETIME;
      value^.filetime.dwLowDateTime := Files_Date[ index ].dwLowDateTime;;
      value^.filetime.dwHighDateTime := Files_Date[ index ].dwHighDateTime;
    end;
    kpidIsFolder:
    begin
      value^.vt := VT_BOOL;
      value^.boolVal := ( Files_Attr[ index ] and faDirectory ) <> 0; //false
    end;
    kpidIsAnti:
    begin
      value^.vt := VT_BOOL;
      value^.boolVal := False;
    end;
    //kpidSize ( 7 ) VT_UI8 ( 21 )
    kpidSize:
    begin
      value^.vt := VT_UI8;
      value^.uhVal.QuadPart := Files_size[ index ];
    end;
  else
{$IFDEF UseLog}
    Log( 'Asking for unknown property' );
{$ENDIF}
    Result := S_FALSE;
  end;
end;

function TMyArchiveUpdateCallback.GetStream( index: DWORD; var inStream: ISequentialInStream ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( 'TMyArchiveUpdateCallback.GetStream' );
{$ENDIF}
  Fprogressfile := files[ index ];
  Fprogressfilesize := files_size[ index ];
  Fprogressfilepos := 0;
  inStream := TMyStreamReader.Create( FSevenZip, Files[ index ], FALSE );
  Result := S_OK;
end;

function TMyArchiveUpdateCallback.SetOperationResult( operationResult: Integer ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveUpdateCallback.SetOperationResult( %d )', [ operationResult ] ) );
{$ENDIF}
  Result := S_OK;
end;

function TMyArchiveUpdateCallback.SetTotal( total: Int64 ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveUpdateCallback.SetTotal( %d )', [ total ] ) );
{$ENDIF}
  Result := S_OK;
end;

function TMyArchiveUpdateCallback.SetCompleted( const completeValue: PInt64 ): Integer; stdcall;
begin
/// Progressfile - Newfile
/// Do it here because it works with Multithreaded 7za interaction.
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveUpdateCallback.SetCompleted( %d )', [ completeValue^ ] ) );
{$ENDIF}

//fileprogress
   if ( FProgressFilePos = 0 ) then
      if assigned( Fsevenzip.OnAddFile ) then Fsevenzip.onAddFile( Fsevenzip,FProgressFile,FProgressFileSize );
   FProgressFilePos := FProgressFilePos + ( completeValue^ - FLastPos );
   FLastPos := completeValue^;

//full and file progress position
   if assigned( Fsevenzip.OnProgress ) then Fsevenzip.OnProgress( Fsevenzip,FProgressFile,completeValue^,FProgressFilePos );

  Result := S_OK;
//rg 24.06
//User cancel operation
  if FSevenzip.FMainCancel then
   begin
     FSevenZip.ErrCode:=FUsercancel;                           //FHO 21.01.2007
     if assigned( Fsevenzip.onMessage ) then
       Fsevenzip.OnMessage( Fsevenzip,FUsercancel,c7zipResMsg[FUsercancel], FProgressFile );  //FHO 21.01.2007
     Result := S_FALSE;
   end;
end;


constructor TMyArchiveExtractCallback.Create( Owner: TSevenZip );
begin
  inherited Create;
  FSevenzip := Owner;
// Shadow 29.11.2006
  if Assigned( FSevenzip ) then
    FPassword := FSevenzip.Password
  else FPassword := '';
end;

function TMyArchiveExtractCallback.GetStream( index: DWORD;
  out outStream: ISequentialOutStream; askExtractMode: DWORD ): Integer; stdcall;
var
 path: Propvariant;
 size: Propvariant;
 date: Propvariant;
 attr: Propvariant;
   sz, origName: Widestring;
   fe,DoOverwrite: boolean;
//   fHnd: Integer;
  MyLastError:Integer;                                           //FHO 22.01.2007
begin
{$IFDEF UseLog}
  Log( Format( '__TMyArchiveExtractCallback.GetStream( %d, %.8x, %d )', [ index, Integer( outStream ), askExtractMode ] ) );
{$ENDIF}
  DoOverwrite := ExtractOverwrite in FsevenZip.FExtractOptions;
  path.vt := VT_EMPTY;
  size.vt := VT_EMPTY;
  date.vt := VT_EMPTY;
  attr.vt := VT_EMPTY;

//Cancel Operation
  if self.FSevenzip.FMainCancel then
   begin
    outStream := nil;
    result := S_FALSE;
    exit;
   end;

  Case askExtractMode of
    kExtract:  begin

                 FSevenzip.inA.GetProperty( index, kpidPath, path );
                 FSevenzip.inA.GetProperty( index, kpidSize, size );
                 FSevenzip.inA.GetProperty( index, kpidattributes, attr );
                 FSevenzip.inA.GetProperty( index, kpidLastWriteTime, date );

//rg 23.8.06
                 if ExtractNoPath in FSevenzip.FExtractOptions then
                   sz := FExtractDirectory + extractfilenameW( path.bstrVal )
                  else
                   sz := FExtractDirectory + path.bstrVal;

                 origName := sz;

                 if assigned( Fsevenzip.OnSetExtractName ) then
                   Fsevenzip.OnSetExtractName( Fsevenzip,index, sz );


                 if not DoOverwrite then
                  if FileExists_( sz ) then
                   begin
                     if assigned( Fsevenzip.OnExtractOverwrite ) then
                         Fsevenzip.OnExtractOverwrite( Fsevenzip, sz, DoOverwrite );

                     if not DoOverwrite then
                      begin
                       Result := S_OK;
                       outStream := nil;
                       exit;
                      end;
                     end;

                    FProgressFile := sz;
                    FProgressFilePos := 0;
                    FprogressFileSize := size.uhVal.QuadPart;

                 if ( attr.uiVal and ( 1 shl 4 ) ) <> 0 then
                  begin
                   if isUnicode then
                     ForceDirectoriesW( sz, attr.uiVal )
                    else
                     ForceDirectories(String(sz));
                  end
                 else
                  begin
                    FFilestoextract := FFilestoextract - 1;
                    if FFilestoextract = 0 then FLastFileToExt := true;
                    outStream := nil;
                    fe := FileExists_( sz );

                    if ( not fe ) or ( fe and DoOverwrite ) then begin
                      if isUnicode then
                        ForceDirectoriesW( ExtractFilePathW( sz ), attr.uiVal )
                       else
                        ForceDirectories(extractfilepath( String( sz ) ) );
                    try
                      outStream := TMyStreamWriter.Create(@MyLastError ,sz,
                                                               //FHO 22.01.2007
                                     FileTimeToDateTime( date.filetime, 2 ), attr.lVal );
                    except
                      outStream := nil;
                      Result := S_FALSE;
                      FSevenzip.LastError:=MyLastError;         //FHO 22.01.2007
                      FSevenzip.ErrCode:=FNoFileCreated;
                      if assigned( FsevenZip.onmessage ) then
                        FsevenZip.onmessage( FsevenZip, FNoFileCreated, c7zipResMsg[FNoFileCreated],origName);
                      Exit;
// did not work here need another place !
// if assigned( FsevenZip.onmessage ) then FsevenZip.onmessage( FsevenZip, 2, 'Could not create file', origName );
                    end;
                  end;
              end;
             end;
    ktest   : begin
                 FSevenzip.inA.GetProperty( index, kpidPath, path );
                 FSevenzip.inA.GetProperty( index, kpidSize, size );
                 FProgressFile := path.bstrVal;
                 FProgressFilePos := 0;
                 FprogressFileSize := size.uhVal.QuadPart ;
               end;
    kskip   : begin
               end;
  end;
  Result := S_OK;
end;
// GetStream OUT: S_OK - OK, S_FALSE - skeep this file

function TMyArchiveExtractCallback.PrepareOperation( askExtractMode: Integer ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveExtractCallback.PrepareOperation( %d )', [ askExtractMode ] ) );
{$ENDIF}
  Result := S_OK;
end;

function TMyArchiveExtractCallback.SetOperationResult( resultEOperationResult: Integer ): Integer; stdcall;
begin
  Result := S_OK;
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveExtractCallback.SetOperationResult( %d )', [ resultEOperationResult ] ) );
{$ENDIF}
  case resultEOperationResult of
    kOK               : FSevenzip.ErrCode:=FNoError;
    kUnSupportedMethod: begin                                  //FHO 21.01.2007
                          FSevenzip.ErrCode:=FUnsupportedMethod;
                          if assigned( Fsevenzip.onmessage ) then
                            Fsevenzip.onmessage( Fsevenzip, FUnsupportedMethod, c7zipResMsg[FUnsupportedMethod], FProgressFile );
                        end;
    kDataError        : begin                                  //FHO 21.01.2007
                          FSevenzip.ErrCode:=FDataError;
                          if assigned( Fsevenzip.onmessage ) then
                            Fsevenzip.onmessage( Fsevenzip, FDataError, c7zipResMsg[FDataError], FProgressFile );
                        end;
    kCRCError         : begin                                  //FHO 21.01.2007
                          FSevenzip.ErrCode:=FCRCError;
                          if assigned( Fsevenzip.onmessage ) then
                          Fsevenzip.onmessage( Fsevenzip, FCRCError, c7zipResMsg[FCRCError], FProgressFile );
                        end;
  end;

  if FLastFileToExt then FAllFilesExt := true; //no more files to extract, we can stop
end;

function TMyArchiveExtractCallback.SetTotal( total: Int64 ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyArchiveExtractCallback.SetTotal( %d )', [ total ] ) );
{$ENDIF}

//all filesizes also skipped ones
  if FFilestoextract = 0 then // we extract all files, so we set FMaxProgress here
     if assigned( FSevenzip.OnPreProgress ) then FSevenzip.OnPreProgress( FSevenzip,total );
  Result := S_OK;
end;

function TMyArchiveExtractCallback.SetCompleted( const completeValue: PInt64 ): Integer; stdcall;
begin

   if ( FProgressFilePos = 0 ) then
      if assigned( Fsevenzip.OnExtractFile ) then Fsevenzip.onExtractfile( Fsevenzip,FProgressFile,FProgressFileSize );

   FProgressFilePos := FProgressFilePos + ( completeValue^ - FLastPos );
   FLastPos := completeValue^;

//full and file progress position
   if assigned( Fsevenzip.OnProgress ) then Fsevenzip.OnProgress( Fsevenzip,FProgressFile,completeValue^,FProgressFilePos );

{$IFDEF UseLog}
  Log( Format( 'TMyArchiveExtractCallback.SetCompleted( %d )', [ completeValue^ ] ) );
{$ENDIF}
  Result := S_OK;

  //have all files extracted. Could stop
  //User cancel operation
  if self.FAllFilesExt then Result := S_FALSE;

  if  Fsevenzip.FMainCancel then begin
     Result := S_FALSE;
     FSevenzip.ErrCode:=FUsercancel;                           //FHO 21.01.2007
     if assigned( Fsevenzip.onMessage ) then
       Fsevenzip.OnMessage( Fsevenzip, FUsercancel, c7zipResMsg[FUsercancel], FProgressFile );
   end;
end;


function TMyArchiveExtractCallback.CryptoGetTextPassword( var Password: PWideChar ): Integer;
begin
  if Length( FPassword ) > 0 then begin
    Password := SysAllocString( @FPassword[ 1 ] );
    Result := S_OK;
  end else Result := S_FALSE;
end;


{============ TMyOpenarchiveCallbackReader =================================================}


function TMyArchiveOpenCallback.CryptoGetTextPassword( var Password: PWideChar ): Integer;
begin
  if Length( FPassword ) > 0 then begin
    Password := SysAllocString( @FPassword[ 1 ] );
    Result := S_OK;
  end else Result := S_FALSE;
end;

constructor TMyArchiveOpenCallback.Create( Owner: TSevenZip );
begin
  inherited Create;
  FSevenzip := Owner;
// Shadow 29.11.2006
  if Assigned( FSevenzip ) then
    FPassword := FSevenzip.Password
  else FPassword := '';
end;

function TMyArchiveOpenCallback.SetTotal( const files: Int64; const bytes: Int64 ): Integer; stdcall;
begin
//
Result := S_OK; //LifePower 07.01.2007
end;

function TMyArchiveOpenCallback.SetCompleted( const files: Int64; const bytes: Int64 ): Integer; stdcall;
begin
//
Result := S_OK;
end;

{============ TMyStreamReader =================================================}

function TMyStreamReader.Seek( Offset: Int64; SeekOrigin: DWORD; NewPosition: PInt64 ): Integer; stdcall;
begin
//  frmMain.mmoLog.Lines.Add( '-> Seek ' + IntToStr( offset ) + ' ' + IntToStr( seekOrigin ) );
  Result := S_OK;
  case SeekOrigin of
    soFromBeginning: arcPosition := Offset;
    soFromCurrent: arcPosition := arcPosition + Offset;
    soFromEnd: begin
      if arcSize > 0 then
        arcPosition := arcSize + Offset
      else Result := S_FALSE;
    end;
  end;
  if newPosition <> nil then newPosition^ := arcPosition;
end;

function TMyStreamReader.Read( var Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
var
  fIdx: Integer;
  fPos : Int64;                                                             //RG 26.01.2007
  pSize, Read: DWORD;
  Vsize : Int64;
  Buff: PChar;
begin
  //frmMain.mmoLog.Lines.Add( '-> Read ' + Format( '%.8x', [ Integer( data ) ] ) + ' ' + IntToStr( size ) );
  if FArchive then begin
    if ( Length( Files ) <= 1 ) and ( arcPosition + Size > Files[ 0 ].Size ) then begin
      arcSize := arcPosition + Size;
      if not OpenLastVolume then begin
        Result := S_FALSE;
        Exit;
      end else FMultivolume := TRUE;
    end;
  end;

  if ( not FArchive ) or ( not FMultivolume ) then begin
    FileSeek( Files[ 0 ].Handle, arcPosition, soFromBeginning );
    if not ReadFile( Files[ 0 ].Handle, Data, Size, pSize, nil ) then begin
      MyLastError:=GetLastError;                               //FHO 22.01.2007
      pSize := 0;
    end;
    Inc( arcPosition, pSize );
    if ProcessedSize <> nil then ProcessedSize^ := pSize;
    Result := S_OK;
    Exit;
  end;

  fIdx := -1;
  vSize := 0;
  repeat
    Inc( fIdx );
    if ( Files[ fIdx ].Handle = INVALID_HANDLE_VALUE ) and ( not OpenVolume( fIdx + 1 ) ) then begin  //FHO 20.01.2007
      Result := S_FALSE;
      Exit;
    end;
    vSize := vSize + Files[ fIdx ].Size;
  until arcPosition < vSize;

  Buff := @Data;
  fPos := arcPosition - ( vSize - Files[ fIdx ].Size );
  Read := 0;
  while Read < Size do begin
    if Read > 0 then begin
      with Files[ fIdx - 1 ] do begin
        FileClose( Handle );
        Handle := INVALID_HANDLE_VALUE;                        //FHO 20.01.2007
        Size := 0;
      end;
      if ( Files[ fIdx ].Handle = INVALID_HANDLE_VALUE ) and ( not OpenVolume( fIdx + 1 ) ) then begin                                        //FHO 20.01.2007
        Result := S_FALSE;
        Exit;
      end;
    end;
    FileSeek( Files[ fIdx ].Handle, fPos, soFromBeginning );
    pSize := Size - Read;
    if Files[ fIdx ].Size < fPos + pSize then pSize := Files[ fIdx ].Size - fPos;
    if not ReadFile( Files[ fIdx ].Handle, Buff[ Read ], pSize, pSize, nil ) then begin
      MyLastError:=GetLastError;                               //FHO 22.01.2007
      Read := 0;
      Break;
    end;
    Inc( Read, pSize );
    Inc( fIdx );
    fPos := 0;
  end;
  Inc( arcPosition, Read );
  if Assigned( ProcessedSize ) then ProcessedSize^ := Read;
  if MyLastError=0 then
    Result := S_OK
  else
    Result := S_False;  
end;

function TMyStreamReader.ReadPart( var data; size: DWORD; processedSize: PDWORD ): Integer; stdcall;
begin
  //frmMain.mmoLog.Lines.Add( '-> ReadPart ' + IntToStr( size ) );
  Result := Read( Data, Size, ProcessedSize );
end;

function TMyStreamReader.GetSize( var size: Int64 ): Integer; stdcall;
begin
  //frmMain.mmoLog.Lines.Add( 'GetSize' );
  if arcSize > 0 then begin
    Size := arcSize;
    Result := S_OK;
  end else Result := S_FALSE;
end;

function TMyStreamReader.BrowseForFile( Title: PWideChar; var Name: WideString ): Boolean;
var
  OpenFileName: TOpenFilenameW;
  FileName: array[ 0..MAX_PATH - 1 ] of WideChar;
  s: WideString;
begin
  Result := FALSE;
  try
    s := ExtractFileNameW( Name );
    s := Copy( s, 1, Length( s ) - Length( ExtractFileExtW( Name ) ) );
    s := s + '-volumes'#0 + s + '.*'#0;
    FillChar( FileName, MAX_PATH, 0 );

    FillChar( OpenFileName, SizeOf( OpenFileName ), 0 );

    OpenFileName.lStructSize := SizeOf( OpenFileName );
    OpenFileName.hWndOwner := Application.Handle;

    OpenFileName.lpstrInitialDir := PWideChar( ExtractFilePathW( Name ) );

    OpenFileName.lpstrFile := @FileName;
    OpenFileName.nMaxFile := MAX_PATH;

    OpenFileName.lpstrFilter := @s[ 1 ];
    OpenFileName.nFilterIndex := 1;
    OpenFileName.Flags := OFN_PATHMUSTEXIST Or OFN_FILEMUSTEXIST;

    if GetOpenFileNameW( OpenFileName ) then begin
      Name := FileName;
      Result := ( GetLastError = 0 );
    end else Result := FALSE;
  except
  end;
end;

function TMyStreamReader.OpenVolume( Index: Integer ): Boolean;
var
  i: Integer;
  s: WideString;
  fCancel: Boolean;
begin
  Result := FALSE;

  if Index <= 0 then
    Exit
  else if Index <= Length( Files ) then begin
    if Files[ Index - 1 ].Handle <> INVALID_HANDLE_VALUE then begin //FHO 20.01.2007
      Result := TRUE;
      Exit;
    end;
  end else begin
    i := Length( Files );
    while i < Index do begin
      SetLength( Files, i + 1 );
      Files[ i ].Handle := INVALID_HANDLE_VALUE;               //FHO 20.01.2007
      Files[ i ].Size := 0;
      Inc( i );
    end;
  end;

  Dec( Index );
  if Length( Files[ Index ].Name ) <= 0 then begin
    s := IntToStr( Index + 1 );
    while Length( s ) < 3 do s := '0' + s;
// Shadow 28.11.2006
    if Assigned( FSevenZip ) and FSevenZip.IsSFX then begin
      Files[ Index ].Name := arcName + '.' + s
    end else Files[ Index ].Name := Copy( arcName, 1, Length( arcName ) - Length( ExtractFileExtW( arcName ) ) ) + '.' + s;
  end;

  while Files[ Index ].Handle = INValid_Handle_Value do begin  //FHO 20.01.2007
    Files[ Index ].Handle := CreateFile_( PwideChar( Files[ Index ].Name ),
                                          GENERIC_READ,
                                          FILE_SHARE_READ,
                                          nil,
                                          OPEN_EXISTING, 0, 0 );
    if Files[ Index ].Handle = INVALID_HANDLE_VALUE then begin //FHO 20.01.2007
      if Assigned( FOnOpenVolume ) then begin
        FOnOpenVolume( Files[ Index ].Name, Files[ Index ].OnRemovableDrive, fCancel );
        if not fCancel then Continue;
      end else begin
        if BrowseForFile( 'Select volume', Files[ Index ].Name ) then Continue;
      end;
      Files[ Index ].Name := '';
      Result := FALSE;
      Exit;
    end;
    Files[ Index ].Size := FileSeek( Files[ Index ].Handle, int64(0), soFromEnd );
    FileSeek( Files[ Index ].Handle, int64(0), soFromBeginning );
  end;

  Result := ( Files[ Index ].Size > 0 );
end;

function TMyStreamReader.OpenNextVolume: Boolean;
begin
  Result := OpenVolume( Length( Files ) + 1 );
end;

function TMyStreamReader.OpenLastVolume: Boolean;
var
  Name: WideString;
  n: Integer;


  function GetLastVolumeFN(first:widestring):widestring;
  var n:integer;
      s,e,lastfound:widestring;
  begin
    Result := '';
    s:= ChangeFileExtW( first,'');
    lastfound := first;
    if not TryStrToInt_( Copy( ExtractFileExtW( first ), 2, MaxInt ), n) then exit;
    e:= '00' + inttostr(n);

    repeat
      lastfound := s + '.' + e;
      inc(n);
      e:= inttostr(n);
      while Length( e ) < 3 do e := '0' + e;

    until not fileexists_( s + '.' + e);
    Result := lastfound;
  end;

begin
  Result := FALSE;
  repeat
{
    if Assigned( FOnOpenVolume ) then begin
      Name := ChangeFileExtW( Files[ 0 ].Name, '.*' );
      FOnOpenVolume( Name, Files[ 0 ].OnRemovableDrive, Result );
      if Result then begin
        Result := FALSE;
        Exit;
      end;
    end else begin
      Name := arcName;
      if not BrowseForFile( 'Select last volume', Name ) then Exit;
    end;
}
   name := '';
   name := GetLastVolumeFN(Arcname);
   if name = '' then
    if not BrowseForFile( 'Select last volume', Name ) then Exit;

// Shadow 28.11.2006
    if Assigned( FSevenZip ) and FSevenZip.IsSFX then begin
      if UpperCaseW_( ChangeFileExtW( ExtractFileNameW( Name ), '' ) ) <>
         UpperCaseW_( ExtractFileNameW( Files[ 0 ].Name ) ) then Continue;
    end else begin
      if UpperCaseW_( ChangeFileExtW( ExtractFileNameW( Name ), ExtractFileExtW( Files[ 0 ].Name ) ) ) <>
         UpperCaseW_( ExtractFileNameW( Files[ 0 ].Name ) ) then Continue;
    end;
    if not TryStrToInt_( Copy( ExtractFileExtW( Name ), 2, MaxInt ), n ) then Continue;
  until n > 1;
  Result := OpenVolume( n );
end;

constructor TMyStreamReader.Create( Owner: TSevenZip; sz: Widestring; asArchive: Boolean );
begin
  inherited Create;
  arcName := sz;
  arcPosition := 0;

  FSevenZip := Owner;
  if Assigned( FSevenZip ) then begin
    if Owner.IsSFX then arcPosition := Owner.SFXOffset;
    FOnOpenVolume := FSevenZip.FOnOpenVolume;
  end else FOnOpenVolume := nil;
  FArchive := asArchive;
  FMultivolume := FALSE;

  SetLength( Files, 1 );
  Files[ 0 ].Name := arcName;
  Files[ 0 ].Handle := CreateFile_( PWideChar( Files[ 0 ].Name ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
  Files[ 0 ].Size := FileSeek( Files[ 0 ].Handle, int64(0), soFromEnd );
  Files[ 0 ].OnRemovableDrive := DriveIsRemovable( Copy( ExtractFilePathW( Files[ 0 ].Name ), 1, 2 ) );

  if not FArchive then
    arcSize := Files[ 0 ].Size
  else arcSize := 0;

//  frmMain.mmoLog.Lines.Add( IntToStr( fIn ) );
end;

destructor TMyStreamReader.Destroy;
var
  i: Integer;
begin
  if MyLastError<>ERROR_SUCCESS then
    fSevenZip.LastError:=MyLastError;                          //FHO 22.01.2007
  for i := 0 to Length( Files ) - 1 do if Files[ i ].Handle <> INVALID_HANDLE_VALUE then begin  //FHO 20.01.2007
    FileClose( Files[ i ].Handle );
    Files[ i ].Name:='';                                       //FHO 20.01.2007
  end;
  SetLength( Files, 0 );

{$IFDEF UseLog}
  Log( 'TMyStreamReader.Destroy' );
{$ENDIF}
  inherited;
end;

{============ TMyStreamWriter =================================================}

function TMyStreamWriter.Write( const Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
var
  fIdx: Integer;
  fPos: Int64;                                                                //RG26.01.2007
  pSize, Written: DWORD;
  Buff: PChar;
begin
{$IFDEF UseLog}
  Log( Format( '-> Write( %.8x, %d )', [ Integer( data ), size ] ) );
{$ENDIF}

  if arcVolumeSize > 0 then begin
    fIdx := ( arcPosition + Size ) div arcVolumeSize;
    while Length( Files ) < Integer( Succ( fIdx ) ) do CreateNewFile;

    fIdx := arcPosition div arcVolumeSize;
    fPos := arcPosition mod arcVolumeSize;
    Buff := @Data;
    Written := 0;
    while Written < Size do begin
      FileSeek( Files[ fIdx ].Handle, fPos, soFromBeginning );
      pSize := Size - Written;
      if arcVolumeSize < fPos + pSize then pSize := arcVolumeSize - fPos;
      if not WriteFile( Files[ fIdx ].Handle, Buff[ Written ], pSize, pSize, nil ) then begin
        MyLastError:=GetLastError;                             //FHO 22.01.2007
        Written := 0;
        Break;
      end;
      Inc( Written, pSize );
      Inc( fIdx );
      fPos := 0;
    end;
  end else begin
    FileSeek( Files[ 0 ].Handle, arcPosition, soFromBeginning );
    if not WriteFile( Files[ 0 ].Handle, Data, Size, Written, nil ) then begin
      MyLastError:=GetLastError;                              //FHO 22.01.2007
      Written := 0;
    end;
  end;
  Inc( arcPosition, Written );
  if arcPosition > arcSize then arcSize := arcPosition;
  if Assigned( ProcessedSize ) then ProcessedSize^ := Written;
  if MyLastError=0 then                                       //FHO 22.01.2007
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TMyStreamWriter.WritePart( const Data; Size: DWORD; ProcessedSize: PDWORD ): Integer; stdcall;
begin
  Result := Write( Data, Size, ProcessedSize );
end;

function TMyStreamWriter.Seek( Offset: Int64; SeekOrigin: DWORD; NewPosition: PInt64 ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyStreamWriter.Seek( %d, %d, %.8x )', [ offset, seekOrigin, Integer( newPosition ) ] ) );
{$ENDIF}
  case SeekOrigin of
    soFromBeginning: arcPosition := Offset;
    soFromCurrent: arcPosition := arcPosition + Offset;
    soFromEnd: arcPosition := arcSize + Offset;
  end;
  if arcPosition > arcSize then arcSize := arcPosition;
  if newPosition <> nil then newPosition^ := arcPosition;
  Result := S_OK;
end;

function TMyStreamWriter.SetSize( newSize: Int64 ): Integer; stdcall;
begin
{$IFDEF UseLog}
  Log( Format( 'TMyStreamWriter.SetSize( %d )', [ newSize ] ) );
{$ENDIF}
  Result := S_FALSE;
end;

destructor TMyStreamWriter.Destroy;
var
  i: Integer;
begin
  if Assigned(FPLastError) and
    (MyLastError<>ERROR_SUCCESS) then                          //FHO 22.01.2007
    FPLastError^:=MyLastError;                                 //FHO 22.01.2007
  for i := Low( Files ) to High( Files ) do begin
    FileClose( Files[ i ].Handle );                            //FHO 17.01.2007
    Files[ i ].Name:='';                                       //FHO 17.01.2007
  end;
  SetLength( Files, 0 );                                       //FHO 17.01.2007

{$IFDEF UseLog}
  Log( 'TMyStreamWriter.Destroy' );
{$ENDIF}
  inherited;
end;

function TMyStreamWriter.CreateNewFile: Boolean;
var
  i: Integer;
  s: WideString;
begin
  i := Length( Files );
  SetLength( Files, i + 1 );
  if arcVolumeSize > 0 then begin
    s := IntToStr( i + 1 );
    while Length( s ) < 3 do s := '0' + s;
    s := arcName + '.' + s;
  end else s := arcName;

  if arcCreateSFX and ( i = 0 ) then begin
// Shadow 27.11.2006
    Files[ 0 ].Name:=arcName;                                  //FHO 17.01.2007
    Files[ 0 ].Handle := CreateFile_( PwideChar( arcName ), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, arcAttr, 0 );
    if Files[ 0 ].Handle <> INVALID_HANDLE_VALUE then arcPosition := FileSeek( Files[ 0 ].Handle, int64(0), soFromEnd );  //FHO 20.01.2007
  end else begin
    Files[ i ].Name:=s;                                        //FHO 17.01.2007
    Files[ i ].Handle := CreateFile_( PwideChar( s ), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, arcAttr, 0 );
  end;

  if Files[ i ].Handle = INVALID_HANDLE_VALUE then begin       //FHO 20.01.2007
     MyLastError:=GetLastError;                                //FHO 22.01.2007
     Abort;
  end;

  MyLastError:=FileSetDate( Files[ i ].Handle, DateTimeToFileDate( arcDate ) );      //FHO 22.01.2007
  if MyLastError<>0 then                                       //FHO 22.01.2007
    Abort;
  Result := TRUE;
end;

constructor TMyStreamWriter.Create(PLastError:PInteger; sz: Widestring; //FHO 22.01.2007
                                 szDate: Tdatetime; FAttr: Cardinal; VolumeSize: Integer; CreateSFX: Boolean );
begin
  inherited Create;
  FPLastError:=PLastError;                                     //FHO 22.01.2007
  arcName := sz;
  arcDate := szDate;
  arcAttr := FAttr;
  arcCreateSFX := CreateSFX;
  arcVolumeSize := VolumeSize;
  arcPosition := 0;
  arcSize := 0;
  SetLength( Files, 0 );
  if not CreateNewFile then Abort;
end;


// ------------------------------------------------------------------------------------------
//functions for SevenZip
{$IFDEF UseLog}
function PropIDToString( propID: Integer ): string;
begin
  case propID of
    kpidNoProperty       : Result := 'kpidNoProperty';
    kpidHandlerItemIndex : Result := 'kpidHandlerItemIndex';
    kpidPath             : Result := 'kpidPath';
    kpidName             : Result := 'kpidName';
    kpidExtension        : Result := 'kpidExtension';
    kpidIsFolder         : Result := 'kpidIsFolder';
    kpidSize             : Result := 'kpidSize';
    kpidPackedSize       : Result := 'kpidPackedSize';
    kpidAttributes       : Result := 'kpidAttributes';
    kpidCreationTime     : Result := 'kpidCreationTime';
    kpidLastAccessTime   : Result := 'kpidLastAccessTime';
    kpidLastWriteTime    : Result := 'kpidLastWriteTime';
    kpidSolid            : Result := 'kpidSolid';
    kpidCommented        : Result := 'kpidCommented';
    kpidEncrypted        : Result := 'kpidEncrypted';
    kpidSplitBefore      : Result := 'kpidSplitBefore';
    kpidSplitAfter       : Result := 'kpidSplitAfter';
    kpidDictionarySize   : Result := 'kpidDictionarySize';
    kpidCRC              : Result := 'kpidCRC';
    kpidType             : Result := 'kpidType';
    kpidIsAnti           : Result := 'kpidIsAnti';
    kpidMethod           : Result := 'kpidMethod';
    kpidHostOS           : Result := 'kpidHostOS';
    kpidFileSystem       : Result := 'kpidFileSystem';
    kpidUser             : Result := 'kpidUser';
    kpidGroup            : Result := 'kpidGroup';
    kpidBlock            : Result := 'kpidBlock';
    kpidComment          : Result := 'kpidComment';
    kpidPosition         : Result := 'kpidPosition';

    kpidTotalSize        : Result := 'kpidTotalSize';
    kpidFreeSpace        : Result := 'kpidFreeSpace';
    kpidClusterSize      : Result := 'kpidClusterSize';
    kpidVolumeName       : Result := 'kpidVolumeName';

    kpidLocalName        : Result := 'kpidLocalName';
    kpidProvider         : Result := 'kpidProvider';
    kpidUserDefined      : Result := 'kpidUserDefined';
  else
    Result := 'unknown';
  end;
end;

function PropTypeToString( propType: Integer ): string;
begin
  case propType of
    VT_EMPTY          : Result := 'VT_EMPTY';
    VT_NULL           : Result := 'VT_NULL';
    VT_I2             : Result := 'VT_I2';
    VT_I4             : Result := 'VT_I4';
    VT_R4             : Result := 'VT_R4';
    VT_R8             : Result := 'VT_R8';
    VT_CY             : Result := 'VT_CY';
    VT_DATE           : Result := 'VT_DATE';
    VT_BSTR           : Result := 'VT_BSTR';
    VT_DISPATCH       : Result := 'VT_DISPATCH';
    VT_ERROR          : Result := 'VT_ERROR';
    VT_BOOL           : Result := 'VT_BOOL';
    VT_VARIANT        : Result := 'VT_VARIANT';
    VT_UNKNOWN        : Result := 'VT_UNKNOWN';
    VT_DECIMAL        : Result := 'VT_DECIMAL';
    VT_I1             : Result := 'VT_I1';
    VT_UI1            : Result := 'VT_UI1';
    VT_UI2            : Result := 'VT_UI2';
    VT_UI4            : Result := 'VT_UI4';
    VT_I8             : Result := 'VT_I8';
    VT_UI8            : Result := 'VT_UI8';
    VT_INT            : Result := 'VT_INT';
    VT_UINT           : Result := 'VT_UINT';
    VT_VOID           : Result := 'VT_VOID';
    VT_HRESULT        : Result := 'VT_HRESULT';
    VT_PTR            : Result := 'VT_PTR';
    VT_SAFEARRAY      : Result := 'VT_SAFEARRAY';
    VT_CARRAY         : Result := 'VT_CARRAY';
    VT_USERDEFINED    : Result := 'VT_USERDEFINED';
    VT_LPSTR          : Result := 'VT_LPSTR';
    VT_LPWSTR         : Result := 'VT_LPWSTR';
    VT_FILETIME       : Result := 'VT_FILETIME';
    VT_BLOB           : Result := 'VT_BLOB';
    VT_STREAM         : Result := 'VT_STREAM';
    VT_STORAGE        : Result := 'VT_STORAGE';
    VT_STREAMED_OBJECT: Result := 'VT_STREAMED_OBJECT';
    VT_STORED_OBJECT  : Result := 'VT_STORED_OBJECT';
    VT_BLOB_OBJECT    : Result := 'VT_BLOB_OBJECT';
    VT_CF             : Result := 'VT_CF';
    VT_CLSID          : Result := 'VT_CLSID';
  else
    Result := 'Unknown';
  end;
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------
//-------------------End SevenZip Interface -------------------------------------------------
//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//-----------------START DEBUG ONLY-----------------------------------------------------------------
//--------------------------------------------------------------------------------------------------
// For Debug only
// Add this to your MainForm public
{
 procedure LogMessage( var msg: TMessage ); message 9999;
}
{
  procedure TForm1.SevenZip1Message( Sender: TObject; ErrCode: Integer; Message: String );
  begin
   memo1.lines.add( message );
  end;
}
// and to Form.Activate this ( or set it when you want with e.g. a Button )
{
 sevenzipvcl.FMainhandle := form1.Handle;
}

{$IFDEF UseLog}
procedure Log( sz: string );
var
  p: PString;
begin
  p := new( PString );
  p^ := sz;
  PostMessage( fMainhandle, 9999, 0, Integer( p ) );
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------
//-----------------END DEBUG ONLY-------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//-----------------Start SevenZip VCL-------------------------------------------------------
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//constructor destructor
//------------------------------------------------------------------------------------------------
(*
procedure TSevenZip.LogMessage( var msg: TMessage );
begin
  if assigned( onMessage ) then OnMessage( Self,0,PString( msg.LParam )^ );
  Dispose( PString( msg.LParam ) );
end;
*)

constructor TSevenZip.Create( AOwner: TComponent );
var OSVerInfo : TOSVersionInfo;
{$IFDEF UseRes7zdll}
  MemoryStream: TResourceStream;
{$ENDIF}
begin
  inherited Create( AOwner );
  ffiles := TWideStringList_.Create;
  ResetCancel;
  FMainHandle := FHandle;
  FNumberOfFiles := -1;
  FPassword := '';
  FSFXModule := '7z.sfx';
  FPPMDSize := 10;
  FPPMDMem := 30;


// Shadow 28.11.2006
  FCreateObject := nil;

{$IFDEF UseRes7zdll}
    MemoryStream := TResourceStream.Create( HInstance, '7zip_library', RT_RCDATA );
    try
      m_DllDataSize := MemoryStream.Size;
      mp_DllData := GetMemory( m_DllDataSize );
      MemoryStream.Read( mp_DllData^, m_DllDataSize );
    finally
      MemoryStream.Free;
    end;

    mp_MemoryModule := BTMemoryLoadLibary( mp_DllData, m_DllDataSize );
    @FCreateObject := BTMemoryGetProcAddress( mp_MemoryModule, 'CreateObject' );
{$ELSE}                                                        //FHO 25.01.2007
  F7zaLibh := LoadLibrary( '7za.dll' );
  if F7zaLibh <> 0 then                                        //FHO 25.01.2007
    @FCreateObject := GetProcAddress( F7zaLibh, 'CreateObject' );
{$ENDIF}

  if not Assigned( FCreateObject ) then begin
    raise Exception.Create( 'Could not load CreateObject function from 7za.dll' + #13#10 + 'Perhaps 7za.dll not found' );
  end else begin
    FCreateObject( @CLSID_CFormat7z, @IID_IInArchive, inA );
    FCreateObject( @CLSID_CFormat7z, @IID_IOutArchive, outA );
    FCreateObject( @CLSID_CFormat7z, @IID_ISetProperties, sp );
  end;

  OSVerInfo.dwOSVersionInfoSize := sizeof(OSVerInfo);
  GetVersionEx(OsVerInfo);
  if osverinfo.dwPlatformID = VER_PLATFORM_WIN32_WINDOWS then
   isUnicode := false
  else
   isUnicode := true;
end;

destructor TSevenzip.Destroy;
begin
  ClearNamesOfVolumeWritten;

//jjw 18.10.2006
  inA := nil;
  outA := nil;
  sp := nil;

{$IFDEF UseRes7zdll}
  if m_DllDataSize > 0 then FreeMemory( mp_DllData );
  if mp_MemoryModule <> nil then BTMemoryFreeLibrary( mp_MemoryModule );
{$ELSE}                                                        //FHO 25.01.2007
  if F7zaLibh > 0 then FreeLibrary( F7zaLibh );
{$ENDIF}

  ffiles.Clear;
  ffiles.Free;

  inherited;
end;
//------------------------------------------------------------------------------------------------
//End constructor destructor
//------------------------------------------------------------------------------------------------

Procedure TSevenZip.Cancel; // public
begin
 FMainCancel := True;
end;

Procedure TSevenZip.ResetCancel; // private
begin
 FMainCancel := False;
end;
//RG 02.06.2006

(*
function TSevenZip.GetIndexByFilename( FileToExtract: Widestring ): Integer;
var
  n: Integer;
  w: DWORD;
  fnameprop: PROPVARIANT;
  fileInArchive: widestring;
  ms: TMyStreamReader;
begin
  try
    Result := -1;
    ms := TMyStreamReader.Create( Self, FSevenZipFileName, TRUE );
    inA.Close;
    inA.Open( ms, nil, nil );
    inA.GetNumberOfItems( w ); //1..end
    FileToExtract := UppercaseW_( FileToExtract );
    for n := 0 to w-1 do begin
      fnameprop.vt := VT_EMPTY;
      inA.GetProperty( n, kpidPath, fnameprop );
      fileInArchive := UppercaseW_( OleStrToString( fnameprop.bstrVal ) );
      if ( fileInArchive = FileToExtract ) then begin
        Result := n;
        Break;
      end;
    end;
  finally
    inA.close;
  end
end;
*)

// ZSA 21.02.2006 -- By splitting GetIndexByFilename into two parts allow
//	the Extract function to translate filenames into indices correctly
//	without closing 'inA'
function TSevenZip.InternalGetIndexByFilename( FileToExtract: Widestring ): Integer;
var
  n: Integer;
  w: DWORD;
  fnameprop: PROPVARIANT;
  fileInArchive: widestring;
begin
  Result := -1;
  inA.GetNumberOfItems( w ); //1..end
  FileToExtract := UppercaseW_( FileToExtract );
  for n := 0 to w-1 do begin
    fnameprop.vt := VT_EMPTY;
    inA.GetProperty( n, kpidPath, fnameprop );
    fileInArchive := UppercaseW_( OleStrToString( fnameprop.bstrVal ) );
    if ( fileInArchive = FileToExtract ) then begin
      Result := n;
      Break;
    end;
  end;
end;

function TSevenZip.GetIndexByFilename( FileToExtract: Widestring ): Integer;
var
  ms: TMyStreamReader;
begin
  try
    Result := -1;
    ms := TMyStreamReader.Create( Self, FSevenZipFileName, TRUE );
    inA.Close;
    inA.Open( ms, nil, nil );
    Result := InternalGetIndexByFilename( FileToExtract );
  finally
    inA.close;
  end
end;


//-------------------------------------------------------
//SFX functions
// Shadow 28.11.2006
function TSevenZip.SFXCheck( Fn: WideString ): Boolean;
const
  ID_7z: Array[ 0..5 ] of byte = ( 55, 122, 188, 175, 39, 28 );
var
  MySize, MyOrigSize: DWORD;
  Source: Integer;
  Buffer: array[ 0..81919 ] of Byte;
  ReadBytes, i: DWORD;

  function MyOriginalSize: DWORD;
  var
    s, d: DWORD;
    w: Word;
  begin
    Result := 0;
    s := FileSeek( Source, 0, soFromCurrent );
    try
      FileSeek( Source, $3C, soFromBeginning );
      FileRead( Source, d, 4 );
      FileSeek( Source, d + $06, soFromBeginning );
      FileRead( Source, w, 2 );
{?????????????}
      Inc( w );
{?????????????}
      FileSeek( Source, ( d + $F8 ) + ( w * $28 ) - $14 , soFromBeginning );
      FileRead( Source, Result, 4 );
    finally
      FileSeek( Source, s, soFromBeginning );
    end;
  end;

  function CheckSignature( Offset: Integer ): Boolean;
  var
    i: Integer;
  begin
    Result := FALSE;
    for i := 0 to 5 do begin
      if ( Buffer[ Offset + i ] <> ID_7z[ i ] ) then Break;
      if i = 5 then Result := TRUE;
    end;
  end;

begin
  Result := FALSE;
  if UpperCaseW_( ExtractFileExtW( Fn ) ) <> '.EXE' then Exit;

  FSFXoffset := 0;
  FIsSFX := FALSE;

  Source := CreateFile_( PWideChar( Fn ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
  try
    MySize := FileSeek( Source, int64(0), soFromEnd );
    FileSeek( Source, 65536, soFromBeginning );
    ReadFile( Source, Buffer[ 0 ], SizeOf( Buffer ), ReadBytes, nil );
    for i := 0 to ReadBytes - 6 do begin
      FIsSFX := CheckSignature( i );
      if FIsSFX then begin
        FSFXOffset := 65536 + i;
        Result := TRUE;
        Break;
      end;
    end;
    if not FIsSFX then begin
      MyOrigSize := MyOriginalSize;
      if MySize <> MyOrigSize then begin
        FileSeek( Source, int64(MyOrigSize), soFromBeginning );
        ReadFile( Source, Buffer[ 0 ], 6, ReadBytes, nil );
        FIsSFX := CheckSignature( 0 );
        if FIsSFX then begin
          FSFXOffset := MyOrigSize;
          Result := TRUE;
        end;
      end;
    end;
  finally
    FileClose( Source );
  end;
end;

function TSevenZip.ConvertSFXto7z( Fn:Widestring ): boolean;
var Source,Dest: Integer;
    DestFn: Widestring;
    buffer: pointer;
    readbytes,writebytes:Dword;
const
    chunksize = 1024*128;
begin
  //ErikGG Begin 08.11.06
  Buffer := Nil;
  Source := -1;
  Dest := -1;
  Result := False;
  //ErikGG End 08.11.06
  try
    DestFn := changefileextW( Fn,'.7z' );
     Source := CreateFile_( PwideChar( Fn ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    Fileseek( Source,SFXoffset,0 ); //goto 7z data
    Dest := CreateFile_( PwideChar( DestFn ), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0 );

    GetMem( Buffer,chunksize ); { allocate the buffer }

    repeat
      readbytes := Fileread( Source,buffer^,chunksize );
      writebytes := Filewrite( Dest,buffer^,readbytes );
    until readbytes < chunksize;

    if writebytes <> readbytes then
        Result := false
    else
        Result := true;//Only reached if no error happend

    //ErikGG 08.11.06
    Result := true;//Only reached if no error happend
  finally
    //ErikGG Begin 07.11.06
    if Buffer <> Nil then freemem( buffer );
    if Source <> 0 then   Fileclose( Source );
    if Dest <> 0 then     Fileclose( Dest );
    //ErikGG End 07.11.06
  end;

end;

function TSevenZip.Convert7ztoSFX( Fn:Widestring ): boolean;
var Source,Dest: Integer;
    DestFn: Widestring;
    buffer: pointer;
    readbytes,writebytes:Dword;
const
    chunksize = 1024*128;
begin
 //ErikGG Begin 07.11.06
 Result := false;
 Buffer := Nil;
 Source := -1;
 Dest := -1;
 //ErikGG End 07.11.06

 DestFn := changefileextW( Fn,'.exe' );
 if not copyfilew( PWidechar( sfxmodule ),PWideChar( DestFn ),True ) then
  begin
   FLastError:=GetLastError;                                   //FHO 22.01.2007
   ErrCode:=FSFXModuleError;
   if assigned( onMessage ) then
     onMessage( self,FSFXModuleError, c7zipResMsg[FSFXModuleError], Fsevenzipfilename );
   exit;
  end;

try
  Source := CreateFile_( PwideChar( Fn ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
  Dest := CreateFile_( PwideChar( DestFn ), GENERIC_Write, 0, nil, OPEN_EXISTING, 0, 0 );
  fileseek( Dest,0,2 );

  GetMem( Buffer,chunksize ); { allocate the buffer }

  repeat
    readbytes := Fileread( Source,buffer^,chunksize );
    writebytes := Filewrite( Dest,buffer^,readbytes );
  until readbytes < chunksize;

   if writebytes <> readbytes then
      Result := false
   else
      Result := true;//Only reached if no error happend
finally

 if Buffer <> Nil then freemem( buffer );
 if Source <> 0 then   Fileclose( Source );
 if Dest <> 0 then     Fileclose( Dest );
 //ErikGG End 07.11.06
end;

end;


//SFX functions end
//-------------------------------------------------------

function TSevenZip.List: Integer;
var
  ms: TMyStreamReader;
  updateOpenCallback: TmyArchiveOpenCallback;
  i: Integer;
  w: DWord;
  name: TBSTR;
  prop: PROPID;
  pType: Integer;
  path: PROPVARIANT;
  size: PROPVARIANT;
  packedsize: PROPVARIANT;
  attr:PROPVARIANT;
  fcrc: PROPVARIANT;
  szMethod: PROPVARIANT;
  sztime: PROPVARIANT;
  szMethod_WS: Widestring;
  blockpid: PROPVARIANT;
begin
  try
    Ffiles.Clear;

    FNumberOfFiles := -1;
    if UppercaseW_( ExtractFileExtW( FSevenZipFileName ) ) = '.EXE' then begin
      if not SFXCheck( FSevenZipFileName ) then begin
        Result := -1;
        ErrCode:=FNoSFXarchive;                                //FHO 21.01.2007
        if assigned( onMessage ) then
          onMessage( self, FNoSFXarchive, c7zipResMsg[FNoSFXarchive], Fsevenzipfilename );
        Exit;
      end;
    end;

    ms := TMyStreamReader.Create( Self, FSevenZipFileName, TRUE );
    inA.Close;

// 24.08.06 - Matteo Riso - Status: experimental
// 25.08.06 Modified by rg
//
// If we loaded an .EXE file, we could start reading from offset 132096.
// Also supported by newerdll

//  if FIsSFX then ms.Seek( FSFXOffset,0,@FSFXOffset );

// End - MR modification

    updateOpenCallback := TMyArchiveOpenCallback.Create( self );
    i := inA.Open(ms, nil,updateOpencallback );

  if i <> 0 then
   begin
     Result := -1;
     ErrCode:=FFileNotFound;                                   //FHO 21.01.2007
     if assigned( onMessage ) then
       onMessage( self, FFileNotFound, c7zipResMsg[FFileNotFound], Fsevenzipfilename );
     Exit;
   end;

(*
   inA.GetNumberOfArchiveProperties( w );
   for i := 0 to w-1 do
    begin
     path.vt := VT_EMPTY;
     ina.GetArchiveProperty( kpidPath,@path );
     if assigned( onMessage ) then onMessage( self,i,path.bstrVal,path.pwszVal );
    end;
*)

  inA.GetNumberOfProperties( w );

  for i := 0 to w - 1 do
  begin
    name := new( TBSTR );
    ptype := 0;
    inA.GetPropertyInfo( i, name, prop, pType );
{$IFDEF UseLog}
    if name = nil then
    begin
      Log( Format( '%d %s %s ( %d ) %s ( %d )', [ i, '', PropIDToString( prop ), prop, PropTypeToString( pType ), pType ] ) )
    end else
    begin
      log( Format( '%d %s %s ( %d ) %s ( %d )', [ i, name, PropIDToString( prop ), prop, PropTypeToString( pType ), pType ] ) );
    end;
{$ENDIF}
  end;


  inA.GetNumberOfItems( w );
  FNumberOfFiles := w;

  for i := 0 to w-1 do
   begin
       path.vt := VT_EMPTY;
       size.vt := VT_EMPTY;
       packedsize.vt := VT_EMPTY;
       attr.vt := VT_EMPTY;
       fcrc.vt := VT_EMPTY;
       szmethod.vt := VT_EMPTY;
       sztime.vt := VT_EMPTY;
       blockpid.vt := VT_EMPTY;

       inA.GetProperty( i, kpidPath, path );
       inA.GetProperty( i, kpidSize, size );
       inA.GetProperty( i, kpidPackedSize, packedsize );
       inA.GetProperty( i, kpidAttributes, attr );
       inA.GetProperty( i, kpidCRC, fcrc );
       inA.GetProperty( i, kpidMethod, szMethod );
       inA.GetProperty( i, kpidLastWriteTime, sztime );
       inA.GetProperty( i, kpidblock, blockpid );

       try
       if ( ( ( attr.uiVal and ( 1 shl 4 ) ) <> 0 ) or ( size.uhVal.QuadPart = 0 ) ) then szMethod_WS := 'None'  // is a directory or 0byte file
        else  //rg 18.04.06
         szMethod_WS := Widestring( szmethod.bstrVal ); //Check for diectoies or 0 byte files, if not an exception happens

       //ErikGG Begin 07.11.06
       //Add all found files and directories to the Files List
       //Is it a directory then add only paths with the backslash
       if ( ( attr.uiVal and ( 1 shl 4 ) ) <> 0 ) then
        ffiles.AddString( Widestring( AppendSlash( path.bstrVal ) ) )
       else
        ffiles.AddString( Widestring( path.bstrVal ) );
       //ErikGG End 07.11.06

       if assigned( Fonlistfile ) then
          Fonlistfile( self,
            Widestring( path.bstrVal ),  //filename 1
            i,                         //fileindex for extracting
            size.uhVal.QuadPart,       //Filesizeunp  2
            packedsize.uhVal.QuadPart, //FilesizeP 3
            ( attr.uhVal.QuadPart and not ( 1 shl 13 ) ), //attr 4 , removes first set bit
            fcrc.uhVal.QuadPart,       //CRC 5
            szMethod_WS,               //method 6
            FileTimeToDateTime( sztime.filetime,2 )  //filetime 7
            );
       except
       end;
   end; //for i:= 0

   Result := FNumberOfFiles;


finally
   ina.Close;
   ResetCancel;
end;

end;

/////// Added MK 30.03.2006
// ErikGG 07.11.06 Rewrote the add method,

function TSevenZip.Add: Integer;
var
  updateCallback: TMyArchiveUpdateCallback;
  intf: IArchiveUpdateCallback;
  MyStreamWriter:TMyStreamWriter;
  outStream: IOutStream;
  i,FileAttr{, FtoAdd, fHnd}: Integer;
  a: Int64; // Bug GDG 21.02.07
  FMaxProgress:int64;
  FileDT:TFiletime;
  FileSize_:int64;
  setProperties: ISetProperties;
  SetP: array[ 0..10 ] of PROPVARIANT;
  SetPNames: array[ 0..2 ] of PWideChar;
  FilesinBuffer, CurrBuffSize, NumOfProps: Cardinal;

//Get compression strength for adding
  function SevenZipCompressionStrengthInt( cs: TCompressStrength ): Cardinal;
  begin
    case cs of
      SAVE: result := 0;
      FAST: result := 3;
      NORMAL: result := 5;
      MAXIMUM: result := 7;
      ULTRA: result := 9;
    else
      result := 5;
    end;
  end;

// Shadow 28.11.2006
//Get directory content and recursive if wanted
//------------------------------------------------------------------------------
  procedure AddFile( _Name: WideString; _Size: Int64; _DateTime: _FILETIME; _Attr: Cardinal );
  begin
    if CurrBuffSize <= FilesinBuffer then begin //Increase the Buffers by 100 entries.
      Inc( CurrBuffSize, 100 );
      Setlength( updateCallback.Files, CurrBuffSize );
      Setlength( updateCallback.Files_size, CurrBuffSize );
      Setlength( updateCallback.Files_Date, CurrBuffSize );
      Setlength( updateCallback.Files_Attr, CurrBuffSize );
    end;

    updateCallback.Files[ FilesinBuffer ] := _Name;
    updateCallback.Files_size[ FilesinBuffer ] := _Size;
    updateCallback.Files_Date[ FilesinBuffer ] := _DateTime;
    updateCallback.Files_Attr[ FilesinBuffer ] := _Attr;

    FMaxProgress := FMaxProgress + _Size;
    Inc( FilesinBuffer );
  end;

  procedure AddRootDir( const Dir: WideString );
  var
    s: WideString;
//    l: Integer;
  begin
    s := ClearSlash( Dir );
    if not DirectoryExistsW( s ) then Exit;
    GetFileSizeandDateTime_Int64( s, FileSize_, FileDT, FileAttr );
//    l := Length( s );
    if (Frootdir <> '') then                 //rg remove path infront of directory 6.2.2007
      delete(s,1,length(Frootdir))
     else
      delete(s,1,3);

//    while ( l > 0 ) and ( s[ l ] <> '\' ) do Dec( l );
//    s := Copy( s, l + 1, MaxInt );

    if s <> '' then AddFile( s, FileSize_, FileDT, FileAttr );
  end;

  procedure GetDirs( Const MainDir, Ext: WideString );
  var
    srw: _Win32_Find_Dataw;
    SearchHandle: Cardinal;
  begin
    srw.dwFileAttributes := faAnyFile;
    SearchHandle := FindFirstFileW( PWideChar( MainDir + '*.*' ), srw );
    if SearchHandle <> INVALID_HANDLE_VALUE then begin
      repeat
        if ( srw.cFileName = Widestring('.') ) then Continue; //Blocks "." and ".." filenames
        if ( srw.cFileName = Widestring('..') ) then Continue; //Blocks "." and ".." filenames

        if ( ( srw.dwFileAttributes and faDirectory ) = faDirectory ) then
         begin//Is a Directory
          if not ( AddStoreOnlyFilename in FAddOptions ) then
           Addfile(
             MainDir + srw.cFileName,
             srw.nFileSizeHigh shl 32 + srw.nFileSizeLow,
             srw.ftLastWriteTime,
             srw.dwFileAttributes
           );

          if ( AddRecurseDirs in FAddoptions ) then GetDirs( AppendSlash( MainDir + srw.cFileName ), Ext );
         end
        else
         begin //Is a file
          if ( Ext <> '.*' ) and ( ExtractFileExtW( srw.cFileName ) <> Ext ) then Continue;
          Addfile(
            MainDir + srw.cFileName,
            srw.nFileSizeHigh shl 32 + srw.nFileSizeLow,
            srw.ftLastWriteTime,
            srw.dwFileAttributes
          );
         end;
      until not FindNextFileW( SearchHandle, srw ) or FMainCancel;
      Windows.FindClose( SearchHandle );
    end;
  end;
{
     procedure SetPassword( Password: String );
     var
       CryptoSetPassword: ICryptoSetPassword;
       Buffer: PChar;
       SizeInBytes: DWORD;
       i: Integer;
     begin
       if not Assigned( SetPwd ) then Exit;
       if SetPwd.QueryInterface( IID_ICryptoSetPassword, CryptoSetPassword ) = S_OK then begin
         SizeInBytes := Length( Password ) * 2;
         GetMem( Buffer, SizeInBytes );
         try
           for i := 0 to Length( Password ) - 1 do begin
             Buffer[ i * 2 ] := Password[ i + 1 ];
             Buffer[ i * 2 + 1 ] := #0;
           end;
           CryptoSetPassword.CryptoSetPassword( Buffer, SizeInBytes );
          finally
           FreeMem( Buffer );
         end;
        end;
     end;
}
begin //main procedure
  try
    ResetCancel; // Modified TM - 30/8/2007
    updateCallback := TMyArchiveUpdateCallback.Create( self );

// Set FRootDir to uppercase for comparing
// Set AddRootdir for relative path or wholepath
// Set Frootdir to '' to add whole path

    FRootDir := UppercaseW_( FRootDir );
    updateCallback.RootDir := AppendSlash( FRootDir );

    FMaxProgress := 0;
    FilesinBuffer := 0;
    CurrBuffSize := 0;

    for i := 0 to Ffiles.Count- 1 do begin
//Contains a directory in the sence of C:\DIR\*.*
      a := Pos( '*', Ffiles.WStrings[ i ] );
      if a > 0 then begin
        AddRootDir( AppendSlash( Copy( Ffiles.WStrings[ i ], 1, a-1 ) ) ); //fehler
        GetDirs( AppendSlash( Copy( Ffiles.WStrings[ i ], 1, a-1 ) ), Copy( ffiles.WStrings[ i ], a + 1, 8 ) );
      end else begin
       a := GetFileSizeandDateTime_Int64( Ffiles.Wstrings[ i ],FileSize_, FileDT, FileAttr );       //rg2.2.2007
       if a >= 0 then
        AddFile( Ffiles.Wstrings[ i ], FileSize_, FileDT, FileAttr );
      end;
    end;

//Reset the Buffers back to the size equaling the number of files.
    SetLength( updateCallback.Files, FilesinBuffer );
    SetLength( updateCallback.Files_size, FilesinBuffer );
    SetLength( updateCallback.Files_Date, FilesinBuffer );
    SetLength( updateCallback.Files_Attr, FilesinBuffer );

//send MaxProgress to App
   if Assigned( OnPreProgress ) then OnPreProgress( Self, FMaxProgress );

   MyStreamWriter := nil;

  if ( FSFXCreate ) and ( FileExists_( FSFXModule ) ) then begin
    FSevenZipFileName := ChangeFileExtW( FSevenZipFileName,'.exe' );
    if CopyFileW( PWidechar( SFXModule ), PWideChar( FSevenZipFileName ), True ) then
// Shadow 27.11.2006                                           //FHO 17.01.2007
     MyStreamWriter := TMyStreamWriter.Create(@fLastError,     //FHO 22.01.2007
                            FSevenZipFileName, Now, FILE_ATTRIBUTE_ARCHIVE, FVolumeSize, TRUE )
    else begin
       FLastError:=GetLastError;                               //FHO 22.01.2007
       ErrCode:=FSXFileCreationError ;
       if Assigned( onMessage ) then
        OnMessage( self, FSXFileCreationError , c7zipResMsg[FSXFileCreationError], FSevenZipFileName);
     end;
   end else                                                    //FHO 17.01.2007
     MyStreamWriter := TMyStreamWriter.Create( @fLastError, FSevenZipFileName, now, FILE_ATTRIBUTE_ARCHIVE, FVolumeSize, false );

   outStream:=MyStreamWriter;                                  //FHO 17.01.2007

  //_______________
  //Setp.vt := VT_EMPTY;
  //Set archive options
  if outA.QueryInterface( IID_ISetProperties, setProperties ) = S_OK then begin
    NumOfProps := 0;
    //rg 17.04.06
    case FCompressType of
     LZMA: begin
            // 7z Profile
            Setp[ NumOfProps ].vt := VT_UI4;
            SetPNames[ NumOfProps ] := StringToOleStr( 'X' );
            Setp[ NumOfProps ].ulVal := SevenZipCompressionStrengthInt( FCompstrength );
            inc( NumOfProps );

            //Solid
            Setp[ NumOfProps ].vt := VT_BSTR;
            SetPNames[ NumOfProps ] := StringToOleStr( 's' );
            if ( AddSolid in FAddoptions ) then
             Setp[ NumOfProps ].bstrVal := SysAllocString( 'on' )
            else
             Setp[ NumOfProps ].bstrVal := SysAllocString( 'off' );

            inc( NumOfProps );


            {
            directorysize 0..27
            No need to set if you use CompressionStrength Profiles
            ( Save...Ultra )
            }
            if FLZMAStrength > 0 then
             begin
              Setp[ NumOfProps ].vt := VT_UI4;
              SetPNames[ NumOfProps ] := StringToOleStr( 'd' );
              Setp[ NumOfProps ].ulVal := FLZMAStrength;
              inc( NumOfProps )
             end;
          end;
     PPMD: begin
            // PPMD compression
            Setp[ NumOfProps ].vt := VT_BSTR;
            SetPNames[ NumOfProps ] := StringToOleStr( '0' );
            Setp[ NumOfProps ].bstrVal := SysAllocString( 'PPMd' );
            inc( NumOfProps );

            // DM 2008-03-20
            // set the compression strength for PPMD compression type
            Setp[ NumOfProps ].vt := VT_UI4;
            SetPNames[ NumOfProps ] := StringToOleStr( 'X' );
            Setp[ NumOfProps ].ulVal := SevenZipCompressionStrengthInt( FCompstrength );
            inc( NumOfProps );

            //PPMD Size
            //No need to set if you use defaults
            if FPPMDsize > 0 then
             begin
              Setp[ NumOfProps ].vt := VT_UI4;
              SetPNames[ NumOfProps ] := StringToOleStr( 'o' );
              Setp[ NumOfProps ].ulVal := FPPMDSize; // DM 2008-03-20 - set the value from property - use to be always "10"
              inc( NumOfProps );
             end;

            //PPMD Mem
            //No need to set if you use defaults
            if FPPMDmem > 0 then
             begin
              Setp[ NumOfProps ].vt := VT_UI4;
              SetPNames[ NumOfProps ] := StringToOleStr( 'mem' );
              Setp[ NumOfProps ].ulVal := FPPMDMem; // DM 2008-03-20 - set the value from property - use to be always "30"
              inc( NumOfProps );
             end;
           end;
    end; //end case

    if (FPassword <> '') and ( AddEnCryptFilename in FAddoptions ) then
      begin  
        Setp[ NumOfProps ].vt := VT_BSTR;
        SetPNames[ NumOfProps ] := StringToOleStr( 'he' );
        Setp[ NumOfProps ].bstrVal := SysAllocString( 'on' );
        inc( NumOfProps );
      end;
	
    // set options
    result := setProperties.SetProperties( @SetPNames, @Setp, NumOfProps );
   end; //if QuerryInterface
  //____________________


  if FilesinBuffer > 0 then Begin
    intf := updateCallback;
    Result := outA.UpdateItems( outStream, FilesinBuffer, updateCallback );
  end else begin
    ErrCode:=FNoFilesToAdd;                                    //FHO 21.01.2007
    if Assigned( OnMessage ) then
      OnMessage( Self, FNoFilesToAdd, c7zipResMsg[FNoFilesToAdd], '' );
    Result := -1;
  end;

  ClearNamesOfVolumeWritten;                                   //FHO 17.01.2007

  With MyStreamWriter do begin                                 //FHO 17.01.2007
    SetLength(FNamesOfVolumesWritten,Length(Files));           //FHO 17.01.2007
    for i:= 0 to Length(FNamesOfVolumesWritten)-1 do           //FHO 17.01.2007
      FNamesOfVolumesWritten[i]:=Files[i].Name;                //FHO 17.01.2007
  end;
//  MyStreamWriter:=NIL;                                         //FHO 17.01.2007

  if OutStream <> nil then
  begin
   OutStream := nil;
  end;

 finally
  ResetCancel;
 end;
end;


function TSevenZip.Extract( TestArchive:Boolean=False ): Integer;
var
  updateCallback: TMyArchiveExtractCallback;
  updateOpenCallback: TmyArchiveOpenCallback;
  ms: TMyStreamReader;
  filesDW: array of DWORD;
  Filestoex,w: DWORD;
  i,j,n: Integer;
  FMaxProgress:int64;
  size: PROPVARIANT;
//  fnameprop: PROPVARIANT;
//  fileInArchive, fileToExtract: WideString;

begin
try
// 24.08.06 - Matteo Riso - Status: experimental
// 25.08.06 Modified by rg
//
// If we loaded an .EXE file, we could start reading from offset 132096.
// Also supported by newerdll

//  if FIsSFX then ms.Seek( FSFXOffset,0,@FSFXOffset );

// End - MR modification

// Shadow 28.11.2006
    if UppercaseW_( ExtractFileExtW( FSevenZipFileName ) ) = '.EXE' then begin
      if not SFXCheck( FSevenZipFileName ) then begin
        Result := -1;
        ErrCode:=FNoSFXarchive;                                //FHO 21.01.2007
        if assigned( onMessage ) then
          onMessage( self, FNoSFXarchive, c7zipResMsg[FNoSFXarchive], Fsevenzipfilename );
        Exit;
      end;
    end;

    ms := TMyStreamReader.Create( Self, FSevenZipFileName, TRUE );
    inA.Close;

    updateOpenCallback := TMyArchiveOpenCallback.Create( self );
    i := inA.Open( ms, nil, updateOpenCallback );

    if i <> 0 then begin
      Result := -1;
      ErrCode:=FFileNotFound;                                  //FHO 21.01.2007
      if assigned( onMessage ) then
        onMessage( self,FFileNotFound, c7zipResMsg[FFileNotFound],FSevenZipFileName );
      ms.Free;                                                //FHO crash at wrong pw 25.01.2007
      Exit;
    end;

    FMaxProgress := 0;
    inA.GetNumberOfItems( w ); //1..end
    dec( w ); //Starting with 0..end-1

    n := 0;
    if FFiles.Count > 0 then begin
      SetLength( filesDW, ffiles.Count );
      for i := 0 to FFiles.count - 1 do begin
        if not TryStrToInt_( Ffiles.WStrings[ i ], j ) then
//          j := InternalGetIndexByFilename( Ffiles.WStrings[ i ] );	//ZSA 21.02.2007
          j := GetINdexbyFilename( Ffiles.WStrings[ i ] );

          if (j < 0) or (abs(j) > abs(w)) then begin
            ErrCode:=FIndexOutOfRange;                         //FHO 21.01.2007
            if Assigned( onMessage ) then
              onMessage( Self, FIndexOutOfRange, c7zipResMsg[FIndexOutOfRange], '' );
            Result := -1;
            Exit;
          end;

          size.vt := VT_EMPTY;
          inA.GetProperty( j, kpidSize, size );
          FMaxProgress := FMaxprogress + size.uhVal.QuadPart;
          filesDW[ n ] := j;
          Inc( n );
        end; // For i := 0

      Filestoex := n;
    end else begin
//   extract all files, FFiles.Count must be 0
     FilestoEx := $FFFFFFFF;
    end;

    SetLength( filesDW, n );

//set FMaxProgress for selected files
  if FMaxProgress > 0 then if assigned( OnPreProgress ) then OnPreProgress( self,FMaxProgress );

// filesdw must be sorted asc
  if length( filesdw ) > 1 then SortDWord( filesDW,low( filesdw ),High( filesdw ) );

  updatecallback := TMyArchiveExtractCallback.Create( self );
  updatecallback.FExtractDirectory := appendslash( Fextrbasedir );
  updatecallback.FFilestoextract   := ffiles.Count; //with all files ffiles.count = 0, thats ok
  updatecallback.FAllFilesExt      := false;        //Stop extracting if no more files to extract
  updatecallback.FLastFileToExt    := false;        //only 1 more to extact


  result := inA.Extract( @filesDW[ 0 ], Filestoex, Integer( TestArchive ), updatecallback )

//  mmoLog.Lines.Add( Format( 'IInArchive.Extract: %d', [ result ] ) );
finally
  ina.close;
  ResetCancel;
end;
end;

procedure TSevenZip.ClearNamesOfVolumeWritten;
var
  i:Integer;
begin
  for i:=0 to length(FNamesOfVolumesWritten)-1 do
    FNamesOfVolumesWritten[i]:='';
  SetLength(FNamesOfVolumesWritten,0);

end;

procedure TSevenZip.SetLastError(const Value: Integer);
begin
  FLastError := Value;
end;



(*
function TSevenZip.Delete: Integer;
begin
//
end;
*)

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//-----------------End SevenZip VCL---------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

{$IFDEF RegisterInThisUnit}
procedure Register;
begin
  RegisterComponents( 'Seven Zip', [ TSevenZip ] );
end;
{$ENDIF}
end.


