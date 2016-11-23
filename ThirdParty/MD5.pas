{ MD5.Pas - components for calculating MD5 Message-Digest values
  written 2/2/94 by

  Peter Sawatzki
  Buchenhof 3
  D58091 Hagen, Germany

  EMail: Peter@Sawatzki.de
  WWW:   http://www.sawatzki.de

  original C Source for MD5 routine was found in Dr. Dobbs Journal Sep 91
  MD5 algorithm from RSA Data Security, Inc.

  modified and enhanced 29.4.2000 PS
  small enhancements 16.6.2007 PS
}
{.$define UsePascal}
{$ifdef CLR} {$define UsePascal} {$endif}
Unit Md5;
{$OVERFLOWCHECKS OFF, RANGECHECKS OFF}
Interface
Uses
  SysUtils,
  RtlConsts,
  Classes,
  Consts;
Type
  TDigestStr = String[32];
  TDigest = Array[0..15] Of Byte;
  TDigestLongWord = Array[0..3] Of LongWord;

  TLongWordBuf = Array[0..15] Of LongWord;
  TByteBuf = Array[0..63] Of Byte;

  TCustomMD5 = class(TObject)
  Private
    FDigest: TDigest;         {the digest to be returned}
    CDigest: TDigestLongWord;  {digest accumulator}
    BitLo, BitHi: LongWord;    { number of _bits_ handled mod 2^64 }
    bBuf: TByteBuf;
    bLen: Cardinal; {bytes in bBuf}
    BufferChanged: Boolean;
    Procedure ResetBuffer;
    Procedure Update (Const ChkBuf; Len: Cardinal);
    Function GetDigest: TDigest;
    Function GetDigestStr: TDigestStr;
    Function GetDigestLongWord: TDigestLongWord;
  Protected
    { Protected declarations }
  Public
    Constructor Create;
    Property Digest: TDigest Read GetDigest;
    Property DigestLongWord: TDigestLongWord Read GetDigestLongWord;
    Property DigestStr: TDigestStr Read GetDigestStr;
  End;

  TMD5 = Class(TCustomMD5)
  Public
    Procedure Reset;
    Procedure Add (Value: Char); Overload;
    Procedure Add (Value: Byte); Overload;
    Procedure Add (Value: Word); Overload;
    Procedure Add (Value: Integer); Overload;
    Procedure Add (Value: Cardinal); Overload;
    Procedure Add (Const Value: String); Overload;
    Procedure Add (Value: TStrings); Overload;
    Procedure AddFile (Value: String);
  End;

Function FileMD5Digest (Const FileName: TFileName): TDigestStr;
Function StringMD5Digest (S: String): TDigestStr;

Implementation
Uses
  Windows;

{$ifdef UsePascal}
Procedure Transform (Var Accu; Const Buf);
Var
  a, b, c, d: LongWord;

  Function ROL (x: LongWord; n: Byte): LongWord; Inline;
  Begin Result:= (x Shl n) Or (x Shr (32-n)) End;

  Function FF (a,b,c,d,x: LongWord; s: Byte; ac: LongWord): LongWord; Inline;
  Begin Result:= ROL (a+x+ac + (b And c Or Not b And d), s) + b End;

  Function GG (a,b,c,d,x: LongWord; s: Byte; ac: LongWord): LongWord; Inline;
  Begin Result:= ROL (a+x+ac + (b And d Or c And Not d), s) + b End;

  Function HH (a,b,c,d,x: LongWord; s: Byte; ac: LongWord): LongWord; Inline;
  Begin Result:= ROL (a+x+ac + (b Xor c Xor d), s) + b End;

  Function II (a,b,c,d,x: LongWord; s: Byte; ac: LongWord): LongWord; Inline;
  Begin Result:= ROL (a+x+ac + (c Xor (b Or Not d)), s) + b End;

Begin
  a:= TDigestLongWord(Accu)[0];
  b:= TDigestLongWord(Accu)[1];
  c:= TDigestLongWord(Accu)[2];
  d:= TDigestLongWord(Accu)[3];

  a:= FF(a,b,c,d, TLongWordBuf(Buf)[ 0],  7, $d76aa478); { 1 }
  d:= FF(d,a,b,c, TLongWordBuf(Buf)[ 1], 12, $e8c7b756); { 2 }
  c:= FF(c,d,a,b, TLongWordBuf(Buf)[ 2], 17, $242070db); { 3 }
  b:= FF(b,c,d,a, TLongWordBuf(Buf)[ 3], 22, $c1bdceee); { 4 }
  a:= FF(a,b,c,d, TLongWordBuf(Buf)[ 4],  7, $f57c0faf); { 5 }
  d:= FF(d,a,b,c, TLongWordBuf(Buf)[ 5], 12, $4787c62a); { 6 }
  c:= FF(c,d,a,b, TLongWordBuf(Buf)[ 6], 17, $a8304613); { 7 }
  b:= FF(b,c,d,a, TLongWordBuf(Buf)[ 7], 22, $fd469501); { 8 }
  a:= FF(a,b,c,d, TLongWordBuf(Buf)[ 8],  7, $698098d8); { 9 }
  d:= FF(d,a,b,c, TLongWordBuf(Buf)[ 9], 12, $8b44f7af); { 10 }
  c:= FF(c,d,a,b, TLongWordBuf(Buf)[10], 17, $ffff5bb1); { 11 }
  b:= FF(b,c,d,a, TLongWordBuf(Buf)[11], 22, $895cd7be); { 12 }
  a:= FF(a,b,c,d, TLongWordBuf(Buf)[12],  7, $6b901122); { 13 }
  d:= FF(d,a,b,c, TLongWordBuf(Buf)[13], 12, $fd987193); { 14 }
  c:= FF(c,d,a,b, TLongWordBuf(Buf)[14], 17, $a679438e); { 15 }
  b:= FF(b,c,d,a, TLongWordBuf(Buf)[15], 22, $49b40821); { 16 }

  a:= GG(a,b,c,d, TLongWordBuf(Buf)[ 1],  5, $f61e2562); { 17 }
  d:= GG(d,a,b,c, TLongWordBuf(Buf)[ 6],  9, $c040b340); { 18 }
  c:= GG(c,d,a,b, TLongWordBuf(Buf)[11], 14, $265e5a51); { 19 }
  b:= GG(b,c,d,a, TLongWordBuf(Buf)[ 0], 20, $e9b6c7aa); { 20 }
  a:= GG(a,b,c,d, TLongWordBuf(Buf)[ 5],  5, $d62f105d); { 21 }
  d:= GG(d,a,b,c, TLongWordBuf(Buf)[10],  9, $02441453); { 22 }
  c:= GG(c,d,a,b, TLongWordBuf(Buf)[15], 14, $d8a1e681); { 23 }
  b:= GG(b,c,d,a, TLongWordBuf(Buf)[ 4], 20, $e7d3fbc8); { 24 }
  a:= GG(a,b,c,d, TLongWordBuf(Buf)[ 9],  5, $21e1cde6); { 25 }
  d:= GG(d,a,b,c, TLongWordBuf(Buf)[14],  9, $c33707d6); { 26 }
  c:= GG(c,d,a,b, TLongWordBuf(Buf)[ 3], 14, $f4d50d87); { 27 }
  b:= GG(b,c,d,a, TLongWordBuf(Buf)[ 8], 20, $455a14ed); { 28 }
  a:= GG(a,b,c,d, TLongWordBuf(Buf)[13],  5, $a9e3e905); { 29 }
  d:= GG(d,a,b,c, TLongWordBuf(Buf)[ 2],  9, $fcefa3f8); { 30 }
  c:= GG(c,d,a,b, TLongWordBuf(Buf)[ 7], 14, $676f02d9); { 31 }
  b:= GG(b,c,d,a, TLongWordBuf(Buf)[12], 20, $8d2a4c8a); { 32 }

  a:= HH(a,b,c,d, TLongWordBuf(Buf)[ 5],  4, $fffa3942); { 33 }
  d:= HH(d,a,b,c, TLongWordBuf(Buf)[ 8], 11, $8771f681); { 34 }
  c:= HH(c,d,a,b, TLongWordBuf(Buf)[11], 16, $6d9d6122); { 35 }
  b:= HH(b,c,d,a, TLongWordBuf(Buf)[14], 23, $fde5380c); { 36 }
  a:= HH(a,b,c,d, TLongWordBuf(Buf)[ 1],  4, $a4beea44); { 37 }
  d:= HH(d,a,b,c, TLongWordBuf(Buf)[ 4], 11, $4bdecfa9); { 38 }
  c:= HH(c,d,a,b, TLongWordBuf(Buf)[ 7], 16, $f6bb4b60); { 39 }
  b:= HH(b,c,d,a, TLongWordBuf(Buf)[10], 23, $bebfbc70); { 40 }
  a:= HH(a,b,c,d, TLongWordBuf(Buf)[13],  4, $289b7ec6); { 41 }
  d:= HH(d,a,b,c, TLongWordBuf(Buf)[ 0], 11, $eaa127fa); { 42 }
  c:= HH(c,d,a,b, TLongWordBuf(Buf)[ 3], 16, $d4ef3085); { 43 }
  b:= HH(b,c,d,a, TLongWordBuf(Buf)[ 6], 23, $04881d05); { 44 }
  a:= HH(a,b,c,d, TLongWordBuf(Buf)[ 9],  4, $d9d4d039); { 45 }
  d:= HH(d,a,b,c, TLongWordBuf(Buf)[12], 11, $e6db99e5); { 46 }
  c:= HH(c,d,a,b, TLongWordBuf(Buf)[15], 16, $1fa27cf8); { 47 }
  b:= HH(b,c,d,a, TLongWordBuf(Buf)[ 2], 23, $c4ac5665); { 48 }

  a:= II(a,b,c,d, TLongWordBuf(Buf)[ 0],  6, $f4292244); { 49 }
  d:= II(d,a,b,c, TLongWordBuf(Buf)[ 7], 10, $432aff97); { 50 }
  c:= II(c,d,a,b, TLongWordBuf(Buf)[14], 15, $ab9423a7); { 51 }
  b:= II(b,c,d,a, TLongWordBuf(Buf)[ 5], 21, $fc93a039); { 52 }
  a:= II(a,b,c,d, TLongWordBuf(Buf)[12],  6, $655b59c3); { 53 }
  d:= II(d,a,b,c, TLongWordBuf(Buf)[ 3], 10, $8f0ccc92); { 54 }
  c:= II(c,d,a,b, TLongWordBuf(Buf)[10], 15, $ffeff47d); { 55 }
  b:= II(b,c,d,a, TLongWordBuf(Buf)[ 1], 21, $85845dd1); { 56 }
  a:= II(a,b,c,d, TLongWordBuf(Buf)[ 8],  6, $6fa87e4f); { 57 }
  d:= II(d,a,b,c, TLongWordBuf(Buf)[15], 10, $fe2ce6e0); { 58 }
  c:= II(c,d,a,b, TLongWordBuf(Buf)[ 6], 15, $a3014314); { 59 }
  b:= II(b,c,d,a, TLongWordBuf(Buf)[13], 21, $4e0811a1); { 60 }
  a:= II(a,b,c,d, TLongWordBuf(Buf)[ 4],  6, $f7537e82); { 61 }
  d:= II(d,a,b,c, TLongWordBuf(Buf)[11], 10, $bd3af235); { 62 }
  c:= II(c,d,a,b, TLongWordBuf(Buf)[ 2], 15, $2ad7d2bb); { 63 }
  b:= II(b,c,d,a, TLongWordBuf(Buf)[ 9], 21, $eb86d391); { 64 }

  Inc(TDigestLongWord(Accu)[0], a);
  Inc(TDigestLongWord(Accu)[1], b);
  Inc(TDigestLongWord(Accu)[2], c);
  Inc(TDigestLongWord(Accu)[3], d)
End;

{$else}

// the following procedure was generated by MD5_makeinline for speed
//
Procedure Transform (Var Accu; Const Buf);
Asm
Push EBx;
Push ESi;
Push EDi;
Push EBp
Mov EBp,EDx {Buf -> EBp};
Push EAx {Accu->Stack};
Mov EDx,[EAx+12];
Mov ECx,[EAx+8];
Mov EBx,[EAx+4];
Mov EAx,[EAx]
Add EAx,[EBp+0];Add EAx,$D76AA478;Mov ESi,EBx;Not ESi;And ESi,EDx;Mov EDi,ECx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,7;Add EAx,EBx
Add EDx,[EBp+4];Add EDx,$E8C7B756;Mov ESi,EAx;Not ESi;And ESi,ECx;Mov EDi,EBx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,12;Add EDx,EAx
Add ECx,[EBp+8];Add ECx,$242070DB;Mov ESi,EDx;Not ESi;And ESi,EBx;Mov EDi,EAx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,17;Add ECx,EDx
Add EBx,[EBp+12];Add EBx,$C1BDCEEE;Mov ESi,ECx;Not ESi;And ESi,EAx;Mov EDi,EDx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,22;Add EBx,ECx
Add EAx,[EBp+16];Add EAx,$F57C0FAF;Mov ESi,EBx;Not ESi;And ESi,EDx;Mov EDi,ECx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,7;Add EAx,EBx
Add EDx,[EBp+20];Add EDx,$4787C62A;Mov ESi,EAx;Not ESi;And ESi,ECx;Mov EDi,EBx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,12;Add EDx,EAx
Add ECx,[EBp+24];Add ECx,$A8304613;Mov ESi,EDx;Not ESi;And ESi,EBx;Mov EDi,EAx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,17;Add ECx,EDx
Add EBx,[EBp+28];Add EBx,$FD469501;Mov ESi,ECx;Not ESi;And ESi,EAx;Mov EDi,EDx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,22;Add EBx,ECx
Add EAx,[EBp+32];Add EAx,$698098D8;Mov ESi,EBx;Not ESi;And ESi,EDx;Mov EDi,ECx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,7;Add EAx,EBx
Add EDx,[EBp+36];Add EDx,$8B44F7AF;Mov ESi,EAx;Not ESi;And ESi,ECx;Mov EDi,EBx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,12;Add EDx,EAx
Add ECx,[EBp+40];Add ECx,$FFFF5BB1;Mov ESi,EDx;Not ESi;And ESi,EBx;Mov EDi,EAx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,17;Add ECx,EDx
Add EBx,[EBp+44];Add EBx,$895CD7BE;Mov ESi,ECx;Not ESi;And ESi,EAx;Mov EDi,EDx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,22;Add EBx,ECx
Add EAx,[EBp+48];Add EAx,$6B901122;Mov ESi,EBx;Not ESi;And ESi,EDx;Mov EDi,ECx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,7;Add EAx,EBx
Add EDx,[EBp+52];Add EDx,$FD987193;Mov ESi,EAx;Not ESi;And ESi,ECx;Mov EDi,EBx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,12;Add EDx,EAx
Add ECx,[EBp+56];Add ECx,$A679438E;Mov ESi,EDx;Not ESi;And ESi,EBx;Mov EDi,EAx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,17;Add ECx,EDx
Add EBx,[EBp+60];Add EBx,$49B40821;Mov ESi,ECx;Not ESi;And ESi,EAx;Mov EDi,EDx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,22;Add EBx,ECx
Add EAx,[EBp+4];Add EAx,$F61E2562;Mov ESi,EDx;Not ESi;And ESi,ECx;Mov EDi,EDx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,5;Add EAx,EBx
Add EDx,[EBp+24];Add EDx,$C040B340;Mov ESi,ECx;Not ESi;And ESi,EBx;Mov EDi,ECx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,9;Add EDx,EAx
Add ECx,[EBp+44];Add ECx,$265E5A51;Mov ESi,EBx;Not ESi;And ESi,EAx;Mov EDi,EBx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,14;Add ECx,EDx
Add EBx,[EBp+0];Add EBx,$E9B6C7AA;Mov ESi,EAx;Not ESi;And ESi,EDx;Mov EDi,EAx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,20;Add EBx,ECx
Add EAx,[EBp+20];Add EAx,$D62F105D;Mov ESi,EDx;Not ESi;And ESi,ECx;Mov EDi,EDx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,5;Add EAx,EBx
Add EDx,[EBp+40];Add EDx,$2441453;Mov ESi,ECx;Not ESi;And ESi,EBx;Mov EDi,ECx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,9;Add EDx,EAx
Add ECx,[EBp+60];Add ECx,$D8A1E681;Mov ESi,EBx;Not ESi;And ESi,EAx;Mov EDi,EBx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,14;Add ECx,EDx
Add EBx,[EBp+16];Add EBx,$E7D3FBC8;Mov ESi,EAx;Not ESi;And ESi,EDx;Mov EDi,EAx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,20;Add EBx,ECx
Add EAx,[EBp+36];Add EAx,$21E1CDE6;Mov ESi,EDx;Not ESi;And ESi,ECx;Mov EDi,EDx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,5;Add EAx,EBx
Add EDx,[EBp+56];Add EDx,$C33707D6;Mov ESi,ECx;Not ESi;And ESi,EBx;Mov EDi,ECx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,9;Add EDx,EAx
Add ECx,[EBp+12];Add ECx,$F4D50D87;Mov ESi,EBx;Not ESi;And ESi,EAx;Mov EDi,EBx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,14;Add ECx,EDx
Add EBx,[EBp+32];Add EBx,$455A14ED;Mov ESi,EAx;Not ESi;And ESi,EDx;Mov EDi,EAx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,20;Add EBx,ECx
Add EAx,[EBp+52];Add EAx,$A9E3E905;Mov ESi,EDx;Not ESi;And ESi,ECx;Mov EDi,EDx;And EDi,EBx;Or ESi,EDi;Add EAx,ESi;Rol EAx,5;Add EAx,EBx
Add EDx,[EBp+8];Add EDx,$FCEFA3F8;Mov ESi,ECx;Not ESi;And ESi,EBx;Mov EDi,ECx;And EDi,EAx;Or ESi,EDi;Add EDx,ESi;Rol EDx,9;Add EDx,EAx
Add ECx,[EBp+28];Add ECx,$676F02D9;Mov ESi,EBx;Not ESi;And ESi,EAx;Mov EDi,EBx;And EDi,EDx;Or ESi,EDi;Add ECx,ESi;Rol ECx,14;Add ECx,EDx
Add EBx,[EBp+48];Add EBx,$8D2A4C8A;Mov ESi,EAx;Not ESi;And ESi,EDx;Mov EDi,EAx;And EDi,ECx;Or ESi,EDi;Add EBx,ESi;Rol EBx,20;Add EBx,ECx
Add EAx,[EBp+20];Add EAx,$FFFA3942;Mov ESi,EDx;Xor ESi,ECx;Xor ESi,EBx;Add EAx,ESi;Rol EAx,4;Add EAx,EBx
Add EDx,[EBp+32];Add EDx,$8771F681;Mov ESi,ECx;Xor ESi,EBx;Xor ESi,EAx;Add EDx,ESi;Rol EDx,11;Add EDx,EAx
Add ECx,[EBp+44];Add ECx,$6D9D6122;Mov ESi,EBx;Xor ESi,EAx;Xor ESi,EDx;Add ECx,ESi;Rol ECx,16;Add ECx,EDx
Add EBx,[EBp+56];Add EBx,$FDE5380C;Mov ESi,EAx;Xor ESi,EDx;Xor ESi,ECx;Add EBx,ESi;Rol EBx,23;Add EBx,ECx
Add EAx,[EBp+4];Add EAx,$A4BEEA44;Mov ESi,EDx;Xor ESi,ECx;Xor ESi,EBx;Add EAx,ESi;Rol EAx,4;Add EAx,EBx
Add EDx,[EBp+16];Add EDx,$4BDECFA9;Mov ESi,ECx;Xor ESi,EBx;Xor ESi,EAx;Add EDx,ESi;Rol EDx,11;Add EDx,EAx
Add ECx,[EBp+28];Add ECx,$F6BB4B60;Mov ESi,EBx;Xor ESi,EAx;Xor ESi,EDx;Add ECx,ESi;Rol ECx,16;Add ECx,EDx
Add EBx,[EBp+40];Add EBx,$BEBFBC70;Mov ESi,EAx;Xor ESi,EDx;Xor ESi,ECx;Add EBx,ESi;Rol EBx,23;Add EBx,ECx
Add EAx,[EBp+52];Add EAx,$289B7EC6;Mov ESi,EDx;Xor ESi,ECx;Xor ESi,EBx;Add EAx,ESi;Rol EAx,4;Add EAx,EBx
Add EDx,[EBp+0];Add EDx,$EAA127FA;Mov ESi,ECx;Xor ESi,EBx;Xor ESi,EAx;Add EDx,ESi;Rol EDx,11;Add EDx,EAx
Add ECx,[EBp+12];Add ECx,$D4EF3085;Mov ESi,EBx;Xor ESi,EAx;Xor ESi,EDx;Add ECx,ESi;Rol ECx,16;Add ECx,EDx
Add EBx,[EBp+24];Add EBx,$4881D05;Mov ESi,EAx;Xor ESi,EDx;Xor ESi,ECx;Add EBx,ESi;Rol EBx,23;Add EBx,ECx
Add EAx,[EBp+36];Add EAx,$D9D4D039;Mov ESi,EDx;Xor ESi,ECx;Xor ESi,EBx;Add EAx,ESi;Rol EAx,4;Add EAx,EBx
Add EDx,[EBp+48];Add EDx,$E6DB99E5;Mov ESi,ECx;Xor ESi,EBx;Xor ESi,EAx;Add EDx,ESi;Rol EDx,11;Add EDx,EAx
Add ECx,[EBp+60];Add ECx,$1FA27CF8;Mov ESi,EBx;Xor ESi,EAx;Xor ESi,EDx;Add ECx,ESi;Rol ECx,16;Add ECx,EDx
Add EBx,[EBp+8];Add EBx,$C4AC5665;Mov ESi,EAx;Xor ESi,EDx;Xor ESi,ECx;Add EBx,ESi;Rol EBx,23;Add EBx,ECx
Add EAx,[EBp+0];Add EAx,$F4292244;Mov ESi,EDx;Not ESi;Or ESi,EBx;Xor ESi,ECx;Add EAx,ESi;Rol EAx,6;Add EAx,EBx
Add EDx,[EBp+28];Add EDx,$432AFF97;Mov ESi,ECx;Not ESi;Or ESi,EAx;Xor ESi,EBx;Add EDx,ESi;Rol EDx,10;Add EDx,EAx
Add ECx,[EBp+56];Add ECx,$AB9423A7;Mov ESi,EBx;Not ESi;Or ESi,EDx;Xor ESi,EAx;Add ECx,ESi;Rol ECx,15;Add ECx,EDx
Add EBx,[EBp+20];Add EBx,$FC93A039;Mov ESi,EAx;Not ESi;Or ESi,ECx;Xor ESi,EDx;Add EBx,ESi;Rol EBx,21;Add EBx,ECx
Add EAx,[EBp+48];Add EAx,$655B59C3;Mov ESi,EDx;Not ESi;Or ESi,EBx;Xor ESi,ECx;Add EAx,ESi;Rol EAx,6;Add EAx,EBx
Add EDx,[EBp+12];Add EDx,$8F0CCC92;Mov ESi,ECx;Not ESi;Or ESi,EAx;Xor ESi,EBx;Add EDx,ESi;Rol EDx,10;Add EDx,EAx
Add ECx,[EBp+40];Add ECx,$FFEFF47D;Mov ESi,EBx;Not ESi;Or ESi,EDx;Xor ESi,EAx;Add ECx,ESi;Rol ECx,15;Add ECx,EDx
Add EBx,[EBp+4];Add EBx,$85845DD1;Mov ESi,EAx;Not ESi;Or ESi,ECx;Xor ESi,EDx;Add EBx,ESi;Rol EBx,21;Add EBx,ECx
Add EAx,[EBp+32];Add EAx,$6FA87E4F;Mov ESi,EDx;Not ESi;Or ESi,EBx;Xor ESi,ECx;Add EAx,ESi;Rol EAx,6;Add EAx,EBx
Add EDx,[EBp+60];Add EDx,$FE2CE6E0;Mov ESi,ECx;Not ESi;Or ESi,EAx;Xor ESi,EBx;Add EDx,ESi;Rol EDx,10;Add EDx,EAx
Add ECx,[EBp+24];Add ECx,$A3014314;Mov ESi,EBx;Not ESi;Or ESi,EDx;Xor ESi,EAx;Add ECx,ESi;Rol ECx,15;Add ECx,EDx
Add EBx,[EBp+52];Add EBx,$4E0811A1;Mov ESi,EAx;Not ESi;Or ESi,ECx;Xor ESi,EDx;Add EBx,ESi;Rol EBx,21;Add EBx,ECx
Add EAx,[EBp+16];Add EAx,$F7537E82;Mov ESi,EDx;Not ESi;Or ESi,EBx;Xor ESi,ECx;Add EAx,ESi;Rol EAx,6;Add EAx,EBx
Add EDx,[EBp+44];Add EDx,$BD3AF235;Mov ESi,ECx;Not ESi;Or ESi,EAx;Xor ESi,EBx;Add EDx,ESi;Rol EDx,10;Add EDx,EAx
Add ECx,[EBp+8];Add ECx,$2AD7D2BB;Mov ESi,EBx;Not ESi;Or ESi,EDx;Xor ESi,EAx;Add ECx,ESi;Rol ECx,15;Add ECx,EDx
Add EBx,[EBp+36];Add EBx,$EB86D391;Mov ESi,EAx;Not ESi;Or ESi,ECx;Xor ESi,EDx;Add EBx,ESi;Rol EBx,21;Add EBx,ECx
Pop ESi{get Accu};
Add [ESi],EAx;
Add [ESi+4],EBx;
Add [ESi+8],ECx;
Add [ESi+12],EDx;
Pop EBp;
Pop EDi;
Pop ESi;
Pop EBx
End;
{$endif}

Constructor TCustomMD5.Create;
Begin
  Inherited Create;
  ResetBuffer;
End;

Procedure TCustomMD5.ResetBuffer;
Begin
  BitLo:= 0;
  BitHi:= 0;
  bLen:= 0;
  {Load magic initialization constants.}
  CDigest[0]:= $67452301;
  CDigest[1]:= $efcdab89;
  CDigest[2]:= $98badcfe;
  CDigest[3]:= $10325476;
  BufferChanged:= True
End;

Procedure TCustomMD5.Update (Const ChkBuf; Len: Cardinal);
Var
  BufPtr: ^Byte;
  Left: Cardinal;
Begin
  BufferChanged:= True;
  If BitLo + LongWord(Len) Shl 3 < BitLo Then
    Inc(BitHi);
  Inc(BitLo, LongWord(Len) Shl 3);
  Inc(BitHi, LongWord(Len) Shr 29);

  BufPtr:= @ChkBuf;
  If bLen>0 Then Begin
    Left:= 64-bLen; If Left>Len Then Left:= Len;
    Move(BufPtr^, bBuf[bLen], Left);
    Inc(bLen, Left); Inc(BufPtr, Left);
    If bLen<64 Then Exit;
    Transform(CDigest, bBuf);
    bLen:= 0;
    Dec(Len, Left)
  End;
  While Len>=64 Do Begin
    Transform(CDigest, BufPtr^);
    Inc(BufPtr, 64);
    Dec(Len, 64)
  End;
  If Len>0 Then Begin
    bLen:= Len;
    Move(BufPtr^, bBuf[0], bLen)
  End
End;

Function TCustomMD5.GetDigest: TDigest;
{-get digest without modifying bBuf, bLen and BitLo/Hi}
Var
  WorkBuf: TByteBuf;
  WorkLen: Cardinal;
Begin
  If BufferChanged Then Begin
    FDigest:= TDigest(CDigest);
    Move(bBuf, WorkBuf, bLen); {make copy of buffer}
    {pad out to block of form (0..55, BitLo, BitHi)}
    WorkBuf[bLen]:= $80;
    WorkLen:= bLen+1;
    If WorkLen>56 Then Begin
      if WorkLen<64 then
        FillChar(WorkBuf[WorkLen], 64-WorkLen, 0);
      TransForm(FDigest, WorkBuf);
      WorkLen:= 0
    End;
    FillChar(WorkBuf[WorkLen], 56-WorkLen, 0);
    TLongWordBuf(WorkBuf)[14]:= BitLo;
    TLongWordBuf(WorkBuf)[15]:= BitHi;
    Transform (FDigest, WorkBuf);
    BufferChanged:= False
  End;
  Result:= FDigest
End;

Function TCustomMD5.GetDigestStr: TDigestStr;
Const
  hc: Array[0..$F] Of Char = '0123456789ABCDEF';
Var
  aDigest: TDigest;
  i: 0..15;
Begin
  aDigest:= Digest;
  Result[0]:= #32;
  For i:= 0 To 15 Do Begin
    Result[1+i Shl 1]:= hc[aDigest[i] Shr 4];
    Result[2+i Shl 1]:= hc[aDigest[i] And $F]
  End
End;

Function TCustomMD5.GetDigestLongWord: TDigestLongWord;
Begin
  TDigest(Result):= Digest
End;

{ TMD5 }

Procedure TMD5.Reset;
Begin
  ResetBuffer
End;

Procedure TMD5.Add (Value: Char);
Begin
  Update(Value, SizeOf(Value))
End;

Procedure TMD5.Add (Value: Byte);
Begin
  Update(Value, SizeOf(Value))
End;

Procedure TMD5.Add (Value: Word);
Begin
  Update(Value, SizeOf(Value))
End;

Procedure TMD5.Add (Value: Integer);
Begin
  Update(Value, SizeOf(Value))
End;

Procedure TMD5.Add (Value: Cardinal);
Begin
  Update(Value, SizeOf(Value))
End;

Procedure TMD5.Add (Const Value: String);
Begin
  Update(PChar(Value)^, Length(Value));
End;

Procedure TMD5.Add (Value: TStrings);
Var
  i: Integer;
Begin
  For i:= 0 To Value.Count-1 Do
    Add(Value[i])
End;

Procedure TMD5.AddFile (Value: String);
var
  aBuf: Pointer;
  Source: Integer; { handle }
  wRd: Cardinal;
Const
  ChunkSize = 8192;
Begin
  GetMem(aBuf, ChunkSize);
  Try
    Source:= FileOpen(Value, fmShareDenyWrite);
    if Source<0 Then
      raise EFOpenError.CreateFmt(SFOpenError, [Value]);
    Try
      repeat
        wRd:= FileRead(Source, aBuf^, ChunkSize);
        if wRd>0 then
          Update(aBuf^, wRd)
      until wRd<ChunkSize
    Finally
      FileClose(Source)
    End
  Finally
    FreeMem(aBuf, ChunkSize)
  End
End;

{ handy procedures }

Function FileMD5Digest (Const FileName: TFileName): TDigestStr;
Begin
  With TMD5.Create Do
  Try
    AddFile(FileName);
    Result:= DigestStr
  Finally
    Free
  End
End;

Function StringMD5Digest (S: String): TDigestStr;
Begin
  With TMD5.Create Do
  Try
    Add(S);
    Result:= DigestStr
  Finally
    Free
  End
End;


{ Some MD5 test vectors found on the WEB:

  From: http://www.di-mgt.com.au/crypto.html
  Debug.Print MD5HexDigest("abc")
  should return "900150983cd24fb0d6963f7d28e17f72".

  From: http://www.unix-ag.uni-kl.de/~conrad/krypto/passphrase-faq.html
  MD5 test vectors:
  d41d8cd98f00b204e9800998ecf8427e ""
  0cc175b9c0f1b6a831c399e269772661 "a"
  900150983cd24fb0d6963f7d28e17f72 "abc"
  f96b697d7cb7938d525a2f31aaf161d0 "message digest"
  c3fcd3d76192e4007dfb496cca67e13b "abcdefghijklmnopqrstuvwxyz"
  d174ab98d277d9f5a5611c2c9f419d9f "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijk\
                                   lmnopqrstuvwxyz0123456789"
  57edf4a22be3c955ac49da2e2107b67a "1234567890123456789012345678901234567\
                                   8901234567890123456789012345678901234567890"
  900150983cd24fb0d6963f7d28e17f72 foo

}

end.
