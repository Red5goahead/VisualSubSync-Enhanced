(*
 *      Copyright (C) 2010-2012 Hendrik Leppkes
 *      http://www.1f0.de
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *  porting for Delphi By Red5goahead (Red5goahead@gmail.com)
 *)

unit LavAudioSettingsInterface;

interface

uses DirectShow9, ActiveX, Windows;

const
  CLSID_LavAudioDecoder: TGuid = '{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}';
  IID_ILAVAudioSettings: TGuid = '{4158A22B-6553-45D0-8069-24716F8FF171}';
  LAV_MIXING_FLAG_UNTOUCHED_STEREO = 1;
  LAV_MIXING_FLAG_NORMALIZE_MATRIX = 2;
  LAV_MIXING_FLAG_CLIP_PROTECTION  = 4;

type

  // Codecs supported in the LAV Audio configuration
  // Codecs not listed here cannot be turned off. You can request codecs to be added to this list, if you wish.
  TLAVAudioCodec = (
   Codec_AAC,
   Codec_AC3,
   Codec_EAC3,
   Codec_DTS,
   Codec_MP2,
   Codec_MP3,
   Codec_TRUEHD,
   Codec_FLAC,
   Codec_VORBIS,
   Codec_LPCM,
   Codec_PCM,
   Codec_WAVPACK,
   Codec_TTA,
   Codec_WMA2,
   Codec_WMAPRO,
   Codec_Cook,
   Codec_RealAudio,
   Codec_WMALL,
   Codec_ALAC,
   Codec_Opus,
   Codec_AMR,
   Codec_Nellymoser,
   Codec_MSPCM,
   Codec_Truespeech,
   Codec_TAK,
   Codec_ATRAC,
   Codec_NB);

  // Supported Sample Formats in LAV Audio
  TLAVBitstreamCodec = (
   Bitstream_AC3,
   Bitstream_EAC3,
   Bitstream_TRUEHD,
   Bitstream_DTS,
   Bitstream_DTSHD,
   Bitstream_NB);

  // Supported Sample Formats in LAV Audio
  TLAVAudioSampleFormat = (
   SampleFormat_16,
   SampleFormat_24,
   SampleFormat_32,
   SampleFormat_U8,
   SampleFormat_FP32,
   SampleFormat_Bitstream,
   SampleFormat_NB);

  TLAVAudioMixingMode = (
   MatrixEncoding_None,
   MatrixEncoding_Dolby,
   MatrixEncoding_DPLII,
   MatrixEncoding_NB);

  // LAV Audio configuration interface
  ILAVAudioSettings = interface(IUnknown)
    ['{4158A22B-6553-45D0-8069-24716F8FF171}']

   // Switch to Runtime Config mode. This will reset all settings to default, and no changes to the settings will be saved
   // You can use this to programmatically configure LAV Video without interfering with the users settings in the registry.
   // Subsequent calls to this function will reset all settings back to defaults, even if the mode does not change.
   //
   // Note that calling this function during playback is not supported and may exhibit undocumented behaviour.
   // For smooth operations, it must be called before LAV Video is connected to other filters.
   Procedure SetRuntimeConfig(bRuntimeConfig : boolean); stdcall;

   // Dynamic Range Compression
   // pbDRCEnabled: The state of DRC
   // piDRCLevel:   The DRC strength (0-100, 100 is maximum)
   Procedure GetDRC(out pbDRCEnabled : boolean; out piDRCLevel : Integer); stdcall;
   Procedure SetDRC(bDRCEnabled : boolean; iDRCLevel : Integer); stdcall;

   // Configure which codecs are enabled
   // If vCodec is invalid (possibly a version difference), Get will return FALSE, and Set E_FAIL.
   Function GetFormatConfiguration(vCodec : TLAVAudioCodec): boolean ; stdcall;
   Procedure SetFormatConfiguration(vCodec : TLAVAudioCodec; bEnabled : boolean); stdcall;

   // Control Bitstreaming
   // If bsCodec is invalid (possibly a version difference), Get will return FALSE, and Set E_FAIL.
   Function GetBitstreamConfig(bsCodec : TLAVBitstreamCodec): boolean ; stdcall;
   Procedure SetBitstreamConfig(bsCodec : TLAVBitstreamCodec; bEnabled : boolean); stdcall;

   // Should "normal" DTS frames be encapsulated in DTS-HD frames when bitstreaming?
   Function GetDTSHDFraming(): boolean ; stdcall;
   Procedure SetDTSHDFraming(bHDFraming : boolean); stdcall;

   // Control Auto A/V syncing
   Function GetAutoAVSync(): boolean ; stdcall;
   Procedure SetAutoAVSync(bAutoSync : boolean); stdcall;

   // Convert all Channel Layouts to standard layouts
   // Standard are: Mono, Stereo, 5.1, 6.1, 7.1
   Function GetOutputStandardLayout(): boolean ; stdcall;
   Procedure SetOutputStandardLayout(bStdLayout : boolean); stdcall;

   // Expand Mono to Stereo by simply doubling the audio
   Function GetExpandMono(): boolean ; stdcall;
   Procedure SetExpandMono(bExpandMono : boolean); stdcall;

   // Expand 6.1 to 7.1 by doubling the back center
   Function GetExpand61(): boolean ; stdcall;
   Procedure SetExpand61(bExpand61 : boolean); stdcall;

   // Allow Raw PCM and SPDIF encoded input
   Function GetAllowRawSPDIFInput(): boolean ; stdcall;
   Procedure SetAllowRawSPDIFInput(bAllow : boolean); stdcall;

   // Configure which sample formats are enabled
   // Note: SampleFormat_Bitstream cannot be controlled by this
   Function GetSampleFormat(format : TLAVAudioSampleFormat): boolean ; stdcall;
   Procedure SetSampleFormat(format : TLAVAudioSampleFormat; bEnabled : boolean); stdcall;

   // Configure a delay for the audio
   Procedure GetAudioDelay(out pbEnabled : boolean; out pDelay : Integer); stdcall;
   Procedure SetAudioDelay(bEnabled : boolean; delay : Integer); stdcall;

   // Enable/Disable Mixing
   Procedure SetMixingEnabled(bEnabled : boolean); stdcall;
   Function GetMixingEnabled(): boolean ; stdcall;

   // Control Mixing Layout
   Procedure SetMixingLayout(dwLayout : DWORD); stdcall;
   Function GetMixingLayout(): DWORD ; stdcall;

   // Set Mixing Flags
   Procedure SetMixingFlags(dwFlags : DWORD); stdcall;
   Function GetMixingFlags(): DWORD ; stdcall;

   // Set Mixing Mode
   Procedure SetMixingMode(mixingMode : TLAVAudioMixingMode); stdcall;
   Function GetMixingMode(): TLAVAudioMixingMode ; stdcall;

   // Set Mixing Levels
   Procedure SetMixingLevels(dwCenterLevel : DWORD; dwSurroundLevel : DWORD; dwLFELevel : DWORD); stdcall;
   Procedure GetMixingLevels(out dwCenterLevel : DWORD; out dwSurroundLevel : DWORD; out dwLFELevel : DWORD); stdcall;

   // Toggle Tray Icon
   Procedure SetTrayIcon(bEnabled : boolean); stdcall;
   Function GetTrayIcon(): boolean ; stdcall;

   // Toggle Dithering for sample format conversion
   Procedure SetSampleConvertDithering(bEnabled : boolean); stdcall;
   Function GetSampleConvertDithering(): boolean ; stdcall;

  // Suppress sample format changes. This will allow channel count to increase, but not to reduce, instead adding empty channels
  // This option is NOT persistent
   Procedure SetSuppressFormatChanges(bEnabled : boolean); stdcall;
   Function GetSuppressFormatChanges(): boolean ; stdcall;

  // Use 5.1 legacy layout (using back channels instead of side)
  Procedure SetOutput51LegacyLayout(b51Legacy : boolean); stdcall;
  Function GetOutput51LegacyLayout(): boolean ; stdcall;

  end;

implementation

end.

