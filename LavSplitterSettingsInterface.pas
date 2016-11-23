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

unit LavSplitterSettingsInterface;

interface

uses DirectShow9, ActiveX, Windows;

const
  CLSID_LavSplitterDecoder: TGuid = '{B98D13E7-55DB-4385-A33D-09FD1BA26338}';
  IID_ILAVFSettings: TGuid = '{774A919D-EA95-4A87-8A1E-F48ABE8499C7}';

type

  TLAVSubtitleMode = (
   LAVSubtitleMode_NoSubs,
   LAVSubtitleMode_ForcedOnly,
   LAVSubtitleMode_Default,
   LAVSubtitleMode_Advanced,
   LAVSubtitleMode);

  // LAV Splitter configuration interface
  ILAVFSettings = interface(IUnknown)
    ['{774A919D-EA95-4A87-8A1E-F48ABE8499C7}']

   // Switch to Runtime Config mode. This will reset all settings to default, and no changes to the settings will be saved
   // You can use this to programmatically configure LAV Splitter without interfering with the users settings in the registry.
   // Subsequent calls to this function will reset all settings back to defaults, even if the mode does not change.
   //
   // Note that calling this function during playback is not supported and may exhibit undocumented behaviour.
   // For smooth operations, it must be called before LAV Splitter opens a file.
   Procedure SetRuntimeConfig(bRuntimeConfig : boolean); stdcall;

   // Retrieve the preferred languages as ISO 639-2 language codes, comma separated
   // If the result is NULL, no language has been set
   // Memory for the string will be allocated, and has to be free'ed by the caller with CoTaskMemFree
   Procedure GetPreferredLanguages(out ppLanguages : LPWSTR); stdcall;

   // Set the preferred languages as ISO 639-2 language codes, comma separated
   // To reset to no preferred language, pass NULL or the empty string
   Procedure SetPreferredLanguages(pLanguages : LPWSTR); stdcall;

   // Retrieve the preferred subtitle languages as ISO 639-2 language codes, comma separated
   // If the result is NULL, no language has been set
   // If no subtitle language is set, the main language preference is used.
   // Memory for the string will be allocated, and has to be free'ed by the caller with CoTaskMemFree
   Procedure GetPreferredSubtitleLanguages(out ppLanguages : LPWSTR); stdcall;

   // Set the preferred subtitle languages as ISO 639-2 language codes, comma separated
   // To reset to no preferred language, pass NULL or the empty string
   // If no subtitle language is set, the main language preference is used.
   Procedure SetPreferredSubtitleLanguages(pLanguages : LPWSTR); stdcall;

   // Get the current subtitle mode
   // See enum for possible values
   Function GetSubtitleMode(): TLAVSubtitleMode; stdcall;

   // Set the current subtitle mode
   // See enum for possible values
   Procedure SetSubtitleMode(mode : TLAVSubtitleMode); stdcall;

   // Get the subtitle matching language flag
   // TRUE = Only subtitles with a language in the preferred list will be used; FALSE = All subtitles will be used
   // @deprecated - do not use anymore, deprecated and non-functional, replaced by advanced subtitle mode
   Function GetSubtitleMatchingLanguage(): boolean; stdcall;

   // Set the subtitle matching language flag
   // TRUE = Only subtitles with a language in the preferred list will be used; FALSE = All subtitles will be used
   // @deprecated - do not use anymore, deprecated and non-functional, replaced by advanced subtitle mode
   Procedure SetSubtitleMatchingLanguage(dwMode : boolean); stdcall;

   // Control whether a special "Forced Subtitles" stream will be created for PGS subs
   Function GetPGSForcedStream(): boolean; stdcall;

   // Control whether a special "Forced Subtitles" stream will be created for PGS subs
   Procedure SetPGSForcedStream(bFlag : boolean); stdcall;

   // Get the PGS forced subs config
   // TRUE = only forced PGS frames will be shown, FALSE = all frames will be shown
   Function GetPGSOnlyForced(): boolean; stdcall;

   // Set the PGS forced subs config
   // TRUE = only forced PGS frames will be shown, FALSE = all frames will be shown
   Procedure SetPGSOnlyForced(bForced : boolean); stdcall;

   // Get the VC-1 Timestamp Processing mode
   // 0 - No Timestamp Correction, 1 - Always Timestamp Correction, 2 - Auto (Correction for Decoders that need it)
   Function GetVC1TimestampMode(): integer; stdcall;

   // Set the VC-1 Timestamp Processing mode
   // 0 - No Timestamp Correction, 1 - Always Timestamp Correction, 2 - Auto (Correction for Decoders that need it)
   Procedure SetVC1TimestampMode(iMode : integer); stdcall;

   // Set whether substreams (AC3 in TrueHD, for example) should be shown as a seperate stream
   Procedure SetSubstreamsEnabled(bSubStreams : boolean); stdcall;

   // Check whether substreams (AC3 in TrueHD, for example) should be shown as a seperate stream
   Function GetSubstreamsEnabled(): boolean; stdcall;

   // @deprecated - no longer required
   Procedure SetVideoParsingEnabled(bEnabled : boolean); stdcall;

   // @deprecated - no longer required
   Function GetVideoParsingEnabled(): boolean; stdcall;

   // Set if LAV Splitter should try to fix broken HD-PVR streams
   // @deprecated - no longer required
   Procedure SetFixBrokenHDPVR(bEnabled : boolean); stdcall;

   // Query if LAV Splitter should try to fix broken HD-PVR streams
   // @deprecated - no longer required
   Function GetFixBrokenHDPVR(): boolean; stdcall;

   // Control whether the given format is enabled
   Function SetFormatEnabled(const strFormat : LPWSTR; bEnabled : boolean): HResult; stdcall;

   // Check if the given format is enabled
   Function IsFormatEnabled(const strFormat : LPWSTR): boolean; stdcall;

   // Set if LAV Splitter should always completely remove the filter connected to its Audio Pin when the audio stream is changed
   Procedure SetStreamSwitchRemoveAudio(bEnabled : boolean); stdcall;

   // Query if LAV Splitter should always completely remove the filter connected to its Audio Pin when the audio stream is changed
   Function GetStreamSwitchRemoveAudio(): boolean; stdcall;

   // Advanced Subtitle configuration. Refer to the documentation for details.
   // If no advanced config exists, will be NULL.
   // Memory for the string will be allocated, and has to be free'ed by the caller with CoTaskMemFree
   Function GetAdvancedSubtitleConfig(out ppAdvancedConfig : LPWSTR): boolean; stdcall;

   // Advanced Subtitle configuration. Refer to the documentation for details.
   // To reset the config, pass NULL or the empty string.
   // If no subtitle language is set, the main language preference is used.
   Procedure SetAdvancedSubtitleConfig(pAdvancedConfig : LPWSTR); stdcall;

   // Set if LAV Splitter should prefer audio streams for the hearing or visually impaired
   Procedure SetUseAudioForHearingVisuallyImpaired(bEnabled : boolean); stdcall;

   // Get if LAV Splitter should prefer audio streams for the hearing or visually impaired
   Function GetUseAudioForHearingVisuallyImpaired(): boolean; stdcall;

   // Set the maximum queue size, in megabytes
   Procedure SetMaxQueueMemSize(dwMaxSize : DWord); stdcall;

   // Get the maximum queue size, in megabytes
   Function GetMaxQueueMemSize(): DWord; stdcall;

   // Toggle Tray Icon
   Procedure SetTrayIcon(bEnabled : boolean); stdcall;

   // Get Tray Icon
   Function GetTrayIcon(): boolean ; stdcall;

   // Toggle whether higher quality audio streams are preferred
   Procedure SetPreferHighQualityAudioStreams(bEnabled : boolean); stdcall;

   // Toggle whether higher quality audio streams are preferred
   Function GetPreferHighQualityAudioStreams(): boolean; stdcall;

   // Toggle whether Matroska Linked Segments should be loaded from other files
   Procedure SetLoadMatroskaExternalSegments(bEnabled : boolean); stdcall;

   // Get whether Matroska Linked Segments should be loaded from other files
   Function GetLoadMatroskaExternalSegments(): boolean; stdcall;

   // Get the list of available formats
   // Memory for the string array will be allocated, and has to be free'ed by the caller with CoTaskMemFree
   Function GetFormats(out formats : LPSTR; out nFormats : UINT ): boolean; stdcall;

   // Set the duration (in ms) of analysis for network streams (to find the streams and codec parameters)
   Procedure SetNetworkStreamAnalysisDuration(dwDuration : DWORD); stdcall;

   // Get the duration (in ms) of analysis for network streams (to find the streams and codec parameters)
   Function GetNetworkStreamAnalysisDuration(): DWORD; stdcall;

   // Set the maximum queue size, in number of packets
   Procedure SetMaxQueueSize(dwMaxSize : DWORD); stdcall;

   // Get whether Matroska Linked Segments should be loaded from other files
   Function GetMaxQueueSize(): DWORD; stdcall;

  end;

implementation

end.

