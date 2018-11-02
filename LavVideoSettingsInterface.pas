(*
 *      Copyright (C) 2010-2018 Hendrik Leppkes
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

unit LavVideoSettingsInterface;

interface

uses DirectShow9, ActiveX, Windows;

const
  CLSID_LavVideoDecoder: TGuid = '{EE30215D-164F-4A92-A4EB-9D4C13390F9F}';
  IID_ILAVVideoSettings: TGuid = '{FA40D6E9-4D38-4761-ADD2-71A9EC5FD32F}';

type

  // Codecs supported in the LAV Video configuration
  // Codecs not listed here cannot be turned off. You can request codecs to be added to this list, if you wish.
  TLAVVideoCodec = (
   Codec_H264,
   Codec_VC1,
   Codec_MPEG1,
   Codec_MPEG2,
   Codec_MPEG4,
   Codec_MSMPEG4,
   Codec_VP8,
   Codec_WMV3,
   Codec_WMV12,
   Codec_MJPEG,
   Codec_Theora,
   Codec_FLV1,
   Codec_VP6,
   Codec_SVQ,
   Codec_H261,
   Codec_H263,
   Codec_Indeo,
   Codec_TSCC,
   Codec_Fraps,
   Codec_HuffYUV,
   Codec_QTRle,
   Codec_DV,
   Codec_Bink,
   Codec_Smacker,
   Codec_RV12,
   Codec_RV34,
   Codec_Lagarith,
   Codec_Cinepak,
   Codec_Camstudio,
   Codec_QPEG,
   Codec_ZLIB,
   Codec_QTRpza,
   Codec_PNG,
   Codec_MSRLE,
   Codec_ProRes,
   Codec_UtVideo,
   Codec_Dirac,
   Codec_DNxHD,
   Codec_MSVideo1,
   Codec_8BPS,
   Codec_LOCO,
   Codec_ZMBV,
   Codec_VCR1,
   Codec_Snow,
   Codec_FFV1,
   Codec_v210,
   Codec_JPEG2000,
   Codec_VMNC,
   Codec_FLIC,
   Codec_G2M,
   Codec_ICOD,
   Codec_THP,
   Codec_HEVC,
   Codec_VP9,
   Codec_TrueMotion,
   Codec_VP7,
   Codec_H264MVC,
   Codec_CineformHD,
   Codec_MagicYUV,
   Codec_NB);

  // Codecs with hardware acceleration
  TLAVVideoHWCodec = (
   HWCodec_H264,
   HWCodec_VC1,
   HWCodec_MPEG2,
   HWCodec_MPEG4,
   HWCodec_MPEG2DVD,
   HWCodec_HEVC,
   HWCodec_NB);

  // Type of hardware accelerations
  TLAVHWAccel = (
    HWAccel_None,
    HWAccel_CUDA,
    HWAccel_QuickSync,
    HWAccel_DXVA2,
    HWAccel_DXVA2CopyBack = HWAccel_DXVA2,
    HWAccel_DXVA2Native,
    HWAccel_D3D11,
    HWAccel_NB);

  // Deinterlace algorithms offered by the hardware decoders
  TLAVHWDeintModes = (
   HWDeintMode_Weave,
   HWDeintMode_BOB,
   HWDeintMode_Hardware);

  // Software deinterlacing algorithms
  TLAVSWDeintModes = (
   SWDeintMode_None,
   SWDeintMode_YADIF,
   SWDeintMode_W3FDIF_Simple,
   SWDeintMode_W3FDIF_Complex);

  // Deinterlacing processing mode
  LAVDeintMode = (
   DeintMode_Auto,
   DeintMode_Aggressive,
   DeintMode_Force,
   DeintMode_Disable);

  // Type of deinterlacing to perform
  // - FramePerField re-constructs one frame from every field, resulting in 50/60 fps.
  // - FramePer2Field re-constructs one frame from every 2 fields, resulting in 25/30 fps.
  // Note: Weave will always use FramePer2Field
  TLAVDeintOutput = (
   DeintOutput_FramePeField,
   DeintOutput_FramePer2Field);

  // Control the field order of the deinterlacer
  TLAVDeintFieldOrder = (
   DeintFieldOrder_Auto,
   DeintFieldOrder_TopFieldFirst,
   DeintFieldOrder_BottomFieldFirst);

  // Supported output pixel formats
  TLAVOutPixFmts = (
   LAVOutPixFmt_None,
   LAVOutPixFmt_YV12,            // 4:2:0, 8bit, planar
   LAVOutPixFmt_NV12,            // 4:2:0, 8bit, Y planar, U/V packed
   LAVOutPixFmt_YUY2,            // 4:2:2, 8bit, packed
   LAVOutPixFmt_UYVY,            // 4:2:2, 8bit, packed
   LAVOutPixFmt_AYUV,            // 4:4:4, 8bit, packed
   LAVOutPixFmt_P010,            // 4:2:0, 10bit, Y planar, U/V packed
   LAVOutPixFmt_P210,            // 4:2:2, 10bit, Y planar, U/V packed
   LAVOutPixFmt_Y410,            // 4:4:4, 10bit, packed
   LAVOutPixFmt_P016,            // 4:2:0, 16bit, Y planar, U/V packed
   LAVOutPixFmt_P216,            // 4:2:2, 16bit, Y planar, U/V packed
   LAVOutPixFmt_Y416,            // 4:4:4, 16bit, packed
   LAVOutPixFmt_RGB32,           // 32-bit RGB (BGRA)
   LAVOutPixFmt_RGB24,           // 24-bit RGB (BGR)

   LAVOutPixFmt_v210,            // 4:2:2, 10bit, packed
   LAVOutPixFmt_v410,            // 4:4:4, 10bit, packed

   LAVOutPixFmt_YV16,            // 4:2:2, 8-bit, planar
   LAVOutPixFmt_YV24,            // 4:4:4, 8-bit, planar
   LAVOutPixFmt_RGB48,           // 48-bit RGB (16-bit per pixel, BGR)

   LAVOutPixFmt_NB);               // Number of formats

  TLAVDitherMode = (
   LAVDither_Ordered,
   LAVDither_Random);

  // LAV Video configuration interface
  ILAVVideoSettings = interface(IUnknown)
    ['{FA40D6E9-4D38-4761-ADD2-71A9EC5FD32F}']

   // Switch to Runtime Config mode. This will reset all settings to default, and no changes to the settings will be saved
   // You can use this to programmatically configure LAV Video without interfering with the users settings in the registry.
   // Subsequent calls to this function will reset all settings back to defaults, even if the mode does not change.
   //
   // Note that calling this function during playback is not supported and may exhibit undocumented behaviour.
   // For smooth operations, it must be called before LAV Video is connected to other filters.
   Procedure SetRuntimeConfig(bRuntimeConfig : boolean); stdcall;

   // Configure which codecs are enabled
   // If vCodec is invalid (possibly a version difference), Get will return FALSE, and Set E_FAIL.
   Function GetFormatConfiguration(vCodec : TLAVVideoCodec): boolean ; stdcall;
   Procedure SetFormatConfiguration(vCodec : TLAVVideoCodec; bEnabled : boolean); stdcall;

   // Set the number of threads to use for Multi-Threaded decoding (where available)
   //  0 = Auto Detect (based on number of CPU cores)
   //  1 = 1 Thread -- No Multi-Threading
   // >1 = Multi-Threading with the specified number of threads
   Procedure SetNumThreads(dwNum : DWORD); stdcall;

   // Get the number of threads to use for Multi-Threaded decoding (where available)
   //  0 = Auto Detect (based on number of CPU cores)
   //  1 = 1 Thread -- No Multi-Threading
   // >1 = Multi-Threading with the specified number of threads
   Function GetNumThreads(): DWord; stdcall;

   // Set whether the aspect ratio encoded in the stream should be forwarded to the renderer,
   // or the aspect ratio specified by the source filter should be kept.
   // TRUE  = AR from the Stream
   // FALSE = AR from the source filter
   Procedure SetStreamAR(bStreamAR : boolean); stdcall;

   // Get whether the aspect ratio encoded in the stream should be forwarded to the renderer,
   // or the aspect ratio specified by the source filter should be kept.
   // TRUE  = AR from the Stream
   // FALSE = AR from the source filter
   Function GetStreamAR(): boolean; stdcall;

   // Configure which pixel formats are enabled for output
   // If pixFmt is invalid, Get will return FALSE and Set E_FAIL
   Function GetPixelFormat(pixFmt : TLAVOutPixFmts): boolean; stdcall;
   Procedure SetPixelFormat(pixFmt : TLAVOutPixFmts; bEnabled : boolean); stdcall;

   // Set the RGB output range for the YUV->RGB conversion
   // 0 = Auto (same as input), 1 = Limited (16-235), 2 = Full (0-255)
   Procedure SetRGBOutputRange(dwRange : DWORD); stdcall;

   // Get the RGB output range for the YUV->RGB conversion
   // 0 = Auto (same as input), 1 = Limited (16-235), 2 = Full (0-255)
   Function GetRGBOutputRange(): DWord; stdcall;

   // Set the deinterlacing field order of the hardware decoder
   Procedure SetDeintFieldOrder(fieldOrder : TLAVDeintFieldOrder); stdcall;

   // get the deinterlacing field order of the hardware decoder
   Function GetDeintFieldOrder(): TLAVDeintFieldOrder ; stdcall;

   // DEPRECATED, use SetDeinterlacingMode
   Procedure SetDeintAggressive(bAggressive : boolean); stdcall;

   // DEPRECATED, use GetDeinterlacingMode
   Function GetDeintAggressive(): boolean ; stdcall;

   // DEPRECATED, use SetDeinterlacingMode
   Procedure SetDeintForce(bForce : boolean); stdcall;

   // DEPRECATED, use GetDeinterlacingMode
   Function GetDeintForce(): boolean ; stdcall;

   // Check if the specified HWAccel is supported
   // Note: This will usually only check the availability of the required libraries (ie. for NVIDIA if a recent enough NVIDIA driver is installed)
   // and not check actual hardware support
   // Returns: 0 = Unsupported, 1 = Supported, 2 = Currently running
   Function CheckHWAccelSupport(hwAccel : TLAVHWAccel): DWord ; stdcall;

   // Set which HW Accel method is used
   // See LAVHWAccel for options.
   Procedure SetHWAccel(hwAccel : TLAVHWAccel); stdcall;

   // Get which HW Accel method is active
   Function GetHWAccel(): boolean; stdcall;

   // Set which codecs should use HW Acceleration
   Procedure SetHWAccelCodec(hwAccelCodec : TLAVVideoHWCodec; bEnabled : boolean); stdcall;

   // Get which codecs should use HW Acceleration
   Function GetHWAccelCodec(hwAccelCodec : TLAVVideoHWCodec): boolean; stdcall;

   // Set the deinterlacing mode used by the hardware decoder
   Procedure SetHWAccelDeintMode(deintMode : TLAVHWDeintModes); stdcall;

   // Get the deinterlacing mode used by the hardware decoder
   Function GetHWAccelDeintMode(): TLAVHWDeintModes; stdcall;

   // Set the deinterlacing output for the hardware decoder
   Procedure SetHWAccelDeintOutput(deintOutput : TLAVDeintOutput); stdcall;

   // Get the deinterlacing output for the hardware decoder
   Function GetHWAccelDeintOutput(): TLAVDeintOutput; stdcall;

   // deprecated. HQ deint is always used when available depending on platform and codec

   Procedure SetHWAccelDeintHQ(bHQ : boolean); stdcall;

   Function GetHWAccelDeintHQ(): boolean; stdcall;

   // Set the software deinterlacing mode used
   Procedure SetSWDeintMode(deintMode : TLAVSWDeintModes); stdcall;

   // Get the software deinterlacing mode used
   Function GetSWDeintMode(): boolean; stdcall;

   // Set the software deinterlacing output
   Procedure SetSWDeintOutput(deintOutput : TLAVDeintOutput); stdcall;

   // Get the software deinterlacing output
   Function GetSWDeintOutput(): TLAVDeintOutput; stdcall;

   // DEPRECATED, use SetDeinterlacingMode
   Procedure SetDeintTreatAsProgressive(bEnabled : boolean); stdcall;

   // DEPRECATED, use GetDeinterlacingMode
   Function GetDeintTreatAsProgressive(): boolean ; stdcall;

   // Set the dithering mode used
   Procedure SetDitherMode(ditherMode : TLAVDitherMode); stdcall;

   // Get the dithering mode used
   Function GetDitherMode(): TLAVDitherMode; stdcall;

   // Set if the MS WMV9 DMO Decoder should be used for VC-1/WMV3
   Procedure SetUseMSWMV9Decoder(bEnabled : boolean); stdcall;

   // Get if the MS WMV9 DMO Decoder should be used for VC-1/WMV3
   Function GetUseMSWMV9Decoder(): boolean ; stdcall;

   // Set if DVD Video support is enabled
   Procedure SetDVDVideoSupport(bEnabled : boolean); stdcall;

   // Get if DVD Video support is enabled
   Function GetDVDVideoSupport(): boolean ; stdcall;

  // Set the HW Accel Resolution Flags
  // flags: bitmask of LAVHWResFlag flags
   Procedure SetHWAccelResolutionFlags(dwResFlags : DWORD); stdcall;
   Function GetHWAccelResolutionFlags(): DWORD; stdcall;

   // Toggle Tray Icon
   Procedure SetTrayIcon(bEnabled : boolean); stdcall;
   Function GetTrayIcon(): boolean ; stdcall;

   // Set the Deint Mode
   Procedure SetDeinterlacingMode(deintMode : LAVDeintMode); stdcall;
   Function GetDeinterlacingMode(): LAVDeintMode ; stdcall;

   // Set the index of the GPU to be used for hardware decoding
   // Only supported for CUVID and DXVA2 copy-back. If the device is not valid, it'll fallback to auto-detection
   // Must be called before an input is connected to LAV Video, and the setting is non-persistent
   // NOTE: For CUVID, the index defines the index of the CUDA capable device, while for DXVA2, the list includes all D3D9 devices
   Procedure SetGPUDeviceIndex(dwDevice : DWORD); stdcall;

   // Get the number of available devices for the specified HWAccel
   Function GetHWAccelNumDevices(hwAccel : TLAVHWAccel): DWord ; stdcall;

   // Get a list of available HWAccel devices for the specified HWAccel
   Procedure GetHWAccelDeviceInfo(hwAccel : TLAVHWAccel; dwIndex : DWord; out pstrDeviceName : PWideChar; out pdwDeviceIdentifier : PWord); stdcall;

   // Get/Set the device for a specified HWAccel
   // In contrast to SetGPUDeviceIndex, this setting is hwaccel-specific and persistent
   // dwDeviceIdentifier is an optional parameter that identifies the selected device (ie. its device id), set to 0 if not used
   Function GetHWAccelDeviceIndex(hwAccel : TLAVHWAccel; out pdwDeviceIdentifier : PWord): DWord ; stdcall;
   Procedure SetHWAccelDeviceIndex(hwAccel : TLAVHWAccel; dwIndex : Dword; dwDeviceIdentifier : DWord); stdcall;

   // Temporary Override for players to disable H.264 MVC decoding
   // This is not a permanent setting and not saved, but can be used by players to offer a "Play in 2D" option, or similar.
   // A setting of FALSE disable MVC decoding temporarily
   // Note that the override cannot force-enable the option if its turned off through SetFormatConfiguration
   Procedure SetH264MVCDecodingOverride(bEnabled : Bool); stdcall;

   //  Enable the creation of the Closed Caption output pin
   Procedure SetEnableCCOutputPin(bEnabled : Bool); stdcall;

  end;

implementation

end.

