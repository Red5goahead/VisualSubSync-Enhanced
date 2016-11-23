unit DirectVobSubInterface;

interface

uses DirectShow9, ActiveX, Windows;

const
  CLSID_DirectVobSubFilter: TGuid = '{93A22E7A-5091-45ef-BA61-6DA26156A5D0}';
  CLSID_DirectVobSubFilterAuto: TGuid = '{9852A670-F845-491B-9BE6-EBD841B8A613}';
  IID_IIDirectVobSub: TGuid = '{EBE1FB08-3957-47ca-AF13-5827E5442E56}';

type
  IDirectVobSub = interface(IUnknown)
    ['{EBE1FB08-3957-47ca-AF13-5827E5442E56}']
    function get_FileName(out fn: PWideChar): HResult; stdcall;
    function put_FileName(fn: PWideChar): HResult; stdcall;

    function get_LanguageCount(out nCount: integer): HResult; stdcall;
    function get_LanguageName(iSelected: integer; out ppName: PWideChar): HResult;
      stdcall;
    function get_SelectedLanguage(out iSelected: integer): HResult; stdcall;
    function put_SelectedLanguages(iSelected: integer): HResult; stdcall;

    function get_HideSubtitles(out Showed: BOOL): HResult; stdcall;
    function put_HideSubtitles(Showed: BOOL): HResult; stdcall;

    function get_PreBuffering(out Prebuffered: BOOL): HResult; stdcall;
    function put_PreBuffering(Prebuffered: BOOL): HResult; stdcall;

    function get_Placement(out fOverrideplacement: BOOL;
      out xperc: integer; out yperc: integer): HResult; stdcall;
    function put_Placement(fOverrideplacement: BOOL; xperc: integer;
      yperc: integer): HResult; stdcall;

    function get_VobSubSettings(out fBuffer: BOOL;
      out fOnlyShowForcedSubs: BOOL; out fPolygonize: BOOL): HResult; stdcall;
    function put_VobSubSettings(fBuffer: BOOL; fOnlyShowForcedSubs: BOOL;
      fPolygonize: BOOL): HResult; stdcall;

    function get_TextSettings(out lf: LOGFONT; lflen: integer;
      out color: COLORREF; out fShadow: BOOL; out fOutline: BOOL;
      out fAdvancedRenderer: BOOL): HResult; stdcall;
    function get_TextSettings2(out lf: LOGFONT; lflen: integer;
      out color1: COLORREF; out color2: COLORREF; out color3: COLORREF; out color4: COLORREF;
      out alpha1: COLORREF; out alpha2: COLORREF; out alpha3: COLORREF; out alpha4: COLORREF;
      out shadowDepthX : Integer; out shadowDepthY : Integer; out outlineWidthX : Integer; out outlineWidthY : Integer;
      out borderStyle : Integer): HResult; stdcall;
    function put_TextSettings(out lf: LOGFONT; lflen: integer;
      color: COLORREF; fShadow: BOOL; fOutline: BOOL;
      out fAdvancedRenderer: BOOL): HResult; stdcall;
    function put_TextSettings2(out lf: LOGFONT; lflen: integer;
      color1: COLORREF; color2: COLORREF; color3: COLORREF; color4: COLORREF;
      alpha1: COLORREF; alpha2: COLORREF; alpha3: COLORREF; alpha4: COLORREF;
      shadowDepthX : Integer; shadowDepthY : Integer; outlineWidthX : Integer; outlineWidthY : Integer;
      borderStyle  : Integer): HResult; stdcall;

    function get_Flip(out fPicture : BOOL; out fSubtitles: BOOL): HResult; stdcall;
    function put_Flip(fPicture : BOOL; fSubtitles: BOOL): HResult; stdcall;

    function get_OSD(out fOSD : BOOL): HResult; stdcall;
    function put_OSD(fOSD : BOOL): HResult; stdcall;

    function get_SaveFullPath(out fSaveFullPath : BOOL): HResult; stdcall;
    function put_SaveFullPath(fSaveFullPath : BOOL): HResult; stdcall;

    function get_SubtitleTiming(out delay : integer; out speedmul : integer;
      out speeddiv : integer): HResult; stdcall;
    function put_SubtitleTiming(delay : integer; speedmul : integer;
      speeddiv : integer): HResult; stdcall;

    function get_MediaFPS(out fEnabled : BOOL; out fps : double): HResult; stdcall;
    function put_MediaFPS(fEnabled : BOOL; fps : double): HResult; stdcall;

		// no longer supported
    function get_ColorFormat(out iPosition : Integer): HResult; stdcall;
    function put_ColorFormat(iPosition : Integer): HResult; stdcall;
		//

    function get_ZoomRect(out rect : NORMALIZEDRECT): HResult; stdcall;
    function put_ZoomRect(rect : PNORMALIZEDRECT): HResult; stdcall;

    function UpdateRegistry(): HResult; stdcall;

    function HasConfigDialog(iSelected : Integer): HResult; stdcall;
    function ShowConfigDialog(iSelected : Integer; hWndParent: HWND): HResult; stdcall;

    function IsSubtitleReloaderLocked(out fLocked : BOOL): HResult; stdcall;
    function LockSubtitleReloader(fLocked : BOOL): HResult; stdcall;
    function get_SubtitleReloader(out fDisabled : BOOL): HResult; stdcall;
    function put_SubtitleReloader(fDisabled : BOOL): HResult; stdcall;

    function get_ExtendPicture(out horizontal : integer; out vertical : integer;
      out resx2 : integer; out resx2minw : integer;
      out resx2minh : integer): HResult; stdcall;
    function put_ExtendPicture(horizontal : integer; vertical : integer;
      resx2 : integer; resx2minw : integer;
      resx2minh : integer): HResult; stdcall;

    function get_LoadSettings(out level : integer; out fExternalLoad : BOOL;
      out fWebLoad : BOOL; out fEmbeddedLoad : BOOL): HResult; stdcall;
    function put_LoadSettings(level : integer; fExternalLoad : BOOL;
      fWebLoad : BOOL; fEmbeddedLoad : BOOL): HResult; stdcall;

    function get_SubPictToBuffer(out uSubPictToBuffer : Integer): HResult; stdcall;
    function put_SubPictToBuffer(uSubPictToBuffer : Integer): HResult; stdcall;

    function get_AnimWhenBuffering(out fAnimWhenBuffering : Boolean): HResult; stdcall;
    function put_AnimWhenBuffering(fAnimWhenBuffering : Boolean): HResult; stdcall;

  end;

implementation

end.

