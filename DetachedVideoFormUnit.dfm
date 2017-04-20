object DetachedVideoForm: TDetachedVideoForm
  Left = 487
  Top = 263
  BorderStyle = bsNone
  Caption = 'Video'
  ClientHeight = 240
  ClientWidth = 320
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PopupMenu = VideoPopupMenu
  Position = poDefaultPosOnly
  OnDblClick = FormDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object VideoPopupMenu: TTntPopupMenu
    OnPopup = VideoPopupMenuPopup
    Left = 32
    Top = 8
    object pmiVideoFullscreen: TTntMenuItem
      Caption = 'Fullscreen'
      GroupIndex = 1
      RadioItem = True
      OnClick = pmiVideoFullscreenClick
    end
    object pmiVideoNormalSize: TTntMenuItem
      Caption = 'Normal size'
      GroupIndex = 1
      RadioItem = True
      OnClick = pmiVideoNormalSizeClick
    end
  end
end
