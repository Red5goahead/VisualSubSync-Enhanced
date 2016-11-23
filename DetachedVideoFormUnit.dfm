object DetachedVideoForm: TDetachedVideoForm
  Left = 333
  Top = 232
  BorderStyle = bsNone
  Caption = 'Video'
  ClientHeight = 240
  ClientWidth = 320
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PopupMenu = VideoPopupMenu
  Position = poDefaultPosOnly
  OnDblClick = FormDblClick
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object VideoPopupMenu: TTntPopupMenu
    OnPopup = VideoPopupMenuPopup
    Left = 8
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
