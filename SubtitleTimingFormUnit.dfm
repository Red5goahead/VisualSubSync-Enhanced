object SubtitleTimingForm: TSubtitleTimingForm
  Left = 433
  Top = 338
  ActiveControl = edTime
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Change subtitle timing...'
  ClientHeight = 85
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object bttClose: TTntButton
    Left = 158
    Top = 48
    Width = 83
    Height = 25
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
    OnClick = bttCloseClick
  end
  object brrOk: TTntButton
    Left = 62
    Top = 48
    Width = 83
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object edTime: TMaskEdit
    Left = 103
    Top = 9
    Width = 91
    Height = 24
    EditMask = '!99:99:99.999;1;0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    MaxLength = 12
    ParentFont = False
    TabOrder = 0
    Text = '  :  :  .   '
  end
end
