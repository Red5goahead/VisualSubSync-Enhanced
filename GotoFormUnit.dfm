object GotoForm: TGotoForm
  Left = 187
  Top = 257
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Go to'
  ClientHeight = 121
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 89
    BevelOuter = bvLowered
    TabOrder = 0
    object rbGotoLine: TTntRadioButton
      Left = 16
      Top = 18
      Width = 57
      Height = 17
      Caption = 'Line :'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbGotoTime: TTntRadioButton
      Left = 16
      Top = 50
      Width = 57
      Height = 17
      Caption = 'Time :'
      TabOrder = 2
    end
    object EditLine: TEdit
      Left = 80
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '1'
      OnChange = EditLineChange
    end
    object MaskEditTime: TMaskEdit
      Left = 80
      Top = 48
      Width = 120
      Height = 21
      EditMask = '!99:99:99.999;1;0'
      MaxLength = 12
      TabOrder = 3
      Text = '  :  :  .   '
      OnChange = MaskEditTimeChange
    end
  end
  object bttGo: TTntButton
    Left = 16
    Top = 96
    Width = 89
    Height = 25
    Caption = 'Go'
    TabOrder = 1
    OnClick = bttGoClick
  end
  object bttCancel: TTntButton
    Left = 120
    Top = 96
    Width = 83
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = bttCancelClick
  end
end
