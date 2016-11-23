object SilentZoneForm: TSilentZoneForm
  Left = 246
  Top = 77
  Width = 314
  Height = 331
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = 'Silent zones'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object TntBevel1: TTntBevel
    Left = 0
    Top = 49
    Width = 290
    Height = 4
    Align = alTop
    Shape = bsSpacer
  end
  object lbSilentZones: TTntListBox
    Left = 0
    Top = 53
    Width = 290
    Height = 234
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbSilentZonesDblClick
  end
  object TntGroupBox1: TTntGroupBox
    Left = 0
    Top = 0
    Width = 290
    Height = 49
    Align = alTop
    Caption = ' Settings '
    TabOrder = 1
    DesignSize = (
      290
      49)
    object TntLabel1: TTntLabel
      Left = 8
      Top = 20
      Width = 50
      Height = 13
      Caption = 'Threshold:'
    end
    object TntLabel2: TTntLabel
      Left = 128
      Top = 20
      Width = 43
      Height = 13
      Caption = 'Duration:'
    end
    object edThreshold: TTntEdit
      Left = 64
      Top = 16
      Width = 41
      Height = 21
      Hint = 'Threshold between 0 and 32767'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '100'
    end
    object edDuration: TTntEdit
      Left = 176
      Top = 16
      Width = 41
      Height = 21
      Hint = 'Minimum duration of the silent zone in ms (100..9999)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '500'
    end
    object udDuration: TTntUpDown
      Left = 217
      Top = 16
      Width = 15
      Height = 21
      Associate = edDuration
      Min = 100
      Max = 9999
      Increment = 100
      Position = 500
      TabOrder = 2
    end
    object udThreshold: TTntUpDown
      Left = 105
      Top = 16
      Width = 15
      Height = 21
      Associate = edThreshold
      Max = 32767
      Increment = 10
      Position = 100
      TabOrder = 3
    end
    object bttUpdate: TTntButton
      Left = 240
      Top = 16
      Width = 47
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update'
      TabOrder = 4
      OnClick = bttUpdateClick
    end
  end
end
