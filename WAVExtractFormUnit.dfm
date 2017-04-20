object ExtractWAVForm: TExtractWAVForm
  Left = 332
  Top = 220
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Extract WAV'
  ClientHeight = 504
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel3: TTntLabel
    Left = 0
    Top = 437
    Width = 47
    Height = 13
    Caption = 'Progress :'
  end
  object TntGroupBox1: TTntGroupBox
    Left = 0
    Top = 0
    Width = 481
    Height = 291
    Caption = ' Video/Audio info : '
    TabOrder = 0
    object MemoVideoInfo: TTntMemo
      Left = 8
      Top = 15
      Width = 464
      Height = 267
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object bttExtract: TTntButton
    Left = 27
    Top = 471
    Width = 75
    Height = 25
    Caption = 'Extract'
    TabOrder = 1
    OnClick = bttExtractClick
  end
  object bttClose: TTntButton
    Left = 379
    Top = 471
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = bttCloseClick
  end
  object ProgressBar1: TProgressBar
    Left = 56
    Top = 431
    Width = 423
    Height = 25
    Max = 1000
    Smooth = True
    TabOrder = 3
  end
  object gbSettings: TTntGroupBox
    Left = 0
    Top = 306
    Width = 481
    Height = 116
    Caption = ' Settings : '
    TabOrder = 4
    object TntLabelStreamToExtract: TTntLabel
      Left = 8
      Top = 24
      Width = 86
      Height = 13
      Caption = 'Stream to extract :'
      Visible = False
    end
    object cbStreamIndex: TComboBox
      Left = 104
      Top = 20
      Width = 369
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Visible = False
    end
    object rbFastConversion: TRadioButton
      Tag = 2
      Left = 8
      Top = 88
      Width = 401
      Height = 17
      Caption = 'Convert to reduce file size (Mono + Sample rate divided by 2)'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      OnClick = rbOnlyPeakClick
    end
    object rbNoConversion: TRadioButton
      Tag = 1
      Left = 8
      Top = 68
      Width = 113
      Height = 17
      Caption = 'Simple extraction'
      TabOrder = 2
      OnClick = rbOnlyPeakClick
    end
    object rbOnlyPeak: TRadioButton
      Left = 8
      Top = 48
      Width = 137
      Height = 17
      Caption = 'Only create peak file'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = rbOnlyPeakClick
    end
  end
  object bttStop: TTntButton
    Left = 107
    Top = 471
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = bttStopClick
  end
  object bttDebug: TTntButton
    Left = 235
    Top = 471
    Width = 75
    Height = 25
    Caption = 'Debug'
    TabOrder = 6
    OnClick = bttDebugClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 368
    Top = 24
  end
end
