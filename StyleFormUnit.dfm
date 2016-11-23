object StyleForm: TStyleForm
  Left = 330
  Top = 211
  AutoSize = True
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'SSA/ASS Style Editor'
  ClientHeight = 377
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 192
    Top = 96
    Width = 209
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Horizontal'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel2: TTntLabel
    Left = 408
    Top = 96
    Width = 177
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Vertical'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel3: TTntLabel
    Left = 192
    Top = 216
    Width = 393
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Colors'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel4: TTntLabel
    Left = 192
    Top = 32
    Width = 393
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Font'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel5: TTntLabel
    Left = 192
    Top = 288
    Width = 89
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Outline'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel6: TTntLabel
    Left = 192
    Top = 232
    Width = 40
    Height = 13
    Caption = 'Primary :'
  end
  object TntLabel7: TTntLabel
    Left = 272
    Top = 232
    Width = 57
    Height = 13
    Caption = 'Secondary :'
  end
  object TntLabel8: TTntLabel
    Left = 352
    Top = 232
    Width = 41
    Height = 13
    Caption = 'Tertiary :'
  end
  object TntLabel9: TTntLabel
    Left = 432
    Top = 232
    Width = 31
    Height = 13
    Caption = 'Back :'
  end
  object TntLabel10: TTntLabel
    Left = 192
    Top = 4
    Width = 58
    Height = 13
    Caption = 'Style name :'
  end
  object TntLabel13: TTntLabel
    Left = 288
    Top = 288
    Width = 89
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Shadow'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel14: TTntLabel
    Left = 384
    Top = 288
    Width = 97
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Spacing*'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel15: TTntLabel
    Left = 488
    Top = 288
    Width = 97
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Angle*'
    Color = clWindowText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel11: TTntLabel
    Left = 192
    Top = 328
    Width = 169
    Height = 13
    AutoSize = False
    Caption = '* available with ASS format only'
  end
  object lstStyles: TTntListBox
    Left = 0
    Top = 0
    Width = 185
    Height = 345
    ItemHeight = 13
    TabOrder = 0
    OnClick = lstStylesClick
  end
  object stFontPreview: TTntStaticText
    Left = 192
    Top = 48
    Width = 313
    Height = 41
    AutoSize = False
    BevelKind = bkSoft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object bttFont: TTntButton
    Left = 512
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 2
    OnClick = bttFontClick
  end
  object rgVAlignment: TTntRadioGroup
    Left = 408
    Top = 112
    Width = 177
    Height = 41
    Caption = ' Alignment : '
    Columns = 3
    Items.Strings = (
      'Top'
      'Middle'
      'Bottom')
    TabOrder = 3
    OnClick = checkChangedSender
  end
  object TntGroupBox2: TTntGroupBox
    Left = 192
    Top = 160
    Width = 65
    Height = 49
    Caption = ' Margin L : '
    TabOrder = 4
    object edLMargin: TTntEdit
      Left = 8
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object rgHAlignment: TTntRadioGroup
    Left = 192
    Top = 112
    Width = 209
    Height = 41
    Caption = ' Alignment : '
    Columns = 3
    Items.Strings = (
      'Left'
      'Center'
      'Right')
    TabOrder = 5
    OnClick = checkChangedSender
  end
  object TntGroupBox1: TTntGroupBox
    Left = 408
    Top = 160
    Width = 81
    Height = 49
    Caption = ' Margin V : '
    TabOrder = 6
    object edVMargin: TTntEdit
      Left = 8
      Top = 20
      Width = 65
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object edOutline: TTntEdit
    Left = 192
    Top = 304
    Width = 89
    Height = 21
    TabOrder = 7
    OnChange = checkChangedSender
  end
  object bttNew: TTntButton
    Left = 0
    Top = 352
    Width = 41
    Height = 25
    Caption = 'New'
    TabOrder = 8
    OnClick = bttNewClick
  end
  object bttDelete: TTntButton
    Left = 96
    Top = 352
    Width = 41
    Height = 25
    Caption = 'Delete'
    TabOrder = 9
    OnClick = bttDeleteClick
  end
  object bttClose: TTntButton
    Left = 512
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 10
    OnClick = bttCloseClick
  end
  object pnlPrimaryColor: TTntStaticText
    Left = 192
    Top = 248
    Width = 73
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 11
    Transparent = False
    OnClick = pnlColorClick
  end
  object pnlSecondaryColor: TTntStaticText
    Left = 272
    Top = 248
    Width = 73
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 12
    Transparent = False
    OnClick = pnlColorClick
  end
  object pnlOutlineColor: TTntStaticText
    Left = 352
    Top = 248
    Width = 73
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 13
    Transparent = False
    OnClick = pnlColorClick
  end
  object pnlBackColor: TTntStaticText
    Left = 432
    Top = 248
    Width = 73
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 14
    Transparent = False
    OnClick = pnlColorClick
  end
  object bttCopy: TTntButton
    Left = 48
    Top = 352
    Width = 41
    Height = 25
    Caption = 'Copy'
    TabOrder = 15
    OnClick = bttCopyClick
  end
  object edStyleName: TTntEdit
    Left = 264
    Top = 0
    Width = 321
    Height = 21
    TabOrder = 16
    OnChange = checkChangedSender
  end
  object edShadow: TTntEdit
    Left = 288
    Top = 304
    Width = 89
    Height = 21
    TabOrder = 17
    OnChange = checkChangedSender
  end
  object bttApply: TTntButton
    Left = 192
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 18
    OnClick = bttApplyClick
  end
  object bttReset: TTntButton
    Left = 272
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 19
    OnClick = bttResetClick
  end
  object gbScaleX: TTntGroupBox
    Left = 336
    Top = 160
    Width = 65
    Height = 49
    Caption = ' Scale X* : '
    TabOrder = 20
    object edScaleX: TTntEdit
      Left = 8
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object gbScaleY: TTntGroupBox
    Left = 504
    Top = 160
    Width = 81
    Height = 49
    Caption = ' Scale Y* : '
    TabOrder = 21
    object edScaleY: TTntEdit
      Left = 8
      Top = 20
      Width = 65
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object edSpacing: TTntEdit
    Left = 384
    Top = 304
    Width = 97
    Height = 21
    TabOrder = 22
    OnChange = checkChangedSender
  end
  object edAngle: TTntEdit
    Left = 488
    Top = 304
    Width = 97
    Height = 21
    TabOrder = 23
    OnChange = checkChangedSender
  end
  object bttHValue: TTntButton
    Left = 536
    Top = 232
    Width = 49
    Height = 17
    Hint = 'Hue'
    Caption = 'H=0'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 24
    OnClick = bttHSLButtonClick
  end
  object bttSValue: TTntButton
    Left = 536
    Top = 248
    Width = 49
    Height = 17
    Hint = 'Saturation'
    Caption = 'S=0'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 25
    OnClick = bttHSLButtonClick
  end
  object bttLValue: TTntButton
    Left = 536
    Top = 264
    Width = 49
    Height = 17
    Hint = 'Lightness'
    Caption = 'L=0'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 26
    OnClick = bttHSLButtonClick
  end
  object TntGroupBox3: TTntGroupBox
    Left = 264
    Top = 160
    Width = 65
    Height = 49
    Caption = ' Margin R : '
    TabOrder = 27
    object edRMargin: TTntEdit
      Left = 8
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdScalableOnly]
    Left = 8
    Top = 280
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen]
    Left = 8
    Top = 312
  end
end
