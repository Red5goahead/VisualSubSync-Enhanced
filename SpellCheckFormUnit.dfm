object SpellCheckForm: TSpellCheckForm
  Left = 410
  Top = 185
  Width = 480
  Height = 360
  BorderWidth = 4
  Caption = 'Spell check'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TntBevel1: TTntBevel
    Left = 376
    Top = 0
    Width = 5
    Height = 313
    Align = alRight
    Shape = bsSpacer
  end
  object TntPanel1: TTntPanel
    Left = 381
    Top = 0
    Width = 75
    Height = 313
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object bttReplace: TTntButton
      Left = 0
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Replace'
      TabOrder = 0
      OnClick = bttReplaceClick
    end
    object bttReplaceAll: TTntButton
      Left = 0
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Replace all'
      TabOrder = 1
      OnClick = bttReplaceAllClick
    end
    object bttIgnore: TTntButton
      Left = 0
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Ignore'
      TabOrder = 2
      OnClick = bttIgnoreClick
    end
    object bttIgnoreAll: TTntButton
      Left = 0
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Ignore all'
      TabOrder = 3
      OnClick = bttIgnoreAllClick
    end
    object bttAdd: TTntButton
      Left = 0
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 4
      OnClick = bttAddClick
    end
    object TntPanel3: TTntPanel
      Left = 0
      Top = 286
      Width = 75
      Height = 27
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      DesignSize = (
        75
        27)
      object bttCancel: TTntButton
        Left = 0
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Close'
        TabOrder = 0
        OnClick = bttCancelClick
      end
    end
    object bttReset: TTntButton
      Left = 0
      Top = 248
      Width = 75
      Height = 25
      Caption = 'Go to First'
      TabOrder = 6
      OnClick = bttResetClick
    end
  end
  object TntPanel2: TTntPanel
    Left = 0
    Top = 0
    Width = 376
    Height = 313
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object TntLabel1: TTntLabel
      Left = 0
      Top = 29
      Width = 376
      Height = 13
      Align = alTop
      Caption = 'Text :'
    end
    object TntLabel3: TTntLabel
      Left = 0
      Top = 111
      Width = 376
      Height = 13
      Align = alTop
      Caption = 'Replace by : '
    end
    object TntLabel2: TTntLabel
      Left = 0
      Top = 155
      Width = 376
      Height = 13
      Align = alTop
      Caption = 'Suggestions :'
    end
    object lblSub: TTntLabel
      Left = 0
      Top = 0
      Width = 376
      Height = 13
      Align = alTop
      Caption = 'Subtitle 0/0 : 00:00:00.000 -> 00:00:00.000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TntBevel2: TTntBevel
      Left = 0
      Top = 13
      Width = 376
      Height = 3
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel3: TTntBevel
      Left = 0
      Top = 42
      Width = 376
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel4: TTntBevel
      Left = 0
      Top = 105
      Width = 376
      Height = 6
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel5: TTntBevel
      Left = 0
      Top = 124
      Width = 376
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel6: TTntBevel
      Left = 0
      Top = 149
      Width = 376
      Height = 6
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel7: TTntBevel
      Left = 0
      Top = 168
      Width = 376
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel8: TTntBevel
      Left = 0
      Top = 26
      Width = 376
      Height = 3
      Align = alTop
      Shape = bsSpacer
    end
    object reSubtitleText: TTntRichEdit
      Left = 0
      Top = 46
      Width = 376
      Height = 59
      TabStop = False
      Align = alTop
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object edReplaceBy: TTntEdit
      Left = 0
      Top = 128
      Width = 376
      Height = 21
      Align = alTop
      TabOrder = 1
    end
    object lbSuggestions: TTntListBox
      Left = 0
      Top = 172
      Width = 376
      Height = 141
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Dialog'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 2
      OnClick = lbSuggestionsClick
    end
    object pbSubs: TTntProgressBar
      Left = 0
      Top = 16
      Width = 376
      Height = 10
      Align = alTop
      TabOrder = 3
    end
  end
end
