object FindForm: TFindForm
  Left = 253
  Top = 448
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Find'
  ClientHeight = 185
  ClientWidth = 339
  Color = clBtnFace
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 0
    Top = 4
    Width = 23
    Height = 13
    Caption = 'Find:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object TntLabel2: TTntLabel
    Left = 0
    Top = 28
    Width = 65
    Height = 13
    Caption = 'Replace with:'
  end
  object cbFind: TTntComboBox
    Left = 72
    Top = 0
    Width = 265
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbFindChange
    OnKeyDown = cbFindKeyDown
  end
  object cbReplaceWith: TTntComboBox
    Left = 72
    Top = 24
    Width = 265
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnKeyDown = cbFindKeyDown
  end
  object TntGroupBox1: TTntGroupBox
    Left = 0
    Top = 56
    Width = 153
    Height = 129
    Caption = 'Options'
    TabOrder = 2
    object chkMatchCase: TTntCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
      OnClick = chkMatchCaseClick
    end
    object chkFromCursor: TCheckBox
      Left = 8
      Top = 40
      Width = 81
      Height = 17
      Caption = 'From cursor'
      TabOrder = 1
    end
    object chkMatchingOnly: TTntCheckBox
      Left = 8
      Top = 91
      Width = 137
      Height = 17
      Caption = 'Show matching sub only'
      TabOrder = 4
      OnClick = chkMatchingOnlyClick
    end
    object chkRegExp: TTntCheckBox
      Left = 8
      Top = 72
      Width = 121
      Height = 17
      Caption = 'Regular Expression'
      TabOrder = 3
      OnClick = chkRegExpClick
    end
    object chkWholeWord: TCheckBox
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Whole Word'
      TabOrder = 2
    end
    object chkDisableVOSearch: TTntCheckBox
      Left = 7
      Top = 109
      Width = 137
      Height = 17
      Caption = 'Disable VO search'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkMatchingOnlyClick
    end
  end
  object Panel1: TPanel
    Left = 160
    Top = 56
    Width = 177
    Height = 73
    BevelOuter = bvNone
    TabOrder = 3
    object bttFind: TTntButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Find'
      TabOrder = 0
      OnClick = bttFindClick
    end
    object bttReplaceFind: TTntButton
      Left = 96
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Replace/Find'
      Enabled = False
      TabOrder = 1
      OnClick = bttReplaceFindClick
    end
    object bttReplace: TTntButton
      Left = 8
      Top = 38
      Width = 81
      Height = 25
      Caption = 'Replace'
      Enabled = False
      TabOrder = 2
      OnClick = bttReplaceClick
    end
    object bttReplaceAll: TTntButton
      Left = 96
      Top = 38
      Width = 81
      Height = 25
      Caption = 'Replace All'
      TabOrder = 3
      OnClick = bttReplaceAllClick
    end
  end
  object bttClose: TTntButton
    Left = 256
    Top = 160
    Width = 83
    Height = 25
    Caption = 'Close'
    TabOrder = 4
    OnClick = bttCloseClick
  end
end
